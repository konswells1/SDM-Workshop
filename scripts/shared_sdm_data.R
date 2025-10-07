
##############
# scripts/shared_data_setup.R

# Load required packages
source("scripts/load_packages.R")

# Load saved data
load("data/workshop_data.RData")

if (!exists("Clim_UK")) {
  Clim_UK <- terra::rast("data/Clim_UK.tif")
}
if (!exists("Elev_UK")) {
  Elev_UK <- terra::rast("data/Elev_UK.tif")
}
if (!exists("Clim_cmip6_2041_2060_UK")) {
  Clim_cmip6_2041_2060_UK <- terra::rast("data/Clim_cmip6_2041_2060_UK.tif")
}


# Resample Elev_UK raster to match Clim_extant_UK_masked raster
Elev_UK_aligned <- terra::resample(Elev_UK, Clim_UK, method = "bilinear")

# Crop to common extent
common_extent <- terra::intersect(terra::ext(Clim_UK), terra::ext(Elev_UK_aligned))
Clim_UK_crop2 <- terra::crop(Clim_UK, common_extent)
Elev_UK_crop2 <- terra::crop(Elev_UK_aligned, common_extent)

# Stack layers and name variables
Env_UK_stack <- c(Clim_UK_crop2 , Elev_UK_crop2)
names(Env_UK_stack) <- c(sub("^wc2\\.1_10m_", "", names(Clim_UK_crop2)), "elevation")

env_stack <- Env_UK_stack

# Prepare environmental stack for projection
proj_stack <- Env_UK_stack[[c("bio_1", "bio_12", "elevation")]]

if (!exists("occ_rhinhipp_cleaned")) {
  
  # Recreate minimal cleaned object (this assumes occ_df_rhinhipp is already loaded)
  occ_unique_rhinhipp <- occ_df_rhinhipp %>% dplyr::distinct()
  
  occ_rhinhipp_flags <- CoordinateCleaner::clean_coordinates(
    occ_unique_rhinhipp,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    countries = "countryCode",
    tests = c("centroids", "duplicates", "equal", "gbif", "institutions", "zeros"),
    inst_rad = 10000
  )
  
  occ_rhinhipp_cleaned <- occ_unique_rhinhipp[occ_rhinhipp_flags$.summary, ]
}

presence_sf <- occ_rhinhipp_cleaned %>%
  # Select only the columns key, decimalLatitude, decimalLongitude
  dplyr::select(species, key, decimalLatitude, decimalLongitude) %>%
  # Remove records with missing coordinates
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%
  # convert to sf object
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# UK boundary (sf object)
study_area <- UK_sf

n_presence <- nrow(presence_sf)
n_background <- n_presence

# Generate random points within UK polygon using sf::st_sample
background_points <- sf::st_sample(study_area, size = n_background, type = "random")

# Convert to sf object with POINT geometry
background_sf <- st_sf(geometry = background_points)

# Assign an ID for plotting/analysis
background_sf <- background_sf %>%
  mutate(type = "background")

# Extract environmental values at presence points
env_pres <- terra::extract(env_stack, vect(presence_sf)) %>%
  dplyr::select(-ID)  # Remove ID column returned by extract

# Combine with presence points
presence_data <- presence_sf %>%
  mutate(pa = 1) %>%  # 1 = presence
  bind_cols(env_pres)

# Extract environmental values at background points
env_back <- terra::extract(env_stack, vect(background_sf)) %>%
  dplyr::select(-ID)

# Combine with background points
background_data <- background_sf %>%
  mutate(pa = 0) %>%  # 0 = pseudo-absence
  bind_cols(env_back)

# Combine presence + background into one dataset for modelling
sdm_data <- bind_rows(presence_data, background_data)

# Drop geometry for model matrix if needed
sdm_df <- st_drop_geometry(sdm_data)

# Drop rows with NA in covariates
sdm_df_clean <- sdm_df %>%
  drop_na(starts_with("bio_"), elevation)

#| label: data_split
set.seed(123)
# Generate an index that assign 70% of data to training data
trainIndex <- caret::createDataPartition(sdm_df_clean$pa, p = .7, list = FALSE)
sdm_df_train <- sdm_df_clean[trainIndex, ]
sdm_df_test <- sdm_df_clean[-trainIndex, ]

