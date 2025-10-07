

#| label: packages-install-load
#| message: false
#| warning: false

# Set CRAN mirror for non-interactive install
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of required packages
required_packages <- c(
  "terra", "geodata", "sf", "raster",
  "rnaturalearth", "rnaturalearthdata",
  "spocc", "rgbif", "ggplot2", "viridis", "tidyverse", "dplyr",
  "leaflet", "CoordinateCleaner", "effects",
  "ggcorrplot", "caret", "pROC", "PresenceAbsence", "ecospat",
  "mgcv", "dismo", "gbm", "randomForest", "maxnet",
  "biomod2", "sdm", "ENMeval"
)

# Identify missing packages
missing_packages <- required_packages[!(required_packages %in% rownames(installed.packages()))]

# Install missing packages with dependencies
if (length(missing_packages) > 0) {
  suppressWarnings(suppressMessages(
    install.packages(missing_packages, dependencies = TRUE, quiet = TRUE)
  ))
}

# Load all required packages
invisible(lapply(required_packages, function(pkg) {
  suppressWarnings(suppressMessages(
    library(pkg, character.only = TRUE)
  ))
}))


