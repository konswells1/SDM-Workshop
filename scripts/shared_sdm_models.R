

#######################
# Shared SDM model algorithms

#| label: glm-multi
# Fit logistic regression with multiple predictors
glm_multi_1 <- glm(pa ~ elevation + bio_1 + bio_12,
                   data = sdm_df_clean,
                   family = binomial)

# Alternatively, fit logistic regression with multiple predictors as polynomial terms
glm_multi <- glm(pa ~ poly(elevation, 2) + poly(bio_1, 2) + poly(bio_12, 2),
                 data = sdm_df_clean,
                 family = binomial)


#| label: glm-diagnostics
#| # Perform stepwise variable selection (forward/backward/both based on AIC)
glm_step <- step(glm_multi, direction = "both", trace = FALSE)


#| label: data_split
#| # Fit model on training data
glm_multi_train <- glm(pa ~ poly(elevation, 2) + poly(bio_1, 2) + poly(bio_12, 2),
                       data = sdm_df_train,
                       family = binomial)

glm_step_train <- step(glm_multi_train , direction = "both", trace = FALSE)


#| label: auc-eval
# Predict on test data
glm_predict_test <- predict(glm_step_train , newdata = sdm_df_test, type = "response")

#| label: projection_current
# Predict relative suitability across the raster stack
glm_suitability_map <- terra::predict(proj_stack, glm_step, type = "response")


# True observed data (presence/pseudoabsence)
obs_test <- sdm_df_test$pa

# Evaluate with ROC and AUC on test data
roc_glm <- pROC::roc(obs_test, glm_predict_test)
auc_glm <- pROC::auc(roc_glm)


#| label: gam-fitting
# Fit GAM with smooth terms for selected predictors
gam_model <- gam(pa ~ s(bio_1) + s(bio_12) + s(elevation), 
data = sdm_df_clean, 
family = binomial(link = "logit"))


#| label: gam-evaluation
# Fit GAM to training data only
gam_model_train <- gam(pa ~ s(bio_1) + s(bio_12) + s(elevation), 
data = sdm_df_train, 
family = binomial(link = "logit"))

# Predict on test data
gam_predict_test <- predict(gam_model_train, newdata = sdm_df_test, type = "response")

# Evaluate on test data
roc_gam <- pROC::roc(sdm_df_test$pa, gam_predict_test)
auc_gam <- pROC::auc(roc_gam)

#| label: brt-fit
# Data needs to in dataframe format for dismo::gbm.step()
sdm_df_clean <- as.data.frame(sdm_df_clean)

brt_model <- gbm.step(data = sdm_df_clean,
                      gbm.x = c("bio_1", "bio_12", "elevation"),
                      gbm.y = "pa",
                      family = "bernoulli",
                      tree.complexity = 3,
                      learning.rate = 0.01,
                      bag.fraction = 0.5,
                      verbose = FALSE)


#| label: brt-eval
#| message: false

# Data needs to in dataframe format for dismo::gbm.step()
sdm_df_train <- as.data.frame(sdm_df_train)

# Fit BRT to training data only
brt_model_train <- gbm.step(data = sdm_df_train,
                            gbm.x = c("bio_1", "bio_12", "elevation"),
                            gbm.y = "pa",
                            family = "bernoulli",
                            tree.complexity = 3,
                            learning.rate = 0.01,
                            bag.fraction = 0.5,
                            verbose = FALSE)

# Predict on test data
brt_predict_test <- predict(brt_model_train, 
                            newdata= sdm_df_test, n.trees = brt_model_train$gbm.call$best.trees, type = "response")

roc_brt <- pROC::roc(sdm_df_test$pa, brt_predict_test)
auc_brt <- pROC::auc(roc_brt )


#| label: rF-fit
# Make sure 'pa' is a factor for classification
sdm_df_clean$pa <- as.factor(sdm_df_clean$pa)

# Fit the model
rF_model <- randomForest(pa ~ bio_1 + bio_12 + elevation,
                         data = sdm_df_clean,
                         ntree = 500,
                         importance = TRUE)

#| label: rF-eval
# Make sure 'pa' is a factor for classification
sdm_df_train$pa <- as.factor(sdm_df_train$pa)
sdm_df_test$pa <- as.factor(sdm_df_test$pa)

# # Fit randomForest to training data only
rF_model_train <- randomForest(pa ~ bio_1 + bio_12 + elevation,
                               data = sdm_df_train,
                               ntree = 500,
                               importance = TRUE)

rF_predict_test <- predict(rF_model_train, newdata= sdm_df_test, type = "prob")[, "1"]

roc_rF <- pROC::roc(sdm_df_test$pa, rF_predict_test)
auc_rF <- pROC::auc(roc_rF)


















