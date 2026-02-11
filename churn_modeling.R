# ==============================================================================
# CHURN PREDICTIVE MODELING
# ==============================================================================
#
# This script builds and evaluates churn prediction models using:
# 1. Logistic Regression (baseline)
# 2. Random Forest (primary model)
# 3. Gradient Boosting (XGBoost)
# 4. Model comparison and ensemble
#
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(pROC)
library(ggplot2)

# Install if needed
# install.packages(c("xgboost", "e1071", "ROSE"))
library(xgboost)

set.seed(123)

# Load data (from previous script)
# churn_data <- readRDS("churn_modeling_data.rds")

# For this example, assuming churn_data exists from previous script
cat("=== CHURN PREDICTIVE MODELING ===\n\n")
cat("Starting model development...\n\n")

# ------------------------------------------------------------------------------
# 2. PREPARE MODELING DATASET
# ------------------------------------------------------------------------------

cat("=== DATA PREPARATION ===\n\n")

# Select features for modeling
model_data <- churn_modeling_data %>%
  select(
    # Target
    Churned,
    
    # Subscription features
    ProductType,
    Channel,
    MonthlyPrice,
    TenureMonths,
    IsWinback,
    IsHighValue,
    
    # Engagement features
    TotalMinutesViewed,
    TotalSessions,
    AvgMinutesPerSession,
    UniqueDaysViewed,
    DaysSinceLastView,
    ViewingFrequency,
    
    # Content preference features
    PctSportsViewing,
    PctEntertainmentViewing,
    
    # Derived features
    IsHighEngagement,
    LowEngagementFlag,
    InactiveFlag,
    
    # Risk score
    ChurnRiskScore
  ) %>%
  # Convert categorical to factors
  mutate(
    Churned = factor(Churned, levels = c(0, 1), labels = c("Retained", "Churned")),
    ProductType = factor(ProductType),
    Channel = factor(Channel),
    IsWinback = factor(IsWinback),
    IsHighValue = factor(IsHighValue),
    IsHighEngagement = factor(IsHighEngagement),
    LowEngagementFlag = factor(LowEngagementFlag),
    InactiveFlag = factor(InactiveFlag)
  ) %>%
  # Remove rows with missing values
  na.omit()

cat("Modeling dataset prepared:\n")
cat("  Total records:", nrow(model_data), "\n")
cat("  Features:", ncol(model_data) - 1, "\n")
cat("  Churn cases:", sum(model_data$Churned == "Churned"), "\n")
cat("  Churn rate:", round(mean(model_data$Churned == "Churned") * 100, 2), "%\n\n")

# Check class imbalance
class_distribution <- table(model_data$Churned)
cat("Class Distribution:\n")
print(class_distribution)
cat("\n")

# ------------------------------------------------------------------------------
# 3. TRAIN-TEST SPLIT
# ------------------------------------------------------------------------------

cat("=== TRAIN-TEST SPLIT ===\n\n")

# Create stratified split (70-30)
train_index <- createDataPartition(
  model_data$Churned, 
  p = 0.7, 
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("Training set:", nrow(train_data), "records\n")
cat("Test set:", nrow(test_data), "records\n")
cat("Training churn rate:", 
    round(mean(train_data$Churned == "Churned") * 100, 2), "%\n")
cat("Test churn rate:", 
    round(mean(test_data$Churned == "Churned") * 100, 2), "%\n\n")

# ------------------------------------------------------------------------------
# 4. MODEL 1: LOGISTIC REGRESSION (BASELINE)
# ------------------------------------------------------------------------------

cat("=== MODEL 1: LOGISTIC REGRESSION ===\n\n")

# Train logistic regression
logit_model <- glm(
  Churned ~ .,
  data = train_data,
  family = binomial()
)

# Predictions
logit_pred_prob <- predict(logit_model, test_data, type = "response")
logit_pred_class <- factor(
  ifelse(logit_pred_prob > 0.5, "Churned", "Retained"),
  levels = c("Retained", "Churned")
)

# Confusion Matrix
logit_cm <- confusionMatrix(logit_pred_class, test_data$Churned, positive = "Churned")

cat("Logistic Regression Results:\n")
cat("  Accuracy:", round(logit_cm$overall["Accuracy"], 4), "\n")
cat("  Precision:", round(logit_cm$byClass["Precision"], 4), "\n")
cat("  Recall (Sensitivity):", round(logit_cm$byClass["Sensitivity"], 4), "\n")
cat("  F1-Score:", round(logit_cm$byClass["F1"], 4), "\n")

# ROC-AUC
logit_roc <- roc(test_data$Churned, logit_pred_prob)
cat("  AUC:", round(auc(logit_roc), 4), "\n\n")

# Feature importance (coefficients)
logit_coef <- summary(logit_model)$coefficients
logit_coef_sorted <- logit_coef[order(abs(logit_coef[, "z value"]), decreasing = TRUE), ]
cat("Top 10 Most Important Features (Logistic Regression):\n")
print(head(logit_coef_sorted, 10))
cat("\n")

# ------------------------------------------------------------------------------
# 5. MODEL 2: RANDOM FOREST (PRIMARY MODEL)
# ------------------------------------------------------------------------------

cat("=== MODEL 2: RANDOM FOREST ===\n\n")

# Train Random Forest with tuning
rf_model <- randomForest(
  Churned ~ .,
  data = train_data,
  ntree = 500,
  mtry = sqrt(ncol(train_data) - 1),
  importance = TRUE,
  proximity = FALSE
)

# Predictions
rf_pred_class <- predict(rf_model, test_data, type = "class")
rf_pred_prob <- predict(rf_model, test_data, type = "prob")[, "Churned"]

# Confusion Matrix
rf_cm <- confusionMatrix(rf_pred_class, test_data$Churned, positive = "Churned")

cat("Random Forest Results:\n")
cat("  Accuracy:", round(rf_cm$overall["Accuracy"], 4), "\n")
cat("  Precision:", round(rf_cm$byClass["Precision"], 4), "\n")
cat("  Recall (Sensitivity):", round(rf_cm$byClass["Sensitivity"], 4), "\n")
cat("  F1-Score:", round(rf_cm$byClass["F1"], 4), "\n")

# ROC-AUC
rf_roc <- roc(test_data$Churned, rf_pred_prob)
cat("  AUC:", round(auc(rf_roc), 4), "\n\n")

# Feature importance
rf_importance <- importance(rf_model)
rf_importance_df <- data.frame(
  Feature = rownames(rf_importance),
  MeanDecreaseAccuracy = rf_importance[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = rf_importance[, "MeanDecreaseGini"]
) %>%
  arrange(desc(MeanDecreaseGini))

cat("Top 10 Most Important Features (Random Forest):\n")
print(head(rf_importance_df, 10))
cat("\n")

# ------------------------------------------------------------------------------
# 6. MODEL 3: XGBOOST
# ------------------------------------------------------------------------------

cat("=== MODEL 3: XGBOOST ===\n\n")

# Prepare data for XGBoost (needs numeric matrix)
train_matrix <- model.matrix(Churned ~ . - 1, data = train_data)
test_matrix <- model.matrix(Churned ~ . - 1, data = test_data)

train_label <- as.numeric(train_data$Churned) - 1
test_label <- as.numeric(test_data$Churned) - 1

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

# Train XGBoost
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_model <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  verbose = 0,
  early_stopping_rounds = 10
)

# Predictions
xgb_pred_prob <- predict(xgb_model, dtest)
xgb_pred_class <- factor(
  ifelse(xgb_pred_prob > 0.5, "Churned", "Retained"),
  levels = c("Retained", "Churned")
)

# Confusion Matrix
xgb_cm <- confusionMatrix(xgb_pred_class, test_data$Churned, positive = "Churned")

cat("XGBoost Results:\n")
cat("  Accuracy:", round(xgb_cm$overall["Accuracy"], 4), "\n")
cat("  Precision:", round(xgb_cm$byClass["Precision"], 4), "\n")
cat("  Recall (Sensitivity):", round(xgb_cm$byClass["Sensitivity"], 4), "\n")
cat("  F1-Score:", round(xgb_cm$byClass["F1"], 4), "\n")

# ROC-AUC
xgb_roc <- roc(test_data$Churned, xgb_pred_prob)
cat("  AUC:", round(auc(xgb_roc), 4), "\n\n")

# Feature importance
xgb_importance <- xgb.importance(
  feature_names = colnames(train_matrix),
  model = xgb_model
)

cat("Top 10 Most Important Features (XGBoost):\n")
print(head(xgb_importance, 10))
cat("\n")

# ------------------------------------------------------------------------------
# 7. MODEL COMPARISON
# ------------------------------------------------------------------------------

cat("=== MODEL COMPARISON ===\n\n")

model_comparison <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "XGBoost"),
  Accuracy = c(
    logit_cm$overall["Accuracy"],
    rf_cm$overall["Accuracy"],
    xgb_cm$overall["Accuracy"]
  ),
  Precision = c(
    logit_cm$byClass["Precision"],
    rf_cm$byClass["Precision"],
    xgb_cm$byClass["Precision"]
  ),
  Recall = c(
    logit_cm$byClass["Sensitivity"],
    rf_cm$byClass["Sensitivity"],
    xgb_cm$byClass["Sensitivity"]
  ),
  F1_Score = c(
    logit_cm$byClass["F1"],
    rf_cm$byClass["F1"],
    xgb_cm$byClass["F1"]
  ),
  AUC = c(
    auc(logit_roc),
    auc(rf_roc),
    auc(xgb_roc)
  )
) %>%
  mutate(across(where(is.numeric), round, 4))

print(model_comparison)
cat("\n")

# Select best model
best_model_idx <- which.max(model_comparison$AUC)
best_model_name <- model_comparison$Model[best_model_idx]
cat("Best performing model:", best_model_name, "\n")
cat("Best AUC:", model_comparison$AUC[best_model_idx], "\n\n")

# ------------------------------------------------------------------------------
# 8. THRESHOLD OPTIMIZATION
# ------------------------------------------------------------------------------

cat("=== THRESHOLD OPTIMIZATION ===\n\n")

# Use Random Forest for threshold tuning (typically best model)
# Find optimal threshold based on F1 score
thresholds <- seq(0.1, 0.9, by = 0.05)
threshold_metrics <- data.frame()

for (thresh in thresholds) {
  pred_class <- factor(
    ifelse(rf_pred_prob > thresh, "Churned", "Retained"),
    levels = c("Retained", "Churned")
  )
  
  cm <- confusionMatrix(pred_class, test_data$Churned, positive = "Churned")
  
  threshold_metrics <- rbind(threshold_metrics, data.frame(
    Threshold = thresh,
    Precision = cm$byClass["Precision"],
    Recall = cm$byClass["Sensitivity"],
    F1_Score = cm$byClass["F1"]
  ))
}

# Find optimal threshold
optimal_threshold <- threshold_metrics$Threshold[which.max(threshold_metrics$F1_Score)]
cat("Optimal threshold for F1-Score:", optimal_threshold, "\n")
cat("F1-Score at optimal threshold:", 
    round(max(threshold_metrics$F1_Score, na.rm = TRUE), 4), "\n\n")

# ------------------------------------------------------------------------------
# 9. PREDICTION PROBABILITIES ANALYSIS
# ------------------------------------------------------------------------------

cat("=== PREDICTION PROBABILITIES ===\n\n")

# Create risk segments based on predicted probability
predictions_df <- data.frame(
  CustomerID = churn_modeling_data[-train_index, ]$CustomerID,
  ActualChurn = test_data$Churned,
  PredictedProb = rf_pred_prob,
  PredictedClass = rf_pred_class
) %>%
  mutate(
    RiskSegment = case_when(
      PredictedProb >= 0.75 ~ "Very High Risk",
      PredictedProb >= 0.50 ~ "High Risk",
      PredictedProb >= 0.25 ~ "Medium Risk",
      TRUE ~ "Low Risk"
    )
  )

# Analyze risk segments
risk_segment_analysis <- predictions_df %>%
  group_by(RiskSegment) %>%
  summarise(
    Customers = n(),
    ActualChurned = sum(ActualChurn == "Churned"),
    ActualChurnRate = mean(ActualChurn == "Churned") * 100,
    AvgPredictedProb = mean(PredictedProb) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(AvgPredictedProb))

cat("Risk Segment Analysis:\n")
print(risk_segment_analysis)
cat("\n")

# ------------------------------------------------------------------------------
# 10. MODEL INTERPRETATION - FEATURE EFFECTS
# ------------------------------------------------------------------------------

cat("=== KEY FEATURE EFFECTS ===\n\n")

# Analyze key features from Random Forest
top_features <- head(rf_importance_df$Feature, 5)

feature_effects <- data.frame()

for (feature in top_features) {
  if (is.numeric(train_data[[feature]])) {
    # For numeric features, compare means
    effect <- train_data %>%
      group_by(Churned) %>%
      summarise(
        Mean = mean(.data[[feature]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(names_from = Churned, values_from = Mean) %>%
      mutate(
        Feature = feature,
        Difference = Churned - Retained,
        PctDifference = (Difference / Retained) * 100
      )
    
    feature_effects <- rbind(feature_effects, effect)
  }
}

cat("Feature Effects (Mean Differences):\n")
print(feature_effects)
cat("\n")

# ------------------------------------------------------------------------------
# 11. BUSINESS IMPACT CALCULATION
# ------------------------------------------------------------------------------

cat("=== BUSINESS IMPACT CALCULATION ===\n\n")

# Assume average monthly price and customer lifetime
avg_monthly_price <- mean(churn_modeling_data$MonthlyPrice)
avg_tenure_months <- mean(churn_modeling_data$TenureMonths)
avg_customer_lifetime_value <- avg_monthly_price * avg_tenure_months

# Calculate potential savings from model
high_risk_customers <- sum(predictions_df$RiskSegment %in% c("Very High Risk", "High Risk"))
assumed_intervention_success_rate <- 0.20  # 20% of at-risk customers retained

potential_customers_saved <- high_risk_customers * assumed_intervention_success_rate
potential_revenue_saved <- potential_customers_saved * avg_customer_lifetime_value

cat("Business Impact (Test Set):\n")
cat("  High-risk customers identified:", high_risk_customers, "\n")
cat("  Potential customers saved (20% success):", round(potential_customers_saved), "\n")
cat("  Average customer lifetime value: $", round(avg_customer_lifetime_value, 2), "\n")
cat("  Potential revenue saved: $", 
    format(round(potential_revenue_saved, 2), big.mark = ","), "\n\n")

# ------------------------------------------------------------------------------
# 12. MODEL DEPLOYMENT OUTPUTS
# ------------------------------------------------------------------------------

cat("=== SAVING MODEL OUTPUTS ===\n\n")

# Save the best model
saveRDS(rf_model, "churn_rf_model.rds")
cat("✓ Model saved: churn_rf_model.rds\n")

# Save predictions
write.csv(predictions_df, "churn_predictions.csv", row.names = FALSE)
cat("✓ Predictions saved: churn_predictions.csv\n")

# Save feature importance
write.csv(rf_importance_df, "feature_importance.csv", row.names = FALSE)
cat("✓ Feature importance saved: feature_importance.csv\n")

# Save model comparison
write.csv(model_comparison, "model_comparison.csv", row.names = FALSE)
cat("✓ Model comparison saved: model_comparison.csv\n\n")

# ------------------------------------------------------------------------------
# 13. RECOMMENDATIONS FOR ACTION
# ------------------------------------------------------------------------------

cat("=== ACTIONABLE RECOMMENDATIONS ===\n\n")

cat("1. IMMEDIATE INTERVENTIONS:\n")
cat("   - Target 'Very High Risk' customers (", 
    sum(predictions_df$RiskSegment == "Very High Risk"), 
    " customers) with retention offers\n")
cat("   - Focus on customers with low engagement (< 30 days viewing)\n")
cat("   - Prioritize high-value customers with declining usage\n\n")

cat("2. PRODUCT IMPROVEMENTS:\n")
if (nrow(churn_by_product) > 0) {
  cat("   - Review ", churn_by_product$ProductType[1], 
      " product (highest churn at ", round(churn_by_product$ChurnRate[1], 1), "%)\n")
}
cat("   - Enhance content recommendations for inactive users\n")
cat("   - Improve viewing experience on high-churn channels\n\n")

cat("3. ENGAGEMENT STRATEGIES:\n")
cat("   - Send re-engagement campaigns to users inactive >14 days\n")
cat("   - Personalized content suggestions based on viewing patterns\n")
cat("   - Create winback campaigns with special offers\n\n")

cat("4. MONITORING:\n")
cat("   - Set up weekly scoring of all active customers\n")
cat("   - Monitor risk segment transitions\n")
cat("   - A/B test retention interventions\n")
cat("   - Track model performance drift monthly\n\n")

cat("=== CHURN MODELING COMPLETE ===\n")
cat("Models trained, evaluated, and saved successfully!\n")
