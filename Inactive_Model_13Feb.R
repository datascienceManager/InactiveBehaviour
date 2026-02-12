# ==============================================================================
# CHURN PREDICTIVE MODELING 
# ==============================================================================
#
# ==============================================================================

library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(ggplot2)

set.seed(123)

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║            CHURN PREDICTIVE MODELING - CUSTOM            ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ------------------------------------------------------------------------------
# 1. PREPARE MODELING DATASET
# ------------------------------------------------------------------------------

cat("Step 1: Preparing modeling dataset...\n")
cat("─────────────────────────────────────────────────────────────\n")

# Select features for modeling based 
model_data <- churn_data %>%
  select(
    # Target
    Churned,
    
    # Product features
    Product_name,
    ProductTier,
    OfferPeriodStd,
    Offer_type,
    IsPremium,
    IsEventPass,
    
    # Geographic features
    Country,
    Region,
    CountryTier,
    
    # Channel & Acquisition
    Dir_indir,
    D2C_B2B,
    AcquisitionChannelGroup,
    Acquisition_channel,
    Source_system,
    HasPartner,
    
    # Customer behavior
    IsNewSubscriber,
    IsWinback,
    IsContinue,
    Subscription_type,
    
    # Temporal features
    MonthsSinceStart,
    SubscriptionMonths,
    DaysUntilExpiry,
    TenureCategory,
    SubscriptionMonth,
    SubscriptionQuarter,
    
    # Payment features
    PaymentCategory,
    Payment_method,
    HasPromo,
    HasCoupon,
    HasCampaign,
    
    # Version & stability
    Subscription_version,
    HasMultipleVersions,
    IsLatestVersion,
    StabilityScore,
    
    # Grace period
    InGracePeriod,
    InGracePeriod90,
    
    # Risk indicators
    ShortTenureFlag,
    ExpiringSoonFlag,
    
    # Value tier
    Tier,
    ValueTier,
    
    # Risk score (from analysis)
    ChurnRiskScore
  ) %>%
  # Convert categorical to factors
  mutate(
    Churned = factor(Churned, levels = c(0, 1), labels = c("Active", "Churned")),
    across(where(is.character), as.factor),
    across(c(IsPremium, IsEventPass, IsNewSubscriber, IsWinback, IsContinue,
             HasPartner, HasPromo, HasCoupon, HasCampaign, HasMultipleVersions,
             IsLatestVersion, InGracePeriod, InGracePeriod90, ShortTenureFlag,
             ExpiringSoonFlag), as.factor)
  ) %>%
  # Remove rows with missing values in key features
  na.omit()

cat("✓ Modeling dataset prepared\n")
cat("  Total records:", nrow(model_data), "\n")
cat("  Features:", ncol(model_data) - 1, "\n")
cat("  Churn cases:", sum(model_data$Churned == "Churned"), "\n")
cat("  Churn rate:", round(mean(model_data$Churned == "Churned") * 100, 2), "%\n\n")

# Class distribution
class_table <- table(model_data$Churned)
cat("Class Distribution:\n")
print(class_table)
cat("\n")

# ------------------------------------------------------------------------------
# 2. TRAIN-TEST SPLIT
# ------------------------------------------------------------------------------

cat("Step 2: Creating train-test split...\n")
cat("─────────────────────────────────────────────────────────────\n")

# Stratified split (70-30)
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
# 3. MODEL 1: LOGISTIC REGRESSION
# ------------------------------------------------------------------------------

cat("Step 3: Building Logistic Regression model...\n")
cat("─────────────────────────────────────────────────────────────\n")

# Select numeric and key categorical features for logistic regression
logit_features <- c(
  "MonthsSinceStart", "SubscriptionMonths", "DaysUntilExpiry",
  "StabilityScore", "ChurnRiskScore", "Subscription_version",
  "Product_name", "Country", "OfferPeriodStd", "AcquisitionChannelGroup",
  "IsNewSubscriber", "IsWinback", "HasPromo", "InGracePeriod",
  "IsPremium", "IsEventPass", "ValueTier"
)

logit_data_train <- train_data %>% select(Churned, all_of(logit_features))
logit_data_test <- test_data %>% select(Churned, all_of(logit_features))

# Train model
logit_model <- glm(
  Churned ~ .,
  data = logit_data_train,
  family = binomial()
)

# Predictions
logit_pred_prob <- predict(logit_model, logit_data_test, type = "response")
logit_pred_class <- factor(
  ifelse(logit_pred_prob > 0.5, "Churned", "Active"),
  levels = c("Active", "Churned")
)

# Evaluation
logit_cm <- confusionMatrix(logit_pred_class, test_data$Churned, positive = "Churned")
logit_roc <- roc(test_data$Churned, logit_pred_prob)

cat("Logistic Regression Results:\n")
cat("  Accuracy:", round(logit_cm$overall["Accuracy"], 4), "\n")
cat("  Precision:", round(logit_cm$byClass["Precision"], 4), "\n")
cat("  Recall:", round(logit_cm$byClass["Sensitivity"], 4), "\n")
cat("  F1-Score:", round(logit_cm$byClass["F1"], 4), "\n")
cat("  AUC:", round(auc(logit_roc), 4), "\n\n")

# Feature importance (top 10 coefficients by z-value)
logit_coef <- summary(logit_model)$coefficients
logit_importance <- data.frame(
  Feature = rownames(logit_coef),
  Coefficient = logit_coef[, "Estimate"],
  Z_value = abs(logit_coef[, "z value"]),
  P_value = logit_coef[, "Pr(>|z|)"]
) %>%
  arrange(desc(Z_value)) %>%
  head(10)

cat("Top 10 Important Features (Logistic Regression):\n")
print(logit_importance)
cat("\n")

# ------------------------------------------------------------------------------
# 4. MODEL 2: RANDOM FOREST
# ------------------------------------------------------------------------------

cat("Step 4: Building Random Forest model...\n")
cat("─────────────────────────────────────────────────────────────\n")

# Train Random Forest
rf_model <- randomForest(
  Churned ~ .,
  data = train_data,
  ntree = 500,
  mtry = sqrt(ncol(train_data) - 1),
  importance = TRUE,
  proximity = FALSE
)

cat("Random Forest trained:\n")
cat("  Trees:", rf_model$ntree, "\n")
cat("  OOB Error Rate:", round(mean(rf_model$err.rate[, "OOB"]) * 100, 2), "%\n\n")

# Predictions
rf_pred_class <- predict(rf_model, test_data, type = "class")
rf_pred_prob <- predict(rf_model, test_data, type = "prob")[, "Churned"]

# Evaluation
rf_cm <- confusionMatrix(rf_pred_class, test_data$Churned, positive = "Churned")
rf_roc <- roc(test_data$Churned, rf_pred_prob)

cat("Random Forest Results:\n")
cat("  Accuracy:", round(rf_cm$overall["Accuracy"], 4), "\n")
cat("  Precision:", round(rf_cm$byClass["Precision"], 4), "\n")
cat("  Recall:", round(rf_cm$byClass["Sensitivity"], 4), "\n")
cat("  F1-Score:", round(rf_cm$byClass["F1"], 4), "\n")
cat("  AUC:", round(auc(rf_roc), 4), "\n\n")

# Feature importance
rf_importance <- importance(rf_model)
rf_importance_df <- data.frame(
  Feature = rownames(rf_importance),
  MeanDecreaseAccuracy = rf_importance[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = rf_importance[, "MeanDecreaseGini"]
) %>%
  arrange(desc(MeanDecreaseGini)) %>%
  head(20)

cat("Top 20 Important Features (Random Forest):\n")
print(rf_importance_df)
cat("\n")

# ------------------------------------------------------------------------------
# 5. MODEL COMPARISON
# ------------------------------------------------------------------------------

cat("Step 5: Model Comparison...\n")
cat("─────────────────────────────────────────────────────────────\n")

model_comparison <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  Accuracy = c(
    logit_cm$overall["Accuracy"],
    rf_cm$overall["Accuracy"]
  ),
  Precision = c(
    logit_cm$byClass["Precision"],
    rf_cm$byClass["Precision"]
  ),
  Recall = c(
    logit_cm$byClass["Sensitivity"],
    rf_cm$byClass["Sensitivity"]
  ),
  F1_Score = c(
    logit_cm$byClass["F1"],
    rf_cm$byClass["F1"]
  ),
  AUC = c(
    auc(logit_roc),
    auc(rf_roc)
  )
) %>%
  mutate(across(where(is.numeric), round, 4))

print(model_comparison)
cat("\n")

best_model_idx <- which.max(model_comparison$AUC)
best_model_name <- model_comparison$Model[best_model_idx]
cat("✓ Best Model:", best_model_name, "\n")
cat("✓ Best AUC:", model_comparison$AUC[best_model_idx], "\n\n")

# ------------------------------------------------------------------------------
# 6. PREDICTIONS AND RISK SEGMENTATION
# ------------------------------------------------------------------------------

cat("Step 6: Generating predictions and risk segments...\n")
cat("─────────────────────────────────────────────────────────────\n")

# Create predictions dataframe
predictions_df <- data.frame(
  Customer_ID = churn_data[-train_index, ]$Customer_ID,
  Subscription_ID = churn_data[-train_index, ]$Subscription_ID,
  Product_name = churn_data[-train_index, ]$Product_name,
  Country = churn_data[-train_index, ]$Country,
  ActualStatus = test_data$Churned,
  PredictedProb = rf_pred_prob,
  PredictedClass = rf_pred_class
) %>%
  mutate(
    RiskSegment = case_when(
      PredictedProb >= 0.75 ~ "Very High Risk",
      PredictedProb >= 0.50 ~ "High Risk",
      PredictedProb >= 0.25 ~ "Medium Risk",
      TRUE ~ "Low Risk"
    ),
    RiskScore = round(PredictedProb * 100, 1)
  )

# Risk segment analysis
risk_segment_summary <- predictions_df %>%
  group_by(RiskSegment) %>%
  summarise(
    Subscriptions = n(),
    ActualChurned = sum(ActualStatus == "Churned"),
    ActualChurnRate = mean(ActualStatus == "Churned") * 100,
    AvgPredictedProb = mean(PredictedProb) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(AvgPredictedProb))

cat("Risk Segment Analysis:\n")
print(risk_segment_summary)
cat("\n")

# By product and risk
product_risk <- predictions_df %>%
  group_by(Product_name, RiskSegment) %>%
  summarise(
    Count = n(),
    ActualChurnRate = mean(ActualStatus == "Churned") * 100,
    .groups = "drop"
  ) %>%
  arrange(Product_name, desc(ActualChurnRate))

cat("Risk Distribution by Product:\n")
print(product_risk)
cat("\n")

# By country and risk
country_risk <- predictions_df %>%
  group_by(Country, RiskSegment) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = RiskSegment, values_from = Count, values_fill = 0)

cat("Risk Distribution by Country:\n")
print(country_risk)
cat("\n")

# ------------------------------------------------------------------------------
# 7. BUSINESS IMPACT CALCULATION
# ------------------------------------------------------------------------------

cat("Step 7: Business Impact Analysis...\n")
cat("─────────────────────────────────────────────────────────────\n")

# High-risk customers
high_risk_count <- sum(predictions_df$RiskSegment %in% c("Very High Risk", "High Risk"))
very_high_risk_count <- sum(predictions_df$RiskSegment == "Very High Risk")

# Assuming intervention success rates
intervention_rates <- data.frame(
  RiskSegment = c("Very High Risk", "High Risk", "Medium Risk"),
  InterventionCost = c(50, 30, 15),  # Cost per customer
  SuccessRate = c(0.25, 0.20, 0.15)  # Percentage saved
)

# Calculate potential impact
impact_by_segment <- predictions_df %>%
  filter(RiskSegment != "Low Risk") %>%
  group_by(RiskSegment) %>%
  summarise(
    AtRiskCustomers = n(),
    .groups = "drop"
  ) %>%
  left_join(intervention_rates, by = "RiskSegment") %>%
  mutate(
    PotentialSaved = round(AtRiskCustomers * SuccessRate),
    TotalInterventionCost = AtRiskCustomers * InterventionCost
  )

cat("Business Impact Estimation:\n")
print(impact_by_segment)
cat("\n")

total_intervention_cost <- sum(impact_by_segment$TotalInterventionCost)
total_saved <- sum(impact_by_segment$PotentialSaved)

cat("Summary:\n")
cat("  High-risk customers identified:", format(high_risk_count, big.mark = ","), "\n")
cat("  Potential customers saved:", format(total_saved, big.mark = ","), "\n")
cat("  Total intervention cost: $", format(total_intervention_cost, big.mark = ","), "\n\n")

# ------------------------------------------------------------------------------
# 8. SAVE OUTPUTS
# ------------------------------------------------------------------------------

cat("Step 8: Saving model outputs...\n")
cat("─────────────────────────────────────────────────────────────\n")

# Save model
saveRDS(rf_model, "  _rf_churn_model.rds")
cat("✓ Model saved:   _rf_churn_model.rds\n")

# Save predictions
write.csv(predictions_df, "  _churn_predictions.csv", row.names = FALSE)
cat("✓ Predictions saved:   _churn_predictions.csv\n")

# Save feature importance
write.csv(rf_importance_df, "  _feature_importance.csv", row.names = FALSE)
cat("✓ Feature importance saved:   _feature_importance.csv\n")

# Save model comparison
write.csv(model_comparison, "  _model_comparison.csv", row.names = FALSE)
cat("✓ Model comparison saved:   _model_comparison.csv\n\n")

# ------------------------------------------------------------------------------
# 9. ACTIONABLE RECOMMENDATIONS
# ------------------------------------------------------------------------------

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║           ACTIONABLE RECOMMENDATIONS -                    ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("IMMEDIATE ACTIONS:\n")
cat("─────────────────\n")
cat("1. Target", very_high_risk_count, "very high-risk subscriptions\n")
cat("   - Focus on AFCON pass holders (event-based, high churn)\n")
cat("   - Prioritize subscribers in first 30 days\n")
cat("   - Offer personalized retention incentives\n\n")

cat("2. Geographic Priority:\n")
highest_churn_country <- churn_by_country$Country[1]
cat("   - Focus on", highest_churn_country, "(highest churn rate)\n")
cat("   - Investigate country-specific issues\n")
cat("   - Localize retention campaigns\n\n")

cat("3. Product Optimization:\n")
highest_churn_product <- churn_by_product$Product_name[1]
cat("   - Review", highest_churn_product, "offering\n")
cat("   - Enhance content for Shows and 4K tiers\n")
cat("   - Bundle event passes with ongoing subscriptions\n\n")

cat("4. Winback Strategy:\n")
cat("   - Winback customers show", 
    round(winback_analysis$ChurnRate[winback_analysis$IsWinback == 1], 1),
    "% churn\n")
cat("   - Implement stronger onboarding for returning customers\n")
cat("   - Offer loyalty rewards for continued subscriptions\n\n")

cat("5. Payment & Billing:\n")
cat("   - Address payment failures (involuntary churn)\n")
cat("   - Implement retry logic for failed payments\n")
cat("   - Offer multiple payment options\n\n")

cat("MONITORING:\n")
cat("───────────\n")
cat("• Score all active subscriptions weekly\n")
cat("• Track intervention success rates by segment\n")
cat("• Monitor grace period effectiveness\n")
cat("• A/B test retention campaigns\n")
cat("• Retrain model monthly with new data\n\n")

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║                 CHURN MODELING COMPLETE!                  ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("Next: Create visualizations with churn_visualizations.R\n")

