# ==============================================================================
# OTT PLATFORM CHURN ANALYSIS
# ==============================================================================
# 
# Business Context:
# - Subscription-based OTT platform (4K, Entertainment, Sports, AFCON pass)
# - Monthly renewals with potential winbacks
# - Voluntary and involuntary churn
# - Multiple data sources: subscriptions and viewing behavior
#
# Objectives:
# 1. Understand churn patterns and drivers
# 2. Build predictive churn model
# 3. Create actionable insights for retention
#
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP AND LIBRARY LOADING
# ------------------------------------------------------------------------------

# Install packages if needed
# install.packages(c("dplyr", "tidyr", "lubridate", "ggplot2", "gt", 
#                    "randomForest", "caret", "pROC", "corrplot", "scales"))

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gt)

# For modeling
library(randomForest)
library(caret)
library(pROC)

# For visualization
library(corrplot)
library(scales)

# Set options
options(scipen = 999)  # Disable scientific notation
set.seed(123)  # For reproducibility

# ------------------------------------------------------------------------------
# 2. DATA LOADING AND UNDERSTANDING
# ------------------------------------------------------------------------------

# Example data structure - Replace with your actual data loading

# Subscription Data
subscriptions <- data.frame(
  SubscriptionID = 1:1000,
  CustomerID = paste0("C", 1:1000),
  ProductType = sample(c("4K", "Entertainment", "Sports", "AFCON Pass"), 1000, 
                       replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
  Channel = sample(c("Web", "Mobile", "TV", "Partner"), 1000, 
                   replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
  SubscriptionDate = seq(as.Date("2023-01-01"), as.Date("2024-12-31"), 
                         length.out = 1000),
  Cancellation = sample(c(0, 1), 1000, replace = TRUE, prob = c(0.75, 0.25)),
  TypeOfChurn = sample(c("None", "Voluntary", "Involuntary"), 1000, 
                       replace = TRUE, prob = c(0.75, 0.15, 0.10)),
  MonthlyPrice = sample(c(9.99, 14.99, 19.99, 24.99), 1000, replace = TRUE),
  IsWinback = sample(c(0, 1), 1000, replace = TRUE, prob = c(0.9, 0.1)),
  TenureMonths = sample(1:36, 1000, replace = TRUE)
)

# Viewing Data
viewing <- data.frame(
  ViewingID = 1:5000,
  CustomerID = sample(paste0("C", 1:1000), 5000, replace = TRUE),
  MinutesViewed = rpois(5000, lambda = 120),
  AssetID = paste0("A", sample(1:500, 5000, replace = TRUE)),
  TypeOfAsset = sample(c("Sports", "Entertainment", "Movies", "Series"), 5000,
                       replace = TRUE, prob = c(0.2, 0.3, 0.25, 0.25)),
  ViewingDate = sample(seq(as.Date("2023-01-01"), as.Date("2024-12-31"), 
                           by = "day"), 5000, replace = TRUE)
)

# Print data overview
cat("=== DATA OVERVIEW ===\n\n")
cat("Subscriptions Data:\n")
cat("Total Subscriptions:", nrow(subscriptions), "\n")
cat("Date Range:", min(subscriptions$SubscriptionDate), "to", 
    max(subscriptions$SubscriptionDate), "\n\n")

cat("Viewing Data:\n")
cat("Total Viewing Records:", nrow(viewing), "\n")
cat("Unique Customers:", n_distinct(viewing$CustomerID), "\n\n")

# ------------------------------------------------------------------------------
# 3. DATA PREPARATION AND FEATURE ENGINEERING
# ------------------------------------------------------------------------------

cat("=== FEATURE ENGINEERING ===\n\n")

# Aggregate viewing behavior by customer
viewing_features <- viewing %>%
  group_by(CustomerID) %>%
  summarise(
    # Engagement metrics
    TotalMinutesViewed = sum(MinutesViewed, na.rm = TRUE),
    AvgMinutesPerSession = mean(MinutesViewed, na.rm = TRUE),
    TotalSessions = n(),
    UniqueDaysViewed = n_distinct(ViewingDate),
    
    # Content preferences
    PctSportsViewing = sum(TypeOfAsset == "Sports") / n() * 100,
    PctEntertainmentViewing = sum(TypeOfAsset == "Entertainment") / n() * 100,
    PctMoviesViewing = sum(TypeOfAsset == "Movies") / n() * 100,
    PctSeriesViewing = sum(TypeOfAsset == "Series") / n() * 100,
    
    # Recency
    DaysSinceLastView = as.numeric(Sys.Date() - max(ViewingDate)),
    
    # Consistency
    ViewingFrequency = n() / as.numeric(max(ViewingDate) - min(ViewingDate) + 1),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Derived features
    AvgSessionsPerDay = TotalSessions / UniqueDaysViewed,
    EngagementScore = scale(TotalMinutesViewed) + 
                      scale(TotalSessions) - 
                      scale(DaysSinceLastView),
    IsHighEngagement = TotalMinutesViewed > median(TotalMinutesViewed),
    IsRegularViewer = ViewingFrequency > 0.5  # Views more than every other day
  )

# Master dataset combining subscription and viewing
churn_data <- subscriptions %>%
  left_join(viewing_features, by = "CustomerID") %>%
  mutate(
    # Handle missing viewing data (new subscribers with no viewing history)
    TotalMinutesViewed = replace_na(TotalMinutesViewed, 0),
    TotalSessions = replace_na(TotalSessions, 0),
    DaysSinceLastView = replace_na(DaysSinceLastView, 999),
    IsHighEngagement = replace_na(IsHighEngagement, FALSE),
    
    # Create binary churn flag
    Churned = ifelse(Cancellation == 1, 1, 0),
    
    # Subscription features
    IsHighValue = MonthlyPrice >= 19.99,
    TenureCategory = case_when(
      TenureMonths <= 3 ~ "New (0-3 months)",
      TenureMonths <= 12 ~ "Growing (4-12 months)",
      TenureMonths <= 24 ~ "Mature (13-24 months)",
      TRUE ~ "Loyal (25+ months)"
    ),
    
    # Risk flags
    LowEngagementFlag = TotalMinutesViewed < quantile(TotalMinutesViewed, 0.25, na.rm = TRUE),
    InactiveFlag = DaysSinceLastView > 30,
    
    # Subscription month for seasonality
    SubscriptionMonth = month(SubscriptionDate, label = TRUE)
  )

cat("Features created:", ncol(churn_data), "\n")
cat("Records in master dataset:", nrow(churn_data), "\n\n")

# ------------------------------------------------------------------------------
# 4. EXPLORATORY DATA ANALYSIS (EDA)
# ------------------------------------------------------------------------------

cat("=== EXPLORATORY DATA ANALYSIS ===\n\n")

# Overall churn rate
overall_churn <- churn_data %>%
  summarise(
    TotalCustomers = n(),
    ChurnedCustomers = sum(Churned),
    ChurnRate = mean(Churned) * 100,
    VoluntaryChurn = sum(TypeOfChurn == "Voluntary"),
    InvoluntaryChurn = sum(TypeOfChurn == "Involuntary"),
    .groups = "drop"
  )

cat("Overall Churn Rate:", round(overall_churn$ChurnRate, 2), "%\n")
cat("Voluntary Churn:", overall_churn$VoluntaryChurn, 
    sprintf("(%.1f%%)", overall_churn$VoluntaryChurn/overall_churn$ChurnedCustomers*100), "\n")
cat("Involuntary Churn:", overall_churn$InvoluntaryChurn,
    sprintf("(%.1f%%)", overall_churn$InvoluntaryChurn/overall_churn$ChurnedCustomers*100), "\n\n")

# Churn by Product Type
churn_by_product <- churn_data %>%
  group_by(ProductType) %>%
  summarise(
    Customers = n(),
    ChurnedCustomers = sum(Churned),
    ChurnRate = mean(Churned) * 100,
    AvgTenure = mean(TenureMonths),
    AvgMinutesViewed = mean(TotalMinutesViewed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(ChurnRate))

cat("Churn by Product Type:\n")
print(churn_by_product)
cat("\n")

# Churn by Channel
churn_by_channel <- churn_data %>%
  group_by(Channel) %>%
  summarise(
    Customers = n(),
    ChurnRate = mean(Churned) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(ChurnRate))

# Churn by Tenure
churn_by_tenure <- churn_data %>%
  group_by(TenureCategory) %>%
  summarise(
    Customers = n(),
    ChurnRate = mean(Churned) * 100,
    AvgMinutesViewed = mean(TotalMinutesViewed, na.rm = TRUE),
    .groups = "drop"
  )

# Engagement vs Churn
engagement_analysis <- churn_data %>%
  group_by(IsHighEngagement) %>%
  summarise(
    Customers = n(),
    ChurnRate = mean(Churned) * 100,
    .groups = "drop"
  )

cat("High Engagement Customers Churn Rate:", 
    round(engagement_analysis$ChurnRate[engagement_analysis$IsHighEngagement], 2), "%\n")
cat("Low Engagement Customers Churn Rate:",
    round(engagement_analysis$ChurnRate[!engagement_analysis$IsHighEngagement], 2), "%\n\n")

# Winback analysis
winback_analysis <- churn_data %>%
  group_by(IsWinback) %>%
  summarise(
    Customers = n(),
    ChurnRate = mean(Churned) * 100,
    AvgTenure = mean(TenureMonths),
    .groups = "drop"
  )

cat("Winback Customer Churn Rate:", 
    round(winback_analysis$ChurnRate[winback_analysis$IsWinback == 1], 2), "%\n")
cat("New Customer Churn Rate:",
    round(winback_analysis$ChurnRate[winback_analysis$IsWinback == 0], 2), "%\n\n")

# ------------------------------------------------------------------------------
# 5. STATISTICAL TESTS
# ------------------------------------------------------------------------------

cat("=== STATISTICAL SIGNIFICANCE TESTS ===\n\n")

# T-test: Minutes viewed between churned and retained
minutes_test <- t.test(
  TotalMinutesViewed ~ Churned, 
  data = churn_data
)

cat("Minutes Viewed - Churned vs Retained:\n")
cat("  Mean (Retained):", round(minutes_test$estimate[1], 2), "minutes\n")
cat("  Mean (Churned):", round(minutes_test$estimate[2], 2), "minutes\n")
cat("  p-value:", format.pval(minutes_test$p.value), "\n")
cat("  Significant:", ifelse(minutes_test$p.value < 0.05, "YES", "NO"), "\n\n")

# Chi-square test: Product Type vs Churn
chi_product <- chisq.test(table(churn_data$ProductType, churn_data$Churned))
cat("Product Type vs Churn:\n")
cat("  Chi-square:", round(chi_product$statistic, 2), "\n")
cat("  p-value:", format.pval(chi_product$p.value), "\n")
cat("  Significant:", ifelse(chi_product$p.value < 0.05, "YES", "NO"), "\n\n")

# ------------------------------------------------------------------------------
# 6. COHORT ANALYSIS
# ------------------------------------------------------------------------------

cat("=== COHORT ANALYSIS ===\n\n")

# Monthly cohort analysis
cohort_data <- churn_data %>%
  mutate(
    CohortMonth = floor_date(SubscriptionDate, "month")
  ) %>%
  group_by(CohortMonth) %>%
  summarise(
    CohortSize = n(),
    Churned = sum(Churned),
    ChurnRate = mean(Churned) * 100,
    AvgTenure = mean(TenureMonths),
    AvgRevenue = mean(MonthlyPrice * TenureMonths),
    .groups = "drop"
  ) %>%
  arrange(CohortMonth)

cat("Sample Cohort Analysis (first 6 months):\n")
print(head(cohort_data))
cat("\n")

# ------------------------------------------------------------------------------
# 7. RFM ANALYSIS (Recency, Frequency, Monetary)
# ------------------------------------------------------------------------------

cat("=== RFM ANALYSIS ===\n\n")

rfm_data <- churn_data %>%
  mutate(
    # Recency (days since last view)
    R_Score = case_when(
      DaysSinceLastView <= 7 ~ 5,
      DaysSinceLastView <= 14 ~ 4,
      DaysSinceLastView <= 30 ~ 3,
      DaysSinceLastView <= 60 ~ 2,
      TRUE ~ 1
    ),
    # Frequency (viewing sessions)
    F_Score = case_when(
      TotalSessions >= quantile(TotalSessions, 0.8, na.rm = TRUE) ~ 5,
      TotalSessions >= quantile(TotalSessions, 0.6, na.rm = TRUE) ~ 4,
      TotalSessions >= quantile(TotalSessions, 0.4, na.rm = TRUE) ~ 3,
      TotalSessions >= quantile(TotalSessions, 0.2, na.rm = TRUE) ~ 2,
      TRUE ~ 1
    ),
    # Monetary (monthly price as proxy)
    M_Score = case_when(
      MonthlyPrice >= 24.99 ~ 5,
      MonthlyPrice >= 19.99 ~ 4,
      MonthlyPrice >= 14.99 ~ 3,
      MonthlyPrice >= 9.99 ~ 2,
      TRUE ~ 1
    ),
    RFM_Score = R_Score + F_Score + M_Score,
    RFM_Segment = case_when(
      RFM_Score >= 13 ~ "Champions",
      RFM_Score >= 11 ~ "Loyal",
      RFM_Score >= 9 ~ "Potential Loyalist",
      RFM_Score >= 7 ~ "At Risk",
      RFM_Score >= 5 ~ "Can't Lose",
      TRUE ~ "Lost"
    )
  )

rfm_summary <- rfm_data %>%
  group_by(RFM_Segment) %>%
  summarise(
    Customers = n(),
    ChurnRate = mean(Churned) * 100,
    AvgRevenue = mean(MonthlyPrice * TenureMonths),
    .groups = "drop"
  ) %>%
  arrange(desc(ChurnRate))

cat("RFM Segmentation:\n")
print(rfm_summary)
cat("\n")

# ------------------------------------------------------------------------------
# 8. FEATURE IMPORTANCE (CORRELATION ANALYSIS)
# ------------------------------------------------------------------------------

cat("=== FEATURE IMPORTANCE ===\n\n")

# Select numeric features for correlation
numeric_features <- churn_data %>%
  select(
    Churned,
    TenureMonths,
    MonthlyPrice,
    TotalMinutesViewed,
    TotalSessions,
    AvgMinutesPerSession,
    DaysSinceLastView,
    ViewingFrequency,
    PctSportsViewing,
    PctEntertainmentViewing,
    IsWinback
  ) %>%
  mutate(IsWinback = as.numeric(IsWinback))

# Calculate correlations with churn
correlations <- cor(numeric_features, use = "complete.obs")
churn_correlations <- correlations[, "Churned"]
churn_correlations <- sort(abs(churn_correlations), decreasing = TRUE)

cat("Top features correlated with churn:\n")
print(head(churn_correlations, 10))
cat("\n")

# ------------------------------------------------------------------------------
# 9. CHURN RISK SCORING
# ------------------------------------------------------------------------------

cat("=== CHURN RISK SCORING ===\n\n")

# Create risk score based on key factors
risk_scored <- churn_data %>%
  mutate(
    # Risk factors (higher = more risk)
    TenureRisk = ifelse(TenureMonths <= 3, 3, 
                       ifelse(TenureMonths <= 6, 2, 1)),
    EngagementRisk = ifelse(TotalMinutesViewed < quantile(TotalMinutesViewed, 0.25, na.rm = TRUE), 3,
                           ifelse(TotalMinutesViewed < quantile(TotalMinutesViewed, 0.5, na.rm = TRUE), 2, 1)),
    RecencyRisk = ifelse(DaysSinceLastView > 30, 3,
                        ifelse(DaysSinceLastView > 14, 2, 1)),
    WinbackRisk = ifelse(IsWinback == 1, 2, 0),
    
    # Total risk score
    ChurnRiskScore = TenureRisk + EngagementRisk + RecencyRisk + WinbackRisk,
    
    # Risk category
    RiskCategory = case_when(
      ChurnRiskScore >= 8 ~ "Very High Risk",
      ChurnRiskScore >= 6 ~ "High Risk",
      ChurnRiskScore >= 4 ~ "Medium Risk",
      TRUE ~ "Low Risk"
    )
  )

# Validate risk scoring
risk_validation <- risk_scored %>%
  group_by(RiskCategory) %>%
  summarise(
    Customers = n(),
    ActualChurnRate = mean(Churned) * 100,
    AvgRiskScore = mean(ChurnRiskScore),
    .groups = "drop"
  ) %>%
  arrange(desc(AvgRiskScore))

cat("Risk Score Validation:\n")
print(risk_validation)
cat("\n")

# Save risk scored data for modeling
churn_modeling_data <- risk_scored

cat("=== DATA PREPARATION COMPLETE ===\n")
cat("Records ready for modeling:", nrow(churn_modeling_data), "\n")
cat("Features available:", ncol(churn_modeling_data), "\n\n")

# ------------------------------------------------------------------------------
# 10. KEY INSIGHTS SUMMARY
# ------------------------------------------------------------------------------

cat("=== KEY INSIGHTS SUMMARY ===\n\n")

insights <- list(
  overall_churn_rate = round(mean(churn_data$Churned) * 100, 2),
  highest_churn_product = churn_by_product$ProductType[1],
  highest_churn_product_rate = round(churn_by_product$ChurnRate[1], 2),
  engagement_impact = round(
    engagement_analysis$ChurnRate[!engagement_analysis$IsHighEngagement] - 
    engagement_analysis$ChurnRate[engagement_analysis$IsHighEngagement], 2
  ),
  avg_tenure_churned = round(mean(churn_data$TenureMonths[churn_data$Churned == 1]), 1),
  avg_tenure_retained = round(mean(churn_data$TenureMonths[churn_data$Churned == 0]), 1)
)

cat("1. Overall churn rate:", insights$overall_churn_rate, "%\n")
cat("2. Highest churn product:", insights$highest_churn_product, 
    "at", insights$highest_churn_product_rate, "%\n")
cat("3. Engagement impact: Low engagement customers churn", 
    insights$engagement_impact, "% more\n")
cat("4. Average tenure (churned):", insights$avg_tenure_churned, "months\n")
cat("5. Average tenure (retained):", insights$avg_tenure_retained, "months\n\n")

cat("=== SCRIPT COMPLETE ===\n")
cat("Next: Run 'churn_modeling.R' for predictive model building\n")

# Save processed data
# saveRDS(churn_modeling_data, "churn_modeling_data.rds")
# write.csv(churn_modeling_data, "churn_modeling_data.csv", row.names = FALSE)
