# ==============================================================================
# MASTER CHURN ANALYSIS WORKFLOW
# ==============================================================================
#
# This script orchestrates the complete churn analysis:
# 1. Data preparation and feature engineering
# 2. Exploratory data analysis
# 3. Statistical testing
# 4. Predictive modeling
# 5. Visualization generation
# 6. Report creation
#
# Run this script to execute the entire analysis pipeline
#
# ==============================================================================

# Clear environment
rm(list = ls())
gc()

# Set working directory (adjust as needed)
# setwd("/path/to/your/project")

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║         OTT PLATFORM CHURN ANALYSIS - MASTER SCRIPT       ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ------------------------------------------------------------------------------
# 0. PACKAGE INSTALLATION AND SETUP
# ------------------------------------------------------------------------------

cat("Step 0: Checking and installing required packages...\n")

required_packages <- c(
  "dplyr", "tidyr", "lubridate", "ggplot2", "gt",
  "randomForest", "caret", "pROC", "corrplot", "scales",
  "xgboost", "e1071"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) {
  cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages, dependencies = TRUE)
} else {
  cat("✓ All required packages are already installed\n")
}

cat("\n")

# ------------------------------------------------------------------------------
# 1. DATA LOADING
# ------------------------------------------------------------------------------

cat("Step 1: Loading data...\n")
cat("─────────────────────────\n")

# REPLACE THIS WITH YOUR ACTUAL DATA LOADING
# Example:
# subscriptions <- read.csv("subscriptions.csv")
# viewing <- read.csv("viewing_data.csv")

# For demonstration, creating sample data
subscriptions <- data.frame(
  SubscriptionID = 1:5000,
  CustomerID = paste0("C", 1:5000),
  ProductType = sample(c("4K", "Entertainment", "Sports", "AFCON Pass"), 5000, 
                       replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
  Channel = sample(c("Web", "Mobile", "TV", "Partner"), 5000, 
                   replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
  SubscriptionDate = seq(as.Date("2022-01-01"), by = "day", length.out = 5000),
  Cancellation = sample(c(0, 1), 5000, replace = TRUE, prob = c(0.70, 0.30)),
  TypeOfChurn = sample(c("None", "Voluntary", "Involuntary"), 5000, 
                       replace = TRUE, prob = c(0.70, 0.20, 0.10)),
  MonthlyPrice = sample(c(9.99, 14.99, 19.99, 24.99), 5000, replace = TRUE),
  IsWinback = sample(c(0, 1), 5000, replace = TRUE, prob = c(0.85, 0.15)),
  TenureMonths = sample(1:48, 5000, replace = TRUE)
)

viewing <- data.frame(
  ViewingID = 1:25000,
  CustomerID = sample(paste0("C", 1:5000), 25000, replace = TRUE),
  MinutesViewed = rpois(25000, lambda = 100),
  AssetID = paste0("A", sample(1:1000, 25000, replace = TRUE)),
  TypeOfAsset = sample(c("Sports", "Entertainment", "Movies", "Series"), 25000,
                       replace = TRUE, prob = c(0.2, 0.3, 0.25, 0.25)),
  ViewingDate = sample(seq(as.Date("2022-01-01"), as.Date("2024-12-31"), 
                           by = "day"), 25000, replace = TRUE)
)

cat("✓ Data loaded successfully\n")
cat("  Subscriptions:", nrow(subscriptions), "records\n")
cat("  Viewing data:", nrow(viewing), "records\n\n")

# ------------------------------------------------------------------------------
# 2. RUN CHURN ANALYSIS
# ------------------------------------------------------------------------------

cat("Step 2: Running churn analysis...\n")
cat("─────────────────────────────────\n")

source("churn_analysis.R")

cat("\n✓ Churn analysis completed\n\n")

# ------------------------------------------------------------------------------
# 3. RUN PREDICTIVE MODELING
# ------------------------------------------------------------------------------

cat("Step 3: Building predictive models...\n")
cat("──────────────────────────────────────\n")

source("churn_modeling.R")

cat("\n✓ Predictive modeling completed\n\n")

# ------------------------------------------------------------------------------
# 4. GENERATE VISUALIZATIONS
# ------------------------------------------------------------------------------

cat("Step 4: Creating visualizations...\n")
cat("───────────────────────────────────\n")

source("churn_visualizations.R")

cat("\n✓ Visualizations generated\n\n")

# ------------------------------------------------------------------------------
# 5. CREATE GT TABLES FOR REPORTING
# ------------------------------------------------------------------------------

cat("Step 5: Creating summary tables...\n")
cat("──────────────────────────────────\n")

library(gt)

# Create output directory for tables
if (!dir.exists("churn_tables")) {
  dir.create("churn_tables")
}

# Table 1: Executive Summary
exec_summary <- data.frame(
  Metric = c(
    "Total Customers",
    "Churned Customers",
    "Overall Churn Rate",
    "Voluntary Churn Rate",
    "Involuntary Churn Rate",
    "Average Customer Tenure",
    "High-Risk Customers",
    "Model Accuracy (RF)"
  ),
  Value = c(
    format(nrow(churn_data), big.mark = ","),
    format(sum(churn_data$Churned), big.mark = ","),
    paste0(round(mean(churn_data$Churned) * 100, 1), "%"),
    paste0(round(sum(churn_data$TypeOfChurn == "Voluntary") / 
                  sum(churn_data$Churned) * 100, 1), "%"),
    paste0(round(sum(churn_data$TypeOfChurn == "Involuntary") / 
                  sum(churn_data$Churned) * 100, 1), "%"),
    paste0(round(mean(churn_data$TenureMonths), 1), " months"),
    format(sum(risk_scored$RiskCategory %in% c("Very High Risk", "High Risk")), 
           big.mark = ","),
    paste0(round(rf_cm$overall["Accuracy"] * 100, 1), "%")
  )
)

exec_table <- exec_summary %>%
  gt() %>%
  tab_header(
    title = md("**Churn Analysis Executive Summary**"),
    subtitle = "Key Metrics and Model Performance"
  ) %>%
  cols_label(
    Metric = "Key Metric",
    Value = "Value"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Metric)
  )

gtsave(exec_table, "churn_tables/01_executive_summary.png", 
       vwidth = 900, vheight = 500)
cat("✓ Executive summary table created\n")

# Table 2: Product Performance
product_table <- churn_by_product %>%
  gt() %>%
  tab_header(
    title = md("**Churn Analysis by Product Type**"),
    subtitle = "Product performance comparison"
  ) %>%
  fmt_percent(
    columns = ChurnRate,
    scale_values = FALSE,
    decimals = 1
  ) %>%
  fmt_number(
    columns = c(Customers, ChurnedCustomers, AvgTenure, AvgMinutesViewed),
    decimals = 0
  ) %>%
  data_color(
    columns = ChurnRate,
    colors = scales::col_numeric(
      palette = c("#70AD47", "#FFC000", "#ED7D31"),
      domain = NULL
    )
  )

gtsave(product_table, "churn_tables/02_product_analysis.png",
       vwidth = 1100, vheight = 400)
cat("✓ Product analysis table created\n")

# Table 3: Model Comparison
model_comp_table <- model_comparison %>%
  gt() %>%
  tab_header(
    title = md("**Predictive Model Comparison**"),
    subtitle = "Performance metrics across different algorithms"
  ) %>%
  fmt_number(
    columns = c(Accuracy, Precision, Recall, F1_Score, AUC),
    decimals = 4
  ) %>%
  data_color(
    columns = AUC,
    colors = scales::col_numeric(
      palette = c("#ED7D31", "#FFC000", "#70AD47"),
      domain = NULL
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = everything(),
      rows = AUC == max(AUC)
    )
  )

gtsave(model_comp_table, "churn_tables/03_model_comparison.png",
       vwidth = 1000, vheight = 350)
cat("✓ Model comparison table created\n")

# Table 4: Risk Segment Analysis
risk_table <- risk_segment_analysis %>%
  gt() %>%
  tab_header(
    title = md("**Customer Risk Segmentation**"),
    subtitle = "Predicted risk vs actual churn rates"
  ) %>%
  fmt_number(
    columns = c(Customers, ActualChurned),
    decimals = 0
  ) %>%
  fmt_percent(
    columns = c(ActualChurnRate, AvgPredictedProb),
    scale_values = FALSE,
    decimals = 1
  ) %>%
  data_color(
    columns = ActualChurnRate,
    colors = scales::col_numeric(
      palette = c("#70AD47", "#FFC000", "#ED7D31"),
      domain = NULL
    )
  )

gtsave(risk_table, "churn_tables/04_risk_segmentation.png",
       vwidth = 1100, vheight = 400)
cat("✓ Risk segmentation table created\n")

# Table 5: Feature Importance
feature_table <- rf_importance_df %>%
  head(10) %>%
  gt() %>%
  tab_header(
    title = md("**Top 10 Most Important Features**"),
    subtitle = "Random Forest feature importance ranking"
  ) %>%
  fmt_number(
    columns = c(MeanDecreaseAccuracy, MeanDecreaseGini),
    decimals = 2
  ) %>%
  data_color(
    columns = MeanDecreaseGini,
    colors = scales::col_numeric(
      palette = c("#FFFFFF", "#4472C4"),
      domain = NULL
    )
  )

gtsave(feature_table, "churn_tables/05_feature_importance.png",
       vwidth = 900, vheight = 500)
cat("✓ Feature importance table created\n")

cat("\n✓ All tables created\n\n")

# ------------------------------------------------------------------------------
# 6. GENERATE FINAL SUMMARY REPORT
# ------------------------------------------------------------------------------

cat("Step 6: Generating summary report...\n")
cat("─────────────────────────────────────\n")

# Create text summary
summary_report <- paste0(
  "╔════════════════════════════════════════════════════════════════════╗\n",
  "║                  CHURN ANALYSIS SUMMARY REPORT                     ║\n",
  "╚════════════════════════════════════════════════════════════════════╝\n\n",
  
  "EXECUTIVE SUMMARY\n",
  "━━━━━━━━━━━━━━━━━\n",
  "Overall Churn Rate: ", round(mean(churn_data$Churned) * 100, 1), "%\n",
  "Total Customers Analyzed: ", format(nrow(churn_data), big.mark = ","), "\n",
  "Churned Customers: ", format(sum(churn_data$Churned), big.mark = ","), "\n",
  "Average Tenure: ", round(mean(churn_data$TenureMonths), 1), " months\n\n",
  
  "KEY FINDINGS\n",
  "━━━━━━━━━━━━\n",
  "1. Product Analysis\n",
  "   - Highest churn: ", churn_by_product$ProductType[1], 
  " (", round(churn_by_product$ChurnRate[1], 1), "%)\n",
  "   - Lowest churn: ", churn_by_product$ProductType[nrow(churn_by_product)],
  " (", round(churn_by_product$ChurnRate[nrow(churn_by_product)], 1), "%)\n\n",
  
  "2. Engagement Impact\n",
  "   - High engagement customers: ",
  round(engagement_analysis$ChurnRate[engagement_analysis$IsHighEngagement], 1), "% churn\n",
  "   - Low engagement customers: ",
  round(engagement_analysis$ChurnRate[!engagement_analysis$IsHighEngagement], 1), "% churn\n",
  "   - Engagement gap: ",
  round(engagement_analysis$ChurnRate[!engagement_analysis$IsHighEngagement] -
        engagement_analysis$ChurnRate[engagement_analysis$IsHighEngagement], 1), "% points\n\n",
  
  "3. Churn Type Breakdown\n",
  "   - Voluntary churn: ", 
  round(sum(churn_data$TypeOfChurn == "Voluntary") / sum(churn_data$Churned) * 100, 1), "%\n",
  "   - Involuntary churn: ",
  round(sum(churn_data$TypeOfChurn == "Involuntary") / sum(churn_data$Churned) * 100, 1), "%\n\n",
  
  "MODEL PERFORMANCE\n",
  "━━━━━━━━━━━━━━━━━\n",
  "Best Model: ", best_model_name, "\n",
  "Accuracy: ", round(rf_cm$overall["Accuracy"] * 100, 1), "%\n",
  "Precision: ", round(rf_cm$byClass["Precision"] * 100, 1), "%\n",
  "Recall: ", round(rf_cm$byClass["Sensitivity"] * 100, 1), "%\n",
  "AUC: ", round(auc(rf_roc), 3), "\n\n",
  
  "RISK SEGMENTATION\n",
  "━━━━━━━━━━━━━━━━━\n",
  "Very High Risk: ", sum(risk_scored$RiskCategory == "Very High Risk"), " customers\n",
  "High Risk: ", sum(risk_scored$RiskCategory == "High Risk"), " customers\n",
  "Medium Risk: ", sum(risk_scored$RiskCategory == "Medium Risk"), " customers\n",
  "Low Risk: ", sum(risk_scored$RiskCategory == "Low Risk"), " customers\n\n",
  
  "TOP 5 CHURN DRIVERS\n",
  "━━━━━━━━━━━━━━━━━━━\n"
)

for (i in 1:5) {
  summary_report <- paste0(
    summary_report,
    i, ". ", rf_importance_df$Feature[i], "\n"
  )
}

summary_report <- paste0(
  summary_report,
  "\nRECOMMENDED ACTIONS\n",
  "━━━━━━━━━━━━━━━━━━━\n",
  "1. Immediate: Target ", sum(predictions_df$RiskSegment == "Very High Risk"),
  " very high-risk customers with retention offers\n",
  "2. Product: Review ", churn_by_product$ProductType[1], " offering (highest churn)\n",
  "3. Engagement: Launch re-engagement campaign for inactive users (>30 days)\n",
  "4. Pricing: Analyze price sensitivity for ", 
  sum(churn_data$IsHighValue), " high-value customers\n",
  "5. Content: Improve content recommendations based on viewing patterns\n\n",
  
  "BUSINESS IMPACT\n",
  "━━━━━━━━━━━━━━━\n",
  "Estimated revenue at risk: $", 
  format(round(sum(churn_data$Churned) * avg_customer_lifetime_value, 0), big.mark = ","), "\n",
  "Potential savings (20% retention improvement): $",
  format(round(potential_revenue_saved, 0), big.mark = ","), "\n\n",
  
  "OUTPUT FILES GENERATED\n",
  "━━━━━━━━━━━━━━━━━━━━━━\n",
  "Visualizations: churn_visuals/ (", 
  length(list.files("churn_visuals", pattern = ".png")), " files)\n",
  "Tables: churn_tables/ (", 
  length(list.files("churn_tables", pattern = ".png")), " files)\n",
  "Model: churn_rf_model.rds\n",
  "Predictions: churn_predictions.csv\n",
  "Feature importance: feature_importance.csv\n\n",
  
  "Analysis completed: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
  "═══════════════════════════════════════════════════════════════════════\n"
)

cat(summary_report)

# Save summary to file
writeLines(summary_report, "CHURN_ANALYSIS_SUMMARY.txt")
cat("\n✓ Summary report saved: CHURN_ANALYSIS_SUMMARY.txt\n\n")

# ------------------------------------------------------------------------------
# 7. COMPLETION
# ------------------------------------------------------------------------------

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║                  ANALYSIS COMPLETE!                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("Next Steps:\n")
cat("───────────\n")
cat("1. Review visualizations in: churn_visuals/\n")
cat("2. Review tables in: churn_tables/\n")
cat("3. Check predictions in: churn_predictions.csv\n")
cat("4. Read summary: CHURN_ANALYSIS_SUMMARY.txt\n")
cat("5. Create PowerPoint presentation using Python script\n\n")

cat("To create PowerPoint:\n")
cat("  python create_churn_presentation.py\n\n")

# Save workspace for later use
save.image("churn_analysis_workspace.RData")
cat("✓ Workspace saved: churn_analysis_workspace.RData\n")
cat("  (To reload: load('churn_analysis_workspace.RData'))\n\n")

cat("════════════════════════════════════════════════════════════\n")
