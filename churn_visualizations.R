# ==============================================================================
# CHURN ANALYSIS VISUALIZATIONS
# ==============================================================================
#
# This script creates publication-quality visualizations for:
# 1. Exploratory analysis charts
# 2. Model performance visualizations
# 3. Business insights dashboards
#
# All outputs saved as PNG for PowerPoint integration
#
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(pROC)
library(gridExtra)

# Create output directory
if (!dir.exists("churn_visuals")) {
  dir.create("churn_visuals")
}

# Set theme for all plots
theme_set(theme_minimal(base_size = 12))

# Custom color palette
colors_primary <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#70AD47")
colors_churn <- c("Retained" = "#70AD47", "Churned" = "#ED7D31")

cat("=== CREATING VISUALIZATIONS ===\n\n")

# ------------------------------------------------------------------------------
# 1. OVERALL CHURN RATE
# ------------------------------------------------------------------------------

p1 <- churn_data %>%
  count(Churned) %>%
  mutate(
    Percentage = n / sum(n) * 100,
    Label = paste0(round(Percentage, 1), "%")
  ) %>%
  ggplot(aes(x = "", y = n, fill = factor(Churned))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors_churn, 
                    labels = c("Retained", "Churned"),
                    name = "Status") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5),
            size = 6, fontface = "bold") +
  labs(title = "Overall Churn Rate") +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

ggsave("churn_visuals/01_overall_churn_rate.png", 
       plot = p1, width = 8, height = 6, dpi = 300, bg = "white")
cat("✓ Overall churn rate visualization saved\n")

# ------------------------------------------------------------------------------
# 2. CHURN BY PRODUCT TYPE
# ------------------------------------------------------------------------------

p2 <- churn_by_product %>%
  ggplot(aes(x = reorder(ProductType, -ChurnRate), y = ChurnRate, fill = ProductType)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(ChurnRate, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = colors_primary) +
  labs(
    title = "Churn Rate by Product Type",
    subtitle = "Which products have the highest customer loss?",
    x = "Product Type",
    y = "Churn Rate (%)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(churn_by_product$ChurnRate) * 1.15)

ggsave("churn_visuals/02_churn_by_product.png", 
       plot = p2, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Churn by product type visualization saved\n")

# ------------------------------------------------------------------------------
# 3. CHURN BY TENURE
# ------------------------------------------------------------------------------

p3 <- churn_by_tenure %>%
  mutate(TenureCategory = factor(TenureCategory, 
                                  levels = c("New (0-3 months)", 
                                           "Growing (4-12 months)",
                                           "Mature (13-24 months)",
                                           "Loyal (25+ months)"))) %>%
  ggplot(aes(x = TenureCategory, y = ChurnRate, fill = TenureCategory)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(ChurnRate, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = colors_primary) +
  labs(
    title = "Churn Rate by Customer Tenure",
    subtitle = "New customers are at highest risk",
    x = "Tenure Category",
    y = "Churn Rate (%)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none"
  ) +
  ylim(0, max(churn_by_tenure$ChurnRate) * 1.15)

ggsave("churn_visuals/03_churn_by_tenure.png", 
       plot = p3, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Churn by tenure visualization saved\n")

# ------------------------------------------------------------------------------
# 4. ENGAGEMENT VS CHURN
# ------------------------------------------------------------------------------

p4 <- churn_data %>%
  ggplot(aes(x = TotalMinutesViewed, fill = factor(Churned))) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = colors_churn,
                    labels = c("Retained", "Churned"),
                    name = "Status") +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Viewing Engagement Distribution",
    subtitle = "Churned customers show significantly lower engagement",
    x = "Total Minutes Viewed",
    y = "Number of Customers"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

ggsave("churn_visuals/04_engagement_distribution.png", 
       plot = p4, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Engagement distribution visualization saved\n")

# ------------------------------------------------------------------------------
# 5. VOLUNTARY VS INVOLUNTARY CHURN
# ------------------------------------------------------------------------------

p5 <- churn_data %>%
  filter(TypeOfChurn != "None") %>%
  count(TypeOfChurn) %>%
  mutate(
    Percentage = n / sum(n) * 100,
    Label = paste0(TypeOfChurn, "\n", round(Percentage, 1), "%")
  ) %>%
  ggplot(aes(x = "", y = n, fill = TypeOfChurn)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Voluntary" = "#ED7D31", 
                               "Involuntary" = "#4472C4")) +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5),
            size = 5, fontface = "bold") +
  labs(title = "Churn Type Breakdown") +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

ggsave("churn_visuals/05_churn_type_breakdown.png", 
       plot = p5, width = 8, height = 6, dpi = 300, bg = "white")
cat("✓ Churn type breakdown visualization saved\n")

# ------------------------------------------------------------------------------
# 6. CHANNEL PERFORMANCE
# ------------------------------------------------------------------------------

p6 <- churn_by_channel %>%
  ggplot(aes(x = reorder(Channel, -ChurnRate), y = ChurnRate, fill = Channel)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(ChurnRate, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = colors_primary) +
  labs(
    title = "Churn Rate by Acquisition Channel",
    subtitle = "Understanding channel quality",
    x = "Channel",
    y = "Churn Rate (%)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(churn_by_channel$ChurnRate) * 1.15)

ggsave("churn_visuals/06_churn_by_channel.png", 
       plot = p6, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Churn by channel visualization saved\n")

# ------------------------------------------------------------------------------
# 7. COHORT RETENTION CURVE
# ------------------------------------------------------------------------------

p7 <- cohort_data %>%
  mutate(RetentionRate = 100 - ChurnRate) %>%
  ggplot(aes(x = CohortMonth, y = RetentionRate)) +
  geom_line(color = "#4472C4", size = 1.2) +
  geom_point(color = "#4472C4", size = 3) +
  geom_hline(yintercept = 75, linetype = "dashed", color = "red", size = 1) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Customer Retention by Cohort",
    subtitle = "Monthly cohort retention trends over time",
    x = "Cohort Month",
    y = "Retention Rate (%)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("churn_visuals/07_cohort_retention.png", 
       plot = p7, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Cohort retention visualization saved\n")

# ------------------------------------------------------------------------------
# 8. RFM SEGMENTATION
# ------------------------------------------------------------------------------

p8 <- rfm_summary %>%
  mutate(RFM_Segment = factor(RFM_Segment, 
                              levels = c("Champions", "Loyal", "Potential Loyalist",
                                       "At Risk", "Can't Lose", "Lost"))) %>%
  ggplot(aes(x = RFM_Segment, y = ChurnRate, fill = RFM_Segment)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(ChurnRate, 1), "%")), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(
    title = "Churn Rate by RFM Segment",
    subtitle = "Recency, Frequency, Monetary value segmentation",
    x = "RFM Segment",
    y = "Churn Rate (%)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  ) +
  ylim(0, max(rfm_summary$ChurnRate) * 1.15)

ggsave("churn_visuals/08_rfm_segmentation.png", 
       plot = p8, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ RFM segmentation visualization saved\n")

# ------------------------------------------------------------------------------
# 9. CONTENT PREFERENCE ANALYSIS
# ------------------------------------------------------------------------------

content_preference <- churn_data %>%
  select(Churned, PctSportsViewing, PctEntertainmentViewing) %>%
  pivot_longer(cols = starts_with("Pct"), 
               names_to = "ContentType", 
               values_to = "Percentage") %>%
  mutate(
    ContentType = gsub("Pct|Viewing", "", ContentType),
    Churned = factor(Churned, levels = c(0, 1), labels = c("Retained", "Churned"))
  ) %>%
  group_by(Churned, ContentType) %>%
  summarise(AvgPercentage = mean(Percentage, na.rm = TRUE), .groups = "drop")

p9 <- ggplot(content_preference, 
             aes(x = ContentType, y = AvgPercentage, fill = Churned)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = colors_churn) +
  labs(
    title = "Content Viewing Preferences",
    subtitle = "Retained vs Churned customers",
    x = "Content Type",
    y = "Average % of Viewing Time",
    fill = "Status"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

ggsave("churn_visuals/09_content_preferences.png", 
       plot = p9, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Content preference visualization saved\n")

# ------------------------------------------------------------------------------
# 10. RISK SCORE DISTRIBUTION
# ------------------------------------------------------------------------------

p10 <- risk_scored %>%
  ggplot(aes(x = ChurnRiskScore, fill = factor(Churned))) +
  geom_histogram(bins = 10, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = colors_churn,
                    labels = c("Retained", "Churned"),
                    name = "Actual Status") +
  labs(
    title = "Churn Risk Score Distribution",
    subtitle = "Higher scores indicate higher churn risk",
    x = "Risk Score",
    y = "Number of Customers"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

ggsave("churn_visuals/10_risk_score_distribution.png", 
       plot = p10, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Risk score distribution visualization saved\n")

# ------------------------------------------------------------------------------
# 11. MODEL PERFORMANCE - ROC CURVES
# ------------------------------------------------------------------------------

# Assuming models have been run (from churn_modeling.R)
# This creates a comparison ROC curve

roc_data <- data.frame(
  Specificity = c(rev(logit_roc$specificities), 
                  rev(rf_roc$specificities),
                  rev(xgb_roc$specificities)),
  Sensitivity = c(rev(logit_roc$sensitivities),
                  rev(rf_roc$sensitivities),
                  rev(xgb_roc$sensitivities)),
  Model = c(rep("Logistic Regression", length(logit_roc$specificities)),
            rep("Random Forest", length(rf_roc$specificities)),
            rep("XGBoost", length(xgb_roc$specificities)))
)

p11 <- ggplot(roc_data, aes(x = 1 - Specificity, y = Sensitivity, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50") +
  scale_color_manual(values = colors_primary[1:3]) +
  labs(
    title = "Model Performance Comparison - ROC Curves",
    subtitle = paste0("Logistic AUC: ", round(auc(logit_roc), 3),
                     " | RF AUC: ", round(auc(rf_roc), 3),
                     " | XGB AUC: ", round(auc(xgb_roc), 3)),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave("churn_visuals/11_roc_curves.png", 
       plot = p11, width = 10, height = 8, dpi = 300, bg = "white")
cat("✓ ROC curves visualization saved\n")

# ------------------------------------------------------------------------------
# 12. FEATURE IMPORTANCE
# ------------------------------------------------------------------------------

p12 <- rf_importance_df %>%
  head(15) %>%
  ggplot(aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "#4472C4", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 15 Most Important Features",
    subtitle = "Random Forest - Mean Decrease in Gini",
    x = "Feature",
    y = "Importance Score"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

ggsave("churn_visuals/12_feature_importance.png", 
       plot = p12, width = 10, height = 8, dpi = 300, bg = "white")
cat("✓ Feature importance visualization saved\n")

# ------------------------------------------------------------------------------
# 13. PREDICTION CALIBRATION
# ------------------------------------------------------------------------------

calibration_data <- predictions_df %>%
  mutate(
    ProbBucket = cut(PredictedProb, 
                     breaks = seq(0, 1, 0.1),
                     labels = paste0(seq(0, 90, 10), "-", seq(10, 100, 10), "%"))
  ) %>%
  group_by(ProbBucket) %>%
  summarise(
    PredictedProb = mean(PredictedProb),
    ActualChurnRate = mean(ActualChurn == "Churned"),
    Count = n(),
    .groups = "drop"
  )

p13 <- ggplot(calibration_data, aes(x = PredictedProb, y = ActualChurnRate)) +
  geom_point(aes(size = Count), color = "#4472C4", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Model Calibration",
    subtitle = "How well do predicted probabilities match actual churn rates?",
    x = "Predicted Churn Probability",
    y = "Actual Churn Rate",
    size = "Customers"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

ggsave("churn_visuals/13_calibration_plot.png", 
       plot = p13, width = 10, height = 8, dpi = 300, bg = "white")
cat("✓ Calibration plot visualization saved\n")

# ------------------------------------------------------------------------------
# 14. RISK SEGMENT VALIDATION
# ------------------------------------------------------------------------------

p14 <- risk_segment_analysis %>%
  mutate(RiskSegment = factor(RiskSegment,
                              levels = c("Very High Risk", "High Risk", 
                                       "Medium Risk", "Low Risk"))) %>%
  ggplot(aes(x = RiskSegment, y = ActualChurnRate, fill = RiskSegment)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(ActualChurnRate, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "YlOrRd", direction = 1) +
  labs(
    title = "Model Risk Segment Validation",
    subtitle = "Actual churn rates by predicted risk segment",
    x = "Risk Segment",
    y = "Actual Churn Rate (%)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none"
  ) +
  ylim(0, max(risk_segment_analysis$ActualChurnRate) * 1.15)

ggsave("churn_visuals/14_risk_validation.png", 
       plot = p14, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Risk validation visualization saved\n")

# ------------------------------------------------------------------------------
# 15. BUSINESS IMPACT DASHBOARD
# ------------------------------------------------------------------------------

# Create a multi-panel summary dashboard
impact_panel1 <- data.frame(
  Metric = c("Total Customers", "Churned", "Retention Rate"),
  Value = c(
    nrow(churn_data),
    sum(churn_data$Churned),
    paste0(round((1 - mean(churn_data$Churned)) * 100, 1), "%")
  )
)

p15a <- ggplot(impact_panel1, aes(x = "", y = 1, label = Value)) +
  geom_text(size = 10, fontface = "bold", color = "#4472C4") +
  facet_wrap(~Metric, ncol = 3) +
  theme_void() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(title = "Key Metrics Overview")

ggsave("churn_visuals/15_business_metrics.png", 
       plot = p15a, width = 12, height = 4, dpi = 300, bg = "white")
cat("✓ Business metrics dashboard visualization saved\n")

cat("\n=== ALL VISUALIZATIONS CREATED ===\n")
cat("Total visualizations:", length(list.files("churn_visuals", pattern = ".png")), "\n")
cat("Location: churn_visuals/ directory\n")
cat("\nThese visualizations are ready for PowerPoint integration!\n")
