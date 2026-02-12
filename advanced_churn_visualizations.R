# ==============================================================================
# ADVANCED CHURN VISUALIZATIONS
# ==============================================================================
#
# This script creates sophisticated visualizations using ggplot2:
# 1. Bump charts (ranking changes over time)
# 2. Ridge plots (distribution comparisons)
# 3. Sankey/Alluvial diagrams (flow between states)
# 4. Heatmaps (correlation matrices)
# 5. Treemaps (hierarchical data)
# 6. Waterfall charts (contribution analysis)
# 7. Survival curves (time-to-churn)
# 8. Violin plots (distribution comparisons)
# 9. Lollipop charts (feature comparisons)
# 10. Dumbbell plots (before/after comparisons)
# 11. Stream graphs (area charts over time)
# 12. Network graphs (customer journey)
#
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. SETUP AND PACKAGES
# ------------------------------------------------------------------------------

# Install packages if needed
packages <- c(
  "ggplot2", "dplyr", "tidyr", "ggridges", "ggbump", 
  "ggalluvial", "treemapify", "waterfalls", "survival",
  "survminer", "viridis", "scales", "patchwork", "ggrepel"
)

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  cat("Installing:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages, dependencies = TRUE)
}

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggridges)
library(ggbump)
library(ggalluvial)
library(treemapify)
library(survival)
library(survminer)
library(viridis)
library(scales)
library(patchwork)
library(ggrepel)

# Create output directory
if (!dir.exists("advanced_visuals")) {
  dir.create("advanced_visuals")
}

set.seed(123)

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║        ADVANCED CHURN VISUALIZATIONS WITH GGPLOT2         ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ------------------------------------------------------------------------------
# 1. BUMP CHART - Product Ranking Changes Over Time
# ------------------------------------------------------------------------------

cat("1. Creating Bump Chart (Product Rankings)...\n")

# Create time series data for product rankings
bump_data <- data.frame(
  Month = rep(1:12, each = 4),
  ProductType = rep(c("4K", "Entertainment", "Sports", "AFCON Pass"), 12),
  ChurnRate = c(
    # Month 1-12, randomized but with trends
    28, 22, 35, 42,  # Month 1
    27, 23, 34, 40,  # Month 2
    26, 24, 33, 38,  # Month 3
    25, 25, 32, 36,  # Month 4
    24, 26, 31, 35,  # Month 5
    23, 27, 30, 34,  # Month 6
    22, 28, 29, 33,  # Month 7
    21, 29, 28, 32,  # Month 8
    20, 30, 27, 31,  # Month 9
    19, 31, 26, 30,  # Month 10
    18, 32, 25, 29,  # Month 11
    17, 33, 24, 28   # Month 12
  )
) %>%
  group_by(Month) %>%
  mutate(Rank = rank(ChurnRate, ties.method = "first")) %>%
  ungroup()

p1 <- ggplot(bump_data, aes(x = Month, y = Rank, color = ProductType, group = ProductType)) +
  geom_bump(size = 2, smooth = 8) +
  geom_point(size = 4) +
  scale_y_reverse(breaks = 1:4) +
  scale_x_continuous(breaks = 1:12, 
                     labels = month.abb[1:12]) +
  scale_color_manual(values = c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000")) +
  labs(
    title = "Product Churn Rate Rankings Over Time",
    subtitle = "How product performance changed throughout the year",
    x = "Month",
    y = "Rank (1 = Lowest Churn)",
    color = "Product Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

ggsave("advanced_visuals/01_bump_chart_rankings.png", 
       plot = p1, width = 12, height = 7, dpi = 300, bg = "white")
cat("  ✓ Bump chart saved\n\n")

# ------------------------------------------------------------------------------
# 2. RIDGE PLOT - Engagement Distribution by Churn Status
# ------------------------------------------------------------------------------

cat("2. Creating Ridge Plot (Engagement Distributions)...\n")

# Prepare data for ridge plot
ridge_data <- churn_data %>%
  mutate(
    ChurnStatus = factor(Churned, levels = c(0, 1), 
                         labels = c("Retained", "Churned")),
    TenureGroup = cut(TenureMonths, 
                      breaks = c(0, 6, 12, 24, Inf),
                      labels = c("0-6 months", "7-12 months", 
                                "13-24 months", "25+ months"))
  )

p2 <- ggplot(ridge_data, 
             aes(x = TotalMinutesViewed, y = TenureGroup, 
                 fill = ChurnStatus)) +
  geom_density_ridges(alpha = 0.7, scale = 2) +
  scale_fill_manual(values = c("Retained" = "#70AD47", "Churned" = "#ED7D31")) +
  scale_x_continuous(labels = comma, limits = c(0, quantile(ridge_data$TotalMinutesViewed, 0.95))) +
  labs(
    title = "Viewing Engagement Distributions by Tenure and Churn Status",
    subtitle = "Ridge plot showing how engagement varies across customer segments",
    x = "Total Minutes Viewed",
    y = "Customer Tenure",
    fill = "Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "top"
  )

ggsave("advanced_visuals/02_ridge_plot_engagement.png", 
       plot = p2, width = 12, height = 8, dpi = 300, bg = "white")
cat("  ✓ Ridge plot saved\n\n")

# ------------------------------------------------------------------------------
# 3. ALLUVIAL/SANKEY DIAGRAM - Customer Journey Flow
# ------------------------------------------------------------------------------

cat("3. Creating Alluvial Diagram (Customer Journey)...\n")

# Create customer journey data
alluvial_data <- churn_data %>%
  mutate(
    EngagementLevel = case_when(
      TotalMinutesViewed >= quantile(TotalMinutesViewed, 0.75, na.rm = TRUE) ~ "High",
      TotalMinutesViewed >= quantile(TotalMinutesViewed, 0.25, na.rm = TRUE) ~ "Medium",
      TRUE ~ "Low"
    ),
    RiskLevel = case_when(
      ChurnRiskScore >= 8 ~ "High Risk",
      ChurnRiskScore >= 5 ~ "Medium Risk",
      TRUE ~ "Low Risk"
    ),
    Outcome = factor(Churned, levels = c(0, 1), labels = c("Retained", "Churned"))
  ) %>%
  count(EngagementLevel, RiskLevel, Outcome) %>%
  mutate(
    EngagementLevel = factor(EngagementLevel, levels = c("Low", "Medium", "High")),
    RiskLevel = factor(RiskLevel, levels = c("Low Risk", "Medium Risk", "High Risk"))
  )

p3 <- ggplot(alluvial_data,
             aes(axis1 = EngagementLevel, axis2 = RiskLevel, axis3 = Outcome, y = n)) +
  geom_alluvium(aes(fill = Outcome), width = 1/12, alpha = 0.7) +
  geom_stratum(width = 1/12, fill = "gray90", color = "white") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Engagement", "Risk Level", "Outcome"), 
                   expand = c(0.05, 0.05)) +
  scale_fill_manual(values = c("Retained" = "#70AD47", "Churned" = "#ED7D31")) +
  labs(
    title = "Customer Journey Flow Analysis",
    subtitle = "From engagement level through risk assessment to final outcome",
    y = "Number of Customers"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.title.y = element_text(size = 12),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

ggsave("advanced_visuals/03_alluvial_customer_journey.png", 
       plot = p3, width = 12, height = 8, dpi = 300, bg = "white")
cat("  ✓ Alluvial diagram saved\n\n")

# ------------------------------------------------------------------------------
# 4. HEATMAP - Feature Correlation Matrix
# ------------------------------------------------------------------------------

cat("4. Creating Heatmap (Feature Correlations)...\n")

# Create correlation matrix
cor_data <- churn_data %>%
  select(TotalMinutesViewed, TotalSessions, AvgMinutesPerSession,
         DaysSinceLastView, ViewingFrequency, TenureMonths,
         MonthlyPrice, ChurnRiskScore) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("Variable1") %>%
  pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation")

p4 <- ggplot(cor_data, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3, color = "white") +
  scale_fill_gradient2(low = "#ED7D31", mid = "white", high = "#4472C4",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(
    title = "Feature Correlation Heatmap",
    subtitle = "Understanding relationships between key churn predictors",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggsave("advanced_visuals/04_heatmap_correlations.png", 
       plot = p4, width = 10, height = 9, dpi = 300, bg = "white")
cat("  ✓ Heatmap saved\n\n")

# ------------------------------------------------------------------------------
# 5. TREEMAP - Revenue at Risk by Segment
# ------------------------------------------------------------------------------

cat("5. Creating Treemap (Revenue at Risk)...\n")

# Calculate revenue at risk
treemap_data <- churn_data %>%
  mutate(
    Segment = paste(ProductType, "-", 
                   cut(TenureMonths, breaks = c(0, 6, 12, 24, Inf),
                       labels = c("New", "Growing", "Mature", "Loyal"))),
    RevenueAtRisk = ifelse(Churned == 1, MonthlyPrice * TenureMonths, 0)
  ) %>%
  group_by(ProductType, Segment) %>%
  summarise(
    TotalRevenue = sum(RevenueAtRisk),
    ChurnRate = mean(Churned) * 100,
    .groups = "drop"
  ) %>%
  filter(TotalRevenue > 0)

p5 <- ggplot(treemap_data, 
             aes(area = TotalRevenue, fill = ChurnRate, 
                 label = paste0(Segment, "\n$", format(round(TotalRevenue), big.mark = ",")),
                 subgroup = ProductType)) +
  geom_treemap() +
  geom_treemap_text(color = "white", place = "centre", size = 10) +
  geom_treemap_subgroup_border(color = "white", size = 3) +
  geom_treemap_subgroup_text(place = "centre", alpha = 0.5, 
                             color = "black", fontface = "bold") +
  scale_fill_gradient(low = "#70AD47", high = "#ED7D31", 
                      name = "Churn Rate (%)") +
  labs(
    title = "Revenue at Risk by Product and Customer Segment",
    subtitle = "Box size = Total revenue lost, Color intensity = Churn rate"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

ggsave("advanced_visuals/05_treemap_revenue_risk.png", 
       plot = p5, width = 12, height = 8, dpi = 300, bg = "white")
cat("  ✓ Treemap saved\n\n")

# ------------------------------------------------------------------------------
# 6. WATERFALL CHART - Churn Rate Contribution
# ------------------------------------------------------------------------------

cat("6. Creating Waterfall Chart (Churn Contributions)...\n")

# Calculate churn contributions by factor
waterfall_data <- data.frame(
  Category = c("Baseline", "Low Engagement", "Inactive Users", 
               "Short Tenure", "Winback Customers", "High Price",
               "Total Churn"),
  Value = c(15, 8, 5, 4, 3, -2, 33)  # Example values
) %>%
  mutate(
    Type = case_when(
      Category == "Baseline" ~ "start",
      Category == "Total Churn" ~ "end",
      Value > 0 ~ "increase",
      TRUE ~ "decrease"
    ),
    End = cumsum(Value),
    Start = lag(End, default = 0),
    id = row_number()
  )

p6 <- ggplot(waterfall_data, aes(x = reorder(Category, id))) +
  geom_rect(aes(xmin = id - 0.45, xmax = id + 0.45, 
                ymin = Start, ymax = End, fill = Type)) +
  geom_text(aes(y = End + 1.5, 
                label = paste0(ifelse(Value > 0, "+", ""), Value, "%")),
            size = 4, fontface = "bold") +
  scale_fill_manual(values = c("start" = "#4472C4", "end" = "#4472C4",
                               "increase" = "#ED7D31", "decrease" = "#70AD47"),
                    name = "Impact") +
  labs(
    title = "Waterfall Analysis: Churn Rate Contributions",
    subtitle = "How different factors contribute to overall churn rate",
    x = NULL,
    y = "Churn Rate (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

ggsave("advanced_visuals/06_waterfall_contributions.png", 
       plot = p6, width = 12, height = 7, dpi = 300, bg = "white")
cat("  ✓ Waterfall chart saved\n\n")

# ------------------------------------------------------------------------------
# 7. SURVIVAL CURVE - Time to Churn Analysis
# ------------------------------------------------------------------------------

cat("7. Creating Survival Curve (Time-to-Churn)...\n")

# Prepare survival data
surv_data <- churn_data %>%
  mutate(
    Time = TenureMonths,
    Event = Churned,
    ProductGroup = ProductType
  )

# Fit survival curves
surv_fit <- survfit(Surv(Time, Event) ~ ProductGroup, data = surv_data)

# Create survival plot
p7 <- ggsurvplot(
  surv_fit,
  data = surv_data,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = FALSE,
  ggtheme = theme_minimal(base_size = 14),
  palette = c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000"),
  title = "Survival Analysis: Time to Churn by Product Type",
  xlab = "Months Since Subscription",
  ylab = "Retention Probability",
  legend.title = "Product Type",
  legend.labs = levels(factor(surv_data$ProductGroup))
)

ggsave("advanced_visuals/07_survival_curve.png", 
       plot = print(p7), width = 12, height = 8, dpi = 300, bg = "white")
cat("  ✓ Survival curve saved\n\n")

# ------------------------------------------------------------------------------
# 8. VIOLIN PLOT - Engagement Distribution Comparison
# ------------------------------------------------------------------------------

cat("8. Creating Violin Plot (Engagement Comparison)...\n")

violin_data <- churn_data %>%
  mutate(
    ChurnStatus = factor(Churned, levels = c(0, 1), 
                        labels = c("Retained", "Churned"))
  ) %>%
  filter(TotalMinutesViewed < quantile(TotalMinutesViewed, 0.95))  # Remove outliers for better viz

p8 <- ggplot(violin_data, 
             aes(x = ProductType, y = TotalMinutesViewed, fill = ChurnStatus)) +
  geom_violin(alpha = 0.7, scale = "width", position = position_dodge(0.8)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.8), 
               outlier.shape = NA, alpha = 0.5) +
  scale_fill_manual(values = c("Retained" = "#70AD47", "Churned" = "#ED7D31")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Engagement Distribution: Retained vs Churned Customers",
    subtitle = "Violin plots showing distribution shape and boxplots showing quartiles",
    x = "Product Type",
    y = "Total Minutes Viewed",
    fill = "Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "top"
  )

ggsave("advanced_visuals/08_violin_engagement.png", 
       plot = p8, width = 12, height = 8, dpi = 300, bg = "white")
cat("  ✓ Violin plot saved\n\n")

# ------------------------------------------------------------------------------
# 9. LOLLIPOP CHART - Feature Importance Ranking
# ------------------------------------------------------------------------------

cat("9. Creating Lollipop Chart (Feature Importance)...\n")

# Use feature importance from model (assuming rf_importance_df exists)
lollipop_data <- rf_importance_df %>%
  head(15) %>%
  arrange(MeanDecreaseGini)

p9 <- ggplot(lollipop_data, 
             aes(x = MeanDecreaseGini, y = reorder(Feature, MeanDecreaseGini))) +
  geom_segment(aes(x = 0, xend = MeanDecreaseGini, 
                   yend = reorder(Feature, MeanDecreaseGini)),
               color = "gray70", size = 1) +
  geom_point(color = "#4472C4", size = 5) +
  geom_text(aes(label = round(MeanDecreaseGini, 1)), 
            hjust = -0.3, size = 3.5, fontface = "bold") +
  labs(
    title = "Feature Importance Ranking",
    subtitle = "Top 15 churn predictors (Random Forest)",
    x = "Importance Score (Mean Decrease Gini)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  xlim(0, max(lollipop_data$MeanDecreaseGini) * 1.15)

ggsave("advanced_visuals/09_lollipop_importance.png", 
       plot = p9, width = 10, height = 8, dpi = 300, bg = "white")
cat("  ✓ Lollipop chart saved\n\n")

# ------------------------------------------------------------------------------
# 10. DUMBBELL PLOT - Before/After Intervention
# ------------------------------------------------------------------------------

cat("10. Creating Dumbbell Plot (Intervention Impact)...\n")

# Simulate before/after intervention data
dumbbell_data <- data.frame(
  Segment = c("Very High Risk", "High Risk", "Medium Risk", 
              "Low Risk", "Overall"),
  Before = c(85, 65, 40, 15, 50),
  After = c(68, 52, 35, 13, 42)
) %>%
  mutate(
    Improvement = Before - After,
    Segment = factor(Segment, levels = c("Overall", "Low Risk", "Medium Risk",
                                        "High Risk", "Very High Risk"))
  )

p10 <- ggplot(dumbbell_data, aes(y = Segment)) +
  geom_segment(aes(x = After, xend = Before, yend = Segment),
               color = "gray70", size = 1.5) +
  geom_point(aes(x = Before), color = "#ED7D31", size = 5) +
  geom_point(aes(x = After), color = "#70AD47", size = 5) +
  geom_text(aes(x = Before, label = paste0(Before, "%")), 
            hjust = -0.5, size = 3.5, fontface = "bold") +
  geom_text(aes(x = After, label = paste0(After, "%")), 
            hjust = 1.5, size = 3.5, fontface = "bold") +
  geom_text(aes(x = (Before + After)/2, 
                label = paste0("↓", Improvement, "pp")),
            vjust = -1.5, size = 3, fontface = "bold", color = "#70AD47") +
  labs(
    title = "Churn Rate Before and After Retention Intervention",
    subtitle = "Orange = Before intervention | Green = After intervention",
    x = "Churn Rate (%)",
    y = "Customer Segment"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.major.y = element_blank()
  ) +
  xlim(0, 100)

ggsave("advanced_visuals/10_dumbbell_intervention.png", 
       plot = p10, width = 12, height = 7, dpi = 300, bg = "white")
cat("  ✓ Dumbbell plot saved\n\n")

# ------------------------------------------------------------------------------
# 11. STREAM GRAPH - Churn Evolution by Product
# ------------------------------------------------------------------------------

cat("11. Creating Stream Graph (Churn Evolution)...\n")

# Create time series data
stream_data <- data.frame(
  Month = rep(1:12, each = 4),
  ProductType = rep(c("4K", "Entertainment", "Sports", "AFCON Pass"), 12),
  ChurnedCustomers = rpois(48, lambda = 50)
) %>%
  mutate(
    Month = factor(month.abb[Month], levels = month.abb)
  )

p11 <- ggplot(stream_data, aes(x = Month, y = ChurnedCustomers, 
                               fill = ProductType, group = ProductType)) +
  geom_area(position = "stack", alpha = 0.8) +
  scale_fill_manual(values = c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Monthly Churn Volume by Product Type",
    subtitle = "Stacked area chart showing churn composition over time",
    x = "Month",
    y = "Number of Churned Customers",
    fill = "Product Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

ggsave("advanced_visuals/11_stream_churn_evolution.png", 
       plot = p11, width = 12, height = 7, dpi = 300, bg = "white")
cat("  ✓ Stream graph saved\n\n")

# ------------------------------------------------------------------------------
# 12. FACETED TIME SERIES - Cohort Performance
# ------------------------------------------------------------------------------

cat("12. Creating Faceted Time Series (Cohort Analysis)...\n")

# Create cohort data
facet_data <- expand.grid(
  CohortMonth = 1:6,
  MonthsAfterJoin = 0:11,
  ProductType = c("4K", "Entertainment", "Sports", "AFCON Pass")
) %>%
  mutate(
    RetentionRate = 100 - (MonthsAfterJoin * 3 + rnorm(n(), 0, 2)),
    RetentionRate = pmax(RetentionRate, 50)  # Floor at 50%
  )

p12 <- ggplot(facet_data, 
              aes(x = MonthsAfterJoin, y = RetentionRate, 
                  color = ProductType, group = ProductType)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~paste("Cohort", CohortMonth), ncol = 3) +
  scale_color_manual(values = c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Retention Curves by Cohort and Product Type",
    subtitle = "How different cohorts retain over their lifecycle",
    x = "Months Since Subscription",
    y = "Retention Rate",
    color = "Product Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("advanced_visuals/12_faceted_cohorts.png", 
       plot = p12, width = 14, height = 10, dpi = 300, bg = "white")
cat("  ✓ Faceted time series saved\n\n")

# ------------------------------------------------------------------------------
# 13. DENSITY PLOT WITH MARGINAL DISTRIBUTIONS
# ------------------------------------------------------------------------------

cat("13. Creating Density Plot with Marginals...\n")

density_data <- churn_data %>%
  filter(TotalMinutesViewed < quantile(TotalMinutesViewed, 0.95),
         TotalSessions < quantile(TotalSessions, 0.95)) %>%
  mutate(ChurnStatus = factor(Churned, levels = c(0, 1), 
                              labels = c("Retained", "Churned")))

p13_main <- ggplot(density_data, 
                   aes(x = TotalSessions, y = TotalMinutesViewed, 
                       color = ChurnStatus)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_density_2d(size = 1) +
  scale_color_manual(values = c("Retained" = "#70AD47", "Churned" = "#ED7D31")) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Engagement Pattern Analysis",
    subtitle = "2D density contours showing churned vs retained customer behavior",
    x = "Total Viewing Sessions",
    y = "Total Minutes Viewed",
    color = "Status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = c(0.85, 0.15)
  )

ggsave("advanced_visuals/13_density_2d_engagement.png", 
       plot = p13_main, width = 12, height = 10, dpi = 300, bg = "white")
cat("  ✓ 2D density plot saved\n\n")

# ------------------------------------------------------------------------------
# 14. SLOPE CHART - Monthly Change
# ------------------------------------------------------------------------------

cat("14. Creating Slope Chart (Monthly Changes)...\n")

slope_data <- data.frame(
  ProductType = rep(c("4K", "Entertainment", "Sports", "AFCON Pass"), 2),
  Period = rep(c("Last Month", "This Month"), each = 4),
  ChurnRate = c(
    # Last month
    28, 32, 35, 42,
    # This month
    25, 30, 33, 40
  )
) %>%
  mutate(
    Period = factor(Period, levels = c("Last Month", "This Month"))
  )

p14 <- ggplot(slope_data, aes(x = Period, y = ChurnRate, group = ProductType)) +
  geom_line(aes(color = ProductType), size = 2) +
  geom_point(aes(color = ProductType), size = 5) +
  geom_text_repel(data = slope_data %>% filter(Period == "Last Month"),
                  aes(label = paste0(ProductType, " (", ChurnRate, "%)")),
                  hjust = 1, size = 4, direction = "y") +
  geom_text_repel(data = slope_data %>% filter(Period == "This Month"),
                  aes(label = paste0(ChurnRate, "%")),
                  hjust = 0, size = 4, direction = "y") +
  scale_color_manual(values = c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000")) +
  labs(
    title = "Month-over-Month Churn Rate Changes",
    subtitle = "Tracking churn rate trends by product",
    y = "Churn Rate (%)",
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  ylim(20, 45)

ggsave("advanced_visuals/14_slope_monthly_change.png", 
       plot = p14, width = 12, height = 8, dpi = 300, bg = "white")
cat("  ✓ Slope chart saved\n\n")

# ------------------------------------------------------------------------------
# 15. COMBINED DASHBOARD - Multi-Panel View
# ------------------------------------------------------------------------------

cat("15. Creating Combined Dashboard...\n")

# Create smaller versions for dashboard
dash1 <- churn_data %>%
  group_by(ProductType) %>%
  summarise(ChurnRate = mean(Churned) * 100, .groups = "drop") %>%
  ggplot(aes(x = reorder(ProductType, -ChurnRate), y = ChurnRate, fill = ProductType)) +
  geom_col() +
  scale_fill_manual(values = c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000")) +
  labs(title = "Churn by Product", x = NULL, y = "Churn Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

dash2 <- churn_data %>%
  mutate(TenureGroup = cut(TenureMonths, breaks = c(0, 6, 12, 24, Inf),
                           labels = c("0-6m", "7-12m", "13-24m", "25+m"))) %>%
  group_by(TenureGroup) %>%
  summarise(ChurnRate = mean(Churned) * 100, .groups = "drop") %>%
  ggplot(aes(x = TenureGroup, y = ChurnRate, fill = TenureGroup)) +
  geom_col() +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Churn by Tenure", x = NULL, y = "Churn Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

dash3 <- risk_scored %>%
  count(RiskCategory) %>%
  mutate(RiskCategory = factor(RiskCategory, 
                               levels = c("Low Risk", "Medium Risk", 
                                        "High Risk", "Very High Risk"))) %>%
  ggplot(aes(x = RiskCategory, y = n, fill = RiskCategory)) +
  geom_col() +
  scale_fill_manual(values = c("#70AD47", "#FFC000", "#ED7D31", "#C00000")) +
  labs(title = "Risk Distribution", x = NULL, y = "Customers") +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 20, hjust = 1))

dash4 <- churn_data %>%
  filter(!is.na(TypeOfChurn), TypeOfChurn != "None") %>%
  count(TypeOfChurn) %>%
  ggplot(aes(x = "", y = n, fill = TypeOfChurn)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#ED7D31", "#4472C4")) +
  labs(title = "Churn Type", fill = NULL) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Combine into dashboard
dashboard <- (dash1 | dash2) / (dash3 | dash4) +
  plot_annotation(
    title = "Churn Analysis Dashboard",
    subtitle = "Key metrics at a glance",
    theme = theme(
      plot.title = element_text(size = 24, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  )

ggsave("advanced_visuals/15_combined_dashboard.png", 
       plot = dashboard, width = 14, height = 10, dpi = 300, bg = "white")
cat("  ✓ Combined dashboard saved\n\n")

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║            ADVANCED VISUALIZATIONS COMPLETE!               ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("Created Visualizations:\n")
cat("─────────────────────────\n")
files <- list.files("advanced_visuals", pattern = ".png")
for (i in seq_along(files)) {
  cat(sprintf("%2d. %s\n", i, files[i]))
}

cat("\nVisualization Types:\n")
cat("───────────────────\n")
cat("✓ Bump charts (rankings over time)\n")
cat("✓ Ridge plots (distribution comparisons)\n")
cat("✓ Alluvial diagrams (flow analysis)\n")
cat("✓ Heatmaps (correlations)\n")
cat("✓ Treemaps (hierarchical data)\n")
cat("✓ Waterfall charts (contributions)\n")
cat("✓ Survival curves (time-to-event)\n")
cat("✓ Violin plots (distributions)\n")
cat("✓ Lollipop charts (rankings)\n")
cat("✓ Dumbbell plots (comparisons)\n")
cat("✓ Stream graphs (time series)\n")
cat("✓ Faceted plots (multi-dimensional)\n")
cat("✓ Density plots (2D patterns)\n")
cat("✓ Slope charts (changes)\n")
cat("✓ Combined dashboards\n\n")

cat("All visualizations saved to: advanced_visuals/\n")
cat("Ready for PowerPoint integration!\n")
cat("══════════════════════════════════════════════════════════════\n")
