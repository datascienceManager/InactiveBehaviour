# Churn Analysis - Complete Project

Comprehensive churn analysis and predictive modeling framework using R (dplyr) and Python (python-pptx).

## 📋 Project Overview

This project provides a complete end-to-end solution for:

1. **Exploratory Data Analysis** - Understanding churn patterns and drivers
2. **Statistical Analysis** - Identifying significant factors
3. **Predictive Modeling** - Building machine learning models to predict churn
4. **Risk Segmentation** - Categorizing customers by churn risk
5. **Visualization** - Creating publication-quality charts and tables
6. **Reporting** - Automated PowerPoint presentation generation

## 🎯 Business Context

**Industry**: OTT Streaming Platform (Media)

**Subscription Model**:
- Monthly renewals
- Multiple product types (4K, Entertainment, Sports, AFCON Pass)
- Winback scenarios (reactivated customers)
- Both voluntary and involuntary churn

**Data Sources**:
- **Subscriptions**: Customer subscription details, product type, channel, cancellation status
- **Viewing**: Customer engagement data, viewing minutes, content preferences

## 📁 Project Structure

```
churn-analysis/
├── R Scripts (Analysis & Modeling)
│   ├── master_churn_workflow.R          # Master script - runs everything
│   ├── churn_analysis.R                  # EDA and feature engineering
│   ├── churn_modeling.R                  # Predictive model development
│   └── churn_visualizations.R            # Chart generation
│
├── Python Scripts (Reporting)
│   └── create_churn_presentation.py      # PowerPoint generation
│
├── Outputs
│   ├── churn_visuals/                    # All charts (PNG)
│   ├── churn_tables/                     # All gt tables (PNG)
│   ├── churn_predictions.csv             # Customer predictions
│   ├── feature_importance.csv            # Feature rankings
│   ├── churn_rf_model.rds               # Trained model
│   └── Churn_Analysis_Report.pptx       # Final presentation
│
└── README.md                             # This file
```

## 🚀 Quick Start

### Prerequisites

**R Packages**:
```r
install.packages(c(
  "dplyr", "tidyr", "lubridate", "ggplot2", "gt",
  "randomForest", "caret", "pROC", "corrplot", "scales",
  "xgboost", "e1071"
))
```

**Python Packages**:
```bash
pip install python-pptx
```

### Step 1: Prepare Your Data

Replace the sample data in `master_churn_workflow.R` with your actual data:

```r
# Load your subscription data
subscriptions <- read.csv("your_subscriptions.csv")

# Load your viewing data  
viewing <- read.csv("your_viewing_data.csv")
```

**Required Subscription Columns**:
- `SubscriptionID` - Unique subscription identifier
- `CustomerID` - Unique customer identifier
- `ProductType` - Product tier (4K, Entertainment, Sports, AFCON Pass)
- `Channel` - Acquisition channel (Web, Mobile, TV, Partner)
- `SubscriptionDate` - Subscription start date
- `Cancellation` - Binary flag (0/1)
- `TypeOfChurn` - None, Voluntary, or Involuntary
- `MonthlyPrice` - Subscription price
- `IsWinback` - Binary flag for reactivated customers
- `TenureMonths` - Months as customer

**Required Viewing Columns**:
- `ViewingID` - Unique viewing record ID
- `CustomerID` - Customer identifier (matches subscriptions)
- `MinutesViewed` - Duration of viewing session
- `AssetID` - Content identifier
- `TypeOfAsset` - Content category (Sports, Entertainment, Movies, Series)
- `ViewingDate` - Date of viewing

### Step 2: Run the Complete Analysis

```r
# Run master script (executes entire workflow)
Rscript master_churn_workflow.R
```

This will:
- ✅ Perform feature engineering
- ✅ Run exploratory data analysis
- ✅ Build 3 predictive models (Logistic, Random Forest, XGBoost)
- ✅ Generate 15+ visualizations
- ✅ Create 5 summary tables
- ✅ Save predictions and model outputs

**Expected Runtime**: 5-15 minutes (depending on data size)

### Step 3: Create PowerPoint Presentation

```bash
python create_churn_presentation.py
```

Output: `Churn_Analysis_Report.pptx` (30+ slides)

## 📊 Analysis Components

### 1. Feature Engineering

**Created Features**:
- Engagement metrics (total minutes, sessions, frequency)
- Recency indicators (days since last view)
- Content preferences (% by category)
- RFM segmentation (Recency, Frequency, Monetary)
- Risk scoring (composite churn risk score)
- Behavioral flags (low engagement, inactive, etc.)

### 2. Exploratory Data Analysis

**Key Analyses**:
- Overall churn rate and trends
- Churn by product type
- Churn by tenure category
- Churn by acquisition channel
- Engagement impact analysis
- Voluntary vs involuntary churn breakdown
- Cohort retention analysis
- RFM segmentation analysis

### 3. Statistical Testing

**Tests Performed**:
- T-tests (engagement differences)
- Chi-square tests (categorical associations)
- Correlation analysis
- Significance testing for all major factors

### 4. Predictive Modeling

**Models Built**:

| Model | Purpose | Expected Accuracy |
|-------|---------|------------------|
| Logistic Regression | Baseline interpretable model | 70-75% |
| Random Forest | Primary production model | 80-85% |
| XGBoost | High-performance alternative | 82-87% |

**Model Outputs**:
- Predicted churn probability for each customer
- Risk segment classification
- Feature importance rankings
- Performance metrics (Accuracy, Precision, Recall, F1, AUC)

### 5. Risk Segmentation

Customers are segmented into risk categories:

| Risk Level | Criteria | Typical Churn Rate |
|------------|----------|-------------------|
| Very High Risk | Probability > 0.75 | 80-95% |
| High Risk | Probability 0.50-0.75 | 50-80% |
| Medium Risk | Probability 0.25-0.50 | 25-50% |
| Low Risk | Probability < 0.25 | 5-25% |

## 📈 Key Visualizations Created

1. **Overall churn rate** (pie chart)
2. **Churn by product type** (bar chart)
3. **Churn by tenure** (bar chart)
4. **Engagement distribution** (histogram)
5. **Churn type breakdown** (pie chart)
6. **Churn by channel** (bar chart)
7. **Cohort retention curve** (line chart)
8. **RFM segmentation** (bar chart)
9. **Content preferences** (grouped bar)
10. **Risk score distribution** (histogram)
11. **ROC curves** (line chart)
12. **Feature importance** (horizontal bar)
13. **Calibration plot** (scatter)
14. **Risk validation** (bar chart)
15. **Business metrics** (KPI dashboard)

All visualizations are saved as high-resolution PNG files (300 DPI) ready for PowerPoint.

## 🎨 GT Tables Created

1. **Executive Summary** - Key metrics overview
2. **Product Analysis** - Performance by product type
3. **Model Comparison** - Algorithm performance metrics
4. **Risk Segmentation** - Customer distribution by risk
5. **Feature Importance** - Top 10 predictive features

All tables include:
- Professional formatting
- Conditional color coding
- Currency/percentage formatting
- Clear headers and labels

## 📋 Model Performance Expectations

Based on industry benchmarks and typical OTT data:

| Metric | Expected Range | Good Performance |
|--------|----------------|------------------|
| Accuracy | 75-85% | > 80% |
| Precision | 65-80% | > 75% |
| Recall | 60-75% | > 70% |
| F1-Score | 65-77% | > 72% |
| AUC | 0.80-0.90 | > 0.85 |

## 🎯 Actionable Outputs

### 1. Customer Predictions

`churn_predictions.csv` contains:
- CustomerID
- Actual churn status
- Predicted churn probability
- Predicted churn class
- Risk segment

**Use Case**: Score all active customers daily/weekly to identify at-risk accounts

### 2. Feature Importance

`feature_importance.csv` shows:
- Most important churn drivers
- Impact scores
- Ranking

**Use Case**: Focus retention efforts on modifiable factors (e.g., engagement, content)

### 3. Risk Segments

**Use Case**: Tailor interventions by risk level:
- **Very High Risk**: Aggressive retention offers, personal outreach
- **High Risk**: Targeted content recommendations, loyalty perks
- **Medium Risk**: Engagement campaigns, feature education
- **Low Risk**: Standard nurture, upsell opportunities

## 📊 Business Impact Calculation

The analysis includes business impact estimates:

```r
# Example calculation
high_risk_customers <- 500
intervention_success_rate <- 0.20  # 20% saved
avg_customer_lifetime_value <- $240

potential_revenue_saved = 500 × 0.20 × $240 = $24,000
```

Customize these assumptions based on your business metrics.

## 🔧 Customization Guide

### Adjusting Thresholds

**Churn Risk Score** (in `churn_analysis.R`):
```r
ChurnRiskScore = TenureRisk + EngagementRisk + RecencyRisk + WinbackRisk

# Adjust weights:
ChurnRiskScore = (TenureRisk * 1.5) + (EngagementRisk * 2.0) + ...
```

**Risk Categories** (in `churn_modeling.R`):
```r
RiskCategory = case_when(
  PredictedProb >= 0.75 ~ "Very High Risk",  # Adjust threshold
  PredictedProb >= 0.50 ~ "High Risk",       # Adjust threshold
  ...
)
```

### Adding Custom Features

In `churn_analysis.R`, add to the feature engineering section:

```r
viewing_features <- viewing %>%
  group_by(CustomerID) %>%
  summarise(
    # Add your custom features here
    WeekendViewingPct = sum(wday(ViewingDate) %in% c(1, 7)) / n(),
    PrimeTimeViewing = sum(hour(ViewingDate) >= 19 & hour(ViewingDate) <= 23),
    # ... more features
  )
```

### Changing Model Parameters

**Random Forest** (in `churn_modeling.R`):
```r
rf_model <- randomForest(
  Churned ~ .,
  data = train_data,
  ntree = 1000,              # Increase trees
  mtry = 5,                  # Change feature sampling
  nodesize = 10,             # Minimum node size
  importance = TRUE
)
```

**XGBoost** (in `churn_modeling.R`):
```r
xgb_params <- list(
  objective = "binary:logistic",
  eta = 0.05,                # Learning rate
  max_depth = 8,             # Tree depth
  subsample = 0.7,           # Row sampling
  colsample_bytree = 0.7     # Column sampling
)
```

## 🐛 Troubleshooting

### Issue: Missing visualizations

**Solution**: Check that `churn_visuals/` directory was created
```r
if (!dir.exists("churn_visuals")) dir.create("churn_visuals")
```

### Issue: Model errors with small datasets

**Solution**: Reduce model complexity
```r
# Use fewer trees
rf_model <- randomForest(..., ntree = 100)

# Or use simpler model
logit_model <- glm(Churned ~ ., family = binomial())
```

### Issue: Memory errors with large datasets

**Solution**: Sample your data
```r
# Work with 10% sample during development
sample_data <- churn_data %>% sample_frac(0.10)
```

### Issue: Python can't find images

**Solution**: Check working directory
```python
import os
print(os.getcwd())
os.chdir("path/to/your/project")
```

## 📚 Understanding the Output

### churn_predictions.csv

| Column | Description | Example |
|--------|-------------|---------|
| CustomerID | Unique identifier | C12345 |
| ActualChurn | True status | Churned |
| PredictedProb | Churn probability | 0.78 |
| PredictedClass | Predicted status | Churned |
| RiskSegment | Risk category | Very High Risk |

**Action**: Filter for "Very High Risk" and target with retention campaigns

### feature_importance.csv

| Column | Description |
|--------|-------------|
| Feature | Variable name |
| MeanDecreaseAccuracy | Impact on accuracy |
| MeanDecreaseGini | Impact on purity |

**Action**: Focus on top 5-10 features for retention strategy

## 🎓 Best Practices

### Data Quality

1. **Handle missing values** appropriately
   - Remove incomplete records
   - Impute with median/mode
   - Create "missing" flags

2. **Remove outliers** carefully
   - Check for data entry errors
   - Use domain knowledge
   - Consider capping extreme values

3. **Validate data types**
   - Dates as Date objects
   - Categories as factors
   - Numbers as numeric

### Model Development

1. **Use train-test-validation split** for large datasets
2. **Cross-validate** to ensure stability
3. **Monitor for overfitting** (train vs test performance)
4. **Retrain regularly** (monthly/quarterly) as patterns change
5. **A/B test** interventions to measure real impact

### Business Implementation

1. **Start with high-risk customers** (highest ROI)
2. **Test interventions** before full rollout
3. **Track intervention costs** vs saved revenue
4. **Monitor** unintended consequences (e.g., training customers to expect discounts)
5. **Iterate** based on results

## 📈 Expected ROI

Industry benchmarks suggest:

- **10-20% churn reduction** is achievable with predictive models
- **$50-200 customer lifetime value** increase through better retention
- **3-5x ROI** on retention campaigns when properly targeted

## 🔄 Maintenance Schedule

- **Daily**: Score active customers, flag new high-risk accounts
- **Weekly**: Review newly at-risk customers, send interventions
- **Monthly**: Analyze intervention effectiveness, adjust strategies
- **Quarterly**: Retrain model, review feature importance changes
- **Annually**: Full model audit, consider new data sources/features

## 📞 Support & Questions

For questions about:
- **R analysis**: Review comments in each .R script
- **Python presentation**: Check create_churn_presentation.py docstrings
- **Customization**: See the Customization Guide above
- **Business strategy**: Review the Recommendations slides in output

## 🎉 Success Metrics

Track these KPIs to measure success:

1. **Model Performance**: Maintain AUC > 0.85
2. **Churn Rate**: Reduce by 15-20%
3. **Revenue Saved**: Track prevented churn value
4. **Intervention Efficiency**: Cost per customer saved
5. **Early Detection**: % of churners caught in advance

## 📄 License

This project is provided as-is for educational and business use.

---

**Version**: 1.0  
**Last Updated**: 2024  
**Maintained by**: Data Analytics Team

For updates and improvements, check the project repository.
