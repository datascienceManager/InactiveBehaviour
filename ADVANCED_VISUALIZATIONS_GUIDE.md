# Advanced ggplot2 Visualizations for Churn Analysis

Complete guide to creating sophisticated data visualizations for business analytics.

## 📊 Visualization Catalog

### 1. **Bump Charts** 🏆
**Purpose**: Track ranking changes over time  
**Use Case**: Monitor which products have highest/lowest churn rates month-over-month  
**Best For**: Competitive analysis, performance tracking  
**Key Insight**: "Which product went from best to worst performer?"

**When to Use**:
- Tracking rankings over time (products, regions, channels)
- Showing position changes
- Highlighting winners and losers

**Business Questions Answered**:
- Which products improved/declined in performance?
- Are there seasonal ranking changes?
- Which segments are consistently top/bottom performers?

---

### 2. **Ridge Plots** 🏔️
**Purpose**: Compare distributions across multiple categories  
**Use Case**: Show engagement patterns across different tenure groups  
**Best For**: Distribution comparisons, pattern recognition  
**Key Insight**: "Do long-term customers engage differently than new ones?"

**When to Use**:
- Comparing distributions across groups
- Showing overlapping patterns
- Visualizing data density

**Business Questions Answered**:
- How does engagement vary by customer segment?
- Are there distinct behavioral patterns?
- Which groups have similar/different distributions?

---

### 3. **Alluvial/Sankey Diagrams** 🌊
**Purpose**: Visualize flow between states  
**Use Case**: Track customer journey from engagement → risk → outcome  
**Best For**: Flow analysis, state transitions, funnel visualization  
**Key Insight**: "What path do customers take from sign-up to churn?"

**When to Use**:
- Customer journey mapping
- State transition analysis
- Understanding flow patterns

**Business Questions Answered**:
- What percentage of low-engagement users become high-risk?
- Which paths lead to churn?
- Where in the journey do most customers drop off?

---

### 4. **Heatmaps** 🔥
**Purpose**: Show correlations between variables  
**Use Case**: Identify which features are related to each other  
**Best For**: Correlation analysis, pattern discovery  
**Key Insight**: "Which factors move together?"

**When to Use**:
- Feature correlation analysis
- Finding redundant variables
- Identifying feature clusters

**Business Questions Answered**:
- Which metrics are most/least correlated?
- Are there multicollinearity issues?
- What feature groups exist?

---

### 5. **Treemaps** 🌳
**Purpose**: Display hierarchical data with size and color  
**Use Case**: Show revenue at risk by product and segment  
**Best For**: Part-to-whole relationships, prioritization  
**Key Insight**: "Where is our biggest revenue risk?"

**When to Use**:
- Showing size and additional dimension
- Hierarchical data
- Portfolio analysis

**Business Questions Answered**:
- Which segments represent the most revenue at risk?
- How do products compare in terms of impact?
- Where should we focus retention efforts?

---

### 6. **Waterfall Charts** 💧
**Purpose**: Show cumulative effect of sequential changes  
**Use Case**: Break down churn rate into contributing factors  
**Best For**: Contribution analysis, variance explanation  
**Key Insight**: "Which factors drive our churn rate up?"

**When to Use**:
- Explaining variance or changes
- Showing cumulative effects
- Breaking down totals

**Business Questions Answered**:
- What contributes most to our churn rate?
- Which factors reduce churn?
- How do we get from baseline to actual?

---

### 7. **Survival Curves** ⏱️
**Purpose**: Analyze time-to-event (time-to-churn)  
**Use Case**: Compare how long different products retain customers  
**Best For**: Lifetime analysis, hazard comparison  
**Key Insight**: "How long do different customer types stay?"

**When to Use**:
- Time-to-churn analysis
- Comparing retention curves
- Understanding customer lifetime

**Business Questions Answered**:
- Which products retain customers longest?
- When is churn risk highest?
- What's the median customer lifetime?

---

### 8. **Violin Plots** 🎻
**Purpose**: Show full distribution shape + quartiles  
**Use Case**: Compare engagement distributions across segments  
**Best For**: Distribution comparison, outlier detection  
**Key Insight**: "Do churned customers have different engagement patterns?"

**When to Use**:
- Comparing full distributions
- Showing bimodal patterns
- Combining density + box plot info

**Business Questions Answered**:
- How do distributions differ between groups?
- Are there outliers or unusual patterns?
- What's the typical range and spread?

---

### 9. **Lollipop Charts** 🍭
**Purpose**: Cleaner alternative to bar charts for rankings  
**Use Case**: Display feature importance scores  
**Best For**: Rankings, comparisons with many categories  
**Key Insight**: "Which features matter most for prediction?"

**When to Use**:
- Showing rankings cleanly
- Many categories to compare
- When bars would be cluttered

**Business Questions Answered**:
- What are the top drivers?
- How much more important is #1 vs #10?
- What factors can we ignore?

---

### 10. **Dumbbell Plots** 🏋️
**Purpose**: Show before/after or gap comparisons  
**Use Case**: Display churn rates before and after intervention  
**Best For**: A/B testing, intervention analysis  
**Key Insight**: "Did our retention campaign work?"

**When to Use**:
- Before/after comparisons
- Measuring change
- Gap analysis

**Business Questions Answered**:
- What was the impact of intervention?
- Which segments improved most?
- Was the change significant?

---

### 11. **Stream Graphs** 🌊
**Purpose**: Show composition changes over time  
**Use Case**: Track how churn composition changes monthly  
**Best For**: Time series with categories, trending  
**Key Insight**: "How has our churn mix evolved?"

**When to Use**:
- Time series with multiple categories
- Showing composition changes
- Trend analysis

**Business Questions Answered**:
- Is churn increasing/decreasing?
- Which segments are growing?
- Are there seasonal patterns?

---

### 12. **Faceted Plots** 📱
**Purpose**: Small multiples for easy comparison  
**Use Case**: Compare cohort retention curves side-by-side  
**Best For**: Multi-dimensional analysis, pattern comparison  
**Key Insight**: "Do different cohorts behave similarly?"

**When to Use**:
- Comparing many groups
- Showing patterns across dimensions
- When overlaying would be messy

**Business Questions Answered**:
- How do cohorts compare?
- Are patterns consistent?
- Which groups are outliers?

---

### 13. **2D Density Plots** 🎯
**Purpose**: Show concentration of points in 2D space  
**Use Case**: Identify engagement patterns of churned vs retained  
**Best For**: Bivariate analysis, cluster detection  
**Key Insight**: "Where do churned customers cluster?"

**When to Use**:
- Two continuous variables
- Finding clusters
- Showing concentration

**Business Questions Answered**:
- Are there distinct customer clusters?
- Where do churned customers concentrate?
- What defines high-risk behavior?

---

### 14. **Slope Charts** 📐
**Purpose**: Show change between two time points  
**Use Case**: Display month-over-month churn rate changes  
**Best For**: Period-over-period comparison  
**Key Insight**: "Which products improved/declined?"

**When to Use**:
- Two time periods
- Emphasizing change direction
- Ranking shifts

**Business Questions Answered**:
- What changed between periods?
- Which improved most/least?
- Are there concerning trends?

---

### 15. **Combined Dashboards** 📊
**Purpose**: Multiple views in one visualization  
**Use Case**: Executive summary with key metrics  
**Best For**: High-level overview, presentations  
**Key Insight**: "What's the complete picture at a glance?"

**When to Use**:
- Executive summaries
- Multi-metric monitoring
- Presentations

**Business Questions Answered**:
- What are all key metrics?
- How do metrics relate?
- What's the overall status?

---

## 🎨 Choosing the Right Visualization

### Decision Tree:

**Comparing Categories?**
- Few categories (< 5): Bar chart or Lollipop
- Many categories (5-20): Lollipop or Treemap
- With hierarchy: Treemap

**Showing Distributions?**
- Single distribution: Histogram or Density
- Multiple distributions: Ridge plot or Violin plot
- Two variables: 2D Density or Scatter

**Tracking Over Time?**
- Single metric: Line chart
- Multiple metrics: Faceted lines or Stream graph
- Rankings: Bump chart
- Two periods: Slope chart

**Showing Relationships?**
- Correlation: Heatmap
- Flow/Journey: Alluvial diagram
- Network: Network graph

**Showing Composition?**
- Static: Pie or Treemap
- Over time: Stream graph or Stacked area

**Showing Change?**
- Before/After: Dumbbell plot
- Cumulative effect: Waterfall chart
- Over time: Line or Area chart

**Survival/Time-to-Event?**
- Kaplan-Meier curves (Survival plots)

---

## 💼 Business Use Cases

### Executive Presentations
**Best visualizations**:
- Combined dashboard (quick overview)
- Treemap (revenue at risk)
- Waterfall (churn drivers)
- Dumbbell (intervention impact)

### Deep Dive Analysis
**Best visualizations**:
- Heatmap (correlations)
- Ridge plots (distributions)
- Alluvial (customer journey)
- Survival curves (lifetime analysis)

### Monthly Reports
**Best visualizations**:
- Bump chart (ranking changes)
- Slope chart (month-over-month)
- Stream graph (trend evolution)
- Faceted cohorts (performance tracking)

### Model Documentation
**Best visualizations**:
- Lollipop (feature importance)
- 2D Density (cluster identification)
- Violin plots (segment differences)
- Heatmap (feature correlations)

---

## 🛠️ Customization Tips

### Color Palettes

```r
# Professional corporate
corporate <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#70AD47")

# Diverging (for before/after, good/bad)
diverging <- c("#ED7D31", "#FFC000", "#FFFFFF", "#A5A5A5", "#4472C4")

# Sequential (for heatmaps, intensity)
sequential <- c("#FFFFFF", "#9DC3E6", "#4472C4", "#203864")

# Categorical (for distinct groups)
categorical <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")

# Use in ggplot
scale_fill_manual(values = corporate)
scale_color_brewer(palette = "Set2")
scale_fill_viridis(option = "D")
```

### Themes

```r
# Clean minimal
theme_minimal(base_size = 14)

# Classic
theme_bw(base_size = 14)

# Economist style
theme_economist()  # requires ggthemes

# Custom theme
my_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )
```

### Saving Options

```r
# High resolution for printing
ggsave("plot.png", width = 12, height = 8, dpi = 300, bg = "white")

# Presentation size
ggsave("plot.png", width = 10, height = 7.5, dpi = 200, bg = "white")

# Social media
ggsave("plot.png", width = 8, height = 8, dpi = 150, bg = "white")

# Vector format (scalable)
ggsave("plot.pdf", width = 12, height = 8)
```

---

## 📚 Package Reference

### Core Packages
- `ggplot2` - Base plotting system
- `dplyr` - Data manipulation
- `tidyr` - Data reshaping

### Specialized Packages
- `ggridges` - Ridge plots
- `ggbump` - Bump charts
- `ggalluvial` - Alluvial/Sankey diagrams
- `treemapify` - Treemaps
- `survival` + `survminer` - Survival curves
- `patchwork` - Combining plots
- `ggrepel` - Non-overlapping labels

### Enhancement Packages
- `viridis` - Color-blind friendly palettes
- `scales` - Axis formatting
- `ggtext` - Rich text in plots
- `gganimate` - Animated plots

---

## 🚀 Quick Start Examples

### Bump Chart
```r
library(ggbump)
ggplot(data, aes(x = time, y = rank, color = group)) +
  geom_bump(size = 2, smooth = 8) +
  geom_point(size = 4) +
  scale_y_reverse()
```

### Ridge Plot
```r
library(ggridges)
ggplot(data, aes(x = value, y = category, fill = category)) +
  geom_density_ridges(alpha = 0.7) +
  theme_ridges()
```

### Alluvial Diagram
```r
library(ggalluvial)
ggplot(data, aes(axis1 = stage1, axis2 = stage2, y = count)) +
  geom_alluvium(aes(fill = outcome)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)))
```

### Treemap
```r
library(treemapify)
ggplot(data, aes(area = value, fill = category, label = name)) +
  geom_treemap() +
  geom_treemap_text(color = "white", place = "centre")
```

### Survival Curve
```r
library(survival)
library(survminer)
fit <- survfit(Surv(time, event) ~ group, data = data)
ggsurvplot(fit, pval = TRUE, conf.int = TRUE)
```

---

## 📈 Integration with PowerPoint

All these visualizations are saved as high-resolution PNG files ready for PowerPoint:

```python
# In Python
builder.add_image_slide(
    "Product Rankings Over Time",
    "01_bump_chart_rankings.png"
)

builder.add_image_slide(
    "Customer Journey Analysis",
    "03_alluvial_customer_journey.png"
)
```

---

## 🎯 Best Practices

1. **Choose the right chart** - Match visualization to data type
2. **Keep it simple** - Don't overcomplicate
3. **Use color purposefully** - Highlight important information
4. **Label clearly** - Make charts self-explanatory
5. **Consider your audience** - Executives vs analysts need different detail
6. **Test readability** - Ensure charts work in print and projection
7. **Be consistent** - Use same colors/style throughout presentation
8. **Tell a story** - Each chart should have a clear message

---

## 🔗 Resources

- [ggplot2 documentation](https://ggplot2.tidyverse.org/)
- [R Graph Gallery](https://r-graph-gallery.com/)
- [Data Visualization Catalogue](https://datavizcatalogue.com/)
- [From Data to Viz](https://www.data-to-viz.com/)

---

**Created for**: OTT Platform Churn Analysis  
**Version**: 1.0  
**Last Updated**: 2024
