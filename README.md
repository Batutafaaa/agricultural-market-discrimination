# To Enter or To Not Enter: Market Discrimination Against Marginalized Farmers

## Project Overview
This research analyzes the **"Final Mile" gap** in Indian agriculture, shifting the focus from production-side disadvantages (land and credit) to structural disadvantages at the market gate. Using the **NSS 77th Round (2019)**, the study investigates whether Scheduled Caste (SC) and Scheduled Tribe (ST) farmers face distinct mechanisms of discrimination: **Exclusion** from formal markets versus **Extraction** through predatory pricing.

## Research Objectives
1.  **Mechanism Identification**: Distinguish between market access barriers for ST farmers and price penalties for SC farmers.
2.  **Intersectional Analysis**: Test if land ownership mitigates or intensifies caste-based bias.
3.  **Spatial vs. Social**: Determine if price gaps are driven by geographic location or social identity.

## Dataset & Variables
* **Source**: NSS 77th Round, Schedule 33.1 (2019).
* **Sample**: 41,641 sales records.
* **Composition**: General (27.3%), OBC (39.6%), SC (11.0%), and ST (22.1%).
* **Key Metrics**: 
    * **Unit Price**: Value sold divided by quantity[cite: 25].
    * **Market Choice**: Formal (Mandi/Coop) vs. Informal (Private Trader).
    * **Land Categories**: Marginal (<0.5ha), Small (0.5-2ha), Medium (2-5ha), and Large (>5ha).

## Technical Implementation

### Econometric Models
* **Model 1 (Market Access)**: A probability model testing the likelihood of selling to private traders based on caste.
* **Model 2 (Price Discrimination)**: A log-linear model using **District Fixed Effects** to compare prices within the same geographic locale.

### File Structure


```bash
├── 01_setup.R                 # Configuration and package loading
├── 02_dataloading_BULLETPROOF.R  # Robust data loading with error handling
├── 03_cleaning_FINAL_FIXED.R  # Data cleaning and variable construction
├── 04_analysis_ROBUST_FIXED.R # Main econometric analysis
├── 05_visualize_land_heterogeneity.R  # Land size heterogeneity plots
├── 06_analysis_ST_Land_Penalty.R  # ST-specific analysis
├── nss77_robust_results.rds   # Saved analysis results
├── nss77_results_summary.csv  # Summary table of key findings
├── price_gaps_by_caste.png    # Visualization 1
├── market_access_by_caste.png # Visualization 2
└── plot_marginal_penalty.png  # Visualization 3
```

## Key Findings 

### 1. The Market Access Divide
* **SC Farmers**: 10.1 percentage points **less likely** to sell to private traders (p < 0.01, highly significant), successfully accessing regulated Mandis.
* **ST Farmers**: Show different market access patterns, with greater reliance on informal channels.
* **Insight**: Different mechanisms—**Geographic Exclusion** for STs versus **Market Access Success** for SCs.

### 2. Price Discrimination Patterns
* **Medium SC Farmers**: Face **statistically significant -3.8% price penalty** (p < 0.05)
* **Medium ST Farmers**: Face **highly significant -5.8% penalty** (p = 0.004)
* **Marginal SC Farmers**: Show -8.2% penalty but with wide confidence intervals [-29.5%, 19.4%] (not statistically significant)
* **Large SC Farmers**: No statistically significant price differences

### 3. Statistical Reality vs. Economic Magnitude
* **Robust Findings**: Market access discrimination against SC farmers (-10.1%, p<0.01) and price penalties for medium farmers
* **Suggestive Patterns**: Large penalties for marginal farmers lack statistical precision
* **Data Anomaly**: Marginal ST farmers show +69.8% unexplained premium (likely data issue)

## Methodological Robustness

### Strengths
* **Fixed Effects**: District (82), Crop, and State fixed effects
* **Clustered Standard Errors**: At Primary Sampling Unit (village) level  
* **Sampling Weights**: NSS population weights applied throughout
* **Multiple Specifications**: 5 price models + 2 agency models with different controls

### Limitations
* **Cross-Sectional Data**: Cannot establish causality
* **Small Marginal Samples**: Imprecise estimates for most vulnerable farmers
* **Missing Mechanisms**: No direct measures of bargaining power or social networks
* **Sample Selection**: Only crop-selling households included
* **Data Issues**: ST marginal premium (+69.8%) suggests potential data quality concerns

## Future Research Directions

### 1. Longitudinal Econometrics (Multi-Year Data)
* **Pseudo-Panel Construction**: Aggregate NSS 70th and 77th rounds at the District-Caste-Land cohort level to track the persistence of discrimination over time.
* **Difference-in-Differences (DiD)**: Use the staggered rollout of e-NAM (electronic markets) to see if digital integration reduces the "intersectional penalty" for marginal SC farmers.

### 2. Capturing Transactional Nuances (Fieldwork)
* **Audit Studies**: Conduct field experiments using identical crop quality to isolate pure caste bias from unobserved quality differences.
* **Interlocked Market Surveys**: Investigate if ST reliance on traders is driven by debt-traps (credit-output linkages) rather than just geographic distance.
* **Bargaining Observations**: Document "soft" barriers at the Mandi, such as wait times, weighing fraud, or arbitrary quality rejections.
