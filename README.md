# Adjusting for adherence in randomized trials when adherence is measured as a continuous variable: an application to the Lipid Research Clinics Coronary Primary Prevention Trial
# Introduction
Here we provide the code to reproduce the analysis described in: 

### Citation

> Wanis KN, Madenci AL, Hernán MA, Murray EJ. Adjusting for adherence in randomized trials when adherence is measured as a continuous variable: An application to the Lipid Research Clinics Coronary Primary Prevention Trial. Clinical Trials. 2020 May 15. doi: 10.1177/1740774520920893

# Abstract

**Background:** Clinicians and patients may be more interested in per-protocol effect estimates than intention-to-treat effect estimates from randomized trials. However, per-protocol effect estimates may be biased due to insufficient adjustment for prognostic factors that predict adherence. Adjustment for this bias is possible when appropriate methods, such as inverse probability (IP) weighting, are used. But, when adherence is measured as a continuous variable, constructing these weights can be challenging.

**Methods:** In the placebo arm of the Lipid Research Clinics Coronary Primary Prevention Trial, we estimated the 7-year cumulative incidence of coronary heart disease under 100% adherence and 0% adherence to placebo. We used dose-response discrete-hazards models with IP weighting to adjust for pre- and post-randomization covariates. We considered several continuous distributions for constructing the IP weights.

**Results:** The risk difference estimate for 100% adherence compared with 0% adherence ranged from -7.7 to -6.1 percentage points without adjustment for baseline and post-baseline covariates, and ranged from -1.8 to 2.2 percentage points with adjustment using inverse probability weights, depending on the dose-response model and IP weight distribution used.  

**Conclusions:** Methods which appropriately adjust for time-varying post-randomization variables can explain away much of the bias in the ‘effect’ of adherence to placebo. When considering continuous adherence, investigators should consider several models as estimates may be sensitive to the model chosen. 

# Organization
- `main.R` — R file which contains further details describing the analysis, and which reproduces the main analyses by running the code contained in `scripts`.
- `scripts`  — Scripts to reproduce the main analyses in the manuscript.
- `data`  — Raw data should be inserted in this folder. Data for the LRCCPPT are available by application to the NHLBI using the BIOLINCC portal at https://biolincc.nhlbi.nih.gov/studies/lrccppt/.

# Correspondence
If you have any questions, comments, or discover an error, please contact Kerollos Wanis at knwanis@g.harvard.edu
