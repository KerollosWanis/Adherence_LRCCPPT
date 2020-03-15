
# This code reproduces the analyses from Wanis K, Madenci A, Hernán M, and Murray E.
# Adjusting for adherence in randomized trials when adherence is measured as a continuous variable: 
# an application to the Lipid Research Clinics Coronary Primary Prevention Trial 
# published in Clinical Trials.

# First load and install the following packages which we will need for data cleaning and analysis.

requiredPackages = c('MASS','tidyverse','data.table',
                     'pbapply','lazyeval', 
                     'parallel', 'nnet',
                     'splines', 'truncnorm',
                     'dglm', 'betareg')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# We will need several functions which can be found in \code\functions.R

source("./scripts/functions.R")

# The data for this analysis can be obtained by application to the NHLBI using the BIOLINCC portal at https://biolincc.nhlbi.nih.gov/studies/lrccppt/.
# We need to clean the data using the code in \code\data_cleaning.R

# The data cleaning step may take several minutes.

source("./scripts/data_cleaning.R")

# Now we define the models which will be used to estimate the numerator and denominator
# of our stabilized inverse probability weights.
# Further details can be found in the manuscript's supplementary material.

source("./scripts/weight_formulas.R")

# In order to obtain the point estimates, we fit the models described in the manuscript.

# The models may take several minutes to fit.

source("./scripts/result_point_estimates.R")


# We can now print the risk differences reported in the manuscript.


# Table 1 with point estimates only

colnames(results) <- c('Unadjusted', 'Adjusted for baseline covariates',
                       'Adjusted for baseline and post-baseline covariates with adherence modelled using a truncated normal distribution assuming constant variance (homoscedastic)',
                       'Adjusted for baseline and post-baseline covariates with adherence modelled using a truncated normal distribution with modeled variance (heteroscedastic)',
                       'Adjusted for baseline and post-baseline covariates with adherence modelled using a hurdle model with a gamma distribution assuming constant dispersion',
                       'Adjusted for baseline and post-baseline covariates with adherence modelled using a hurdle model with a gamma distribution with modeled dispersion',
                       'Adjusted for baseline and post-baseline covariates with adherence modelled using a hurdle model with a beta distribution assuming constant dispersion',
                       'Adjusted for baseline and post-baseline covariates with adherence modelled using a hurdle model with a beta distribution with modeled dispersion')

rownames(results) <- c('Average adherence over entire follow-up',
                       'Average adherence in the last visit plus average adherence prior to the last visit',
                       'Average adherence in the last 6 visits plus average adherence prior to the last 6 visits')

results %>% {round(.*100, 1)} %>% View()



# To obtain confidence intervals, we perform a nonparametric bootstrap with 1000 iterations.

# The bootstrapping will take several hours (this step can be skipped if only interested in point estimates).

source("./scripts/bootstrap.R")
