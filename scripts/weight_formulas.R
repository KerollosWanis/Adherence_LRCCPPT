#This code creates the formulas which we will use to fit the stabilized weight numerator and denominator models
baseline_only_vars = "RSTRAT + V5AGE + PHYSA_bl + EDUC + RACE"
all_baseline_vars = "RSTRAT + V5AGE + PHYSA_bl + EDUC + RACE +
    SYSBPSTD_bl + DSTBPSTD_bl + BMI_bl +
    NAUS_bl + VOMIT_bl + DIARR_bl +
    CHOL_bl + HDL_bl + LDL_bl + TG_bl +
    CAL_bl + PROT_bl + FAT_bl + CARBT_bl + GXT_bl +
    AMSMK_bl + ASPCMP_bl + PHYSB_bl + ISCT_bl +
    PSRATIO_bl + SODIUM_bl + PHOS_bl + POT_bl + THYR_bl +
    IRON_bl + BILT_bl + BILD_bl + ALK_bl + CREAT_bl +
    GLUC_bl + ALBU_bl + CALC_bl + WBC_bl"
post_baseline_vars = "SYSBPSTD_prev + DSTBPSTD_prev + BMI_prev + NAUS_prev + VOMIT_prev + DIARR_prev +
ns(CHOL_prev_yearav, knots=quantile(CHOL_prev_yearav, probs=c(0.35,0.65)), Boundary.knots=quantile(CHOL_prev_yearav, probs=c(0.05,0.95))) +
ns(HDL_prev_yearav, knots=quantile(HDL_prev_yearav, probs=c(0.35,0.65)), Boundary.knots=quantile(HDL_prev_yearav, probs=c(0.05,0.95))) +
ns(LDL_prev_yearav, knots=quantile(LDL_prev_yearav, probs=c(0.35,0.65)), Boundary.knots=quantile(LDL_prev_yearav, probs=c(0.05,0.95))) +
ns(TG_prev_yearav, knots=quantile(TG_prev_yearav, probs=c(0.35,0.65)), Boundary.knots=quantile(TG_prev_yearav, probs=c(0.05,0.95))) +
CAL_prev + PROT_prev + FAT_prev + CARBT_prev + RQANGINA_prev + GXT_prev + AMSMK_prev + ISCT_prev + ASPCMP_prev + PHYSB_prev +
PSRATIO_prev + SODIUM_prev + PHOS_prev + POT_prev + THYR_prev + IRON_prev + BILT_prev + BILD_prev + ALK_prev + CREAT_prev + GLUC_prev + ALBU_prev + CALC_prev + WBC_prev +
vascular_prev.indicator + other.coronary_prev.indicator + vascular_prev_hx + other.coronary_prev_hx"
post_baseline_vars_cur = "SYSBPSTD_cur + DSTBPSTD_cur + BMI_cur + NAUS_cur + VOMIT_cur + DIARR_cur +
ns(CHOL_cur_yearav, knots=quantile(CHOL_cur_yearav, probs=c(0.35,0.65)), Boundary.knots=quantile(CHOL_cur_yearav, probs=c(0.05,0.95))) +
ns(HDL_cur_yearav, knots=quantile(HDL_cur_yearav, probs=c(0.35,0.65)), Boundary.knots=quantile(HDL_cur_yearav, probs=c(0.05,0.95))) +
ns(LDL_cur_yearav, knots=quantile(LDL_cur_yearav, probs=c(0.35,0.65)), Boundary.knots=quantile(LDL_cur_yearav, probs=c(0.05,0.95))) +
ns(TG_cur_yearav, knots=quantile(TG_cur_yearav, probs=c(0.35,0.65)), Boundary.knots=quantile(TG_cur_yearav, probs=c(0.05,0.95))) +
CAL_cur + PROT_cur + FAT_cur + CARBT_cur + RQANGINA_cur + GXT_cur + AMSMK_cur + ISCT_cur + ASPCMP_cur + PHYSB_cur +
PSRATIO_cur + SODIUM_cur + PHOS_cur + POT_cur + THYR_cur + IRON_cur + BILT_cur + BILD_cur + ALK_cur + CREAT_cur + GLUC_cur + ALBU_cur + CALC_cur + WBC_cur +
vascular_cur.indicator + other.coronary_cur.indicator + vascular_cur_hx + other.coronary_cur_hx"


num_measured_formula <-
  paste("measured ~ ns(VISIT, knots=quantile(VISIT, probs=c(0.35,0.65)), Boundary.knots=quantile(VISIT, probs=c(0.05,0.95))) +
ns(PAC_prev, knots=quantile(PAC_prev, probs=c(0.35,0.65)), Boundary.knots=quantile(PAC_prev, probs=c(0.05,0.95))) +", baseline_only_vars)

denom_measured_formula <-
  paste("measured ~ ns(VISIT, knots=quantile(VISIT, probs=c(0.35,0.65)), Boundary.knots=quantile(VISIT, probs=c(0.05,0.95))) +
ns(PAC_prev, knots=quantile(PAC_prev, probs=c(0.35,0.65)), Boundary.knots=quantile(PAC_prev, probs=c(0.05,0.95))) +", baseline_only_vars, 
        "+", post_baseline_vars)

num_censored_formula <-
  paste("censored ~ ns(VISIT, knots=quantile(VISIT, probs=c(0.35,0.65)), Boundary.knots=quantile(VISIT, probs=c(0.05,0.95))) +
ns(PAC, knots=quantile(PAC, probs=c(0.35,0.65)), Boundary.knots=quantile(PAC, probs=c(0.05,0.95))) +", baseline_only_vars)

denom_censored_formula <-
  paste("censored ~ ns(VISIT, knots=quantile(VISIT, probs=c(0.35,0.65)), Boundary.knots=quantile(VISIT, probs=c(0.05,0.95))) +
ns(PAC, knots=quantile(PAC, probs=c(0.35,0.65)), Boundary.knots=quantile(PAC, probs=c(0.05,0.95))) +", baseline_only_vars, 
        "+", post_baseline_vars_cur)

num_formula <-
  paste("PAC ~ ns(VISIT, knots=quantile(VISIT, probs=c(0.35,0.65)), Boundary.knots=quantile(VISIT, probs=c(0.05,0.95))) +
ns(PAC_prev, knots=quantile(PAC_prev, probs=c(0.35,0.65)), Boundary.knots=quantile(PAC_prev, probs=c(0.05,0.95))) +", baseline_only_vars)

denom_formula <-
  paste("PAC ~ ns(VISIT, knots=quantile(VISIT, probs=c(0.35,0.65)), Boundary.knots=quantile(VISIT, probs=c(0.05,0.95))) +
ns(PAC_prev, knots=quantile(PAC_prev, probs=c(0.35,0.65)), Boundary.knots=quantile(PAC_prev, probs=c(0.05,0.95))) +", baseline_only_vars,
        "+", post_baseline_vars)

num_formula_var <-
  paste("sigma2_num_trt ~ ns(VISIT, knots=quantile(VISIT, probs=c(0.35,0.65)), Boundary.knots=quantile(VISIT, probs=c(0.05,0.95))) +
ns(PAC_prev, knots=quantile(PAC_prev, probs=c(0.35,0.65)), Boundary.knots=quantile(PAC_prev, probs=c(0.05,0.95))) +", baseline_only_vars)

denom_formula_var <-
  paste("sigma2_denom_trt ~ ns(VISIT, knots=quantile(VISIT, probs=c(0.35,0.65)), Boundary.knots=quantile(VISIT, probs=c(0.05,0.95))) +
ns(PAC_prev, knots=quantile(PAC_prev, probs=c(0.35,0.65)), Boundary.knots=quantile(PAC_prev, probs=c(0.05,0.95))) +", baseline_only_vars)