results <- resultsfnc(
  data = data_for_analysis,
  formula1 = paste("outcome ~ ns(cumPAC, knots=quantile(cumPAC, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +", baseline_only_vars),
  formula_unadjusted1 = "outcome ~ ns(cumPAC, knots=quantile(cumPAC, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42))",
  formula_baseline1 = paste("outcome ~ ns(cumPAC, knots=quantile(cumPAC, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +", all_baseline_vars),
  formula2 = paste("outcome ~ ns(PAC, knots=quantile(PAC, probs=c(0.35,0.65)), Boundary.knots=quantile(PAC, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +
    ns(cumPAC_minuslast, knots=quantile(cumPAC_minuslast, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC_minuslast, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +", baseline_only_vars),
  formula_unadjusted2 = "outcome ~ ns(PAC, knots=quantile(PAC, probs=c(0.35,0.65)), Boundary.knots=quantile(PAC, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +
    ns(cumPAC_minuslast, knots=quantile(cumPAC_minuslast, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC_minuslast, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42))",
  formula_baseline2 = paste("outcome ~ ns(PAC, knots=quantile(PAC, probs=c(0.35,0.65)), Boundary.knots=quantile(PAC, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +
    ns(cumPAC_minuslast, knots=quantile(cumPAC_minuslast, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC_minuslast, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +", all_baseline_vars),
  formula3 = paste("outcome ~ ns(cumPAC_year, knots=quantile(cumPAC_year, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC_year, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +
    ns(cumPAC_minusyear, knots=quantile(cumPAC_minusyear, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC_minusyear, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +", baseline_only_vars),
  formula_unadjusted3 = "outcome ~ ns(cumPAC_year, knots=quantile(cumPAC_year, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC_year, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +
    ns(cumPAC_minusyear, knots=quantile(cumPAC_minusyear, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC_minusyear, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42))",
  formula_baseline3 = paste("outcome ~ ns(cumPAC_year, knots=quantile(cumPAC_year, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC_year, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +
    ns(cumPAC_minusyear, knots=quantile(cumPAC_minusyear, probs=c(0.35,0.65)), Boundary.knots=quantile(cumPAC_minusyear, probs=c(0.05,0.95)))*
    ns(VISIT, knots=c(6,12,24,36), Boundary.knots=c(1,42)) +", all_baseline_vars),
  num_measured_formula = num_measured_formula,
  denom_measured_formula = denom_measured_formula,
  num_censored_formula = num_censored_formula,
  denom_censored_formula = denom_censored_formula,
  num_formula = num_formula,
  denom_formula = denom_formula,
  num_formula_var = num_formula_var,
  denom_formula_var = denom_formula_var
)
