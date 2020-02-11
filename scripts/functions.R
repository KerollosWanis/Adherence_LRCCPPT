resultsfnc <- function(data, formula1, formula_unadjusted1, formula_baseline1, 
                       formula2, formula_unadjusted2, formula_baseline2,
                       formula3, formula_unadjusted3, formula_baseline3,
                       num_measured_formula, denom_measured_formula, 
                       num_censored_formula, denom_censored_formula, 
                       num_formula, denom_formula, 
                       num_formula_var, denom_formula_var){
  
  standard_data <- data %>% group_by(ID_CPPT) %>% filter(VISIT == first(VISIT)) %>%
    slice(rep(1:n(), each=(lastVISIT-firstVISIT))) %>% mutate(VISIT = seq_along(VISIT)) %>% ungroup()
  
  standardization_temp <- list()
  
  SWeights <- stabilizedweightsfnc(data, num_measured_formula, denom_measured_formula, 
                                         num_censored_formula, denom_censored_formula, 
                                         num_formula, denom_formula, 
                                         num_formula_var, denom_formula_var)
  
  formula <- list(formula1, formula2, formula3)
  formula_unadjusted <- list(formula_unadjusted1, formula_unadjusted2, formula_unadjusted3)
  formula_baseline <- list(formula_baseline1, formula_baseline2, formula_baseline3)
  
  output <- vector()
  outputs <- list()
  
  for(j in 1:3) {
  
  standardization_temp[[1]] <- glm(formula=formula_unadjusted[[j]], data=data %>% filter(censored==0), family = binomial())
  standardization_temp[[2]] <- glm(formula=formula_baseline[[j]], data=data %>% filter(censored==0), family = binomial())
  standardization_temp[[3]] <- glm(formula=formula[[j]], 
                                   data=data %>% mutate(SWeights = SWeights[[1]]) %>% filter(censored==0), 
                                   family = quasibinomial(), weights=SWeights)
  standardization_temp[[4]] <- glm(formula=formula[[j]], 
                                   data=data %>% mutate(SWeights = SWeights[[2]]) %>% filter(censored==0), 
                                   family = quasibinomial(), weights=SWeights)
  standardization_temp[[5]] <- glm(formula=formula[[j]], 
                                   data=data %>% mutate(SWeights = SWeights[[3]]) %>% filter(censored==0), 
                                   family = quasibinomial(), weights=SWeights)
  standardization_temp[[6]] <- glm(formula=formula[[j]], 
                                   data=data %>% mutate(SWeights = SWeights[[4]]) %>% filter(censored==0), 
                                   family = quasibinomial(), weights=SWeights)
  standardization_temp[[7]] <- glm(formula=formula[[j]], 
                                   data=data %>% mutate(SWeights = SWeights[[5]]) %>% filter(censored==0), 
                                   family = quasibinomial(), weights=SWeights)
  standardization_temp[[8]] <- glm(formula=formula[[j]], 
                                   data=data %>% mutate(SWeights = SWeights[[6]]) %>% filter(censored==0), 
                                   family = quasibinomial(), weights=SWeights)
  
  for(i in 1:8){
    
    if (j == 1) {
      alladhfull <-
        standard_data %>% mutate(cumPAC = 1) %>%
        mutate(cond_surv = 1 - predict(standardization_temp[[i]], ., type =
                                         "response")) %>% group_by(ID_CPPT) %>% mutate(CI = 1 - cumprod(cond_surv)) %>% ungroup() %>%
        group_by(VISIT) %>% summarise(mean(CI)) %>% as.data.frame()
      alladh <- filter(alladhfull, VISIT == lastVISIT - firstVISIT)
      
      allnonadhfull <-
        standard_data %>% mutate(cumPAC = 0) %>%
        mutate(cond_surv = 1 - predict(standardization_temp[[i]], ., type =
                                         "response")) %>% group_by(ID_CPPT) %>% mutate(CI = 1 - cumprod(cond_surv)) %>% ungroup() %>%
        group_by(VISIT) %>% summarise(mean(CI)) %>% as.data.frame()
      allnonadh <- filter(allnonadhfull, VISIT == lastVISIT - firstVISIT)
      
      output[[i]] <- alladh[[2]] - allnonadh[[2]]
    }
  
    if (j == 2) {
      alladhfull <-
        standard_data %>% mutate(PAC = 1,
                                 cumPAC_minuslast = case_when(VISIT==1 ~ 0, TRUE ~ 1)) %>%
        mutate(cond_surv = 1 - predict(standardization_temp[[i]], ., type =
                                         "response")) %>% group_by(ID_CPPT) %>% mutate(CI = 1 - cumprod(cond_surv)) %>% ungroup() %>%
        group_by(VISIT) %>% summarise(mean(CI)) %>% as.data.frame()
      alladh <- filter(alladhfull, VISIT == lastVISIT - firstVISIT)
      
      allnonadhfull <-
        standard_data %>% mutate(PAC = 0,
                                 cumPAC_minuslast = 0) %>%
        mutate(cond_surv = 1 - predict(standardization_temp[[i]], ., type =
                                         "response")) %>% group_by(ID_CPPT) %>% mutate(CI = 1 - cumprod(cond_surv)) %>% ungroup() %>%
        group_by(VISIT) %>% summarise(mean(CI)) %>% as.data.frame()
      allnonadh <- filter(allnonadhfull, VISIT == lastVISIT - firstVISIT)
      
      output[[i]] <- alladh[[2]] - allnonadh[[2]]
    }
    
    if (j == 3) {
      alladhfull <-
        standard_data %>% mutate(cumPAC_year = 1,
                                 cumPAC_minusyear = case_when(VISIT <= 6 ~ 0, TRUE ~ 1)) %>%
        mutate(cond_surv = 1 - predict(standardization_temp[[i]], ., type =
                                         "response")) %>% group_by(ID_CPPT) %>% mutate(CI = 1 - cumprod(cond_surv)) %>% ungroup() %>%
        group_by(VISIT) %>% summarise(mean(CI)) %>% as.data.frame()
      alladh <- filter(alladhfull, VISIT == lastVISIT - firstVISIT)
      
      allnonadhfull <-
        standard_data %>% mutate(cumPAC_year = 0,
                                 cumPAC_minusyear = 0) %>%
        mutate(cond_surv = 1 - predict(standardization_temp[[i]], ., type =
                                         "response")) %>% group_by(ID_CPPT) %>% mutate(CI = 1 - cumprod(cond_surv)) %>% ungroup() %>%
        group_by(VISIT) %>% summarise(mean(CI)) %>% as.data.frame()
      allnonadh <-
        filter(allnonadhfull, VISIT == lastVISIT - firstVISIT)
      
      output[[i]] <- alladh[[2]] - allnonadh[[2]]
    }
  }
  
  outputs[[j]] <- output
  
  }
  
  return(rbind(outputs[[1]], outputs[[2]], outputs[[3]]))
}


bootstrapfnc <- function(data, formula1, formula_unadjusted1, formula_baseline1, 
                         formula2, formula_unadjusted2, formula_baseline2,
                         formula3, formula_unadjusted3, formula_baseline3,
                         num_measured_formula, denom_measured_formula, 
                         num_censored_formula, denom_censored_formula, 
                         num_formula, denom_formula, 
                         num_formula_var, denom_formula_var){
  
  IDs <- sample(unique(data$ID_CPPT), length(unique(data$ID_CPPT)), replace = TRUE)
  datatemp <- as.data.table(data)
  setkey(datatemp, "ID_CPPT")
  datatemp <- datatemp[J(IDs), allow.cartesian = TRUE]
  datatemp <- datatemp %>% mutate(ID_CPPT = case_when(VISIT==1 ~ 1, TRUE ~ 0)) %>% mutate(ID_CPPT = cumsum(ID_CPPT))
  
  resultsfnc(datatemp, formula1, formula_unadjusted1, formula_baseline1, 
             formula2, formula_unadjusted2, formula_baseline2,
             formula3, formula_unadjusted3, formula_baseline3,
             num_measured_formula, denom_measured_formula, 
             num_censored_formula, denom_censored_formula, 
             num_formula, denom_formula, 
             num_formula_var, denom_formula_var)
}


stabilizedweightsfnc <- function(data, num_measured_formula, denom_measured_formula, 
                                 num_censored_formula, denom_censored_formula, 
                                 num_formula, denom_formula, 
                                 num_formula_var, denom_formula_var, 
                                 num_zero_formula, denom_zero_formula){
  
  num_measured <- predict(glm(formula=num_measured_formula, data=data, family=binomial()), data, type="response")
  denom_measured <- predict(glm(formula=denom_measured_formula, data=data, family=binomial()), data, type="response")
  
  num_censored <- predict(glm(formula=num_censored_formula, data=data, family=binomial()), data, type="response")
  denom_censored <- predict(glm(formula=denom_censored_formula, data=data, family=binomial()), data, type="response")
  
    num_trt_model <- lm(formula=num_formula, data=(data %>% filter(measured==1)))
    denom_trt_model <- lm(formula=denom_formula, data=(data %>% filter(measured==1)))
    num_p_trt <- predict(num_trt_model, newdata=data, type="response")
    denom_p_trt <- predict(denom_trt_model, newdata=data, type="response")
    num_trt <- dtruncnorm(data$PAC, a=0, b=1.3, num_p_trt, summary(num_trt_model)$sigma)
    denom_trt <- dtruncnorm(data$PAC, a=0, b=1.3, denom_p_trt, summary(denom_trt_model)$sigma)
    
      data <- data %>% mutate(n_m = num_measured, d_m = denom_measured, 
                                    n_c = num_censored, d_c = denom_censored,
                                    n_t = num_trt, d_t = denom_trt) %>%
        mutate(n_m = case_when(measured == 0 ~ 1-n_m,
                               measured == 1 ~ n_m),
               d_m = case_when(measured == 0 ~ 1-d_m,
                               measured == 1 ~ d_m),
               n_c = case_when(censored == 0 ~ 1-n_c,
                               censored == 1 ~ n_c),
               d_c = case_when(censored == 0 ~ 1-d_c,
                               censored == 1 ~ d_c),
               n_t = case_when(measured == 0 ~ 1,
                               measured == 1 ~ n_t),
               d_t = case_when(measured == 0 ~ 1,
                               measured == 1 ~ d_t),
               wt_homosked = (n_m*n_c*n_t)/(d_m*d_c*d_t))

    sigma2_num_trt <- num_trt_model$residuals^2
    sigma2_denom_trt <- denom_trt_model$residuals^2
    var_num_model <- glm(formula=num_formula_var, data=(data %>% filter(measured==1) %>% mutate(sigma2_num_trt = sigma2_num_trt)), family=gaussian(link="log"))
    var_denom_model <- glm(formula=denom_formula_var, data=(data %>% filter(measured==1)  %>% mutate(sigma2_denom_trt = sigma2_denom_trt)), family=gaussian(link="log"))
    var_num <- predict(var_num_model, data, type="response")
    var_denom <- predict(var_denom_model, data, type="response")
    num_trt_heterosked <- dtruncnorm(data$PAC, a=0, b=1.3, num_p_trt, sqrt(var_num))
    denom_trt_heterosked <- dtruncnorm(data$PAC, a=0, b=1.3, denom_p_trt, sqrt(var_denom))
    
    data <- data %>% mutate(n_t = num_trt_heterosked, d_t = denom_trt_heterosked) %>%
      mutate(n_t = case_when(measured == 0 ~ 1,
                             measured == 1 ~ n_t),
             d_t = case_when(measured == 0 ~ 1,
                             measured == 1 ~ d_t),
             n_t = case_when(measured == 0 ~ 1,
                             measured == 1 ~ n_t),
             d_t = case_when(measured == 0 ~ 1,
                             measured == 1 ~ d_t),
             wt_heterosked = (n_m*n_c*n_t)/(d_m*d_c*d_t))
    
    data <- data %>% mutate(zero_pac = case_when(PAC == 0 ~ 1, TRUE ~ 0))

    num_trt_zero <- predict(glm(formula=paste("zero_pac ~", unlist(strsplit(num_formula, split="~", fixed=T))[2]), 
                                data=(data %>% filter(measured==1)), family=binomial()), type="response", newdata=data)
    denom_trt_zero <- predict(glm(formula=paste("zero_pac ~", unlist(strsplit(denom_formula, split="~", fixed=T))[2]), 
                                  data=(data %>% filter(measured==1)), family=binomial()), type="response", newdata=data)
    
    num_trt_nonzero_model <- glm(formula=num_formula, data=(data %>% filter(measured==1 & zero_pac==0)), family=Gamma())
    denom_trt_nonzero_model <- glm(formula=denom_formula, data=(data %>% filter(measured==1 & zero_pac==0)), family=Gamma())
    
    num_trt_nonzero <- dgamma(x=data$PAC, shape=gamma.shape(num_trt_nonzero_model)$alpha, 
                              scale=(1/gamma.shape(num_trt_nonzero_model)$alpha)*predict(num_trt_nonzero_model, type="response", newdata=data))
    
    denom_trt_nonzero <- dgamma(x=data$PAC, shape=gamma.shape(denom_trt_nonzero_model)$alpha, 
                                scale=(1/gamma.shape(denom_trt_nonzero_model)$alpha)*predict(denom_trt_nonzero_model, type="response", newdata=data))
    
    data <- data %>% mutate(n_t_zero = num_trt_zero, d_t_zero = denom_trt_zero, 
                            n_t_nonzero = num_trt_nonzero, d_t_nonzero = denom_trt_nonzero) %>%
      mutate(n_t = case_when(measured == 0 ~ 1,
                             measured == 1 & zero_pac == 1 ~ n_t_zero,
                             measured == 1 & zero_pac == 0 ~ (1-n_t_zero)*n_t_nonzero),
             d_t = case_when(measured == 0 ~ 1,
                             measured == 1 & zero_pac == 1 ~ d_t_zero,
                             measured == 1 & zero_pac == 0 ~ (1-d_t_zero)*d_t_nonzero),
             wt_gamma = (n_m*n_c*n_t)/(d_m*d_c*d_t))
    
    sigma2_num_trt <- residuals(num_trt_nonzero_model, type="deviance")^2
    sigma2_denom_trt <- residuals(denom_trt_nonzero_model, type="deviance")^2 
    disp_num_model <- glm(formula=num_formula_var, data=(data %>% filter(measured==1 & zero_pac==0) %>% mutate(sigma2_num_trt = sigma2_num_trt)), family=Digamma, maxit=100)
    disp_denom_model <- glm(formula=denom_formula_var, data=(data %>% filter(measured==1 & zero_pac==0)  %>% mutate(sigma2_denom_trt = sigma2_denom_trt)), family=Digamma, maxit=100)
    disp_num <- predict(disp_num_model, data, type="response")
    disp_denom <- predict(disp_denom_model, data, type="response")
    
    num_trt_nonzero_disp <- dgamma(x=data$PAC, shape=(1/disp_num), 
                              scale=disp_num*predict(num_trt_nonzero_model, type="response", newdata=data))
    
    denom_trt_nonzero_disp <- dgamma(x=data$PAC, shape=(1/disp_denom), 
                                scale=disp_denom*predict(denom_trt_nonzero_model, type="response", newdata=data))
    
    data <- data %>% mutate(n_t_zero = num_trt_zero, d_t_zero = denom_trt_zero, 
                            n_t_nonzero_disp = num_trt_nonzero_disp, d_t_nonzero_disp = denom_trt_nonzero_disp) %>%
      mutate(n_t = case_when(measured == 0 ~ 1,
                             measured == 1 & zero_pac == 1 ~ n_t_zero,
                             measured == 1 & zero_pac == 0 ~ (1-n_t_zero)*n_t_nonzero_disp),
             d_t = case_when(measured == 0 ~ 1,
                             measured == 1 & zero_pac == 1 ~ d_t_zero,
                             measured == 1 & zero_pac == 0 ~ (1-d_t_zero)*d_t_nonzero_disp),
             wt_gamma_disp = (n_m*n_c*n_t)/(d_m*d_c*d_t))
    
    data <- data %>% mutate(scaled_pac = PAC/1.3,
                            cat_pac = as.factor(case_when(scaled_pac == 0 ~ "1",
                                                scaled_pac == 1 ~ "3",
                                                scaled_pac != 0 & scaled_pac != 1 ~ "2")))
    data$cat_pac <- fct_relevel(data$cat_pac, "1", after=3)
    
    zero_one_num_model <- multinom(formula=paste("cat_pac ~", unlist(strsplit(num_formula, split="~", fixed=T))[2]), 
                                   data=(data %>% filter(measured==1)), maxit=1000)
    zero_one_denom_model <- multinom(formula=paste("cat_pac ~", unlist(strsplit(denom_formula, split="~", fixed=T))[2]), 
                                     data=(data %>% filter(measured==1)), maxit=1000)
    
    zero_one_num <- predict(zero_one_num_model, newdata=data, type="probs")
    zero_one_denom <- predict(zero_one_denom_model, newdata=data, type="probs")
    
    num_beta_model <- betareg(formula=paste("scaled_pac ~", unlist(strsplit(num_formula, split="~", fixed=T))[2]), 
                              data=(data %>% filter(measured==1 & cat_pac==2)))
    denom_beta_model <- betareg(formula=paste("scaled_pac ~", unlist(strsplit(denom_formula, split="~", fixed=T))[2]), 
                                data=(data %>% filter(measured==1 & cat_pac==2)))
    
    alpha_num <- (num_beta_model %>% predict(newdata=data, type="response"))*num_beta_model$coefficients$precision
    beta_num <- alpha_num*((1/(num_beta_model %>% predict(newdata=data, type="response"))) - 1)
    
    alpha_denom <- (denom_beta_model %>% predict(newdata=data, type="response"))*denom_beta_model$coefficients$precision
    beta_denom <- alpha_denom*((1/(denom_beta_model %>% predict(newdata=data, type="response"))) - 1)
    
    num_trt_beta <- dbeta(x=data$scaled_pac, shape1=alpha_num, shape2=beta_num)
    denom_trt_beta <- dbeta(x=data$scaled_pac, shape1=alpha_denom, shape2=beta_denom)
    
    data <- data %>% mutate(n_t_zero = zero_one_num[,"1"], d_t_zero = zero_one_denom[,"1"], 
                            n_t_one = zero_one_num[,"3"], d_t_one = zero_one_denom[,"3"], 
                            n_t_non_zero_one = zero_one_num[,"2"], d_t_non_zero_one = zero_one_denom[,"2"],
                            n_t_beta = num_trt_beta, d_t_beta = denom_trt_beta) %>%
      mutate(n_t = case_when(measured == 0 ~ 1,
                             measured == 1 & cat_pac == "1" ~ n_t_zero,
                             measured == 1 & cat_pac == "3" ~ n_t_one,
                             measured == 1 & cat_pac == "2" ~ n_t_non_zero_one*n_t_beta),
             d_t = case_when(measured == 0 ~ 1,
                             measured == 1 & cat_pac == "1" ~ d_t_zero,
                             measured == 1 & cat_pac == "3" ~ d_t_one,
                             measured == 1 & cat_pac == "2" ~ d_t_non_zero_one*d_t_beta),
             wt_beta = (n_m*n_c*n_t)/(d_m*d_c*d_t))
    
    num_beta_model_disp <- betareg(formula=paste("scaled_pac ~", unlist(strsplit(num_formula, split="~", fixed=T))[2], "|", 
                                            unlist(strsplit(num_formula_var, split="~", fixed=T))[2]), 
                              data=(data %>% filter(measured==1 & cat_pac==2)))
    denom_beta_model_disp <- betareg(formula=paste("scaled_pac ~", unlist(strsplit(denom_formula, split="~", fixed=T))[2], "|",
                                              unlist(strsplit(denom_formula_var, split="~", fixed=T))[2]), 
                                data=(data %>% filter(measured==1 & cat_pac==2)))
    
    alpha_num_disp <- (num_beta_model_disp %>% predict(newdata=data, type="response"))*(num_beta_model_disp %>% predict(newdata=data, type="precision"))
    beta_num_disp <- alpha_num_disp*((1/(num_beta_model_disp %>% predict(newdata=data, type="response"))) - 1)
    
    alpha_denom_disp <- (denom_beta_model_disp %>% predict(newdata=data, type="response"))*(denom_beta_model_disp %>% predict(newdata=data, type="precision"))
    beta_denom_disp <- alpha_denom_disp*((1/(denom_beta_model_disp %>% predict(newdata=data, type="response"))) - 1)
    
    num_trt_beta_disp <- dbeta(x=data$scaled_pac, shape1=alpha_num_disp, shape2=beta_num_disp)
    denom_trt_beta_disp <- dbeta(x=data$scaled_pac, shape1=alpha_denom_disp, shape2=beta_denom_disp)

    data <- data %>% mutate(n_t_zero = zero_one_num[,"1"], d_t_zero = zero_one_denom[,"1"], 
                            n_t_one = zero_one_num[,"3"], d_t_one = zero_one_denom[,"3"], 
                            n_t_non_zero_one = zero_one_num[,"2"], d_t_non_zero_one = zero_one_denom[,"2"],
                            n_t_beta_disp = num_trt_beta_disp, d_t_beta_disp = denom_trt_beta_disp) %>%
      mutate(n_t = case_when(measured == 0 ~ 1,
                             measured == 1 & cat_pac == "1" ~ n_t_zero,
                             measured == 1 & cat_pac == "3" ~ n_t_one,
                             measured == 1 & cat_pac == "2" ~ n_t_non_zero_one*n_t_beta_disp),
             d_t = case_when(measured == 0 ~ 1,
                             measured == 1 & cat_pac == "1" ~ d_t_zero,
                             measured == 1 & cat_pac == "3" ~ d_t_one,
                             measured == 1 & cat_pac == "2" ~ d_t_non_zero_one*d_t_beta_disp),
             wt_beta_disp = (n_m*n_c*n_t)/(d_m*d_c*d_t))
    
  data <- data %>% group_by(ID_CPPT) %>% 
    mutate(
      wts_homosked = cumprod(wt_homosked),
      wts_heterosked = cumprod(wt_heterosked),
      wts_gamma = cumprod(wt_gamma),
      wts_gamma_disp = cumprod(wt_gamma_disp),
      wts_beta = cumprod(wt_beta),
      wts_beta_disp = cumprod(wt_beta_disp)
    ) %>% ungroup()
  
  upper_homosked <- quantile(data$wts_homosked, probs=0.999)
  upper_heterosked <- quantile(data$wts_heterosked, probs=0.999)
  upper_gamma <- quantile(data$wts_gamma, probs=0.999)
  upper_gamma_disp <- quantile(data$wts_gamma_disp, probs=0.999)
  upper_beta <- quantile(data$wts_beta, probs=0.999)
  upper_beta_disp <- quantile(data$wts_beta_disp, probs=0.999)
  
  data <- data %>% mutate(
    wts_homosked = case_when(wts_homosked > upper_homosked ~ upper_homosked,
                           TRUE ~ wts_homosked),
    wts_heterosked = case_when(wts_heterosked > upper_heterosked ~ upper_heterosked,
                           TRUE ~ wts_heterosked),
    wts_gamma = case_when(wts_gamma > upper_gamma ~ upper_gamma,
                               TRUE ~ wts_gamma),
    wts_gamma_disp = case_when(wts_gamma_disp > upper_gamma_disp ~ upper_gamma_disp,
                          TRUE ~ wts_gamma_disp),
    wts_beta = case_when(wts_beta > upper_beta ~ upper_beta,
                               TRUE ~ wts_beta),
    wts_beta_disp = case_when(wts_beta_disp > upper_beta_disp ~ upper_beta_disp,
                               TRUE ~ wts_beta_disp)
  )
  
  return(data.frame(data$wts_homosked, data$wts_heterosked, data$wts_gamma, data$wts_gamma_disp, data$wts_beta, data$wts_beta_disp))
}