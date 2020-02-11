clean_data <- function(first.visit=6, censor.at.missed.visit=6){
clinic  <- read_csv("./data/CTC_LAD1.csv") %>% rename("SODIUM"=X37)
events  <- read_csv("./data/CTE_LAD1.csv")
trt  <- read_csv("./data/CPPT_TRT.csv")

censor.visit <- censor.at.missed.visit # 6 for main analysis (censors on 6th missed visit); 4 or 5 for sensitivity analyses

firstVISIT <- first.visit # 6 for main analysis (visit 6 is first visit); 7 for sensitivity analysis 
lastVISIT <- 49; totalVISIT <- lastVISIT-firstVISIT # set first and last visit: time zero is 5th visit

clinic_plac <-plyr::join(trt,clinic,by="ID_CPPT") %>% filter(TRTMENT == "PLAC") # restrict to placebo arm
clinic_plac2 <- clinic_plac %>% group_by(ID_CPPT) %>% 
  mutate(flag.anymissingFUTIME=ifelse(is.na(FUTIME) & VISIT <=49,1,NA)) %>%  # create flag to deal with FUTIME==NA
  mutate(flag.futime.error=ifelse(lag(FUTIME)>=FUTIME & VISIT <=49,1,NA_real_)) %>% # create flag to deal with erroneous/past FUTIME
  mutate(flag.max.visit=ifelse(VISIT==max(VISIT),1,0), # identify maximum visit for each participant
         flag.missed.visit=ifelse(VISIT-lag(VISIT) != 1,1,0), # createe indicator for visit following one or more missed visits (i.e. values to be imputed)
         flag.missed.visit=ifelse(VISIT==1,0,flag.missed.visit),
         flag.last.visit_consec=ifelse(((lead(VISIT)-VISIT)>censor.visit  & VISIT<=49), 1, 0)) %>% # create indicator for the visit prior to a certain number or more consecutive missed visits
  fill(flag.last.visit_consec, .direction="down") %>% ungroup()
temp.visits <- data.frame(ID_CPPT=rep(unique(clinic_plac2$ID_CPPT),each=(49+censor.visit)), 
                          VISIT2=rep(1:(49+censor.visit), times=length(unique(clinic_plac2$ID_CPPT))),
                          stringsAsFactors = FALSE)
covariates <- list()
covariates$labs <- c("CHOL", "HDL", "LDL", "TG", "CAL", "PROT", "PSRATIO", "FAT", "CARBT", "CREAT", "GLUC", "SODIUM", "PHOS", "POT", 
                     "THYR","IRON","BILT","BILD","ALK","ALBU","CALC","WBC")
covariates$symptoms <- c("ABDOM", "DIARR","HRTBR","NAUS","GAS","VOMIT","ISCT", "GXT", "TMA", "RQANGINA","COMSTC")
covariates$vitals <- c( "BMI", "SYSBPSTD","DSTBPSTD","PULSE")
covariates$other <- c("ASPCMP", "AMSMK", "DALC","PHYSA","PHYSB")
covariates$baseline <- c("EDUC", "V5AGE", "MODE_CON", "RSTRAT")

clinic_plac3 <- clinic_plac2 %>% group_by(ID_CPPT) %>% 
  mutate_at(.vars=c(covariates$labs, covariates$vitals, covariates$symptoms, covariates$other), 
            .funs = list(cur = function(x) x,
                         prev = function(x) lag(x))) %>% ungroup()

clinic_plac4 <- full_join(clinic_plac3, temp.visits, by=c("ID_CPPT" = "ID_CPPT", "VISIT" = "VISIT2")) %>% 
  group_by(ID_CPPT) %>% arrange(ID_CPPT, VISIT) %>% 
  fill(c(flag.anymissingFUTIME, flag.futime.error), .direction=c("down")) %>% # take over all VISIT times if is.na(FUTIME) or erroneous FUTIME 
  fill(c(flag.anymissingFUTIME, flag.futime.error), .direction=c("up")) %>%
  mutate(FUTIME=case_when((flag.anymissingFUTIME==1 | flag.futime.error==1) & VISIT==6 ~ 14, # impute is.na(FUTIME) or erroneous FUTIME
                          (flag.anymissingFUTIME==1 | flag.futime.error==1) & VISIT==7 ~ 28,
                          (flag.anymissingFUTIME==1 | flag.futime.error==1) & VISIT>7 ~ (28+(VISIT-7)*61),
                          TRUE~as.numeric(FUTIME))) %>% 
  mutate(flag.last.visit_consec.censor = ifelse(lag(flag.last.visit_consec, n=censor.visit)==1,1,NA),
         flag.last.visit_consec.censor = lag(flag.last.visit_consec.censor)) %>% # create indicator for visit after n'th consecutive missed visit (which will be removed; and n'th visit will later have an indicaator for "censored")
  fill(flag.last.visit_consec.censor, .direction="down") %>% 
  mutate(flag.max.visit.censor=lag(flag.max.visit,censor.visit), 
         flag.max.visit.censor=lag(flag.max.visit.censor)) %>% # flag.max.visit.censor indicates visit after n'th visit after last visit (which will be removed; and n'th visit will later have an indicaator for "censored")
  fill(flag.max.visit.censor,.direction="down") %>%
  mutate(indicator.missed.visit=ifelse(is.na(flag.missed.visit),1,0)) %>% # indicator for missed VISIT
  mutate(indicator.missing.pac = ifelse(is.na(PAC),1,0)) %>% # indictor for missing PAC
  mutate(flag.missing.pac_consec = ifelse(VISIT>=firstVISIT & indicator.missing.pac==1 & 
                                            eval(parse(text=paste(sapply(1:(censor.visit-1), 
                                                                         function(x) paste0("lag(indicator.missing.pac,",x,")==1")), collapse=' & '))),
                                          1,NA)) %>% # create indicator for missing n in a row PAC
  mutate(flag.missing.pac_consec=lag(flag.missing.pac_consec)) %>% # create indicator for visit after missing n in a row PAC (which will be removed; and n'th visit will later have an indicaator for "censored")
  fill(flag.missing.pac_consec,.direction="down") %>%
  ungroup()

clinic_plac5 <- clinic_plac4 %>% group_by(ID_CPPT) %>% fill(flag.max.visit, .direction="down") %>% 
  mutate(new.flag.max.visit=ifelse(flag.max.visit==1 & indicator.missed.visit==1,1,0),
         sum.flag.max.visit=cumsum(new.flag.max.visit)) %>%
  mutate(FUTIME = case_when(new.flag.max.visit==1 ~ eval(parse(text=paste0("case_when(",paste(sapply(1:censor.visit, 
                                                                                                     function(x) paste0("sum.flag.max.visit==",x,
                                                                                                                        "~lag(FUTIME,",x,
                                                                                                                        ")+61*sum.flag.max.visit")), collapse=", "),")"), 
  )), TRUE ~ FUTIME)) %>% ungroup()
clinic_plac6 <- clinic_plac5 %>% group_by(ID_CPPT) %>% 
  mutate(FUTIME=case_when(indicator.missed.visit==1 & VISIT==6 ~ 14, # impute interposed visits 6 and 7
                          indicator.missed.visit==1 & VISIT==7 & is.na(FUTIME) & lag(FUTIME)>=28 ~ lag(FUTIME)+14,
                          indicator.missed.visit==1 & VISIT==7 & is.na(FUTIME) & lag(FUTIME)<28 ~ 28,
                          indicator.missed.visit==1 & VISIT==7 & FUTIME<28 ~ 28,
                          indicator.missed.visit==1 & VISIT==7 & FUTIME>=28 ~ FUTIME+14,
                          TRUE~FUTIME)) %>% ungroup() %>% 
  filter(flag.last.visit_consec.censor==0 | is.na(flag.last.visit_consec.censor)) %>% #remove visits after nth consecutive missed visit
  filter(flag.max.visit.censor==0 | is.na(flag.max.visit.censor))   # remove visits after nth consecutive visit after last observed (max) visit

# now impute remaining interposed visits (after visit 7)  
tempNA <- rle(is.na(clinic_plac6$FUTIME))$lengths[rle(is.na(clinic_plac6$FUTIME))$values==TRUE]
tempNA2 <- lapply(tempNA, function(x) rep(x, each=x)) %>% unlist
tempNA.count <- lapply(tempNA, function(x) seq(from=1, to=x)) %>% unlist
clinic_plac6$tempNA <- NA
clinic_plac6[is.na(clinic_plac6$FUTIME)==TRUE,]$tempNA <- tempNA2
clinic_plac6$tempNA.count <- NA
clinic_plac6[is.na(clinic_plac6$FUTIME)==TRUE,]$tempNA.count <- tempNA.count

a <- clinic_plac6 %>% group_by(ID_CPPT) %>% select(ID_CPPT, VISIT, FUTIME) %>% 
  mutate_at(.vars="FUTIME", .funs=funs(zoo::na.locf(., fromLast=FALSE, na.rm=FALSE)))
b <- clinic_plac6 %>% group_by(ID_CPPT) %>% select(ID_CPPT, VISIT, FUTIME) %>% 
  mutate_at(.vars="FUTIME", .funs=funs(zoo::na.locf(., fromLast=TRUE, na.rm=FALSE)))
clinic_plac6$last <- b$FUTIME
clinic_plac6$first <- a$FUTIME
clinic_plac7 <- clinic_plac6 %>% mutate(indicator.missed.visit2=ifelse(is.na(FUTIME),1,0)) %>% 
  group_by(ID_CPPT) %>%  
  mutate(new.diff=tempNA.count*(last-first)/(tempNA+1),
         copyFUTIME=FUTIME) %>% fill(copyFUTIME) %>% 
  mutate(FUTIME=case_when(indicator.missed.visit2==1 & !is.na(new.diff) ~ copyFUTIME + new.diff,
                          indicator.missed.visit2==1 & is.na(new.diff) ~ copyFUTIME + 61*tempNA.count,
                          TRUE~copyFUTIME)) %>% 
  fill(flag.last.visit_consec, .direction="down") %>% ungroup()
var.vector <- paste0(c(covariates$labs, covariates$vitals, covariates$symptoms, covariates$other), "_prev")
clinic_plac8 <- fill(clinic_plac7 %>% group_by(ID_CPPT), everything(), .direction="down") %>%  # impute by carrying forward
  ungroup() %>% mutate(FUTIME=ifelse(VISIT>lastVISIT,NA,FUTIME)) # remove visit after last visit

clinic_plac9 <- clinic_plac8 %>% group_by(ID_CPPT) %>% 
  mutate_at(.vars=paste0(c(covariates$labs),"_prev"), # Year average labs
            .funs = list(yearav = function(x) zoo::rollapplyr(FUN=mean,x,6,align="right",na.rm=TRUE, fill=NA))) %>% 
  mutate_at(.vars=paste0(c(covariates$labs),"_cur"), # Current year average labs
            .funs = list(yearav = function(x) zoo::rollapplyr(FUN=mean,x,6,align="right",na.rm=TRUE, fill=NA))) %>% 
  ungroup() %>% 
  filter(flag.missing.pac_consec==0 | is.na(flag.missing.pac_consec)) # remove visits after n'th consecutive visit w/ missing PAC

# Outcome and covariate events: 
events2 <- events %>%
  mutate(other.coronary=ifelse(MDEVENTA %in% c(4,13,17,20,22,32),1,0),  # Non-fatal/non-MI coronary events: MDEVENTA %in% {4, 13, 17, 20, 22, 32}
         vascular=ifelse(MDEVENTA %in% c(6:9, 15:16),1,0), # Vascular events: MDEVENTA %in% {6-9, 15-16}
         other.mortality=ifelse(MDEVENTA %in% c(2,5,18),1,0), # Competing Events. Include death from other causes: MDEVENTA %in% {2, 5, 18}
         outcome=ifelse(MDEVENTA %in% c(1,3),1,0) # CHD Death (definite) or MI (definite): MDEVENTA %in% {1,3}
  )

visit.fun <- function(event, dat=events2){
  dat %>% arrange(ID_CPPT, EFUTIME) %>% filter(!!rlang::sym(event)==1) %>% 
    group_by(ID_CPPT) %>% summarise(EFUTIME:=min(EFUTIME)) %>%
    mutate(!!paste0(event,"_cur") := 1) %>% ungroup() %>% 
    rename(!!paste0("EFUTIME.",event):=EFUTIME)
}

events.all <- lapply(setNames(c("outcome", "other.coronary", "vascular", "other.mortality"),
                              c("outcome", "other.coronary", "vascular", "other.mortality")),
                     function(x) visit.fun(event=x)); rm(visit.fun)

# Merge events/covariates with main dataset
clinic_plac10 <- left_join(clinic_plac9, events.all$outcome, by=c("ID_CPPT" = "ID_CPPT")) %>% 
  left_join(., events.all$other.coronary, by=c("ID_CPPT" = "ID_CPPT")) %>% 
  left_join(., events.all$vascular, by=c("ID_CPPT" = "ID_CPPT")) %>% 
  left_join(., events.all$other.mortality, by=c("ID_CPPT" = "ID_CPPT"))

clinic_plac11 <- clinic_plac10 %>% group_by(ID_CPPT) %>% 
  mutate(outcome_cur = ifelse(EFUTIME.outcome>=FUTIME & EFUTIME.outcome<lead(FUTIME),outcome_cur,NA),
         other.mortality_cur = ifelse(EFUTIME.other.mortality>=FUTIME & EFUTIME.other.mortality<lead(FUTIME),other.mortality_cur,NA),
         other.coronary_cur = ifelse(EFUTIME.other.coronary>=FUTIME & EFUTIME.other.coronary<lead(FUTIME),other.coronary_cur,NA),
         vascular_cur = ifelse(EFUTIME.vascular>=FUTIME & EFUTIME.vascular<lead(FUTIME),vascular_cur,NA)) %>% 
  mutate(other.coronary_cur.indicator=lag(other.coronary_cur,1), other.coronary_prev.indicator=lag(other.coronary_cur,2),
         vascular_cur.indicator=lag(vascular_cur), vascular_prev.indicator=lag(vascular_cur,2)) %>% 
  fill(c(outcome_cur, other.mortality_cur, other.coronary_cur, vascular_cur),
       .direction="down") %>% 
  mutate_at(.vars=c("other.coronary_cur", "vascular_cur"),
            .funs=list(curhx=function(x) lag(x,2),
                       prevhx=function(x) lag(x,3))) %>% 
  rename_all(~sub('_cur_curhx', '_cur_hx', .x)) %>% rename_all(~sub('_cur_prevhx', '_prev_hx', .x)) %>% 
  fill(c(other.mortality_cur, outcome_cur, 
         other.coronary_prev_hx, vascular_prev_hx,
         other.coronary_cur_hx, vascular_cur_hx,
         starts_with("EFUTIME")), 
       .direction="down") %>% ungroup() %>% 
  mutate_at(vars(c(paste0(events.all %>% names, "_cur"),
                   "other.coronary_prev.indicator", "vascular_prev.indicator",
                   "other.coronary_prev_hx", "vascular_prev_hx",
                   "other.coronary_cur.indicator", "vascular_cur.indicator",
                   "other.coronary_cur_hx", "vascular_cur_hx")), 
            function(x) ifelse(is.na(x),0,x)) %>% 
  rename("outcome"="outcome_cur", "other.mortality"="other.mortality_cur") %>% 
  select(-c("other.coronary_cur", "vascular_cur"))


# Truncate observations following the occurrence of competing events or outcome
clinic_plac12 <- clinic_plac11 %>% group_by(ID_CPPT) %>% 
  filter(lag(other.mortality)==0 | VISIT==1) %>% filter(lag(outcome)==0 | VISIT==1) %>% ungroup()
# Restrict to participants who do not have a competing event or outcome prior to first visit
early.event.ids <- clinic_plac12 %>% filter((VISIT<firstVISIT & other.mortality==1) | (VISIT<firstVISIT & outcome==1)) %>% .$ID_CPPT
# Restrict to participants not missing PAC on visit 6
missing.initial.pac.ids6 <- clinic_plac12 %>% filter(indicator.missing.pac==1 & VISIT==6) %>% .$ID_CPPT 
missing.initial.pac.ids7 <- clinic_plac12 %>% filter(indicator.missing.pac==1 & VISIT==7) %>% .$ID_CPPT 

clinic_plac13 <- clinic_plac12 %>% filter(!(ID_CPPT %in% early.event.ids)) %>% 
  filter(!(ID_CPPT %in% intersect(missing.initial.pac.ids6, missing.initial.pac.ids7))) %>% 
  mutate(measured = ifelse(indicator.missing.pac==1,0,1),
         indicator.missing.pac.censor = indicator.missing.pac) %>% 
  fill(indicator.missing.pac.censor, .direction="down"); rm(missing.initial.pac.ids6, missing.initial.pac.ids7)


# Standardize FUTIME
clinic_plac14 <- clinic_plac13 %>% 
  filter(VISIT<=lastVISIT) %>%  # remove visits > last visit
  mutate(VISIT=VISIT-(firstVISIT-1)) %>%  # location shift for VISIT
  filter(VISIT>0) %>% # remove visits before first post-randomization visit
  group_by(ID_CPPT) %>% mutate(censored=ifelse(VISIT==max(VISIT) & max(VISIT)<(lastVISIT-firstVISIT+1) & outcome==0,1,0)) %>% ungroup()


# Categorize exposure PAC
clinic_plac15 <- clinic_plac14 %>% mutate(adherence_80 = ifelse(PAC>80,1,0),
                                         PAC = PAC/100,
                                         PAC_doses = case_when(VISIT<=0 ~ NA_real_,
                                                               VISIT==1 ~ PAC*(4*14),
                                                               VISIT==2 ~ PAC*(6*14),
                                                               VISIT>2 ~ PAC*(6*61))) %>% 
  group_by(ID_CPPT) %>% 
  mutate(cumPAC = cumsum(PAC)/VISIT) %>% mutate_at(.vars=c("PAC", "adherence_80"), 
                                                   .funs = list(cur = function(x) x,
                                                                prev = function(x) lag(x)))

# Set baseline covariates
clinic_plac16 <- clinic_plac15 %>% group_by(ID_CPPT) %>% 
  mutate_at(.vars=c(paste0(c(covariates$labs, covariates$vitals, covariates$symptoms, covariates$other),"_prev"),
                    covariates$baseline, "PAC", "adherence_80"),
            .funs = list(bl = function(x) first(x))) %>% ungroup() %>% 
  rename_all(~sub('_prev_bl', '_bl', .x))

# Collapse some covariate levels
clinic_plac17 <- clinic_plac16 %>% mutate(
  PHYSA_bl = case_when(PHYSA_bl == 6 ~ 5, TRUE ~ as.numeric(PHYSA_bl)),
  PHYSB_prev = case_when(PHYSB_prev == 5 ~ 4, TRUE ~ as.numeric(PHYSB_prev)),
  PHYSB_bl = case_when(PHYSB_bl == 5 ~ 4, TRUE ~ as.numeric(PHYSB_bl)),
  PHYSB_cur = case_when(PHYSB_cur == 5 ~ 4, TRUE ~ as.numeric(PHYSB_cur)),
  NAUS_prev = case_when(NAUS_prev %in% c(3,4) ~ 2, TRUE ~ as.numeric(NAUS_prev)),
  NAUS_bl = case_when(NAUS_bl %in% c(3,4) ~ 2, TRUE ~ as.numeric(NAUS_bl)),
  NAUS_cur = case_when(NAUS_cur %in% c(3,4) ~ 2, TRUE ~ as.numeric(NAUS_cur)),
  DIARR_prev = case_when(DIARR_prev %in% c(3,4) ~ 2, TRUE ~ as.numeric(DIARR_prev)),
  DIARR_bl = case_when(DIARR_bl %in% c(3,4) ~ 2, TRUE ~ as.numeric(DIARR_bl)),
  DIARR_cur = case_when(DIARR_cur %in% c(3,4) ~ 2, TRUE ~ as.numeric(DIARR_cur)),
  VOMIT_prev = case_when(VOMIT_prev %in% c(3,4) ~ 2, TRUE ~ as.numeric(VOMIT_prev)),
  VOMIT_bl = case_when(VOMIT_bl %in% c(3,4) ~ 2, TRUE ~ as.numeric(VOMIT_bl)),
  VOMIT_cur = case_when(VOMIT_cur %in% c(3,4) ~ 2, TRUE ~ as.numeric(VOMIT_cur))
) %>% mutate(VOMIT_cur=case_when(VOMIT_cur==2~1,VOMIT_cur==1~0),
             VOMIT_prev=case_when(VOMIT_prev==2~1,VOMIT_prev==1~0),
             VOMIT_bl=case_when(VOMIT_bl==2~1,VOMIT_bl==1~0),
             DIARR_cur=case_when(DIARR_cur==2~1,DIARR_cur==1~0),
             DIARR_prev=case_when(DIARR_prev==2~1,DIARR_prev==1~0),
             DIARR_bl=case_when(DIARR_bl==2~1,DIARR_bl==1~0),
             NAUS_cur=case_when(NAUS_cur==2~1,NAUS_cur==1~0),
             NAUS_prev=case_when(NAUS_prev==2~1,NAUS_prev==1~0),
             NAUS_bl=case_when(NAUS_bl==2~1,NAUS_bl==1~0)) %>% 
  mutate(PAC_prev=ifelse(is.na(PAC_prev),PAC_bl,PAC_prev), 
         cumPAC_prev=lag(cumPAC),
         cumPAC_prev=ifelse(VISIT==1,1,cumPAC_prev), 
         adherence_80_prev=ifelse(VISIT==1,1,adherence_80_prev))
clinic_plac17$PAC_prev[clinic_plac17$VISIT==1] <- 1
clinic_plac18 <- clinic_plac17 %>% group_by(ID_CPPT) %>% 
  mutate(adherence_80.sum = cumsum(adherence_80)/VISIT,
         adherence_80.sum_prev = lag(adherence_80.sum),
         cumPAC_minuslast=lag(cumPAC,1),
         cumPAC_minuslast=ifelse(VISIT==1,0,cumPAC_minuslast),
         cumPAC_minusyear=case_when(VISIT<=6 ~ 0,
                                    VISIT>6 ~ lag(cumPAC,6) ),
         cumPAC_year=case_when(VISIT==1 ~ cumPAC,
                               VISIT==2 ~ (PAC + lag(PAC,1)) / 2,
                               VISIT==3 ~ (PAC + lag(PAC,1) + lag(PAC,2)) / 3,
                               VISIT==4 ~ (PAC + lag(PAC,1) + lag(PAC,2) + lag(PAC,3)) / 4,
                               VISIT==5 ~ (PAC + lag(PAC,1) + lag(PAC,2) + lag(PAC,3) + lag(PAC,4)) / 5,
                               VISIT>=6 ~ (PAC + lag(PAC,1) + lag(PAC,2) + lag(PAC,3) + lag(PAC,4) + lag(PAC,5)) / 6
         )
         ) %>% ungroup()
clinic_plac18$adherence_80.sum_prev[clinic_plac18$VISIT==1] <- 1

# SELECT VARIABLES
allvars <- c("ID_CPPT", "TRTMENT", "RACE", "VISIT", "FUTIME",
             #exposure
             "PAC", "adherence_80", 
             "cumPAC",
             "cumPAC_prev", "PAC_bl", "PAC_prev", "adherence_80_bl", "adherence_80_prev",
             "adherence_80.sum", "adherence_80.sum_prev",
             "cumPAC_minuslast", "cumPAC_minusyear", "cumPAC_year",
             #bl
             "CHOL_bl", "HDL_bl", "LDL_bl", "TG_bl", "CAL_bl", "PROT_bl", "PSRATIO_bl", "FAT_bl", "CARBT_bl", 
             "CREAT_bl", "GLUC_bl", "SODIUM_bl", "PHOS_bl", "POT_bl", "THYR_bl", "IRON_bl", "BILT_bl", "BILD_bl", "ALK_bl", "ALBU_bl", 
             "CALC_bl", "WBC_bl", "BMI_bl", "SYSBPSTD_bl", "DSTBPSTD_bl", "ABDOM_bl", "DIARR_bl", "HRTBR_bl", "NAUS_bl", "GAS_bl", 
             "VOMIT_bl", "ISCT_bl", "GXT_bl", "TMA_bl", "RQANGINA_bl", "COMSTC_bl", "ASPCMP_bl", "AMSMK_bl", "DALC_bl", "PHYSA_bl", "PHYSB_bl", "EDUC_bl", 
             "V5AGE_bl", "MODE_CON_bl", "RSTRAT_bl", 
             #current
             "CHOL_cur", "HDL_cur", "LDL_cur", "TG_cur", "CAL_cur", "PROT_cur", "PSRATIO_cur", "FAT_cur", "CARBT_cur", 
             "CREAT_cur", "GLUC_cur", "SODIUM_cur", "PHOS_cur", "POT_cur", "THYR_cur", "IRON_cur", "BILT_cur", "BILD_cur", "ALK_cur", 
             "ALBU_cur", "CALC_cur", "WBC_cur", "BMI_cur", "SYSBPSTD_cur", "DSTBPSTD_cur", "PULSE_cur", "ABDOM_cur", "DIARR_cur", "HRTBR_cur", 
             "NAUS_cur", "GAS_cur", "VOMIT_cur", "ISCT_cur", "GXT_cur", "TMA_cur", "RQANGINA_cur", "COMSTC_cur", "ASPCMP_cur", "AMSMK_cur", "DALC_cur", 
             "PHYSA_cur", "PHYSB_cur",
             #prev
             "CHOL_prev", "HDL_prev", "LDL_prev", "TG_prev", "CAL_prev", "PROT_prev", "PSRATIO_prev", "FAT_prev", 
             "CARBT_prev", "CREAT_prev", "GLUC_prev", "SODIUM_prev", "PHOS_prev", "POT_prev", "THYR_prev", "IRON_prev", "BILT_prev", 
             "BILD_prev", "ALK_prev", "ALBU_prev", "CALC_prev", "WBC_prev", "BMI_prev", "SYSBPSTD_prev", "DSTBPSTD_prev", "PULSE_prev", "ABDOM_prev", 
             "DIARR_prev", "HRTBR_prev", "NAUS_prev", "GAS_prev", "VOMIT_prev", "ISCT_prev", "GXT_prev", "TMA_prev", "RQANGINA_prev", "COMSTC_prev", 
             "ASPCMP_prev", "AMSMK_prev", "DALC_prev", "PHYSA_prev", "PHYSB_prev",
             # yearav
             "CHOL_prev_yearav", "HDL_prev_yearav", "LDL_prev_yearav", "TG_prev_yearav", 
             "CHOL_cur_yearav", "HDL_cur_yearav", "LDL_cur_yearav", "TG_cur_yearav", 
             # baseline vars
             "EDUC", "V5AGE", "MODE_CON", "RSTRAT",
             # event vars (non-outcome, non-competing risk)
             "EFUTIME.other.coronary", "EFUTIME.vascular",
             "other.coronary_cur.indicator", "other.coronary_prev.indicator", "vascular_cur.indicator", "vascular_prev.indicator", 
             "other.coronary_cur_hx", "vascular_cur_hx", 
             "other.coronary_prev_hx", "vascular_prev_hx",
             #indicators
             "indicator.missed.visit", "indicator.missing.pac", "indicator.missing.pac.censor", "measured", "censored",
             #outcome / competing risk
             "EFUTIME.outcome", "outcome", "EFUTIME.other.mortality", "other.mortality") 
catvars <- c("TRTMENT", "RSTRAT", "PHYSA_bl", "PHYSB_bl", "RACE",
             "RSTRAT_bl", 
             "PHYSB_prev",
             "PHYSB_cur")
clinic_plac19 <- clinic_plac18 %>% select(allvars)

data_for_analysis <- clinic_plac19 %>% 
  filter(VISIT<=totalVISIT) %>% 
  filter(ID_CPPT %in% {clinic_plac19 %>% select(-starts_with("EFUTIME.")) %>% {.[complete.cases(.),]} %>% .$ID_CPPT %>% unique})
data_for_analysis <- data_for_analysis %>% mutate_at(.vars=catvars,
                                                     .funs=function(x) as.factor(x))
return(data_for_analysis)
}

data_for_analysis <- clean_data(first.visit=6, censor.at.missed.visit=6)