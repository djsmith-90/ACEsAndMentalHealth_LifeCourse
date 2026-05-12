### ACEs, Mental Health and SLCMA
### Script 3 - Multiple imputation script
### Created 29/10/2025
### R version 4.3.1

## Set working directory, clear workspace, and load packages
rm(list = ls())

setwd("X:/Studies/RSBB Team/Dan/B4563 - ACEs and MH")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("mice")
library(mice)


###########################################################################################
###### Read in the processed data
load("./Data/data_analytic_B4563.RData")

# Or, if using the synthetic data
#load("./AnalysisCode_ACEsMH_B4563/SyntheticData/syntheticData_B4563.RData")
#dat <- dat_syn_df %>%
#  dplyr::select(-FALSE_DATA) # Drop the 'FALSE_DATA' column

head(dat)

## Convert relevant variables to factors
dat <- dat %>%
  mutate(male = factor(male)) %>%
  mutate(home = factor(home)) %>%
  mutate(marital = factor(marital)) %>%
  mutate(edu = factor(edu)) %>%
  mutate(imd = factor(imd)) %>%
  mutate(ethnic = factor(ethnic)) %>%
  mutate(priorMH = factor(priorMH)) %>%
  mutate(mat_pregsmk = factor(mat_pregsmk)) %>%
  mutate(mat_soc = factor(mat_soc)) %>%
  mutate(pat_soc = factor(pat_soc)) %>%
  mutate(b596 = factor(b596)) %>%
  mutate(pb186a = factor(pb186a)) %>%
  mutate(f246a = factor(f246a)) %>%
  mutate(pd247a = factor(pd247a)) %>%
  mutate(g326a = factor(g326a)) %>%
  mutate(pe327a = factor(pe327a)) %>%
  mutate(h236a = factor(h236a)) %>%
  mutate(pf5027 = factor(pf5027)) %>%
  mutate(j326a = factor(j326a)) %>%
  mutate(pg3027 = factor(pg3027)) %>%
  mutate(f247a = factor(f247a)) %>%
  mutate(pd246a = factor(pd246a)) %>%
  mutate(g327a = factor(g327a)) %>%
  mutate(pe326a = factor(pe326a)) %>%
  mutate(h237a = factor(h237a)) %>%
  mutate(pf5026 = factor(pf5026)) %>%
  mutate(j327a = factor(j327a)) %>%
  mutate(pg3026 = factor(pg3026)) %>%
  mutate(b608 = factor(b608)) %>%
  mutate(f257a = factor(f257a)) %>%
  mutate(pd258a = factor(pd258a)) %>%
  mutate(g337a = factor(g337a)) %>%
  mutate(pe338a = factor(pe338a)) %>%
  mutate(h247a = factor(h247a)) %>%
  mutate(pf5036 = factor(pf5036)) %>%
  mutate(j337a = factor(j337a)) %>%
  mutate(pg3036 = factor(pg3036)) %>%
  mutate(pc236a = factor(pc236a)) %>%
  mutate(f258a = factor(f258a)) %>%
  mutate(pd257a = factor(pd257a)) %>%
  mutate(g338a = factor(g338a)) %>%
  mutate(pe337a = factor(pe337a)) %>%
  mutate(h248a = factor(h248a)) %>%
  mutate(pf5037 = factor(pf5037)) %>%
  mutate(j338a = factor(j338a)) %>%
  mutate(pg3037 = factor(pg3037)) %>%
  mutate(pc222a = factor(pc222a)) %>%
  mutate(pe322a = factor(pe322a)) %>%
  mutate(pf5022 = factor(pf5022)) %>%
  mutate(pg3022 = factor(pg3022)) %>%
  mutate(e422 = factor(e422)) %>%
  mutate(f242a = factor(f242a)) %>%
  mutate(pd242a = factor(pd242a)) %>%
  mutate(g322a = factor(g322a)) %>%
  mutate(h232a = factor(h232a)) %>%
  mutate(j322a = factor(j322a)) %>%
  mutate(f256a = factor(f256a)) %>%
  mutate(kd505b = factor(kd505b)) %>%
  mutate(kf455a = factor(kf455a)) %>%
  mutate(kj465a = factor(kj465a)) %>%
  mutate(j548 = factor(j548)) %>%
  mutate(pg4148 = factor(pg4148)) %>%
  mutate(ccc250 = factor(ccc250)) %>%
  mutate(ccf149 = factor(ccf149)) %>%
  mutate(kq338 = factor(kq338)) %>%
  mutate(ccc290 = factor(ccc290)) %>%
  mutate(ku698 = factor(ku698)) %>%
  mutate(f8fp151 = factor(f8fp151)) %>%
  mutate(f8fp161 = factor(f8fp161)) %>%
  mutate(fdfp151 = factor(fdfp151)) %>%
  mutate(fdfp161 = factor(fdfp161)) %>%
  mutate(n8358 = factor(n8358)) %>%
  mutate(ph4026 = factor(ph4026)) %>%
  mutate(l4026 = factor(l4026)) %>%
  mutate(pj4027 = factor(pj4027)) %>%
  mutate(p2026 = factor(p2026)) %>%
  mutate(pm2027 = factor(pm2027)) %>%
  mutate(k4026 = factor(k4026)) %>%
  mutate(k4027 = factor(k4027)) %>%
  mutate(ph4027 = factor(ph4027)) %>%
  mutate(l4027 = factor(l4027)) %>%
  mutate(pj4026 = factor(pj4026)) %>%
  mutate(p2027 = factor(p2027)) %>%
  mutate(pm2026 = factor(pm2026)) %>%
  mutate(YPB8004 = factor(YPB8004)) %>%
  mutate(YPB8023 = factor(YPB8023)) %>%
  mutate(YPB8002 = factor(YPB8002)) %>%
  mutate(YPB8006 = factor(YPB8006)) %>%
  mutate(YPB8007 = factor(YPB8007)) %>%
  mutate(kw4041 = factor(kw4041)) %>%
  mutate(ph4038 = factor(ph4038)) %>%
  mutate(l4037 = factor(l4037)) %>%
  mutate(pj4038 = factor(pj4038)) %>%
  mutate(p2037 = factor(p2037)) %>%
  mutate(pm2038 = factor(pm2038)) %>%
  mutate(k4038 = factor(k4038)) %>%
  mutate(ph4037 = factor(ph4037)) %>%
  mutate(l4038 = factor(l4038)) %>%
  mutate(pj4037 = factor(pj4037)) %>%
  mutate(p2038 = factor(p2038)) %>%
  mutate(pm2037 = factor(pm2037)) %>%
  mutate(YPB8005 = factor(YPB8005)) %>%
  mutate(YPB8001 = factor(YPB8001)) %>%
  mutate(pj4036 = factor(pj4036)) %>%
  mutate(k4037 = factor(k4037)) %>%
  mutate(YPB8022 = factor(YPB8022)) %>%
  mutate(k4022 = factor(k4022)) %>%
  mutate(l4022 = factor(l4022)) %>%
  mutate(p2022 = factor(p2022)) %>%
  mutate(ph4022 = factor(ph4022)) %>%
  mutate(pj4022 = factor(pj4022)) %>%
  mutate(pm3155 = factor(pm3155)) %>%
  mutate(pm2022 = factor(pm2022)) %>%
  mutate(kl475 = factor(kl475)) %>%
  mutate(kn4005 = factor(kn4005)) %>%
  mutate(kq365a = factor(kq365a)) %>%
  mutate(kt5005 = factor(kt5005)) %>%
  mutate(YPB8040 = factor(YPB8040)) %>%
  mutate(YPB8030 = factor(YPB8030)) %>%
  mutate(ff5318 = factor(ff5318)) %>%
  mutate(fg7118 = factor(fg7118)) %>%
  mutate(fh8200 = factor(fh8200)) %>%
  mutate(fh9821 = factor(fh9821)) %>%
  mutate(ccxa243 = factor(ccxa243)) %>%
  mutate(ccl201 = factor(ccl201)) %>%
  mutate(ta7018 = factor(ta7018)) %>%
  mutate(tc4018 = factor(tc4018)) %>%
  mutate(ff6011 = factor(ff6011)) %>%
  mutate(ff6021 = factor(ff6021)) %>%
  mutate(pp5037 = factor(pp5037)) %>%
  mutate(r5038 = factor(r5038)) %>%
  mutate(pp5038 = factor(pp5038)) %>%
  mutate(r5037 = factor(r5037)) %>%
  mutate(YPB8055 = factor(YPB8055)) %>%
  mutate(YPB8051 = factor(YPB8051)) %>%
  mutate(YPB8072 = factor(YPB8072)) %>%
  mutate(fg4432 = factor(fg4432)) %>%
  mutate(fg4422 = factor(fg4422)) %>%
  mutate(fg4424 = factor(fg4424)) %>%
  mutate(fg4426 = factor(fg4426)) %>%
  mutate(tc1148 = factor(tc1148)) %>%
  mutate(tc1174 = factor(tc1174)) %>%
  mutate(pp5027 = factor(pp5027)) %>%
  mutate(r5026 = factor(r5026)) %>%
  mutate(pp5026 = factor(pp5026)) %>%
  mutate(r5027 = factor(r5027)) %>%
  mutate(YPB8057 = factor(YPB8057)) %>%
  mutate(YPB8052 = factor(YPB8052)) %>%
  mutate(YPB8054 = factor(YPB8054)) %>%
  mutate(YPB8056 = factor(YPB8056)) %>%
  mutate(YPB8073 = factor(YPB8073)) %>%
  mutate(YPA5004 = factor(YPA5004)) %>%
  mutate(YPA5006 = factor(YPA5006)) %>%
  mutate(YPB8080 = factor(YPB8080)) %>%
  mutate(YPB8090 = factor(YPB8090)) %>%
  mutate(YPA5008 = factor(YPA5008)) %>%
  mutate(YPA5010 = factor(YPA5010)) %>%
  mutate(YPA5012 = factor(YPA5012)) %>%
  mutate(YPA5014 = factor(YPA5014)) %>%
  mutate(pp5022 = factor(pp5022)) %>%
  mutate(r5022 = factor(r5022)) %>%
  mutate(pq3154 = factor(pq3154)) %>%
  mutate(s3154 = factor(s3154)) %>%
  mutate(anx17 = factor(anx17)) %>%
  mutate(dep17 = factor(dep17)) %>%
  mutate(anx24 = factor(anx24)) %>%
  mutate(dep24 = factor(dep24))


glimpse(dat)
summary(dat)


#### Prep the MI procedure (note that only performing MI once, as can use imputations here for all subsequent analyses)

### This is a lil complicated, as have to impute the individual ACE variables first, before passively imputing the derived ACE variables based on these values. Due to the massive number of variables, will also impute in batches. That is, for physical abuse 0-5 will impute these variables based on all other physical abuse 0-5 variables, plus all other derived ACE variables, plus exposures, outcomes and auxiliary variables. Then repeat for emotional abuse 0-5, and so on. Will specify these in formulae

## Method - Will update maternal age to 'norm' and specify passive imputation for derived ACES
meth <- make.method(dat)
meth["mat_age"] <- "norm"
meth["clon140"] <- "~ I(pmax(as.numeric(b596) - 1, as.numeric(pb186a) - 1, as.numeric(f246a) - 1, as.numeric(pd247a) - 1, as.numeric(g326a) - 1, as.numeric(pe327a) - 1, as.numeric(h236a) - 1, as.numeric(pf5027) - 1, as.numeric(j326a) - 1, as.numeric(pg3027) - 1, as.numeric(f247a) - 1, as.numeric(pd246a) - 1, as.numeric(g327a) - 1, as.numeric(pe326a) - 1, as.numeric(h237a) - 1, as.numeric(pf5026) - 1, as.numeric(j327a) - 1, as.numeric(pg3026) - 1))"
meth["clon141"] <- "~ I(pmax(as.numeric(b608) - 1, as.numeric(f257a) - 1, as.numeric(pd258a) - 1, as.numeric(g337a) - 1, as.numeric(pe338a) - 1, as.numeric(h247a) - 1, as.numeric(pf5036) - 1, as.numeric(j337a) - 1, as.numeric(pg3036) - 1, as.numeric(pc236a) - 1, as.numeric(f258a) - 1, as.numeric(pd257a) - 1, as.numeric(g338a) - 1, as.numeric(pe337a) - 1, as.numeric(h248a) - 1, as.numeric(pf5037) - 1, as.numeric(j338a) - 1, as.numeric(pg3037) - 1))"
meth["clon142"] <- "~ I(pmax(as.numeric(pc222a) - 1, as.numeric(pe322a) - 1, as.numeric(pf5022) - 1, as.numeric(pg3022) - 1, as.numeric(e422) - 1, as.numeric(f242a) - 1, as.numeric(pd242a) - 1, as.numeric(g322a) - 1, as.numeric(h232a) - 1, as.numeric(j322a) - 1, as.numeric(f256a) - 1))"
meth["clon143"] <- "~ I(pmax(as.numeric(kd505b) - 1, as.numeric(kf455a) - 1, as.numeric(kj465a) - 1))"
meth["clon144"] <- "~ I(pmax(as.numeric(j548) - 1, as.numeric(pg4148) - 1))"
meth["clon146"] <- "~ I(pmax(as.numeric(ccc250) - 1, as.numeric(ccf149) - 1))"
meth["clon147"] <- "~ I(pmax(as.numeric(kq338) - 1, as.numeric(ccc290) - 1, as.numeric(ku698) - 1, as.numeric(f8fp151) - 1, as.numeric(f8fp161) - 1, as.numeric(fdfp151) - 1, as.numeric(fdfp161) - 1, as.numeric(n8358) - 1))"
meth["clon148"] <- "~ I(pmax(as.numeric(ph4026) - 1, as.numeric(l4026) - 1, as.numeric(pj4027) - 1, as.numeric(p2026) - 1, as.numeric(pm2027) - 1, as.numeric(k4026) - 1, as.numeric(k4027) - 1, as.numeric(ph4027) - 1, as.numeric(l4027) - 1, as.numeric(pj4026) - 1, as.numeric(p2027) - 1, as.numeric(pm2026) - 1, as.numeric(YPB8004) - 1, as.numeric(YPB8023) - 1, as.numeric(YPB8002) - 1, as.numeric(YPB8006) - 1, as.numeric(YPB8007) - 1, as.numeric(kw4041) - 1))"
meth["clon149"] <- "~ I(pmax(as.numeric(ph4038) - 1, as.numeric(l4037) - 1, as.numeric(pj4038) - 1, as.numeric(p2037) - 1, as.numeric(pm2038) - 1, as.numeric(k4038) - 1, as.numeric(ph4037) - 1, as.numeric(l4038) - 1, as.numeric(pj4037) - 1, as.numeric(p2038) - 1, as.numeric(pm2037) - 1, as.numeric(YPB8005) - 1, as.numeric(YPB8001) - 1, as.numeric(pj4036) - 1, as.numeric(k4037) - 1, as.numeric(YPB8022) - 1))"
meth["clon150"] <- "~ I(pmax(as.numeric(k4022) - 1, as.numeric(l4022) - 1, as.numeric(p2022) - 1, as.numeric(ph4022) - 1, as.numeric(pj4022) - 1, as.numeric(pm3155) - 1, as.numeric(pm2022) - 1))"
meth["clon151"] <- "~ I(pmax(as.numeric(kl475) - 1, as.numeric(kn4005) - 1, as.numeric(kq365a) - 1, as.numeric(kt5005) - 1, as.numeric(YPB8040) - 1, as.numeric(YPB8030) - 1))"
meth["clon153"] <- "~ I(pmax(as.numeric(ff5318) - 1, as.numeric(fg7118) - 1, as.numeric(fh8200) - 1, as.numeric(fh9821) - 1, as.numeric(ccxa243) - 1))"
meth["clon154"] <- "~ I(pmax(as.numeric(ccl201) - 1, as.numeric(ta7018) - 1, as.numeric(tc4018) - 1, as.numeric(ff6011) - 1, as.numeric(ff6021) - 1))"
meth["clon155"] <- "~ I(pmax(as.numeric(pp5037) - 1, as.numeric(r5038) - 1, as.numeric(pp5038) - 1, as.numeric(r5037) - 1, as.numeric(YPB8055) - 1, as.numeric(YPB8051) - 1, as.numeric(YPB8072) - 1))"
meth["clon156"] <- "~ I(pmax(as.numeric(fg4432) - 1, as.numeric(fg4422) - 1, as.numeric(fg4424) - 1, as.numeric(fg4426) - 1, as.numeric(tc1148) - 1, as.numeric(tc1174) - 1, as.numeric(pp5027) - 1, as.numeric(r5026) - 1, as.numeric(pp5026) - 1, as.numeric(r5027) - 1, as.numeric(YPB8057) - 1, as.numeric(YPB8052) - 1, as.numeric(YPB8054) - 1, as.numeric(YPB8056) - 1, as.numeric(YPB8073) - 1, as.numeric(YPA5004) - 1, as.numeric(YPA5006) - 1))"
meth["clon157"] <- "~ I(pmax(as.numeric(YPB8080) - 1, as.numeric(YPB8090) - 1, as.numeric(YPA5008) - 1, as.numeric(YPA5010) - 1, as.numeric(YPA5012) - 1, as.numeric(YPA5014) - 1))"
meth["clon158"] <- "~ I(pmax(as.numeric(pp5022) - 1, as.numeric(r5022) - 1, as.numeric(pq3154) - 1, as.numeric(s3154) - 1))"
meth["clon145"] <- "~ I(pmax(clon140, clon141, clon142, clon143, clon144))"
meth["clon152"] <- "~ I(pmax(clon146, clon147, clon148, clon149, clon150, clon151))"
meth["clon159"] <- "~ I(pmax(clon153, clon154, clon155, clon156, clon157, clon158))"
meth[meth == "logreg"] <- "pmm" ## MI with logistic regression for ACE variables gives non-sensical results (e.g., over-inflated rates of ACEs in imputed sample). Is plausibly due to estimation issues with logistic regression, so will use PMM as this gives much more plausible results
meth["home"] <- "logreg"
meth["marital"] <- "logreg"
meth["ethnic"] <- "logreg"
meth["priorMH"] <- "logreg"
meth["mat_pregsmk"] <- "logreg"
meth



## Formulae
form <- make.formulas(dat)

# Physical abuse 0-5
form$b596 <- as.formula(b596 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pb186a + f246a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pb186a <- as.formula(pb186a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + f246a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$f246a <- as.formula(f246a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pd247a <- as.formula(pd247a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$g326a <- as.formula(g326a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pe327a <- as.formula(pe327a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$h236a <- as.formula(h236a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pf5027 <- as.formula(pf5027 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + h236a + j326a + pg3027 + f247a + pd246a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$j326a <- as.formula(j326a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + h236a + pf5027 + pg3027 + f247a + pd246a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pg3027 <- as.formula(pg3027 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + f247a + pd246a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$f247a <- as.formula(f247a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + pd246a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pd246a <- as.formula(pd246a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + g327a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$g327a <- as.formula(g327a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + pe326a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pe326a <- as.formula(pe326a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + h237a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$h237a <- as.formula(h237a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + pe326a + pf5026 + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pf5026 <- as.formula(pf5026 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + pe326a + h237a + j327a + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$j327a <- as.formula(j327a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + pe326a + h237a + pf5026 + pg3026 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pg3026 <- as.formula(pg3026 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b596 + pb186a + f246a + pd247a + g326a + pe327a + h236a + pf5027 + j326a + pg3027 + f247a + pd246a + g327a + pe326a + h237a + pf5026 + j327a + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Emotional abuse 0-5
form$b608 <- as.formula(b608 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + f257a + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$f257a <- as.formula(f257a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pd258a <- as.formula(pd258a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$g337a <- as.formula(g337a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pe338a <- as.formula(pe338a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$h247a <- as.formula(h247a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pf5036 <- as.formula(pf5036 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + j337a + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$j337a <- as.formula(j337a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + pf5036 + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pg3036 <- as.formula(pg3036 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pc236a + f258a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pc236a <- as.formula(pc236a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + f258a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$f258a <- as.formula(f258a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pd257a <- as.formula(pd257a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + g338a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$g338a <- as.formula(g338a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + pe337a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pe337a <- as.formula(pe337a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + g338a + h248a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$h248a <- as.formula(h248a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + pf5037 + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pf5037 <- as.formula(pf5037 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + h248a + j338a + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$j338a <- as.formula(j338a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + h248a + pf5037 + pg3037 + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pg3037 <- as.formula(pg3037 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + b608 + f257a + pd258a + g337a + pe338a + h247a + pf5036 + j337a + pg3036 + pc236a + f258a + pd257a + g338a + pe337a + h248a + pf5037 + j338a + clon140 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Domestic violence 0-5
form$pc222a <- as.formula(pc222a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pe322a + pf5022 + pg3022 + e422 + f242a + pd242a + g322a + h232a + j322a + f256a + clon140 + clon141 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pe322a <- as.formula(pe322a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pc222a + pf5022 + pg3022 + e422 + f242a + pd242a + g322a + h232a + j322a + f256a + clon140 + clon141 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pf5022 <- as.formula(pf5022 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pc222a + pe322a + pg3022 + e422 + f242a + pd242a + g322a + h232a + j322a + f256a + clon140 + clon141 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pg3022 <- as.formula(pg3022 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pc222a + pe322a + pf5022 + e422 + f242a + pd242a + g322a + h232a + j322a + f256a + clon140 + clon141 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$e422 <- as.formula(e422 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pc222a + pe322a + pf5022 + pg3022 + f242a + pd242a + g322a + h232a + j322a + f256a + clon140 + clon141 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$f242a <- as.formula(f242a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pc222a + pe322a + pf5022 + pg3022 + e422 + pd242a + g322a + h232a + j322a + f256a + clon140 + clon141 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pd242a <- as.formula(pd242a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pc222a + pe322a + pf5022 + pg3022 + e422 + f242a + g322a + h232a + j322a + f256a + clon140 + clon141 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$g322a <- as.formula(g322a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pc222a + pe322a + pf5022 + pg3022 + e422 + f242a + pd242a + h232a + j322a + f256a + clon140 + clon141 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$h232a <- as.formula(h232a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pc222a + pe322a + pf5022 + pg3022 + e422 + f242a + pd242a + g322a + j322a + f256a + clon140 + clon141 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$j322a <- as.formula(j322a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pc222a + pe322a + pf5022 + pg3022 + e422 + f242a + pd242a + g322a + h232a + f256a + clon140 + clon141 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$f256a <- as.formula(f256a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pc222a + pe322a + pf5022 + pg3022 + e422 + f242a + pd242a + g322a + h232a + j322a + clon140 + clon141 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Sexual abuse 0-5
form$kd505b <- as.formula(kd505b ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kf455a + kj465a + clon140 + clon141 + clon142 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$kf455a <- as.formula(kf455a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kd505b + kj465a + clon140 + clon141 + clon142 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$kj465a <- as.formula(kj465a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kd505b + kf455a + clon140 + clon141 + clon142 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Bullying 0-5
form$j548 <- as.formula(j548 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pg4148 + clon140 + clon141 + clon142 + clon143 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pg4148 <- as.formula(pg4148 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + j548 + clon140 + clon141 + clon142 + clon143 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Emotional neglect 5-11
form$ccc250 <- as.formula(ccc250 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ccf149 + clon140 + clon141 + clon142 + clon143 + clon144 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$ccf149 <- as.formula(ccf149 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ccc250 + clon140 + clon141 + clon142 + clon143 + clon144 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Bullying 5-11
form$kq338 <- as.formula(kq338 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ccc290 + ku698 + f8fp151 + f8fp161 + fdfp151 + fdfp161 + n8358 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$ccc290 <- as.formula(ccc290 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kq338 + ku698 + f8fp151 + f8fp161 + fdfp151 + fdfp161 + n8358 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$ku698 <- as.formula(ku698 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kq338 + ccc290 + f8fp151 + f8fp161 + fdfp151 + fdfp161 + n8358 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$f8fp151 <- as.formula(f8fp151 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kq338 + ccc290 + ku698 + f8fp161 + fdfp151 + fdfp161 + n8358 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$f8fp161 <- as.formula(f8fp161 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kq338 + ccc290 + ku698 + f8fp151 + fdfp151 + fdfp161 + n8358 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$fdfp151 <- as.formula(fdfp151 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kq338 + ccc290 + ku698 + f8fp151 + f8fp161 + fdfp161 + n8358 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$fdfp161 <- as.formula(fdfp161 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kq338 + ccc290 + ku698 + f8fp151 + f8fp161 + fdfp151 + n8358 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$n8358 <- as.formula(n8358 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kq338 + ccc290 + ku698 + f8fp151 + f8fp161 + fdfp151 + fdfp161 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Physical abuse 5-11
form$ph4026 <- as.formula(ph4026 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$l4026 <- as.formula(l4026 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pj4027 <- as.formula(pj4027 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$p2026 <- as.formula(p2026 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pm2027 <- as.formula(pm2027 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$k4026 <- as.formula(k4026 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$k4027 <- as.formula(k4027 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$ph4027 <- as.formula(ph4027 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$l4027 <- as.formula(l4027 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pj4026 <- as.formula(pj4026 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$p2027 <- as.formula(p2027 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pm2026 <- as.formula(pm2026 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8004 <- as.formula(YPB8004 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8023 <- as.formula(YPB8023 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8002 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8002 <- as.formula(YPB8002 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8006 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8006 <- as.formula(YPB8006 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8007 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8007 <- as.formula(YPB8007 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + kw4041 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$kw4041 <- as.formula(kw4041 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4026 + l4026 + pj4027 + p2026 + pm2027 + k4026 + k4027 + ph4027 + l4027 + pj4026 + p2027 + pm2026 + YPB8004 + YPB8023 + YPB8002 + YPB8006 + YPB8007 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Emotional abuse 5-11
form$ph4038 <- as.formula(ph4038 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + l4037 + pj4038 + p2037 + pm2038 + k4038 + ph4037 + l4038 + pj4037 + p2038 + pm2037 + YPB8005 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$l4037 <- as.formula(l4037 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + pj4038 + p2037 + pm2038 + k4038 + ph4037 + l4038 + pj4037 + p2038 + pm2037 + YPB8005 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pj4038 <- as.formula(pj4038 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + p2037 + pm2038 + k4038 + ph4037 + l4038 + pj4037 + p2038 + pm2037 + YPB8005 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$p2037 <- as.formula(p2037 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + pm2038 + k4038 + ph4037 + l4038 + pj4037 + p2038 + pm2037 + YPB8005 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pm2038 <- as.formula(pm2038 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + k4038 + ph4037 + l4038 + pj4037 + p2038 + pm2037 + YPB8005 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$k4038 <- as.formula(k4038 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + pm2038 + ph4037 + l4038 + pj4037 + p2038 + pm2037 + YPB8005 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$ph4037 <- as.formula(ph4037 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + pm2038 + k4038 + l4038 + pj4037 + p2038 + pm2037 + YPB8005 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$l4038 <- as.formula(l4038 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + pm2038 + k4038 + ph4037 + pj4037 + p2038 + pm2037 + YPB8005 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pj4037 <- as.formula(pj4037 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + pm2038 + k4038 + ph4037 + l4038 + p2038 + pm2037 + YPB8005 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$p2038 <- as.formula(p2038 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + pm2038 + k4038 + ph4037 + l4038 + pj4037 + pm2037 + YPB8005 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pm2037 <- as.formula(pm2037 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + pm2038 + k4038 + ph4037 + l4038 + pj4037 + p2038 + YPB8005 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8005 <- as.formula(YPB8005 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + pm2038 + k4038 + ph4037 + l4038 + pj4037 + p2038 + pm2037 + YPB8001 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8001 <- as.formula(YPB8001 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + pm2038 + k4038 + ph4037 + l4038 + pj4037 + p2038 + pm2037 + YPB8005 + pj4036 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pj4036 <- as.formula(pj4036 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + pm2038 + k4038 + ph4037 + l4038 + pj4037 + p2038 + pm2037 + YPB8005 + YPB8001 + k4037 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$k4037 <- as.formula(k4037 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + pm2038 + k4038 + ph4037 + l4038 + pj4037 + p2038 + pm2037 + YPB8005 + YPB8001 + pj4036 + YPB8022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8022 <- as.formula(YPB8022 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ph4038 + l4037 + pj4038 + p2037 + pm2038 + k4038 + ph4037 + l4038 + pj4037 + p2038 + pm2037 + YPB8005 + YPB8001 + pj4036 + k4037 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Domestic violence 5-11
form$k4022 <- as.formula(k4022 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + l4022 + p2022 + ph4022 + pj4022 + pm3155 + pm2022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$l4022 <- as.formula(l4022 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + k4022 + p2022 + ph4022 + pj4022 + pm3155 + pm2022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$p2022 <- as.formula(p2022 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + k4022 + l4022 + ph4022 + pj4022 + pm3155 + pm2022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$ph4022 <- as.formula(ph4022 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + k4022 + l4022 + p2022 + pj4022 + pm3155 + pm2022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pj4022 <- as.formula(pj4022 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + k4022 + l4022 + p2022 + ph4022 + pm3155 + pm2022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pm3155 <- as.formula(pm3155 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + k4022 + l4022 + p2022 + ph4022 + pj4022 + pm2022 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pm2022 <- as.formula(pm2022 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + k4022 + l4022 + p2022 + ph4022 + pj4022 + pm3155 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Sexual abuse 5-11
form$kl475 <- as.formula(kl475 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kn4005 + kq365a + kt5005 + YPB8040 + YPB8030 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$kn4005 <- as.formula(kn4005 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kl475 + kq365a + kt5005 + YPB8040 + YPB8030 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$kq365a <- as.formula(kq365a ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kl475 + kn4005 + kt5005 + YPB8040 + YPB8030 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$kt5005 <- as.formula(kt5005 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kl475 + kn4005 + kq365a + YPB8040 + YPB8030 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8040 <- as.formula(YPB8040 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kl475 + kn4005 + kq365a + kt5005 + YPB8030 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8030 <- as.formula(YPB8030 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + kl475 + kn4005 + kq365a + kt5005 + YPB8040 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Emotional neglect 11-17
form$ff5318 <- as.formula(ff5318 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg7118 + fh8200 + fh9821 + ccxa243 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$fg7118 <- as.formula(fg7118 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ff5318 + fh8200 + fh9821 + ccxa243 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$fh8200 <- as.formula(fh8200 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ff5318 + fg7118 + fh9821 + ccxa243 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$fh9821 <- as.formula(fh9821 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ff5318 + fg7118 + fh8200 + ccxa243 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$ccxa243 <- as.formula(ccxa243 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ff5318 + fg7118 + fh8200 + fh9821 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Bullying 11-17
form$ccl201 <- as.formula(ccl201 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ta7018 + tc4018 + ff6011 + ff6021 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$ta7018 <- as.formula(ta7018 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ccl201 + tc4018 + ff6011 + ff6021 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$tc4018 <- as.formula(tc4018 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ccl201 + ta7018 + ff6011 + ff6021 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$ff6011 <- as.formula(ff6011 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ccl201 + ta7018 + tc4018 + ff6021 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$ff6021 <- as.formula(ff6021 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + ccl201 + ta7018 + tc4018 + ff6011 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Emotional abuse 11-17
form$pp5037 <- as.formula(pp5037 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + r5038 + pp5038 + r5037 + YPB8055 + YPB8051 + YPB8072 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$r5038 <- as.formula(r5038 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pp5037 + pp5038 + r5037 + YPB8055 + YPB8051 + YPB8072 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pp5038 <- as.formula(pp5038 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pp5037 + r5038 + r5037 + YPB8055 + YPB8051 + YPB8072 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$r5037 <- as.formula(r5037 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pp5037 + r5038 + pp5038 + YPB8055 + YPB8051 + YPB8072 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8055 <- as.formula(YPB8055 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pp5037 + r5038 + pp5038 + r5037 + YPB8051 + YPB8072 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8051 <- as.formula(YPB8051 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pp5037 + r5038 + pp5038 + r5037 + YPB8055 + YPB8072 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8072 <- as.formula(YPB8072 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pp5037 + r5038 + pp5038 + r5037 + YPB8055 + YPB8051 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Physical abuse 11-17
form$fg4432 <- as.formula(fg4432 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$fg4422 <- as.formula(fg4422 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$fg4424 <- as.formula(fg4424 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$fg4426 <- as.formula(fg4426 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$tc1148 <- as.formula(tc1148 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$tc1174 <- as.formula(tc1174 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pp5027 <- as.formula(pp5027 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$r5026 <- as.formula(r5026 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pp5026 <- as.formula(pp5026 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$r5027 <- as.formula(r5027 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8057 <- as.formula(YPB8057 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8052 <- as.formula(YPB8052 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8054 <- as.formula(YPB8054 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8056 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8056 <- as.formula(YPB8056 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8073 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8073 <- as.formula(YPB8073 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPA5004 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPA5004 <- as.formula(YPA5004 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5006 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPA5006 <- as.formula(YPA5006 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + fg4432 + fg4422 + fg4424 + fg4426 + tc1148 + tc1174 + pp5027 + r5026 + pp5026 + r5027 + YPB8057 + YPB8052 + YPB8054 + YPB8056 + YPB8073 + YPA5004 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Sexual abuse 11-17
form$YPB8080 <- as.formula(YPB8080 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + YPB8090 + YPA5008 + YPA5010 + YPA5012 + YPA5014 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPB8090 <- as.formula(YPB8090 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + YPB8080 + YPA5008 + YPA5010 + YPA5012 + YPA5014 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPA5008 <- as.formula(YPA5008 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + YPB8080 + YPB8090 + YPA5010 + YPA5012 + YPA5014 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPA5010 <- as.formula(YPA5010 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + YPB8080 + YPB8090 + YPA5008 + YPA5012 + YPA5014 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPA5012 <- as.formula(YPA5012 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + YPB8080 + YPB8090 + YPA5008 + YPA5010 + YPA5014 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon158 + anx17 + dep17 + anx24 + dep24)

form$YPA5014 <- as.formula(YPA5014 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + YPB8080 + YPB8090 + YPA5008 + YPA5010 + YPA5012 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon158 + anx17 + dep17 + anx24 + dep24)

# Domestic violence 11-17
form$pp5022 <- as.formula(pp5022 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + r5022 + pq3154 + s3154 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + anx17 + dep17 + anx24 + dep24)

form$r5022 <- as.formula(r5022 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pp5022 + pq3154 + s3154 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + anx17 + dep17 + anx24 + dep24)

form$pq3154 <- as.formula(pq3154 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pp5022 + r5022 + s3154 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + anx17 + dep17 + anx24 + dep24)

form$s3154 <- as.formula(s3154 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + pp5022 + r5022 + pq3154 + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + anx17 + dep17 + anx24 + dep24)

# Confounders
form$mat_age <- as.formula(mat_age ~ male + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$home <- as.formula(home ~ male + mat_age + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$marital <- as.formula(marital ~ male + mat_age + home + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$edu <- as.formula(edu ~ male + mat_age + home + marital + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$imd <- as.formula(imd ~ male + mat_age + home + marital + edu + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$ethnic <- as.formula(ethnic ~ male + mat_age + home + marital + edu + imd + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$mat_dep <- as.formula(mat_dep ~ male + mat_age + home + marital + edu + imd + ethnic + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$mat_anx <- as.formula(mat_anx ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$mat_aces <- as.formula(mat_aces ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Auxiliary variables
form$priorMH <- as.formula(priorMH ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$mat_pregsmk <- as.formula(mat_pregsmk ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$mat_soc <- as.formula(mat_soc ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

form$pat_soc <- as.formula(pat_soc ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24 + dep24)

# Outcomes

form$anx17 <- as.formula(anx17 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + dep17 + anx24 + dep24)

form$dep17 <- as.formula(dep17 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + anx24 + dep24)

form$anx24 <- as.formula(anx24 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + dep24)

form$dep24 <- as.formula(dep24 ~ male + mat_age + home + marital + edu + imd + ethnic + mat_dep + mat_anx + mat_aces + priorMH + mat_pregsmk + mat_soc + pat_soc + clon140 + clon141 + clon142 + clon143 + clon144 + clon146 + clon147 + clon148 + clon149 + clon150 + clon151 + clon153 + clon154 + clon155 + clon156 + clon157 + clon158 + anx17 + dep17 + anx24)

form


## Visit sequence - Make sure passively-imputed ACE variables are after their constituent variables - They are, so no need to change anything here
visit <- make.visitSequence(dat)
visit


## Run a test imputation to make sure it looks okay, and check the amount of missing data in each variable
test <- mice(dat, m = 5, maxit = 0, 
             method = meth, formulas = form, print = TRUE)
test
table(test$nmis)


## Proper imputation model. As running SLCMA in different imputed datasets to check are consistent, will only create 5 imputed datasets. As is a complex model, will run for 20 iterations per imputation
imp <- mice(dat, m = 5, maxit = 20, 
             method = meth, formulas = form, print = TRUE, seed = 29102025)


## Save imputations, to avoid having to run again
save(imp, file = "./Data/data_imputed_B4563.RData")
#load("./Data/data_imputed_B4563.RData")

## Check imputations worked correctly and imputed data are sensible
imp1 <- complete(imp, 1)
head(imp1)

table(imp1$clon145); table(dat$clon145)
table(imp1$clon152); table(dat$clon152)
table(imp1$clon159); table(dat$clon159)

table(imp1$clon140); table(dat$clon140)
table(imp1$clon141); table(dat$clon141)
table(imp1$clon142); table(dat$clon142)
table(imp1$clon143); table(dat$clon143)
table(imp1$clon144); table(dat$clon144)

table(imp1$clon146); table(dat$clon146)
table(imp1$clon147); table(dat$clon147)
table(imp1$clon148); table(dat$clon148)
table(imp1$clon149); table(dat$clon149)
table(imp1$clon150); table(dat$clon150)
table(imp1$clon151); table(dat$clon151)

table(imp1$clon153); table(dat$clon153)
table(imp1$clon154); table(dat$clon154)
table(imp1$clon155); table(dat$clon155)
table(imp1$clon156); table(dat$clon156)
table(imp1$clon157); table(dat$clon157)
table(imp1$clon158); table(dat$clon158)

