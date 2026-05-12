### ACEs, Mental Health and SLCMA
### Script 1 - Data processing/cleaning script
### Created 28/10/2025
### R version 4.3.1

## Set working directory, clear workspace, and load packages
rm(list = ls())

setwd("X:/Studies/RSBB Team/Dan/B4563 - ACEs and MH")

#remotes::install_github("explodecomputer/alspac")
library(alspac)
setDataDir("//ads.bris.ac.uk/filestore/SSCM ALSPAC/Data")

#install.packages("tidyverse")
library(tidyverse)


######################################################################################################
## Extract the ALSPAC data using the 'alspac' R package (https://github.com/explodecomputer/alspac)
varnames <- c(
  
  # Confounders (offspring sex, maternal age, maternal marital status, maternal education, IMD, ethnnicity, MH, and ACEs)
  "kz011b", "kz021", "mz028b", "a006", "a525", "c645a", "jan1993imd2010q5_M", "c804", "b370", "b351", "c433",
  
  # Outcomes (depression and anxiety at ages 17 and 24)
  "FJCI602", "FJCI603", "FKDQ1000", "FKDQ1030",
  
  # Auxiliary variables (offspring MH at age 10, maternal smoking, and parental occupational social class)
  "kv8616", "b663", "b665", "b667", "c755", "c765",
  
  ## ACE exposures
  # Physical abuse 0-5
  "b596", "pb186a", "f246a", "pd247a", "g326a", "pe327a", "h236a", "pf5027", "j326a", "pg3027", "f247a", "pd246a", "g327a", "pe326a", "h237a", "pf5026", "j327a", "pg3026", "clon140",
  
  # Emotional abuse 0-5
  "b608", "f257a", "pd258a", "g337a", "pe338a", "h247a", "pf5036", "j337a", "pg3036", "pc236a", "f258a", "pd257a", "g338a", "pe337a", "h248a", "pf5037", "j338a", "pg3037", "clon141",
  
  # Domestic violence 0-5
  "pc222a", "pe322a", "pf5022", "pg3022", "e422", "f242a", "pd242a", "g322a", "h232a", "j322a", "f256a", "clon142",
  
  # Sexual abuse 0-5
  "kd505b", "kf455a", "kj465a", "clon143",
  
  # Bullying 0-5
  "j548", "pg4148", "clon144",
  
  # Emotional neglect 5-11
  "ccc250", "ccf149", "clon146",
  
  # Bullying 5-11
  "kq338", "ccc290", "ku698", "f8fp151", "f8fp161", "fdfp151", "fdfp161", "n8358", "clon147",
  
  # Physical abuse 5-11
  "ph4026", "l4026", "pj4027", "p2026", "pm2027", "k4026", "k4027", "ph4027", "l4027", "pj4026", "p2027", "pm2026", "YPB8004", "YPB8023", "YPB8002", "YPB8006", "YPB8007", "kw4041", "clon148",
  
  # Emotional abuse 5-11
  "ph4038", "l4037", "pj4038", "p2037", "pm2038", "k4038", "ph4037", "l4038", "pj4037", "p2038", "pm2037", "YPB8005", "YPB8001", "pj4036", "k4037", "YPB8022", "clon149",
  
  # Domestic violence 5-11
  "k4022", "l4022", "p2022", "ph4022", "pj4022", "pm3155", "pm2022", "clon150",
  
  # Sexual abuse 5-11
  "kl475", "kn4005", "kq365a", "kt5005", "YPB8040", "YPB8030", "clon151",
  
  # Emotional neglect 11-17
  "ff5318", "fg7118", "fh8200", "fh9821", "ccxa243", "clon153",
  
  # Bullying 11-17
  "ccl201", "ta7018", "tc4018", "ff6011", "ff6021", "clon154",
  
  # Emotional abuse 11-17
  "pp5037", "r5038", "pp5038", "r5037", "YPB8055", "YPB8051", "YPB8072", "clon155",
  
  # Physical abuse 11-17
  "fg4432", "fg4422", "fg4424", "fg4426", "tc1148", "tc1174", "pp5027", "r5026", "pp5026", "r5027", "YPB8057", "YPB8052", "YPB8054", "YPB8056", "YPB8073", "YPA5004", "YPA5005", "YPA5006", "YPA5007", "clon156",
  
  # Sexual abuse 11-17
  "YPB8080", "YPB8090", "YPA5008", "YPA5009", "YPA5010", "YPA5011", "YPA5012", "YPA5013", "YPA5014", "YPA5015", "clon157",
  
  # Domestic violence 11-17
  "pp5022", "r5022", "pq3154", "s3154", "clon158",
  
  # Any traumas in 3 age groups
  "clon145", "clon152", "clon159"

  )


# Check no duplicate variable names
varnames[duplicated(varnames)]


## Find the variables listed above
vars <- findVars(varnames)

# Make sure all listed names have been found
varnames[varnames %in% vars$name == FALSE]


# Drop if names don't match exactly (e.g., if looking for 'b596', 'pb596' will also be included)
vars_final <- subset(vars, subset = name %in% varnames)

# Make sure all listed names are still present
varnames[varnames %in% vars_final$name == FALSE]


## Make a dataset from these variables
dat_master <- extractVars(vars_final)

# Save data, so don't have to load in again
save(dat_master, file = "./Data/data_master_B4563.RData")
#load("./Data/data_master_B4563.RData")

dat <- dat_master

head(dat)
summary(dat)
names(dat)


#################################################################################
#### Processing and tidying data

### Drop if offspring not alive at 1 year of age and if from phase 5 of data collection
table(dat$kz011b, useNA = "ifany")

dat <- dat %>%
  filter(kz011b == 1)

dat <- dat %>%
  filter(in_core == 1 | in_phase2 == 1 | in_phase3 == 1 | in_phase4 == 1)
  


## Edit order of variables and drop unnecessary variables
dat <- dat %>%
  select(kz021, mz028b, a006, a525, c645a, jan1993imd2010q5_M, c804, b370, b351, c433,
         kv8616, b663, b665, b667, c755, c765,
         b596, pb186a, f246a, pd247a, g326a, pe327a, h236a, pf5027, j326a, pg3027, f247a, pd246a, g327a, pe326a, h237a, pf5026, j327a, pg3026, clon140,
         b608, f257a, pd258a, g337a, pe338a, h247a, pf5036, j337a, pg3036, pc236a, f258a, pd257a, g338a, pe337a, h248a, pf5037, j338a, pg3037, clon141,
         pc222a, pe322a, pf5022, pg3022, e422, f242a, pd242a, g322a, h232a, j322a, f256a, clon142,
         kd505b, kf455a, kj465a, clon143,
         j548, pg4148, clon144,
         ccc250, ccf149, clon146,
         kq338, ccc290, ku698, f8fp151, f8fp161, fdfp151, fdfp161, n8358, clon147,
         ph4026, l4026, pj4027, p2026, pm2027, k4026, k4027, ph4027, l4027, pj4026, p2027, pm2026, YPB8004, YPB8023, YPB8002, YPB8006, YPB8007, kw4041, clon148,
         ph4038, l4037, pj4038, p2037, pm2038, k4038, ph4037, l4038, pj4037, p2038, pm2037, YPB8005, YPB8001, pj4036, k4037, YPB8022, clon149,
         k4022, l4022, p2022, ph4022, pj4022, pm3155, pm2022, clon150,
         kl475, kn4005, kq365a, kt5005, YPB8040, YPB8030, clon151,
         ff5318, fg7118, fh8200, fh9821, ccxa243, clon153,
         ccl201, ta7018, tc4018, ff6011, ff6021, clon154,
         pp5037, r5038, pp5038, r5037, YPB8055, YPB8051, YPB8072, clon155,
         fg4432, fg4422, fg4424, fg4426, tc1148, tc1174, pp5027, r5026, pp5026, r5027, YPB8057, YPB8052, YPB8054, YPB8056, YPB8073, YPA5004, YPA5005, YPA5006, YPA5007, clon156,
         YPB8080, YPB8090, YPA5008, YPA5009, YPA5010, YPA5011, YPA5012, YPA5013, YPA5014, YPA5015, clon157,
         pp5022, r5022, pq3154, s3154, clon158,
         clon145, clon152, clon159,
         FJCI602, FJCI603, FKDQ1000, FKDQ1030) %>%
  relocate(kz021, mz028b, a006, a525, c645a, jan1993imd2010q5_M, c804, b370, b351, c433,
           kv8616, b663, b665, b667, c755, c765,
           b596, pb186a, f246a, pd247a, g326a, pe327a, h236a, pf5027, j326a, pg3027, f247a, pd246a, g327a, pe326a, h237a, pf5026, j327a, pg3026, clon140,
           b608, f257a, pd258a, g337a, pe338a, h247a, pf5036, j337a, pg3036, pc236a, f258a, pd257a, g338a, pe337a, h248a, pf5037, j338a, pg3037, clon141,
           pc222a, pe322a, pf5022, pg3022, e422, f242a, pd242a, g322a, h232a, j322a, f256a, clon142,
           kd505b, kf455a, kj465a, clon143,
           j548, pg4148, clon144,
           ccc250, ccf149, clon146,
           kq338, ccc290, ku698, f8fp151, f8fp161, fdfp151, fdfp161, n8358, clon147,
           ph4026, l4026, pj4027, p2026, pm2027, k4026, k4027, ph4027, l4027, pj4026, p2027, pm2026, YPB8004, YPB8023, YPB8002, YPB8006, YPB8007, kw4041, clon148,
           ph4038, l4037, pj4038, p2037, pm2038, k4038, ph4037, l4038, pj4037, p2038, pm2037, YPB8005, YPB8001, pj4036, k4037, YPB8022, clon149,
           k4022, l4022, p2022, ph4022, pj4022, pm3155, pm2022, clon150,
           kl475, kn4005, kq365a, kt5005, YPB8040, YPB8030, clon151,
           ff5318, fg7118, fh8200, fh9821, ccxa243, clon153,
           ccl201, ta7018, tc4018, ff6011, ff6021, clon154,
           pp5037, r5038, pp5038, r5037, YPB8055, YPB8051, YPB8072, clon155,
           fg4432, fg4422, fg4424, fg4426, tc1148, tc1174, pp5027, r5026, pp5026, r5027, YPB8057, YPB8052, YPB8054, YPB8056, YPB8073, YPA5004, YPA5005, YPA5006, YPA5007, clon156,
           YPB8080, YPB8090, YPA5008, YPA5009, YPA5010, YPA5011, YPA5012, YPA5013, YPA5014, YPA5015, clon157,
           pp5022, r5022, pq3154, s3154, clon158,
           clon145, clon152, clon159,
           FJCI602, FJCI603, FKDQ1000, FKDQ1030)


#### Go through each variable and tidy (i.e., removing missing values, coding binary variables as 0/1 for consistency, etc.)

### Confounders

## Offspring sex
table(dat$kz021, useNA = "ifany")

dat <- dat %>%
  rename(male = kz021) %>%
  mutate(male = ifelse(male == 2, 0, male))

table(dat$male, useNA = "ifany")


## Maternal age
table(dat$mz028b, useNA = "ifany")

dat <- dat %>%
  rename(mat_age = mz028b) %>%
  mutate(mat_age = ifelse(is.na(mat_age), NA,
                          ifelse(mat_age == -10, NA, mat_age)))

table(dat$mat_age, useNA = "ifany")


## Maternal home ownership status
table(dat$a006, useNA = "ifany")

dat <- dat %>%
  rename(home = a006) %>%
  mutate(home = ifelse(is.na(home), NA,
                          ifelse(home < 0, NA, 
                                 ifelse(home == 0 | home == 1, 1, 0))))

table(dat$home, useNA = "ifany")


## Maternal marital status
table(dat$a525, useNA = "ifany")

dat <- dat %>%
  rename(marital = a525) %>%
  mutate(marital = ifelse(is.na(marital), NA,
                       ifelse(marital < 0, NA, 
                              ifelse(marital == 5 | marital == 6, 1, 0))))

table(dat$marital, useNA = "ifany")


## Maternal education
table(dat$c645a, useNA = "ifany")

dat <- dat %>%
  rename(edu = c645a) %>%
  mutate(edu = ifelse(is.na(edu), NA,
                          ifelse(edu < 0, NA, edu)))

table(dat$edu, useNA = "ifany")


## IMD
table(dat$jan1993imd2010q5_M, useNA = "ifany")

dat <- dat %>%
  rename(imd = jan1993imd2010q5_M) %>%
  mutate(imd = ifelse(is.na(imd), NA,
                      ifelse(imd < 0, NA, imd)))

table(dat$imd, useNA = "ifany")


## Offspring ethnicity
table(dat$c804, useNA = "ifany")

dat <- dat %>%
  rename(ethnic = c804) %>%
  mutate(ethnic = ifelse(is.na(ethnic), NA,
                         ifelse(ethnic < 0, NA, 
                                ifelse(ethnic == 1, 0, 1))))

table(dat$ethnic, useNA = "ifany")


## Maternal depression
table(dat$b370, useNA = "ifany")

dat <- dat %>%
  rename(mat_dep = b370) %>%
  mutate(mat_dep = ifelse(is.na(mat_dep), NA,
                         ifelse(mat_dep < 0, NA, mat_dep)))
         
table(dat$mat_dep, useNA = "ifany")


## Maternal anxiety
table(dat$b351, useNA = "ifany")

dat <- dat %>%
  rename(mat_anx = b351) %>%
  mutate(mat_anx = ifelse(is.na(mat_anx), NA,
                          ifelse(mat_anx < 0, NA, mat_anx)))

table(dat$mat_anx, useNA = "ifany")


## Maternal ACEs
table(dat$c433, useNA = "ifany")

dat <- dat %>%
  rename(mat_aces = c433) %>%
  mutate(mat_aces = ifelse(is.na(mat_aces), NA,
                          ifelse(mat_aces < 0, NA, mat_aces)))

table(dat$mat_aces, useNA = "ifany")


### Auxiliary variables

## Offspring mental health in childhood
table(dat$kv8616, useNA = "ifany")

dat <- dat %>%
  rename(priorMH = kv8616) %>%
  mutate(priorMH = ifelse(is.na(priorMH), NA,
                           ifelse(priorMH < 0, NA, priorMH)))

table(dat$priorMH, useNA = "ifany")


## Mother smoked in pregnancy
table(dat$b665, useNA = "ifany")
table(dat$b667, useNA = "ifany")

dat <- dat %>%
  mutate(b665 = ifelse(is.na(b665), NA,
                       ifelse(b665 == 1, 0, 1))) %>%
  mutate(b667 = ifelse(is.na(b667), NA,
                       ifelse(b667 == 1, 0, 1))) %>%
  mutate(mat_pregsmk = ifelse(is.na(b665) & is.na(b667), NA,
                              ifelse(b665 == 1 | b667 == 1, 1, 0))) %>%
  select(-c(b663, b665, b667)) %>%
  relocate(mat_pregsmk, .after = priorMH)

table(dat$mat_pregsmk, useNA = "ifany")


## Maternal occupational social class
table(dat$c755, useNA = "ifany")

dat <- dat %>%
  rename(mat_soc = c755) %>%
  mutate(mat_soc = ifelse(is.na(mat_soc), NA,
                          ifelse(mat_soc < 0 | mat_soc == 65, NA, mat_soc)))

table(dat$mat_soc, useNA = "ifany")


## Paternal occupational social class
table(dat$c765, useNA = "ifany")

dat <- dat %>%
  rename(pat_soc = c765) %>%
  mutate(pat_soc = ifelse(is.na(pat_soc), NA,
                          ifelse(pat_soc < 0 | pat_soc == 65, NA, pat_soc)))

table(dat$pat_soc, useNA = "ifany")


### ACE exposures

## Physical abuse 0-5

# b596
table(dat$b596, useNA = "ifany")

dat <- dat %>%
  mutate(b596 = ifelse(is.na(b596), NA,
                          ifelse(b596 < 0, NA, 
                                 ifelse(b596 == 5, 0, 1))))

table(dat$b596, useNA = "ifany")

# pb186a
table(dat$pb186a, useNA = "ifany")

dat <- dat %>%
  mutate(pb186a = ifelse(is.na(pb186a), NA,
                       ifelse(pb186a < 0, NA, 
                              ifelse(pb186a == 2, 0, 1))))

table(dat$pb186a, useNA = "ifany")

# f246a
table(dat$f246a, useNA = "ifany")

dat <- dat %>%
  mutate(f246a = ifelse(is.na(f246a), NA,
                         ifelse(f246a < 0, NA, 
                                ifelse(f246a == 2, 0, 1))))

table(dat$f246a, useNA = "ifany")

# pd247a
table(dat$pd247a, useNA = "ifany")

dat <- dat %>%
  mutate(pd247a = ifelse(is.na(pd247a), NA,
                        ifelse(pd247a < 0, NA, 
                               ifelse(pd247a == 2, 0, 1))))

table(dat$pd247a, useNA = "ifany")

# g326a
table(dat$g326a, useNA = "ifany")

dat <- dat %>%
  mutate(g326a = ifelse(is.na(g326a), NA,
                         ifelse(g326a < 0, NA, 
                                ifelse(g326a == 2, 0, 1))))

table(dat$g326a, useNA = "ifany")

# pe327a
table(dat$pe327a, useNA = "ifany")

dat <- dat %>%
  mutate(pe327a = ifelse(is.na(pe327a), NA,
                        ifelse(pe327a < 0, NA, 
                               ifelse(pe327a == 2, 0, 1))))

table(dat$pe327a, useNA = "ifany")

# h236a
table(dat$h236a, useNA = "ifany")

dat <- dat %>%
  mutate(h236a = ifelse(is.na(h236a), NA,
                         ifelse(h236a < 0, NA, 
                                ifelse(h236a == 2, 0, 1))))

table(dat$h236a, useNA = "ifany")

# pf5027
table(dat$pf5027, useNA = "ifany")

dat <- dat %>%
  mutate(pf5027 = ifelse(is.na(pf5027), NA,
                        ifelse(pf5027 < 0, NA, 
                               ifelse(pf5027 == 5, 0, 1))))

table(dat$pf5027, useNA = "ifany")

# j326a
table(dat$j326a, useNA = "ifany")

# pg3027
table(dat$pg3027, useNA = "ifany")

dat <- dat %>%
  mutate(pg3027 = ifelse(is.na(pg3027), NA,
                         ifelse(pg3027 < 0, NA, 
                                ifelse(pg3027 == 5, 0, 1))))

table(dat$pg3027, useNA = "ifany")

# f247a
table(dat$f247a, useNA = "ifany")

dat <- dat %>%
  mutate(f247a = ifelse(is.na(f247a), NA,
                         ifelse(f247a < 0, NA, 
                                ifelse(f247a == 2, 0, 1))))

table(dat$f247a, useNA = "ifany")

# pd246a
table(dat$pd246a, useNA = "ifany")

dat <- dat %>%
  mutate(pd246a = ifelse(is.na(pd246a), NA,
                        ifelse(pd246a < 0, NA, 
                               ifelse(pd246a == 2, 0, 1))))

table(dat$pd246a, useNA = "ifany")

# g327a
table(dat$g327a, useNA = "ifany")

dat <- dat %>%
  mutate(g327a = ifelse(is.na(g327a), NA,
                         ifelse(g327a < 0, NA, 
                                ifelse(g327a == 2, 0, 1))))

table(dat$g327a, useNA = "ifany")

# pe326a
table(dat$pe326a, useNA = "ifany")

dat <- dat %>%
  mutate(pe326a = ifelse(is.na(pe326a), NA,
                        ifelse(pe326a < 0, NA, 
                               ifelse(pe326a == 2, 0, 1))))

table(dat$pe326a, useNA = "ifany")

# h237a
table(dat$h237a, useNA = "ifany")

dat <- dat %>%
  mutate(h237a = ifelse(is.na(h237a), NA,
                         ifelse(h237a < 0, NA, 
                                ifelse(h237a == 2, 0, 1))))

table(dat$h237a, useNA = "ifany")

# pf5026
table(dat$pf5026, useNA = "ifany")

dat <- dat %>%
  mutate(pf5026 = ifelse(is.na(pf5026), NA,
                        ifelse(pf5026 < 0, NA, 
                               ifelse(pf5026 == 5, 0, 1))))

table(dat$pf5026, useNA = "ifany")

# j327a
table(dat$j327a, useNA = "ifany")

# pg3026
table(dat$pg3026, useNA = "ifany")

dat <- dat %>%
  mutate(pg3026 = ifelse(is.na(pg3026), NA,
                         ifelse(pg3026 < 0, NA, 
                                ifelse(pg3026 == 5, 0, 1))))

table(dat$pg3026, useNA = "ifany")

# clon140 (any physical abuse 0-5) - Recode if any variables above are missing, as will impute later
table(dat$clon140, useNA = "ifany")

dat <- dat %>%
  mutate(clon140 = ifelse(clon140 < 0, NA,
                          ifelse(complete.cases(b596, pb186a, f246a, pd247a, g326a, pe327a, h236a, pf5027, j326a, pg3027, f247a, pd246a, g327a, pe326a, h237a, pf5026, j327a, pg3026), 
                                 pmax(b596, pb186a, f246a, pd247a, g326a, pe327a, h236a, pf5027, j326a, pg3027, f247a, pd246a, g327a, pe326a, h237a, pf5026, j327a, pg3026), NA)))

table(dat$clon140, useNA = "ifany")


## Emotional abuse 0-5

# b608
table(dat$b608, useNA = "ifany")

dat <- dat %>%
  mutate(b608 = ifelse(is.na(b608), NA,
                         ifelse(b608 < 0, NA, 
                                ifelse(b608 == 5, 0, 1))))

table(dat$b608, useNA = "ifany")

# f257a
table(dat$f257a, useNA = "ifany")

dat <- dat %>%
  mutate(f257a = ifelse(is.na(f257a), NA,
                       ifelse(f257a < 0, NA, 
                              ifelse(f257a == 2, 0, 1))))

table(dat$f257a, useNA = "ifany")

# pd258a
table(dat$pd258a, useNA = "ifany")

dat <- dat %>%
  mutate(pd258a = ifelse(is.na(pd258a), NA,
                        ifelse(pd258a < 0, NA, 
                               ifelse(pd258a == 2, 0, 1))))

table(dat$pd258a, useNA = "ifany")

# g337a
table(dat$g337a, useNA = "ifany")

dat <- dat %>%
  mutate(g337a = ifelse(is.na(g337a), NA,
                         ifelse(g337a < 0, NA, 
                                ifelse(g337a == 2, 0, 1))))

table(dat$g337a, useNA = "ifany")

# pe338a
table(dat$pe338a, useNA = "ifany")

dat <- dat %>%
  mutate(pe338a = ifelse(is.na(pe338a), NA,
                        ifelse(pe338a < 0, NA, 
                               ifelse(pe338a == 2, 0, 1))))

table(dat$pe338a, useNA = "ifany")

# h247a
table(dat$h247a, useNA = "ifany")

dat <- dat %>%
  mutate(h247a = ifelse(is.na(h247a), NA,
                         ifelse(h247a < 0, NA, 
                                ifelse(h247a == 2, 0, 1))))

table(dat$h247a, useNA = "ifany")

# pf5036
table(dat$pf5036, useNA = "ifany")

dat <- dat %>%
  mutate(pf5036 = ifelse(is.na(pf5036), NA,
                        ifelse(pf5036 < 0, NA, 
                               ifelse(pf5036 == 5, 0, 1))))

table(dat$pf5036, useNA = "ifany")

# j337a
table(dat$j337a, useNA = "ifany")

# pg3036
table(dat$pg3036, useNA = "ifany")

dat <- dat %>%
  mutate(pg3036 = ifelse(is.na(pg3036), NA,
                         ifelse(pg3036 < 0, NA, 
                                ifelse(pg3036 == 5, 0, 1))))

table(dat$pg3036, useNA = "ifany")

# pc236a
table(dat$pc236a, useNA = "ifany")

dat <- dat %>%
  mutate(pc236a = ifelse(is.na(pc236a), NA,
                         ifelse(pc236a < 0, NA, 
                                ifelse(pc236a == 2, 0, 1))))

table(dat$pc236a, useNA = "ifany")

# f258a
table(dat$f258a, useNA = "ifany")

dat <- dat %>%
  mutate(f258a = ifelse(is.na(f258a), NA,
                         ifelse(f258a < 0, NA, 
                                ifelse(f258a == 2, 0, 1))))

table(dat$f258a, useNA = "ifany")

# pd257a
table(dat$pd257a, useNA = "ifany")

dat <- dat %>%
  mutate(pd257a = ifelse(is.na(pd257a), NA,
                        ifelse(pd257a < 0, NA, 
                               ifelse(pd257a == 2, 0, 1))))

table(dat$pd257a, useNA = "ifany")

# g338a
table(dat$g338a, useNA = "ifany")

dat <- dat %>%
  mutate(g338a = ifelse(is.na(g338a), NA,
                         ifelse(g338a < 0, NA, 
                                ifelse(g338a == 2, 0, 1))))

table(dat$g338a, useNA = "ifany")

# pe337a
table(dat$pe337a, useNA = "ifany")

dat <- dat %>%
  mutate(pe337a = ifelse(is.na(pe337a), NA,
                        ifelse(pe337a < 0, NA, 
                               ifelse(pe337a == 2, 0, 1))))

table(dat$pe337a, useNA = "ifany")

# h248a
table(dat$h248a, useNA = "ifany")

dat <- dat %>%
  mutate(h248a = ifelse(is.na(h248a), NA,
                         ifelse(h248a < 0, NA, 
                                ifelse(h248a == 2, 0, 1))))

table(dat$h248a, useNA = "ifany")

# pf5037
table(dat$pf5037, useNA = "ifany")

dat <- dat %>%
  mutate(pf5037 = ifelse(is.na(pf5037), NA,
                        ifelse(pf5037 < 0, NA, 
                               ifelse(pf5037 == 5, 0, 1))))

table(dat$pf5037, useNA = "ifany")

# j338a
table(dat$j338a, useNA = "ifany")

# pg3037
table(dat$pg3037, useNA = "ifany")

dat <- dat %>%
  mutate(pg3037 = ifelse(is.na(pg3037), NA,
                         ifelse(pg3037 < 0, NA, 
                                ifelse(pg3037 == 5, 0, 1))))

table(dat$pg3037, useNA = "ifany")

# clon141 (any emotional abuse 0-5) - Recode if any variables above are missing, as will impute later
table(dat$clon141, useNA = "ifany")

dat <- dat %>%
  mutate(clon141 = ifelse(clon141 < 0, NA,
                          ifelse(complete.cases(b608, f257a, pd258a, g337a, pe338a, h247a, pf5036, j337a, pg3036, pc236a, f258a, pd257a, g338a, pe337a, h248a, pf5037, j338a, pg3037), 
                                 pmax(b608, f257a, pd258a, g337a, pe338a, h247a, pf5036, j337a, pg3036, pc236a, f258a, pd257a, g338a, pe337a, h248a, pf5037, j338a, pg3037), NA)))

table(dat$clon141, useNA = "ifany")


## Domestic violence 0-5

# pc222a
table(dat$pc222a, useNA = "ifany")

dat <- dat %>%
  mutate(pc222a = ifelse(is.na(pc222a), NA,
                         ifelse(pc222a < 0, NA, 
                                ifelse(pc222a == 2, 0, 1))))

table(dat$pc222a, useNA = "ifany")

# pe322a
table(dat$pe322a, useNA = "ifany")

dat <- dat %>%
  mutate(pe322a = ifelse(is.na(pe322a), NA,
                         ifelse(pe322a < 0, NA, 
                                ifelse(pe322a == 2, 0, 1))))

table(dat$pe322a, useNA = "ifany")

# pf5022
table(dat$pf5022, useNA = "ifany")

dat <- dat %>%
  mutate(pf5022 = ifelse(is.na(pf5022), NA,
                         ifelse(pf5022 < 0, NA, 
                                ifelse(pf5022 == 5, 0, 1))))

table(dat$pf5022, useNA = "ifany")

# pg3022
table(dat$pg3022, useNA = "ifany")

dat <- dat %>%
  mutate(pg3022 = ifelse(is.na(pg3022), NA,
                         ifelse(pg3022 < 0, NA, 
                                ifelse(pg3022 == 5, 0, 1))))

table(dat$pg3022, useNA = "ifany")

# e422
table(dat$e422, useNA = "ifany")

dat <- dat %>%
  mutate(e422 = ifelse(is.na(e422), NA,
                         ifelse(e422 < 0, NA, 
                                ifelse(e422 == 5, 0, 1))))

table(dat$e422, useNA = "ifany")

# f242a
table(dat$f242a, useNA = "ifany")

dat <- dat %>%
  mutate(f242a = ifelse(is.na(f242a), NA,
                       ifelse(f242a < 0, NA, 
                              ifelse(f242a == 2, 0, 1))))

table(dat$f242a, useNA = "ifany")

# pd242a
table(dat$pd242a, useNA = "ifany")

dat <- dat %>%
  mutate(pd242a = ifelse(is.na(pd242a), NA,
                        ifelse(pd242a < 0, NA, 
                               ifelse(pd242a == 2, 0, 1))))

table(dat$pd242a, useNA = "ifany")

# g322a
table(dat$g322a, useNA = "ifany")

dat <- dat %>%
  mutate(g322a = ifelse(is.na(g322a), NA,
                         ifelse(g322a < 0, NA, 
                                ifelse(g322a == 2, 0, 1))))

table(dat$g322a, useNA = "ifany")

# h232a
table(dat$h232a, useNA = "ifany")

dat <- dat %>%
  mutate(h232a = ifelse(is.na(h232a), NA,
                        ifelse(h232a < 0, NA, 
                               ifelse(h232a == 2, 0, 1))))

table(dat$h232a, useNA = "ifany")

# j322a
table(dat$j322a, useNA = "ifany")

# f256a
table(dat$f256a, useNA = "ifany")

dat <- dat %>%
  mutate(f256a = ifelse(is.na(f256a), NA,
                        ifelse(f256a < 0, NA, 
                               ifelse(f256a == 2, 0, 1))))

table(dat$f256a, useNA = "ifany")

# clon142 (any domestic violence 0-5) - Recode if any variables above are missing, as will impute later
table(dat$clon142, useNA = "ifany")

dat <- dat %>%
  mutate(clon142 = ifelse(clon142 < 0, NA,
                          ifelse(complete.cases(pc222a, pe322a, pf5022, pg3022, e422, f242a, pd242a, g322a, h232a, j322a, f256a), 
                                 pmax(pc222a, pe322a, pf5022, pg3022, e422, f242a, pd242a, g322a, h232a, j322a, f256a), NA)))

table(dat$clon142, useNA = "ifany")


## Sexual abuse 0-5

# kd505b
table(dat$kd505b, useNA = "ifany")

dat <- dat %>%
  mutate(kd505b = ifelse(is.na(kd505b), NA,
                        ifelse(kd505b < 0, NA, 
                               ifelse(kd505b == 2, 0, 1))))

table(dat$kd505b, useNA = "ifany")

# kf455a
table(dat$kf455a, useNA = "ifany")

dat <- dat %>%
  mutate(kf455a = ifelse(is.na(kf455a), NA,
                         ifelse(kf455a < 0, NA, 
                                ifelse(kf455a == 2, 0, 1))))

table(dat$kf455a, useNA = "ifany")

# kj465a
table(dat$kj465a, useNA = "ifany")

dat <- dat %>%
  mutate(kj465a = ifelse(is.na(kj465a), NA,
                         ifelse(kj465a < 0, NA, 
                                ifelse(kj465a == 2, 0, 1))))

table(dat$kj465a, useNA = "ifany")

# clon143 (any sexual abuse 0-5) - Recode if any variables above are missing, as will impute later
table(dat$clon143, useNA = "ifany")

dat <- dat %>%
  mutate(clon143 = ifelse(clon143 < 0, NA,
                          ifelse(complete.cases(kd505b, kf455a, kj465a), 
                                 pmax(kd505b, kf455a, kj465a), NA)))

table(dat$clon143, useNA = "ifany")



## Bullying 0-5

# j548
table(dat$j548, useNA = "ifany")

dat <- dat %>%
  mutate(j548 = ifelse(is.na(j548), NA,
                         ifelse(j548 < 0, NA, 
                                ifelse(j548 == 1 | j548 == 2, 0, 1))))

table(dat$j548, useNA = "ifany")

# pg4148
table(dat$pg4148, useNA = "ifany")

dat <- dat %>%
  mutate(pg4148 = ifelse(is.na(pg4148), NA,
                       ifelse(pg4148 < 0, NA, 
                              ifelse(pg4148 == 1 | pg4148 == 2, 0, 1))))

table(dat$pg4148, useNA = "ifany")

# clon144 (any bullying 0-5) - Recode if any variables above are missing, as will impute later
table(dat$clon144, useNA = "ifany")

dat <- dat %>%
  mutate(clon144 = ifelse(clon144 < 0, NA,
                          ifelse(complete.cases(j548, pg4148), 
                                 pmax(j548, pg4148), NA)))

table(dat$clon144, useNA = "ifany")



## Emotional neglect 5-11

# ccc250
table(dat$ccc250, useNA = "ifany")

dat <- dat %>%
  mutate(ccc250 = ifelse(is.na(ccc250), NA,
                         ifelse(ccc250 < 0, NA, 
                                ifelse(ccc250 == 4, 1, 0))))

table(dat$ccc250, useNA = "ifany")

# ccf149
table(dat$ccf149, useNA = "ifany")

dat <- dat %>%
  mutate(ccf149 = ifelse(is.na(ccf149), NA,
                         ifelse(ccf149 < 0, NA, 
                                ifelse(ccf149 == 1 | ccf149 == 2, 1, 0))))

table(dat$ccf149, useNA = "ifany")

# clon146 (any emotional neglect 5-11) - Recode if any variables above are missing, as will impute later
table(dat$clon146, useNA = "ifany")

dat <- dat %>%
  mutate(clon146 = ifelse(clon146 < 0, NA,
                          ifelse(complete.cases(ccc250, ccf149), 
                                 pmax(ccc250, ccf149), NA)))

table(dat$clon146, useNA = "ifany")



## Bullying 5-11

# kq338
table(dat$kq338, useNA = "ifany")

dat <- dat %>%
  mutate(kq338 = ifelse(is.na(kq338), NA,
                         ifelse(kq338 < 0, NA, 
                                ifelse(kq338 == 3, 1, 0))))

table(dat$kq338, useNA = "ifany")

# ccc290
table(dat$ccc290, useNA = "ifany")

dat <- dat %>%
  mutate(ccc290 = ifelse(is.na(ccc290), NA,
                        ifelse(ccc290 < 0, NA, 
                               ifelse(ccc290 == 1, 1, 0))))

table(dat$ccc290, useNA = "ifany")

# ku698
table(dat$ku698, useNA = "ifany")

dat <- dat %>%
  mutate(ku698 = ifelse(is.na(ku698), NA,
                         ifelse(ku698 < 0, NA, 
                                ifelse(ku698 == 3, 1, 0))))

table(dat$ku698, useNA = "ifany")

# f8fp151
table(dat$f8fp151, useNA = "ifany")

dat <- dat %>%
  mutate(f8fp151 = ifelse(is.na(f8fp151), NA,
                        ifelse(f8fp151 == -2 | f8fp151 == -1, NA, 
                               ifelse(f8fp151 == -3 | f8fp151 == 1, 0, 1))))

table(dat$f8fp151, useNA = "ifany")

# f8fp161
table(dat$f8fp161, useNA = "ifany")

dat <- dat %>%
  mutate(f8fp161 = ifelse(is.na(f8fp161), NA,
                          ifelse(f8fp161 == -2 | f8fp161 == -1, NA, 
                                 ifelse(f8fp161 == -3 | f8fp161 == 1, 0, 1))))

table(dat$f8fp161, useNA = "ifany")

# fdfp151
table(dat$fdfp151, useNA = "ifany")

dat <- dat %>%
  mutate(fdfp151 = ifelse(is.na(fdfp151), NA,
                          ifelse(fdfp151 == -2 | fdfp151 == -1 | fdfp151 == -9, NA, 
                                 ifelse(fdfp151 == -3 | fdfp151 == 1, 0, 1))))

table(dat$fdfp151, useNA = "ifany")

# fdfp161
table(dat$fdfp161, useNA = "ifany")

dat <- dat %>%
  mutate(fdfp161 = ifelse(is.na(fdfp161), NA,
                          ifelse(fdfp161 == -2 | fdfp161 == -1 | fdfp161 == -9, NA, 
                                 ifelse(fdfp161 == -3 | fdfp161 == 1, 0, 1))))

table(dat$fdfp161, useNA = "ifany")

# n8358
table(dat$n8358, useNA = "ifany")

dat <- dat %>%
  mutate(n8358 = ifelse(is.na(n8358), NA,
                        ifelse(n8358 < 0, NA, 
                               ifelse(n8358 == 3, 1, 0))))

table(dat$n8358, useNA = "ifany")

# clon147 (any bullying 5-11) - Recode if any variables above are missing, as will impute later
table(dat$clon147, useNA = "ifany")

dat <- dat %>%
  mutate(clon147 = ifelse(clon147 < 0, NA,
                          ifelse(complete.cases(kq338, ccc290, ku698, f8fp151, f8fp161, fdfp151, fdfp161, n8358), 
                                 pmax(kq338, ccc290, ku698, f8fp151, f8fp161, fdfp151, fdfp161, n8358), NA)))

table(dat$clon147, useNA = "ifany")


## Physical abuse 5-11

# ph4026
table(dat$ph4026, useNA = "ifany")

dat <- dat %>%
  mutate(ph4026 = ifelse(is.na(ph4026), NA,
                        ifelse(ph4026 < 0, NA, 
                               ifelse(ph4026 == 5, 0, 1))))

table(dat$ph4026, useNA = "ifany")

# l4026
table(dat$l4026, useNA = "ifany")

dat <- dat %>%
  mutate(l4026 = ifelse(is.na(l4026), NA,
                         ifelse(l4026 < 0, NA, 
                                ifelse(l4026 == 5, 0, 1))))

table(dat$l4026, useNA = "ifany")

# pj4027
table(dat$pj4027, useNA = "ifany")

dat <- dat %>%
  mutate(pj4027 = ifelse(is.na(pj4027), NA,
                        ifelse(pj4027 < 0, NA, 
                               ifelse(pj4027 == 5, 0, 1))))

table(dat$pj4027, useNA = "ifany")

# p2026
table(dat$p2026, useNA = "ifany")

dat <- dat %>%
  mutate(p2026 = ifelse(is.na(p2026), NA,
                         ifelse(p2026 < 0, NA, 
                                ifelse(p2026 == 4, 0, 1))))

table(dat$p2026, useNA = "ifany")

# pm2027
table(dat$pm2027, useNA = "ifany")

dat <- dat %>%
  mutate(pm2027 = ifelse(is.na(pm2027), NA,
                        ifelse(pm2027 < 0, NA, 
                               ifelse(pm2027 == 4, 0, 1))))

table(dat$pm2027, useNA = "ifany")

# k4026
table(dat$k4026, useNA = "ifany")

dat <- dat %>%
  mutate(k4026 = ifelse(is.na(k4026), NA,
                         ifelse(k4026 < 0, NA, 
                                ifelse(k4026 == 5, 0, 1))))

table(dat$k4026, useNA = "ifany")

# k4027
table(dat$k4027, useNA = "ifany")

dat <- dat %>%
  mutate(k4027 = ifelse(is.na(k4027), NA,
                        ifelse(k4027 < 0, NA, 
                               ifelse(k4027 == 5, 0, 1))))

table(dat$k4027, useNA = "ifany")

# ph4027
table(dat$ph4027, useNA = "ifany")

dat <- dat %>%
  mutate(ph4027 = ifelse(is.na(ph4027), NA,
                        ifelse(ph4027 < 0, NA, 
                               ifelse(ph4027 == 5, 0, 1))))

table(dat$ph4027, useNA = "ifany")

# l4027
table(dat$l4027, useNA = "ifany")

dat <- dat %>%
  mutate(l4027 = ifelse(is.na(l4027), NA,
                         ifelse(l4027 < 0, NA, 
                                ifelse(l4027 == 5, 0, 1))))

table(dat$l4027, useNA = "ifany")

# pj4026
table(dat$pj4026, useNA = "ifany")

dat <- dat %>%
  mutate(pj4026 = ifelse(is.na(pj4026), NA,
                        ifelse(pj4026 < 0, NA, 
                               ifelse(pj4026 == 5, 0, 1))))

table(dat$pj4026, useNA = "ifany")

# p2027
table(dat$p2027, useNA = "ifany")

dat <- dat %>%
  mutate(p2027 = ifelse(is.na(p2027), NA,
                         ifelse(p2027 < 0, NA, 
                                ifelse(p2027 == 4, 0, 1))))

table(dat$p2027, useNA = "ifany")

# pm2026
table(dat$pm2026, useNA = "ifany")

dat <- dat %>%
  mutate(pm2026 = ifelse(is.na(pm2026), NA,
                        ifelse(pm2026 < 0, NA, 
                               ifelse(pm2026 == 4, 0, 1))))

table(dat$pm2026, useNA = "ifany")

# YPB8004
table(dat$YPB8004, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8004 = ifelse(is.na(YPB8004), NA,
                         ifelse(YPB8004 < 0, NA, 
                                ifelse(YPB8004 == 5 | YPB8004 == 4, 1, 0))))

table(dat$YPB8004, useNA = "ifany")

# YPB8023 (as this is quite an extreme event [actually hit by adult outside family)], and messes up imputations as are no cases otherwise, will include 'sometimes' [value 3] here as well)
table(dat$YPB8023, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8023 = ifelse(is.na(YPB8023), NA,
                          ifelse(YPB8023 < 0, NA, 
                                 ifelse(YPB8023 == 5 | YPB8023 == 4 | YPB8023 == 3, 1, 0))))

table(dat$YPB8023, useNA = "ifany")

# YPB8002
table(dat$YPB8002, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8002 = ifelse(is.na(YPB8002), NA,
                          ifelse(YPB8002 < 0, NA, 
                                 ifelse(YPB8002 == 5 | YPB8002 == 4, 1, 0))))

table(dat$YPB8002, useNA = "ifany")

# YPB8006
table(dat$YPB8006, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8006 = ifelse(is.na(YPB8006), NA,
                          ifelse(YPB8006 < 0, NA, 
                                 ifelse(YPB8006 == 5 | YPB8006 == 4, 1, 0))))

table(dat$YPB8006, useNA = "ifany")

# YPB8007
table(dat$YPB8007, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8007 = ifelse(is.na(YPB8007), NA,
                          ifelse(YPB8007 < 0, NA, 
                                 ifelse(YPB8007 == 5 | YPB8007 == 4, 1, 0))))

table(dat$YPB8007, useNA = "ifany")

# kw4041
table(dat$kw4041, useNA = "ifany")

dat <- dat %>%
  mutate(kw4041 = ifelse(is.na(kw4041), NA,
                          ifelse(kw4041 < 0, NA, 
                                 ifelse(kw4041 == 1 | kw4041 == 2 | kw4041 == 3 | kw4041 == 4, 1, 0))))

table(dat$kw4041, useNA = "ifany")

# clon148 (any physical abuse 5-11) - Recode if any variables above are missing, as will impute later
table(dat$clon148, useNA = "ifany")

dat <- dat %>%
  mutate(clon148 = ifelse(clon148 < 0, NA,
                          ifelse(complete.cases(ph4026, l4026, pj4027, p2026, pm2027, k4026, k4027, ph4027, l4027, pj4026, p2027, pm2026, YPB8004, YPB8023, YPB8002, YPB8006, YPB8007, kw4041), 
                                 pmax(ph4026, l4026, pj4027, p2026, pm2027, k4026, k4027, ph4027, l4027, pj4026, p2027, pm2026, YPB8004, YPB8023, YPB8002, YPB8006, YPB8007, kw4041), NA)))

table(dat$clon148, useNA = "ifany")


## Emotional abuse 5-11

# ph4038
table(dat$ph4038, useNA = "ifany")

dat <- dat %>%
  mutate(ph4038 = ifelse(is.na(ph4038), NA,
                         ifelse(ph4038 < 0, NA, 
                                ifelse(ph4038 == 5, 0, 1))))

table(dat$ph4038, useNA = "ifany")

# l4037
table(dat$l4037, useNA = "ifany")

dat <- dat %>%
  mutate(l4037 = ifelse(is.na(l4037), NA,
                         ifelse(l4037 < 0, NA, 
                                ifelse(l4037 == 5, 0, 1))))

table(dat$l4037, useNA = "ifany")

# pj4038
table(dat$pj4038, useNA = "ifany")

dat <- dat %>%
  mutate(pj4038 = ifelse(is.na(pj4038), NA,
                        ifelse(pj4038 < 0, NA, 
                               ifelse(pj4038 == 5, 0, 1))))

table(dat$pj4038, useNA = "ifany")

# p2037
table(dat$p2037, useNA = "ifany")

dat <- dat %>%
  mutate(p2037 = ifelse(is.na(p2037), NA,
                         ifelse(p2037 < 0, NA, 
                                ifelse(p2037 == 4, 0, 1))))

table(dat$p2037, useNA = "ifany")

# pm2038
table(dat$pm2038, useNA = "ifany")

dat <- dat %>%
  mutate(pm2038 = ifelse(is.na(pm2038), NA,
                        ifelse(pm2038 < 0, NA, 
                               ifelse(pm2038 == 4, 0, 1))))

table(dat$pm2038, useNA = "ifany")

# k4038
table(dat$k4038, useNA = "ifany")

dat <- dat %>%
  mutate(k4038 = ifelse(is.na(k4038), NA,
                         ifelse(k4038 < 0, NA, 
                                ifelse(k4038 == 5, 0, 1))))

table(dat$k4038, useNA = "ifany")

# ph4037
table(dat$ph4037, useNA = "ifany")

dat <- dat %>%
  mutate(ph4037 = ifelse(is.na(ph4037), NA,
                        ifelse(ph4037 < 0, NA, 
                               ifelse(ph4037 == 5, 0, 1))))

table(dat$ph4037, useNA = "ifany")

# l4038
table(dat$l4038, useNA = "ifany")

dat <- dat %>%
  mutate(l4038 = ifelse(is.na(l4038), NA,
                         ifelse(l4038 < 0, NA, 
                                ifelse(l4038 == 5, 0, 1))))

table(dat$l4038, useNA = "ifany")

# pj4037
table(dat$pj4037, useNA = "ifany")

dat <- dat %>%
  mutate(pj4037 = ifelse(is.na(pj4037), NA,
                        ifelse(pj4037 < 0, NA, 
                               ifelse(pj4037 == 5, 0, 1))))

table(dat$pj4037, useNA = "ifany")

# p2038
table(dat$p2038, useNA = "ifany")

dat <- dat %>%
  mutate(p2038 = ifelse(is.na(p2038), NA,
                         ifelse(p2038 < 0, NA, 
                                ifelse(p2038 == 4, 0, 1))))

table(dat$p2038, useNA = "ifany")

# pm2037
table(dat$pm2037, useNA = "ifany")

dat <- dat %>%
  mutate(pm2037 = ifelse(is.na(pm2037), NA,
                        ifelse(pm2037 < 0, NA, 
                               ifelse(pm2037 == 4, 0, 1))))

table(dat$pm2037, useNA = "ifany")

# YPB8005
table(dat$YPB8005, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8005 = ifelse(is.na(YPB8005), NA,
                          ifelse(YPB8005 < 0, NA, 
                                 ifelse(YPB8005 == 5 | YPB8005 == 4, 1, 0))))

table(dat$YPB8005, useNA = "ifany")

# YPB8001
table(dat$YPB8001, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8001 = ifelse(is.na(YPB8001), NA,
                          ifelse(YPB8001 < 0, NA, 
                                 ifelse(YPB8001 == 5 | YPB8001 == 4, 1, 0))))

table(dat$YPB8001, useNA = "ifany")

# pj4036
table(dat$pj4036, useNA = "ifany")

dat <- dat %>%
  mutate(pj4036 = ifelse(is.na(pj4036), NA,
                          ifelse(pj4036 < 0, NA, 
                                 ifelse(pj4036 == 5, 0, 1))))

table(dat$pj4036, useNA = "ifany")

# k4037
table(dat$k4037, useNA = "ifany")

dat <- dat %>%
  mutate(k4037 = ifelse(is.na(k4037), NA,
                         ifelse(k4037 < 0, NA, 
                                ifelse(k4037 == 5, 0, 1))))

table(dat$k4037, useNA = "ifany")

# YPB8022
table(dat$YPB8022, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8022 = ifelse(is.na(YPB8022), NA,
                          ifelse(YPB8022 < 0, NA, 
                                 ifelse(YPB8022 == 5 | YPB8022 == 4, 1, 0))))

table(dat$YPB8022, useNA = "ifany")

# clon149 (any emotional abuse 5-11) - Recode if any variables above are missing, as will impute later
table(dat$clon149, useNA = "ifany")

dat <- dat %>%
  mutate(clon149 = ifelse(clon149 < 0, NA,
                          ifelse(complete.cases(ph4038, l4037, pj4038, p2037, pm2038, k4038, ph4037, l4038, pj4037, p2038, pm2037, YPB8005, YPB8001, pj4036, k4037, YPB8022), 
                                 pmax(ph4038, l4037, pj4038, p2037, pm2038, k4038, ph4037, l4038, pj4037, p2038, pm2037, YPB8005, YPB8001, pj4036, k4037, YPB8022), NA)))

table(dat$clon149, useNA = "ifany")


## Domestic violence 5-11

# k4022
table(dat$k4022, useNA = "ifany")

dat <- dat %>%
  mutate(k4022 = ifelse(is.na(k4022), NA,
                          ifelse(k4022 < 0, NA, 
                                 ifelse(k4022 == 5, 0, 1))))

table(dat$k4022, useNA = "ifany")

# l4022
table(dat$l4022, useNA = "ifany")

dat <- dat %>%
  mutate(l4022 = ifelse(is.na(l4022), NA,
                        ifelse(l4022 < 0, NA, 
                               ifelse(l4022 == 5, 0, 1))))

table(dat$l4022, useNA = "ifany")

# p2022
table(dat$p2022, useNA = "ifany")

dat <- dat %>%
  mutate(p2022 = ifelse(is.na(p2022), NA,
                        ifelse(p2022 < 0, NA, 
                               ifelse(p2022 == 4, 0, 1))))

table(dat$p2022, useNA = "ifany")

# p2022
table(dat$ph4022, useNA = "ifany")

dat <- dat %>%
  mutate(ph4022 = ifelse(is.na(ph4022), NA,
                        ifelse(ph4022 < 0, NA, 
                               ifelse(ph4022 == 5, 0, 1))))

table(dat$ph4022, useNA = "ifany")

# pj4022
table(dat$pj4022, useNA = "ifany")

dat <- dat %>%
  mutate(pj4022 = ifelse(is.na(pj4022), NA,
                         ifelse(pj4022 < 0, NA, 
                                ifelse(pj4022 == 5, 0, 1))))

table(dat$pj4022, useNA = "ifany")

# pm3155
table(dat$pm3155, useNA = "ifany")

dat <- dat %>%
  mutate(pm3155 = ifelse(is.na(pm3155), NA,
                         ifelse(pm3155 < 0, NA, 
                                ifelse(pm3155 == 4, 0, 1))))

table(dat$pm3155, useNA = "ifany")

# pm2022
table(dat$pm2022, useNA = "ifany")

dat <- dat %>%
  mutate(pm2022 = ifelse(is.na(pm2022), NA,
                         ifelse(pm2022 < 0, NA, 
                                ifelse(pm2022 == 4, 0, 1))))

table(dat$pm2022, useNA = "ifany")

# clon150 (any domestic violence 5-11) - Recode if any variables above are missing, as will impute later
table(dat$clon150, useNA = "ifany")

dat <- dat %>%
  mutate(clon150 = ifelse(clon150 < 0, NA,
                          ifelse(complete.cases(k4022, l4022, p2022, ph4022, pj4022, pm3155, pm2022), 
                                 pmax(k4022, l4022, p2022, ph4022, pj4022, pm3155, pm2022), NA)))

table(dat$clon150, useNA = "ifany")



## Sexual abuse 5-11

# kl475
table(dat$kl475, useNA = "ifany")

dat <- dat %>%
  mutate(kl475 = ifelse(is.na(kl475), NA,
                         ifelse(kl475 < 0, NA, 
                                ifelse(kl475 == 5, 0, 1))))

table(dat$kl475, useNA = "ifany")

# kn4005
table(dat$kn4005, useNA = "ifany")

dat <- dat %>%
  mutate(kn4005 = ifelse(is.na(kn4005), NA,
                        ifelse(kn4005 < 0, NA, 
                               ifelse(kn4005 == 5, 0, 1))))

table(dat$kn4005, useNA = "ifany")

# kq365a
table(dat$kq365a, useNA = "ifany")

dat <- dat %>%
  mutate(kq365a = ifelse(is.na(kq365a), NA,
                         ifelse(kq365a < 0, NA, 
                                ifelse(kq365a == 2, 0, 1))))

table(dat$kq365a, useNA = "ifany")

# kt5005
table(dat$kt5005, useNA = "ifany")

dat <- dat %>%
  mutate(kt5005 = ifelse(is.na(kt5005), NA,
                         ifelse(kt5005 < 0, NA, 
                                ifelse(kt5005 == 5, 0, 1))))

table(dat$kt5005, useNA = "ifany")

# YPB8040
table(dat$YPB8040, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8040 = ifelse(is.na(YPB8040), NA,
                         ifelse(YPB8040 < 0, NA, 
                                ifelse(YPB8040 == 2 | YPB8040 == 3, 1, 0))))

table(dat$YPB8040, useNA = "ifany")

# YPB8030
table(dat$YPB8030, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8030 = ifelse(is.na(YPB8030), NA,
                          ifelse(YPB8030 < 0, NA, 
                                 ifelse(YPB8030 == 2 | YPB8030 == 3, 1, 0))))

table(dat$YPB8030, useNA = "ifany")

# clon151 (any sexual abuse 5-11) - Recode if any variables above are missing, as will impute later
table(dat$clon151, useNA = "ifany")

dat <- dat %>%
  mutate(clon151 = ifelse(clon151 < 0, NA,
                          ifelse(complete.cases(kl475, kn4005, kq365a, kt5005, YPB8040, YPB8030), 
                                 pmax(kl475, kn4005, kq365a, kt5005, YPB8040, YPB8030), NA)))

table(dat$clon151, useNA = "ifany")



## Emotional neglect 11-17

# ff5318
table(dat$ff5318, useNA = "ifany")

dat <- dat %>%
  mutate(ff5318 = ifelse(is.na(ff5318), NA,
                          ifelse(ff5318 < 0, NA, 
                                 ifelse(ff5318 == 1, 1, 0))))

table(dat$ff5318, useNA = "ifany")

# fg7118
table(dat$fg7118, useNA = "ifany")

dat <- dat %>%
  mutate(fg7118 = ifelse(is.na(fg7118), NA,
                         ifelse(fg7118 < 0, NA, 
                                ifelse(fg7118 == 1, 1, 0))))

table(dat$fg7118, useNA = "ifany")

# fh8200
table(dat$fh8200, useNA = "ifany")

dat <- dat %>%
  mutate(fh8200 = ifelse(is.na(fh8200), NA,
                         ifelse(fh8200 < 0, NA, 
                                ifelse(fh8200 == 1, 1, 0))))

table(dat$fh8200, useNA = "ifany")

# fh9821
table(dat$fh9821, useNA = "ifany")

dat <- dat %>%
  mutate(fh9821 = ifelse(is.na(fh9821), NA,
                         ifelse(fh9821 < 0, NA, 
                                ifelse(fh9821 == 1, 1, 0))))

table(dat$fh9821, useNA = "ifany")

# ccxa243
table(dat$ccxa243, useNA = "ifany")

dat <- dat %>%
  mutate(ccxa243 = ifelse(is.na(ccxa243), NA,
                         ifelse(ccxa243 < 0, NA, 
                                ifelse(ccxa243 == 4, 1, 0))))

table(dat$ccxa243, useNA = "ifany")

# clon153 (any emotional neglect 11-17) - Recode if any variables above are missing, as will impute later
table(dat$clon153, useNA = "ifany")

dat <- dat %>%
  mutate(clon153 = ifelse(clon153 < 0, NA,
                          ifelse(complete.cases(ff5318, fg7118, fh8200, fh9821, ccxa243), 
                                 pmax(ff5318, fg7118, fh8200, fh9821, ccxa243), NA)))

table(dat$clon153, useNA = "ifany")




## Bullying 11-17

# ccl201
table(dat$ccl201, useNA = "ifany")

dat <- dat %>%
  mutate(ccl201 = ifelse(is.na(ccl201), NA,
                          ifelse(ccl201 < 0, NA, 
                                 ifelse(ccl201 == 1, 1, 0))))

table(dat$ccl201, useNA = "ifany")

# ta7018
table(dat$ta7018, useNA = "ifany")

dat <- dat %>%
  mutate(ta7018 = ifelse(is.na(ta7018), NA,
                         ifelse(ta7018 < 0 | ta7018 == 9, NA, 
                                ifelse(ta7018 == 3, 1, 0))))

table(dat$ta7018, useNA = "ifany")

# tc4018
table(dat$tc4018, useNA = "ifany")

dat <- dat %>%
  mutate(tc4018 = ifelse(is.na(tc4018), NA,
                         ifelse(tc4018 < 0 | tc4018 == 9, NA, 
                                ifelse(tc4018 == 3, 1, 0))))

table(dat$tc4018, useNA = "ifany")

# ff6011
table(dat$ff6011, useNA = "ifany")

dat <- dat %>%
  mutate(ff6011 = ifelse(is.na(ff6011), NA,
                         ifelse(ff6011 == -10 | ff6011 == -6 | ff6011 == -5 | ff6011 == -1, NA, 
                                ifelse(ff6011 == 3 | ff6011 == 2, 1, 0))))

table(dat$ff6011, useNA = "ifany")

# ff6021
table(dat$ff6021, useNA = "ifany")

dat <- dat %>%
  mutate(ff6021 = ifelse(is.na(ff6021), NA,
                         ifelse(ff6021 == -10 | ff6021 == -6 | ff6021 == -5 | ff6021 == -1 | ff6021 == 5, NA, 
                                ifelse(ff6021 == 3 | ff6021 == 2, 1, 0))))

table(dat$ff6021, useNA = "ifany")

# clon154 (any bullying 11-17) - Recode if any variables above are missing, as will impute later
table(dat$clon154, useNA = "ifany")

dat <- dat %>%
  mutate(clon154 = ifelse(clon154 < 0, NA,
                          ifelse(complete.cases(ccl201, ta7018, tc4018, ff6011, ff6021), 
                                 pmax(ccl201, ta7018, tc4018, ff6011, ff6021), NA)))

table(dat$clon154, useNA = "ifany")


## Emotional abuse 11-17

# pp5037
table(dat$pp5037, useNA = "ifany")

dat <- dat %>%
  mutate(pp5037 = ifelse(is.na(pp5037), NA,
                         ifelse(pp5037 < 0, NA, 
                                ifelse(pp5037 == 4, 0, 1))))

table(dat$pp5037, useNA = "ifany")

# r5038
table(dat$r5038, useNA = "ifany")

dat <- dat %>%
  mutate(r5038 = ifelse(is.na(r5038), NA,
                         ifelse(r5038 < 0, NA, 
                                ifelse(r5038 == 4, 0, 1))))

table(dat$r5038, useNA = "ifany")

# pp5038
table(dat$pp5038, useNA = "ifany")

dat <- dat %>%
  mutate(pp5038 = ifelse(is.na(pp5038), NA,
                        ifelse(pp5038 < 0, NA, 
                               ifelse(pp5038 == 4, 0, 1))))

table(dat$pp5038, useNA = "ifany")

# r5037
table(dat$r5037, useNA = "ifany")

dat <- dat %>%
  mutate(r5037 = ifelse(is.na(r5037), NA,
                         ifelse(r5037 < 0, NA, 
                                ifelse(r5037 == 4, 0, 1))))

table(dat$r5037, useNA = "ifany")

# YPB8055
table(dat$YPB8055, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8055 = ifelse(is.na(YPB8055), NA,
                        ifelse(YPB8055 < 0, NA, 
                               ifelse(YPB8055 == 5 | YPB8055 == 4, 1, 0))))

table(dat$YPB8055, useNA = "ifany")

# YPB8051
table(dat$YPB8051, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8051 = ifelse(is.na(YPB8051), NA,
                          ifelse(YPB8051 < 0, NA, 
                                 ifelse(YPB8051 == 5 | YPB8051 == 4, 1, 0))))

table(dat$YPB8051, useNA = "ifany")

# YPB8072
table(dat$YPB8072, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8072 = ifelse(is.na(YPB8072), NA,
                          ifelse(YPB8072 < 0, NA, 
                                 ifelse(YPB8072 == 5 | YPB8072 == 4, 1, 0))))

table(dat$YPB8072, useNA = "ifany")

# clon155 (any emotional abuse 11-17) - Recode if any variables above are missing, as will impute later
table(dat$clon155, useNA = "ifany")

dat <- dat %>%
  mutate(clon155 = ifelse(clon155 < 0, NA,
                          ifelse(complete.cases(pp5037, r5038, pp5038, r5037, YPB8055, YPB8051, YPB8072), 
                                 pmax(pp5037, r5038, pp5038, r5037, YPB8055, YPB8051, YPB8072), NA)))

table(dat$clon155, useNA = "ifany")


## Physical abuse 11-17

# fg4432
table(dat$fg4432, useNA = "ifany")

dat <- dat %>%
  mutate(fg4432 = ifelse(is.na(fg4432), NA,
                          ifelse(fg4432 < 0, NA, 
                                 ifelse(fg4432 == 2, 0, 1))))

table(dat$fg4432, useNA = "ifany")

# fg4422
table(dat$fg4422, useNA = "ifany")

dat <- dat %>%
  mutate(fg4422 = ifelse(is.na(fg4422), NA,
                         ifelse(fg4422 < 0, NA, 
                                ifelse(fg4422 == 2, 0, 1))))

table(dat$fg4422, useNA = "ifany")

# fg4424
table(dat$fg4424, useNA = "ifany")

dat <- dat %>%
  mutate(fg4424 = ifelse(is.na(fg4424), NA,
                         ifelse(fg4424 < 0, NA, 
                                ifelse(fg4424 == 2, 0, 1))))

table(dat$fg4424, useNA = "ifany")

# fg4426
table(dat$fg4426, useNA = "ifany")

dat <- dat %>%
  mutate(fg4426 = ifelse(is.na(fg4426), NA,
                         ifelse(fg4426 < 0, NA, 
                                ifelse(fg4426 == 2, 0, 1))))

table(dat$fg4426, useNA = "ifany")

# tc1148
table(dat$tc1148, useNA = "ifany")

dat <- dat %>%
  mutate(tc1148 = ifelse(is.na(tc1148), NA,
                         ifelse(tc1148 < 0, NA, 
                                ifelse(tc1148 == 4, 0, 1))))

table(dat$tc1148, useNA = "ifany")

# tc1174
table(dat$tc1174, useNA = "ifany")

dat <- dat %>%
  mutate(tc1174 = ifelse(is.na(tc1174), NA,
                         ifelse(tc1174 < 0, NA, 
                                ifelse(tc1174 == 4, 0, 1))))

table(dat$tc1174, useNA = "ifany")

# pp5027
table(dat$pp5027, useNA = "ifany")

dat <- dat %>%
  mutate(pp5027 = ifelse(is.na(pp5027), NA,
                         ifelse(pp5027 < 0, NA, 
                                ifelse(pp5027 == 4, 0, 1))))

table(dat$pp5027, useNA = "ifany")

# r5026
table(dat$r5026, useNA = "ifany")

dat <- dat %>%
  mutate(r5026 = ifelse(is.na(r5026), NA,
                         ifelse(r5026 < 0, NA, 
                                ifelse(r5026 == 4, 0, 1))))

table(dat$r5026, useNA = "ifany")

# pp5026
table(dat$pp5026, useNA = "ifany")

dat <- dat %>%
  mutate(pp5026 = ifelse(is.na(pp5026), NA,
                        ifelse(pp5026 < 0, NA, 
                               ifelse(pp5026 == 4, 0, 1))))

table(dat$pp5026, useNA = "ifany")

# r5027
table(dat$r5027, useNA = "ifany")

dat <- dat %>%
  mutate(r5027 = ifelse(is.na(r5027), NA,
                         ifelse(r5027 < 0, NA, 
                                ifelse(r5027 == 4, 0, 1))))

table(dat$r5027, useNA = "ifany")

# YPB8057
table(dat$YPB8057, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8057 = ifelse(is.na(YPB8057), NA,
                        ifelse(YPB8057 < 0, NA, 
                               ifelse(YPB8057 == 5 | YPB8057 == 4, 1, 0))))

table(dat$YPB8057, useNA = "ifany")

# YPB8052
table(dat$YPB8052, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8052 = ifelse(is.na(YPB8052), NA,
                          ifelse(YPB8052 < 0, NA, 
                                 ifelse(YPB8052 == 5 | YPB8052 == 4, 1, 0))))

table(dat$YPB8052, useNA = "ifany")

# YPB8054
table(dat$YPB8054, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8054 = ifelse(is.na(YPB8054), NA,
                          ifelse(YPB8054 < 0, NA, 
                                 ifelse(YPB8054 == 5 | YPB8054 == 4, 1, 0))))

table(dat$YPB8054, useNA = "ifany")

# YPB8056
table(dat$YPB8056, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8056 = ifelse(is.na(YPB8056), NA,
                          ifelse(YPB8056 < 0, NA, 
                                 ifelse(YPB8056 == 5 | YPB8056 == 4, 1, 0))))

table(dat$YPB8056, useNA = "ifany")

# YPB8073
table(dat$YPB8073, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8073 = ifelse(is.na(YPB8073), NA,
                          ifelse(YPB8073 < 0, NA, 
                                 ifelse(YPB8073 == 5 | YPB8073 == 4, 1, 0))))

table(dat$YPB8073, useNA = "ifany")

# YPA5004/5
table(dat$YPA5004, useNA = "ifany")
table(dat$YPA5005, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5004 = ifelse(is.na(YPA5004), NA,
                          ifelse(YPA5004 < 0, NA, 
                                 ifelse(YPA5004 == 1, 0, 1))))

table(dat$YPA5004, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5004 = ifelse(YPA5005 == 2 & !is.na(YPA5004), 0, YPA5004))

table(dat$YPA5004, useNA = "ifany")

# YPA5006/7
table(dat$YPA5006, useNA = "ifany")
table(dat$YPA5007, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5006 = ifelse(is.na(YPA5006), NA,
                          ifelse(YPA5006 < 0, NA, 
                                 ifelse(YPA5006 == 1, 0, 1))))

table(dat$YPA5006, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5006 = ifelse(YPA5007 == 2 & !is.na(YPA5006), 0, YPA5006)) 

table(dat$YPA5006, useNA = "ifany")

dat <- dat %>%
  select(-c(YPA5005, YPA5007))

# clon156 (any physical abuse 11-17) - Recode if any variables above are missing, as will impute later
table(dat$clon156, useNA = "ifany")

dat <- dat %>%
  mutate(clon156 = ifelse(clon156 < 0, NA,
                          ifelse(complete.cases(fg4432, fg4422, fg4424, fg4426, tc1148, tc1174, pp5027, r5026, pp5026, r5027, YPB8057, YPB8052, YPB8054, YPB8056, YPB8073, YPA5004, YPA5006), 
                                 pmax(fg4432, fg4422, fg4424, fg4426, tc1148, tc1174, pp5027, r5026, pp5026, r5027, YPB8057, YPB8052, YPB8054, YPB8056, YPB8073, YPA5004, YPA5006), NA)))

table(dat$clon156, useNA = "ifany")


## Sexual abuse 11-17

# YPB8080
table(dat$YPB8080, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8080 = ifelse(is.na(YPB8080), NA,
                          ifelse(YPB8080 < 0, NA, 
                                 ifelse(YPB8080 == 2 | YPB8080 == 3, 1, 0))))

table(dat$YPB8080, useNA = "ifany")

# YPB8090
table(dat$YPB8090, useNA = "ifany")

dat <- dat %>%
  mutate(YPB8090 = ifelse(is.na(YPB8090), NA,
                          ifelse(YPB8090 < 0, NA, 
                                 ifelse(YPB8090 == 2 | YPB8090 == 3, 1, 0))))

table(dat$YPB8090, useNA = "ifany")

# YPA5008/9
table(dat$YPA5008, useNA = "ifany")
table(dat$YPA5009, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5008 = ifelse(is.na(YPA5008), NA,
                          ifelse(YPA5008 < 0, NA, 
                                 ifelse(YPA5008 == 1, 0, 1))))

table(dat$YPA5008, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5008 = ifelse(YPA5009 == 2 & !is.na(YPA5008), 0, YPA5008)) 

table(dat$YPA5008, useNA = "ifany")

# YPA5010/11
table(dat$YPA5010, useNA = "ifany")
table(dat$YPA5011, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5010 = ifelse(is.na(YPA5010), NA,
                          ifelse(YPA5010 < 0, NA, 
                                 ifelse(YPA5010 == 1, 0, 1))))

table(dat$YPA5010, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5010 = ifelse(YPA5011 == 2 & !is.na(YPA5010), 0, YPA5010)) 

table(dat$YPA5010, useNA = "ifany")

# YPA5012/13
table(dat$YPA5012, useNA = "ifany")
table(dat$YPA5013, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5012 = ifelse(is.na(YPA5012), NA,
                          ifelse(YPA5012 < 0, NA, 
                                 ifelse(YPA5012 == 1, 0, 1))))

table(dat$YPA5012, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5012 = ifelse(YPA5013 == 2 & !is.na(YPA5012), 0, YPA5012)) 

table(dat$YPA5012, useNA = "ifany")

# YPA5014/15
table(dat$YPA5014, useNA = "ifany")
table(dat$YPA5015, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5014 = ifelse(is.na(YPA5014), NA,
                          ifelse(YPA5014 < 0, NA, 
                                 ifelse(YPA5014 == 1, 0, 1))))

table(dat$YPA5014, useNA = "ifany")

dat <- dat %>%
  mutate(YPA5014 = ifelse(YPA5015 == 2 & !is.na(YPA5014), 0, YPA5014)) 

table(dat$YPA5014, useNA = "ifany")

dat <- dat %>%
  select(-c(YPA5009, YPA5011, YPA5013, YPA5015))

# clon157 (any sexual abuse 11-17) - Recode if any variables above are missing, as will impute later
table(dat$clon157, useNA = "ifany")

dat <- dat %>%
  mutate(clon157 = ifelse(clon157 < 0, NA,
                          ifelse(complete.cases(YPB8080, YPB8090, YPA5008, YPA5010, YPA5012, YPA5014), 
                                 pmax(YPB8080, YPB8090, YPA5008, YPA5010, YPA5012, YPA5014), NA)))

table(dat$clon157, useNA = "ifany")


## Domestic violence 11-17

# pp5022
table(dat$pp5022, useNA = "ifany")

dat <- dat %>%
  mutate(pp5022 = ifelse(is.na(pp5022), NA,
                          ifelse(pp5022 < 0, NA, 
                                 ifelse(pp5022 == 4, 0, 1))))

table(dat$pp5022, useNA = "ifany")

# r5022
table(dat$r5022, useNA = "ifany")

dat <- dat %>%
  mutate(r5022 = ifelse(is.na(r5022), NA,
                         ifelse(r5022 < 0, NA, 
                                ifelse(r5022 == 4, 0, 1))))

table(dat$r5022, useNA = "ifany")

# pq3154
table(dat$pq3154, useNA = "ifany")

dat <- dat %>%
  mutate(pq3154 = ifelse(is.na(pq3154), NA,
                        ifelse(pq3154 < 0, NA, 
                               ifelse(pq3154 == 4, 0, 1))))

table(dat$pq3154, useNA = "ifany")

# s3154
table(dat$s3154, useNA = "ifany")

dat <- dat %>%
  mutate(s3154 = ifelse(is.na(s3154), NA,
                         ifelse(s3154 < 0, NA, 
                                ifelse(s3154 == 4, 0, 1))))

table(dat$s3154, useNA = "ifany")

# clon158 (any domestic violence 11-17) - Recode if any variables above are missing, as will impute later
table(dat$clon158, useNA = "ifany")

dat <- dat %>%
  mutate(clon158 = ifelse(clon158 < 0, NA,
                          ifelse(complete.cases(pp5022, r5022, pq3154, s3154), 
                                 pmax(pp5022, r5022, pq3154, s3154), NA)))

table(dat$clon158, useNA = "ifany")


## Any traumas in 3 age groups

# clon145 (0-5)
table(dat$clon145, useNA = "ifany")

dat <- dat %>%
  mutate(clon145 = ifelse(clon145 < 0, NA,
                          ifelse(complete.cases(clon140, clon141, clon142, clon143, clon144), 
                                 pmax(clon140, clon141, clon142, clon143, clon144), NA)))

table(dat$clon145, useNA = "ifany")

# clon152 (5-11)
table(dat$clon152, useNA = "ifany")

dat <- dat %>%
  mutate(clon152 = ifelse(clon152 < 0, NA,
                          ifelse(complete.cases(clon146, clon147, clon148, clon149, clon150, clon151), 
                                 pmax(clon146, clon147, clon148, clon149, clon150, clon151), NA)))

table(dat$clon152, useNA = "ifany")

# clon159 (11-17)
table(dat$clon159, useNA = "ifany")

dat <- dat %>%
  mutate(clon159 = ifelse(clon159 < 0, NA,
                          ifelse(complete.cases(clon153, clon154, clon155, clon156, clon157, clon158), 
                                 pmax(clon153, clon154, clon155, clon156, clon157, clon158), NA)))

table(dat$clon159, useNA = "ifany")


### Outcomes

## Anxiety age 17
table(dat$FJCI602, useNA = "ifany")

dat <- dat %>%
  rename(anx17 = FJCI602) %>%
  mutate(anx17 = ifelse(is.na(anx17), NA,
                        ifelse(anx17 < 0, NA, anx17)))

table(dat$anx17, useNA = "ifany")

## Depression age 17
table(dat$FJCI603, useNA = "ifany")

dat <- dat %>%
  rename(dep17 = FJCI603) %>%
  mutate(dep17 = ifelse(is.na(dep17), NA,
                          ifelse(dep17 < 0, NA, dep17)))

table(dat$dep17, useNA = "ifany")

## Anxiety age 24
table(dat$FKDQ1030, useNA = "ifany")

dat <- dat %>%
  rename(anx24 = FKDQ1030) %>%
  mutate(anx24 = ifelse(is.na(anx24), NA,
                          ifelse(anx24 < 0, NA, anx24)))

table(dat$anx24, useNA = "ifany")

## Depression age 24
table(dat$FKDQ1000, useNA = "ifany")

dat <- dat %>%
  rename(dep24 = FKDQ1000) %>%
  mutate(dep24 = ifelse(is.na(dep24), NA,
                          ifelse(dep24 < 0, NA, dep24)))

table(dat$dep24, useNA = "ifany")


### Save data
save(dat, file = "./Data/data_full_B4563.RData")



#### Basic descriptive stats in full vs analytic sample (analytic sample = anyone with either age 17 or 24 outcome data; i.e., the sample will will impute to later)

## Load data form here is necessary
#load("./Data/data_full_B4563.RData")

# Make marker for analytic sample
dat <- dat %>%
  mutate(for_mi = ifelse(!is.na(anx17) | !is.na(dep17) | !is.na(anx24) | !is.na(dep24), 1, 0))

table(dat$for_mi, useNA = "ifany")


## Home ownership

# Full sample
table(dat$home)
round(prop.table(table(dat$home)) * 100, 1)

sum(is.na(dat$home))
round(sum(is.na(dat$home) / nrow(dat)) * 100, 1)

# Analytic sample
table(dat$home[dat$for_mi == 1])
round(prop.table(table(dat$home[dat$for_mi == 1])) * 100, 1)

sum(is.na(dat$home[dat$for_mi == 1]))
round(sum(is.na(dat$home[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## Offspring sex

# Full sample
table(dat$male)
round(prop.table(table(dat$male)) * 100, 1)

sum(is.na(dat$male))
round(sum(is.na(dat$male) / nrow(dat)) * 100, 1)

# Analytic sample
table(dat$male[dat$for_mi == 1])
round(prop.table(table(dat$male[dat$for_mi == 1])) * 100, 1)

sum(is.na(dat$male[dat$for_mi == 1]))
round(sum(is.na(dat$male[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## Offspring ethnic background

# Full sample
table(dat$ethnic)
round(prop.table(table(dat$ethnic)) * 100, 1)

sum(is.na(dat$ethnic))
round(sum(is.na(dat$ethnic) / nrow(dat)) * 100, 1)

# Analytic sample
table(dat$ethnic[dat$for_mi == 1])
round(prop.table(table(dat$ethnic[dat$for_mi == 1])) * 100, 1)

sum(is.na(dat$ethnic[dat$for_mi == 1]))
round(sum(is.na(dat$ethnic[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## Maternal age

# Full sample
summary(dat$mat_age)
sd(dat$mat_age, na.rm = TRUE)

sum(is.na(dat$mat_age))
round(sum(is.na(dat$mat_age) / nrow(dat)) * 100, 1)

# Analytic sample
summary(dat$mat_age[dat$for_mi == 1])
sd(dat$mat_age[dat$for_mi == 1], na.rm = TRUE)

sum(is.na(dat$mat_age[dat$for_mi == 1]))
round(sum(is.na(dat$mat_age[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## Marital status

# Full sample
table(dat$marital)
round(prop.table(table(dat$marital)) * 100, 1)

sum(is.na(dat$marital))
round(sum(is.na(dat$marital) / nrow(dat)) * 100, 1)

# Analytic sample
table(dat$marital[dat$for_mi == 1])
round(prop.table(table(dat$marital[dat$for_mi == 1])) * 100, 1)

sum(is.na(dat$marital[dat$for_mi == 1]))
round(sum(is.na(dat$marital[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## Education

# Full sample
table(dat$edu)
round(prop.table(table(dat$edu)) * 100, 1)

sum(is.na(dat$edu))
round(sum(is.na(dat$edu) / nrow(dat)) * 100, 1)

# Analytic sample
table(dat$edu[dat$for_mi == 1])
round(prop.table(table(dat$edu[dat$for_mi == 1])) * 100, 1)

sum(is.na(dat$edu[dat$for_mi == 1]))
round(sum(is.na(dat$edu[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## IMD 

# Full sample
table(dat$imd)
round(prop.table(table(dat$imd)) * 100, 1)

sum(is.na(dat$imd))
round(sum(is.na(dat$imd) / nrow(dat)) * 100, 1)

# Analytic sample
table(dat$imd[dat$for_mi == 1])
round(prop.table(table(dat$imd[dat$for_mi == 1])) * 100, 1)

sum(is.na(dat$imd[dat$for_mi == 1]))
round(sum(is.na(dat$imd[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


### Maternal depression

# Full sample
summary(dat$mat_dep)
sd(dat$mat_dep, na.rm = TRUE)

sum(is.na(dat$mat_dep))
round(sum(is.na(dat$mat_dep) / nrow(dat)) * 100, 1)

# Analytic sample
summary(dat$mat_dep[dat$for_mi == 1])
sd(dat$mat_dep[dat$for_mi == 1], na.rm = TRUE)

sum(is.na(dat$mat_dep[dat$for_mi == 1]))
round(sum(is.na(dat$mat_dep[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## Maternal anxiety

# Full sample
summary(dat$mat_anx)
sd(dat$mat_anx, na.rm = TRUE)

sum(is.na(dat$mat_anx))
round(sum(is.na(dat$mat_anx) / nrow(dat)) * 100, 1)

# Analytic sample
summary(dat$mat_anx[dat$for_mi == 1])
sd(dat$mat_anx[dat$for_mi == 1], na.rm = TRUE)

sum(is.na(dat$mat_anx[dat$for_mi == 1]))
round(sum(is.na(dat$mat_anx[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## Maternal ACEs

# Full sample
summary(dat$mat_aces)
sd(dat$mat_aces, na.rm = TRUE)

sum(is.na(dat$mat_aces))
round(sum(is.na(dat$mat_aces) / nrow(dat)) * 100, 1)

# Analytic sample
summary(dat$mat_aces[dat$for_mi == 1])
sd(dat$mat_aces[dat$for_mi == 1], na.rm = TRUE)

sum(is.na(dat$mat_aces[dat$for_mi == 1]))
round(sum(is.na(dat$mat_aces[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## Depression at age 17

# Full sample
table(dat$dep17)
round(prop.table(table(dat$dep17)) * 100, 1)

sum(is.na(dat$dep17))
round(sum(is.na(dat$dep17) / nrow(dat)) * 100, 1)

# Analytic sample
table(dat$dep17[dat$for_mi == 1])
round(prop.table(table(dat$dep17[dat$for_mi == 1])) * 100, 1)

sum(is.na(dat$dep17[dat$for_mi == 1]))
round(sum(is.na(dat$dep17[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## Anxiety at age 17

# Full sample
table(dat$anx17)
round(prop.table(table(dat$anx17)) * 100, 1)

sum(is.na(dat$anx17))
round(sum(is.na(dat$anx17) / nrow(dat)) * 100, 1)

# Analytic sample
table(dat$anx17[dat$for_mi == 1])
round(prop.table(table(dat$anx17[dat$for_mi == 1])) * 100, 1)

sum(is.na(dat$anx17[dat$for_mi == 1]))
round(sum(is.na(dat$anx17[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## Depression at age 24

# Full sample
table(dat$dep24)
round(prop.table(table(dat$dep24)) * 100, 1)

sum(is.na(dat$dep24))
round(sum(is.na(dat$dep24) / nrow(dat)) * 100, 1)

# Analytic sample
table(dat$dep24[dat$for_mi == 1])
round(prop.table(table(dat$dep24[dat$for_mi == 1])) * 100, 1)

sum(is.na(dat$dep24[dat$for_mi == 1]))
round(sum(is.na(dat$dep24[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


## Anxiety at age 24

# Full sample
table(dat$anx24)
round(prop.table(table(dat$anx24)) * 100, 1)

sum(is.na(dat$anx24))
round(sum(is.na(dat$anx24) / nrow(dat)) * 100, 1)

# Analytic sample
table(dat$anx24[dat$for_mi == 1])
round(prop.table(table(dat$anx24[dat$for_mi == 1])) * 100, 1)

sum(is.na(dat$anx24[dat$for_mi == 1]))
round(sum(is.na(dat$anx24[dat$for_mi == 1]) / nrow(dat[dat$for_mi == 1, ])) * 100, 1)


### Reduce data down to analytic sample
dat <- dat %>%
  filter(for_mi == 1) %>%
  select(-for_mi)


### Save data
save(dat, file = "./Data/data_analytic_B4563.RData")




