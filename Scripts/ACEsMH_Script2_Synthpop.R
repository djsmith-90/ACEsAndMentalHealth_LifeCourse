### ACEs, Mental Health and SLCMA
### Script 2 - Synthetic dataset creation script
### Created 29/10/2025
### R version 4.3.1

## Set working directory, clear workspace, and load packages
rm(list = ls())

setwd("X:/Studies/RSBB Team/Dan/B4563 - ACEs and MH")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("synthpop")
library(synthpop)


###########################################################################################
###### Read in the processed data and create synthetic datasets

## Read in the data
load("./Data/data_analytic_B4563.RData")

head(dat)


## Drop all the derived 'clon' ACE variables, as will re-derive these later to ensure are consistent with the data
dat <- dat %>%
  select(-c(clon140, clon141, clon142, clon143, clon144, clon146, clon147, clon148, clon148, clon149,
                   clon150, clon151, clon153, clon154, clon155, clon156, clon157, clon158,
                   clon145, clon152, clon159))


# Get information about variables in the dataset
codebook.syn(dat)$tab


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

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
dat_syn <- syn(dat, seed = 291025)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, are 4 unique replicates (0.07% of sample)
replicated.uniques(dat_syn, dat)
dat_syn <- sdc(dat_syn, dat, rm.replicated.uniques = TRUE)

## Take a few unique true observations, and make sure not fully-replicated in synthetic dataset (based on the 'replicated.uniques' command from the 'synthpop' package)

# Make a dataset just of unique individuals using the observed data (as if two or more participants share exactly the same data, then it's impossible to link back to a unique individual)
sum(!(duplicated(dat) | duplicated(dat, fromLast = TRUE)))
dat_unique <- dat[!(duplicated(dat) | duplicated(dat, fromLast = TRUE)), ]

# Make a dataset just of unique individuals from the synthetic dataset
sum(!(duplicated(dat_syn$syn) | duplicated(dat_syn$syn, fromLast = TRUE)))
syn_unique <- dat_syn$syn[!(duplicated(dat_syn$syn) | 
                                          duplicated(dat_syn$syn, fromLast = TRUE)), ]

# Select a random row from the observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 1), ])

# Combine observed row with the synthetic data, and see if any duplicates
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

# Repeat for a few more rows of observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 10), ])
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))


# Explore this synthetic dataset
dat_syn
summary(dat_syn)


# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is good). Save this as a PDF
#compare(dat_syn, dat, stat = "counts")

#pdf("./Results/ComparingDescStats_synthetic.pdf", height = 6, width = 10)
#compare(dat_syn, dat, stat = "counts", nrow = 4, ncol = 5)
#dev.off()



### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
dat_syn$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(dat_syn$syn)), dat_syn$syn)
summary(dat_syn)

# Extract the synthetic dataset (rather than it being stored within a list)
dat_syn_df <- dat_syn$syn
head(dat_syn_df)
glimpse(dat_syn_df)
summary(dat_syn_df)


## Add back in the derived ACE variables

# Convert variables back to numeric first
dat_syn_df <- dat_syn_df %>%
  mutate(male = as.numeric(male) - 1) %>%
  mutate(home = as.numeric(home) - 1) %>%
  mutate(marital = as.numeric(marital) - 1) %>%
  mutate(edu = as.numeric(edu)) %>%
  mutate(imd = as.numeric(imd)) %>%
  mutate(ethnic = as.numeric(ethnic) - 1) %>%
  mutate(priorMH = as.numeric(priorMH) - 1) %>%
  mutate(mat_pregsmk = as.numeric(mat_pregsmk) - 1) %>%
  mutate(mat_soc = as.numeric(mat_soc)) %>%
  mutate(pat_soc = as.numeric(pat_soc)) %>%
  mutate(b596 = as.numeric(b596) - 1) %>%
  mutate(pb186a = as.numeric(pb186a) - 1) %>%
  mutate(f246a = as.numeric(f246a) - 1) %>%
  mutate(pd247a = as.numeric(pd247a) - 1) %>%
  mutate(g326a = as.numeric(g326a) - 1) %>%
  mutate(pe327a = as.numeric(pe327a) - 1) %>%
  mutate(h236a = as.numeric(h236a) - 1) %>%
  mutate(pf5027 = as.numeric(pf5027) - 1) %>%
  mutate(j326a = as.numeric(j326a) - 1) %>%
  mutate(pg3027 = as.numeric(pg3027) - 1) %>%
  mutate(f247a = as.numeric(f247a) - 1) %>%
  mutate(pd246a = as.numeric(pd246a) - 1) %>%
  mutate(g327a = as.numeric(g327a) - 1) %>%
  mutate(pe326a = as.numeric(pe326a) - 1) %>%
  mutate(h237a = as.numeric(h237a) - 1) %>%
  mutate(pf5026 = as.numeric(pf5026) - 1) %>%
  mutate(j327a = as.numeric(j327a) - 1) %>%
  mutate(pg3026 = as.numeric(pg3026) - 1) %>%
  mutate(b608 = as.numeric(b608) - 1) %>%
  mutate(f257a = as.numeric(f257a) - 1) %>%
  mutate(pd258a = as.numeric(pd258a) - 1) %>%
  mutate(g337a = as.numeric(g337a) - 1) %>%
  mutate(pe338a = as.numeric(pe338a) - 1) %>%
  mutate(h247a = as.numeric(h247a) - 1) %>%
  mutate(pf5036 = as.numeric(pf5036) - 1) %>%
  mutate(j337a = as.numeric(j337a) - 1) %>%
  mutate(pg3036 = as.numeric(pg3036) - 1) %>%
  mutate(pc236a = as.numeric(pc236a) - 1) %>%
  mutate(f258a = as.numeric(f258a) - 1) %>%
  mutate(pd257a = as.numeric(pd257a) - 1) %>%
  mutate(g338a = as.numeric(g338a) - 1) %>%
  mutate(pe337a = as.numeric(pe337a) - 1) %>%
  mutate(h248a = as.numeric(h248a) - 1) %>%
  mutate(pf5037 = as.numeric(pf5037) - 1) %>%
  mutate(j338a = as.numeric(j338a) - 1) %>%
  mutate(pg3037 = as.numeric(pg3037) - 1) %>%
  mutate(pc222a = as.numeric(pc222a) - 1) %>%
  mutate(pe322a = as.numeric(pe322a) - 1) %>%
  mutate(pf5022 = as.numeric(pf5022) - 1) %>%
  mutate(pg3022 = as.numeric(pg3022) - 1) %>%
  mutate(e422 = as.numeric(e422) - 1) %>%
  mutate(f242a = as.numeric(f242a) - 1) %>%
  mutate(pd242a = as.numeric(pd242a) - 1) %>%
  mutate(g322a = as.numeric(g322a) - 1) %>%
  mutate(h232a = as.numeric(h232a) - 1) %>%
  mutate(j322a = as.numeric(j322a) - 1) %>%
  mutate(f256a = as.numeric(f256a) - 1) %>%
  mutate(kd505b = as.numeric(kd505b) - 1) %>%
  mutate(kf455a = as.numeric(kf455a) - 1) %>%
  mutate(kj465a = as.numeric(kj465a) - 1) %>%
  mutate(j548 = as.numeric(j548) - 1) %>%
  mutate(pg4148 = as.numeric(pg4148) - 1) %>%
  mutate(ccc250 = as.numeric(ccc250) - 1) %>%
  mutate(ccf149 = as.numeric(ccf149) - 1) %>%
  mutate(kq338 = as.numeric(kq338) - 1) %>%
  mutate(ccc290 = as.numeric(ccc290) - 1) %>%
  mutate(ku698 = as.numeric(ku698) - 1) %>%
  mutate(f8fp151 = as.numeric(f8fp151) - 1) %>%
  mutate(f8fp161 = as.numeric(f8fp161) - 1) %>%
  mutate(fdfp151 = as.numeric(fdfp151) - 1) %>%
  mutate(fdfp161 = as.numeric(fdfp161) - 1) %>%
  mutate(n8358 = as.numeric(n8358) - 1) %>%
  mutate(ph4026 = as.numeric(ph4026) - 1) %>%
  mutate(l4026 = as.numeric(l4026) - 1) %>%
  mutate(pj4027 = as.numeric(pj4027) - 1) %>%
  mutate(p2026 = as.numeric(p2026) - 1) %>%
  mutate(pm2027 = as.numeric(pm2027) - 1) %>%
  mutate(k4026 = as.numeric(k4026) - 1) %>%
  mutate(k4027 = as.numeric(k4027) - 1) %>%
  mutate(ph4027 = as.numeric(ph4027) - 1) %>%
  mutate(l4027 = as.numeric(l4027) - 1) %>%
  mutate(pj4026 = as.numeric(pj4026) - 1) %>%
  mutate(p2027 = as.numeric(p2027) - 1) %>%
  mutate(pm2026 = as.numeric(pm2026) - 1) %>%
  mutate(YPB8004 = as.numeric(YPB8004) - 1) %>%
  mutate(YPB8023 = as.numeric(YPB8023) - 1) %>%
  mutate(YPB8002 = as.numeric(YPB8002) - 1) %>%
  mutate(YPB8006 = as.numeric(YPB8006) - 1) %>%
  mutate(YPB8007 = as.numeric(YPB8007) - 1) %>%
  mutate(kw4041 = as.numeric(kw4041) - 1) %>%
  mutate(ph4038 = as.numeric(ph4038) - 1) %>%
  mutate(l4037 = as.numeric(l4037) - 1) %>%
  mutate(pj4038 = as.numeric(pj4038) - 1) %>%
  mutate(p2037 = as.numeric(p2037) - 1) %>%
  mutate(pm2038 = as.numeric(pm2038) - 1) %>%
  mutate(k4038 = as.numeric(k4038) - 1) %>%
  mutate(ph4037 = as.numeric(ph4037) - 1) %>%
  mutate(l4038 = as.numeric(l4038) - 1) %>%
  mutate(pj4037 = as.numeric(pj4037) - 1) %>%
  mutate(p2038 = as.numeric(p2038) - 1) %>%
  mutate(pm2037 = as.numeric(pm2037) - 1) %>%
  mutate(YPB8005 = as.numeric(YPB8005) - 1) %>%
  mutate(YPB8001 = as.numeric(YPB8001) - 1) %>%
  mutate(pj4036 = as.numeric(pj4036) - 1) %>%
  mutate(k4037 = as.numeric(k4037) - 1) %>%
  mutate(YPB8022 = as.numeric(YPB8022) - 1) %>%
  mutate(k4022 = as.numeric(k4022) - 1) %>%
  mutate(l4022 = as.numeric(l4022) - 1) %>%
  mutate(p2022 = as.numeric(p2022) - 1) %>%
  mutate(ph4022 = as.numeric(ph4022) - 1) %>%
  mutate(pj4022 = as.numeric(pj4022) - 1) %>%
  mutate(pm3155 = as.numeric(pm3155) - 1) %>%
  mutate(pm2022 = as.numeric(pm2022) - 1) %>%
  mutate(kl475 = as.numeric(kl475) - 1) %>%
  mutate(kn4005 = as.numeric(kn4005) - 1) %>%
  mutate(kq365a = as.numeric(kq365a) - 1) %>%
  mutate(kt5005 = as.numeric(kt5005) - 1) %>%
  mutate(YPB8040 = as.numeric(YPB8040) - 1) %>%
  mutate(YPB8030 = as.numeric(YPB8030) - 1) %>%
  mutate(ff5318 = as.numeric(ff5318) - 1) %>%
  mutate(fg7118 = as.numeric(fg7118) - 1) %>%
  mutate(fh8200 = as.numeric(fh8200) - 1) %>%
  mutate(fh9821 = as.numeric(fh9821) - 1) %>%
  mutate(ccxa243 = as.numeric(ccxa243) - 1) %>%
  mutate(ccl201 = as.numeric(ccl201) - 1) %>%
  mutate(ta7018 = as.numeric(ta7018) - 1) %>%
  mutate(tc4018 = as.numeric(tc4018) - 1) %>%
  mutate(ff6011 = as.numeric(ff6011) - 1) %>%
  mutate(ff6021 = as.numeric(ff6021) - 1) %>%
  mutate(pp5037 = as.numeric(pp5037) - 1) %>%
  mutate(r5038 = as.numeric(r5038) - 1) %>%
  mutate(pp5038 = as.numeric(pp5038) - 1) %>%
  mutate(r5037 = as.numeric(r5037) - 1) %>%
  mutate(YPB8055 = as.numeric(YPB8055) - 1) %>%
  mutate(YPB8051 = as.numeric(YPB8051) - 1) %>%
  mutate(YPB8072 = as.numeric(YPB8072) - 1) %>%
  mutate(fg4432 = as.numeric(fg4432) - 1) %>%
  mutate(fg4422 = as.numeric(fg4422) - 1) %>%
  mutate(fg4424 = as.numeric(fg4424) - 1) %>%
  mutate(fg4426 = as.numeric(fg4426) - 1) %>%
  mutate(tc1148 = as.numeric(tc1148) - 1) %>%
  mutate(tc1174 = as.numeric(tc1174) - 1) %>%
  mutate(pp5027 = as.numeric(pp5027) - 1) %>%
  mutate(r5026 = as.numeric(r5026) - 1) %>%
  mutate(pp5026 = as.numeric(pp5026) - 1) %>%
  mutate(r5027 = as.numeric(r5027) - 1) %>%
  mutate(YPB8057 = as.numeric(YPB8057) - 1) %>%
  mutate(YPB8052 = as.numeric(YPB8052) - 1) %>%
  mutate(YPB8054 = as.numeric(YPB8054) - 1) %>%
  mutate(YPB8056 = as.numeric(YPB8056) - 1) %>%
  mutate(YPB8073 = as.numeric(YPB8073) - 1) %>%
  mutate(YPA5004 = as.numeric(YPA5004) - 1) %>%
  mutate(YPA5006 = as.numeric(YPA5006) - 1) %>%
  mutate(YPB8080 = as.numeric(YPB8080) - 1) %>%
  mutate(YPB8090 = as.numeric(YPB8090) - 1) %>%
  mutate(YPA5008 = as.numeric(YPA5008) - 1) %>%
  mutate(YPA5010 = as.numeric(YPA5010) - 1) %>%
  mutate(YPA5012 = as.numeric(YPA5012) - 1) %>%
  mutate(YPA5014 = as.numeric(YPA5014) - 1) %>%
  mutate(pp5022 = as.numeric(pp5022) - 1) %>%
  mutate(r5022 = as.numeric(r5022) - 1) %>%
  mutate(pq3154 = as.numeric(pq3154) - 1) %>%
  mutate(s3154 = as.numeric(s3154) - 1) %>%
  mutate(anx17 = as.numeric(anx17) - 1) %>%
  mutate(dep17 = as.numeric(dep17) - 1) %>%
  mutate(anx24 = as.numeric(anx24) - 1) %>%
  mutate(dep24 = as.numeric(dep24) - 1)

# clon140 (any physical abuse 0-5)
dat_syn_df <- dat_syn_df %>%
  mutate(clon140 = ifelse(complete.cases(b596, pb186a, f246a, pd247a, g326a, pe327a, h236a, pf5027, j326a, pg3027, f247a, pd246a, g327a, pe326a, h237a, pf5026, j327a, pg3026), 
                                 pmax(b596, pb186a, f246a, pd247a, g326a, pe327a, h236a, pf5027, j326a, pg3027, f247a, pd246a, g327a, pe326a, h237a, pf5026, j327a, pg3026), NA)) %>%
  relocate(clon140, .after = pg3026)

table(dat_syn_df$clon140, useNA = "ifany")

# clon141 (any emotional abuse 0-5)
dat_syn_df <- dat_syn_df %>%
  mutate(clon141 = ifelse(complete.cases(b608, f257a, pd258a, g337a, pe338a, h247a, pf5036, j337a, pg3036, pc236a, f258a, pd257a, g338a, pe337a, h248a, pf5037, j338a, pg3037), 
                          pmax(b608, f257a, pd258a, g337a, pe338a, h247a, pf5036, j337a, pg3036, pc236a, f258a, pd257a, g338a, pe337a, h248a, pf5037, j338a, pg3037), NA)) %>%
  relocate(clon141, .after = pg3037)

table(dat_syn_df$clon141, useNA = "ifany")

# clon142 (any domestic violence 0-5)
dat_syn_df <- dat_syn_df %>%
  mutate(clon142 = ifelse(complete.cases(pc222a, pe322a, pf5022, pg3022, e422, f242a, pd242a, g322a, h232a, j322a, f256a), 
                          pmax(pc222a, pe322a, pf5022, pg3022, e422, f242a, pd242a, g322a, h232a, j322a, f256a), NA)) %>%
  relocate(clon142, .after = f256a)

table(dat_syn_df$clon142, useNA = "ifany")

# clon143 (any sexual abuse 0-5)
dat_syn_df <- dat_syn_df %>%
  mutate(clon143 = ifelse(complete.cases(kd505b, kf455a, kj465a), 
                          pmax(kd505b, kf455a, kj465a), NA)) %>%
  relocate(clon143, .after = kj465a)

table(dat_syn_df$clon143, useNA = "ifany")

# clon144 (any bullying 0-5)
dat_syn_df <- dat_syn_df %>%
  mutate(clon144 = ifelse(complete.cases(j548, pg4148), 
                          pmax(j548, pg4148), NA)) %>%
  relocate(clon144, .after = pg4148)

table(dat_syn_df$clon144, useNA = "ifany")

# clon146 (any emotional neglect 5-11)
dat_syn_df <- dat_syn_df %>%
  mutate(clon146 = ifelse(complete.cases(ccc250, ccf149), 
                          pmax(ccc250, ccf149), NA)) %>%
  relocate(clon146, .after = ccf149)

table(dat_syn_df$clon146, useNA = "ifany")

# clon147 (any bullying 5-11)
dat_syn_df <- dat_syn_df %>%
  mutate(clon147 = ifelse(complete.cases(kq338, ccc290, ku698, f8fp151, f8fp161, fdfp151, fdfp161, n8358), 
                          pmax(kq338, ccc290, ku698, f8fp151, f8fp161, fdfp151, fdfp161, n8358), NA)) %>%
  relocate(clon147, .after = n8358)

table(dat_syn_df$clon147, useNA = "ifany")

# clon148 (any physical abuse 5-11)
dat_syn_df <- dat_syn_df %>%
  mutate(clon148 = ifelse(complete.cases(ph4026, l4026, pj4027, p2026, pm2027, k4026, k4027, ph4027, l4027, pj4026, p2027, pm2026, YPB8004, YPB8023, YPB8002, YPB8006, YPB8007, kw4041), 
                          pmax(ph4026, l4026, pj4027, p2026, pm2027, k4026, k4027, ph4027, l4027, pj4026, p2027, pm2026, YPB8004, YPB8023, YPB8002, YPB8006, YPB8007, kw4041), NA)) %>%
  relocate(clon148, .after = kw4041)

table(dat_syn_df$clon148, useNA = "ifany")

# clon149 (any emotional abuse 5-11)
dat_syn_df <- dat_syn_df %>%
  mutate(clon149 = ifelse(complete.cases(ph4038, l4037, pj4038, p2037, pm2038, k4038, ph4037, l4038, pj4037, p2038, pm2037, YPB8005, YPB8001, pj4036, k4037, YPB8022), 
                          pmax(ph4038, l4037, pj4038, p2037, pm2038, k4038, ph4037, l4038, pj4037, p2038, pm2037, YPB8005, YPB8001, pj4036, k4037, YPB8022), NA)) %>%
  relocate(clon149, .after = YPB8022)

table(dat_syn_df$clon149, useNA = "ifany")

# clon150 (any domestic violence 5-11)
dat_syn_df <- dat_syn_df %>%
  mutate(clon150 = ifelse(complete.cases(k4022, l4022, p2022, ph4022, pj4022, pm3155, pm2022), 
                          pmax(k4022, l4022, p2022, ph4022, pj4022, pm3155, pm2022), NA)) %>%
  relocate(clon150, .after = pm2022)

table(dat_syn_df$clon150, useNA = "ifany")

# clon151 (any sexual abuse 5-11)
dat_syn_df <- dat_syn_df %>%
  mutate(clon151 = ifelse(complete.cases(kl475, kn4005, kq365a, kt5005, YPB8040, YPB8030), 
                          pmax(kl475, kn4005, kq365a, kt5005, YPB8040, YPB8030), NA)) %>%
  relocate(clon151, .after = YPB8030)

table(dat_syn_df$clon151, useNA = "ifany")

# clon153 (any emotional neglect 11-17)
dat_syn_df <- dat_syn_df %>%
  mutate(clon153 = ifelse(complete.cases(ff5318, fg7118, fh8200, fh9821, ccxa243), 
                          pmax(ff5318, fg7118, fh8200, fh9821, ccxa243), NA)) %>%
  relocate(clon153, .after = ccxa243)

table(dat_syn_df$clon153, useNA = "ifany")

# clon154 (any bullying 11-17)
dat_syn_df <- dat_syn_df %>%
  mutate(clon154 = ifelse(complete.cases(ccl201, ta7018, tc4018, ff6011, ff6021), 
                          pmax(ccl201, ta7018, tc4018, ff6011, ff6021), NA)) %>%
  relocate(clon154, .after = ff6021)

table(dat_syn_df$clon154, useNA = "ifany")

# clon155 (any emotional abuse 11-17)
dat_syn_df <- dat_syn_df %>%
  mutate(clon155 = ifelse(complete.cases(pp5037, r5038, pp5038, r5037, YPB8055, YPB8051, YPB8072), 
                          pmax(pp5037, r5038, pp5038, r5037, YPB8055, YPB8051, YPB8072), NA)) %>%
  relocate(clon155, .after = YPB8072)

table(dat_syn_df$clon155, useNA = "ifany")

# clon156 (any physical abuse 11-17)
dat_syn_df <- dat_syn_df %>%
  mutate(clon156 = ifelse(complete.cases(fg4432, fg4422, fg4424, fg4426, tc1148, tc1174, pp5027, r5026, pp5026, r5027, YPB8057, YPB8052, YPB8054, YPB8056, YPB8073, YPA5004, YPA5006), 
                          pmax(fg4432, fg4422, fg4424, fg4426, tc1148, tc1174, pp5027, r5026, pp5026, r5027, YPB8057, YPB8052, YPB8054, YPB8056, YPB8073, YPA5004, YPA5006), NA)) %>%
  relocate(clon156, .after = YPA5006)

table(dat_syn_df$clon156, useNA = "ifany")

# clon157 (any sexual abuse 11-17)
dat_syn_df <- dat_syn_df %>%
  mutate(clon157 = ifelse(complete.cases(YPB8080, YPB8090, YPA5008, YPA5010, YPA5012, YPA5014), 
                          pmax(YPB8080, YPB8090, YPA5008, YPA5010, YPA5012, YPA5014), NA)) %>%
  relocate(clon157, .after = YPA5014)

table(dat_syn_df$clon157, useNA = "ifany")

# clon158 (any domestic violence 11-17)
dat_syn_df <- dat_syn_df %>%
  mutate(clon158 = ifelse(complete.cases(pp5022, r5022, pq3154, s3154), 
                          pmax(pp5022, r5022, pq3154, s3154), NA)) %>%
  relocate(clon158, .after = s3154)

table(dat_syn_df$clon158, useNA = "ifany")


## Any traumas in 3 age groups

# clon145 (0-5)
dat_syn_df <- dat_syn_df %>%
  mutate(clon145 = ifelse(complete.cases(clon140, clon141, clon142, clon143, clon144), 
                                 pmax(clon140, clon141, clon142, clon143, clon144), NA)) %>%
  relocate(clon145, .after = clon158)

table(dat_syn_df$clon145, useNA = "ifany")

# clon152 (5-11)
dat_syn_df <- dat_syn_df %>%
  mutate(clon152 = ifelse(complete.cases(clon146, clon147, clon148, clon149, clon150, clon151), 
                                 pmax(clon146, clon147, clon148, clon149, clon150, clon151), NA)) %>%
  relocate(clon152, .after = clon145)

table(dat_syn_df$clon152, useNA = "ifany")

# clon159 (11-17)
dat_syn_df <- dat_syn_df %>%
  mutate(clon159 = ifelse(complete.cases(clon153, clon154, clon155, clon156, clon157, clon158), 
                                 pmax(clon153, clon154, clon155, clon156, clon157, clon158), NA)) %>%
  relocate(clon159, .after = clon152)

table(dat_syn_df$clon159, useNA = "ifany")


## Final check of the data
head(dat_syn_df)
glimpse(dat_syn_df)
summary(dat_syn_df)


### Store the synthetic dataset for others to use
save(dat_syn_df, 
     file = "./AnalysisCode_ACEsMH_B4563/SyntheticData/syntheticData_B4563.RData")
write_csv(dat_syn_df, 
          file = "./AnalysisCode_ACEsMH_B4563/SyntheticData/syntheticData_B4563.csv")

