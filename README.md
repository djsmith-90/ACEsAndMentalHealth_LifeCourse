## Git repository for 'A life-course approach to the relationship between adverse childhood experiences and mental health in a longitudinal UK birth cohort (ALSPAC)' ALSPAC project (B4563)

The main directory in this repository contains two folders.

The 'Scripts' folder contains seven scripts for the data analyses. These files are:
 - ACEsMH_Script1_DataPrep.R - R script to clean and process the raw ALSPAC data
 - ACEsMH_Script2_Synthpop.R - R script to create synthetic datasets using the 'synthpop' package
 - ACEsMH_Script3_MultipleImputation.R - R script to perform the multiple imputation analyses
 - ACEsMH_Script4a_MI_MainAnalyses.R - R script perform the main analyses on the imputed data
 - ACEsMH_Script4b_MI_EmoNeglectAnalyses.R - R script perform the 'emotional neglect ACE' analyses on the imputed data
 - ACEsMH_Script5a_CCA_MainAnalyses.R - R script perform the main analyses on the complete-case data
 - ACEsMH_Script5b_CCA_EmoNeglectAnalyses.R - R script perform the 'emotional neglect ACE' analyses on the complete-case data
 
 
The 'SyntheticData' folder also contains synthetic versions of the ALSPAC dataset, created using Script 2 above. As raw ALSPAC data cannot be released, these synthesised datasets are modelled on the original ALSPAC data, thus maintaining variable distributions and relations among variables (albeit not pefectly), while at the same time preserving participant anonymity and confidentiality. Please note that while these synthetic datasets can be used to follow the analysis scripts, as data are simulated they should *not* be used for research purposes; only the actual, observed, ALSPAC data should be used for formal research and analyses reported in published work.

These synthetic datasets have the file name 'syntheticData_B4563' and are available in R ('.RData'), and CSV ('.csv') formats.

Note that ALSPAC data access is through a system of managed open access. Information about access to ALSPAC data is given on the ALSPAC website (http://www.bristol.ac.uk/alspac/researchers/access/). The datasets used in these scripts are linked to ALSPAC project number B4563; if you are interested in accessing these datasets, please quote this number during your application.
