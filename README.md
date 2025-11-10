# Community Detection Analysis (CDA)

The goal of this project was to run a CDA on a behavioral dataset in order to obtain groups of patients with similar profiles. This behavioral dataset consisted of cognitive data from 41 patients with post stroke aphasia. Then functional connectivity (FC) values would be correlated between groups. FC values came from resting state fMRI. The steps of this project are as follows:
  1) Data Imputation (to resolve the issue of missing data)
  2) Data pooling (for ease of analysis)
  3) Data normalization 
  4) Data thresholding (for item reduction)
  5) CDA execution
  6) Correlation of FC between groups

The code to excute this project comes in three files: data_prep_for_CDA.R (data imputation, data pooling, data normalization, data thresholding), 
CDA.R
CDA_FC_correlation.R
