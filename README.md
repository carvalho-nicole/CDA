# Community Detection Analysis (CDA)

The goal of this project was to run a CDA on a behavioral dataset in order to obtain groups of patients with similar profiles. This behavioral dataset consisted of cognitive data from 41 patients with post stroke aphasia. Then functional connectivity (FC) values would be correlated between groups. FC values came from resting state fMRI. The steps of this project are as follows:
  1) Data Imputation (to resolve the issue of missing data)
  2) Data pooling (
  3) Data normalization
  4) Data thresholding
  5) Run the CDA
  6) Correlate FC between groups.

The code to excute this project comes in three files: data_prep_for_CDA (data imputaiton, data pooling, data normalization, data thresholding), 
CDA
Correlation of FC between groups
