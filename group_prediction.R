#This code uses neural functional connectivity measures to predict behavioral cluster membership.

library(dplyr)

###Load and clean up neural data
# Load all CSV files in the directory containing neural functional connectivity values
csv_files <- list.files(pattern = "*.csv", full.names = TRUE)

# Read all CSV files into a list of data frames
data_list <- lapply(csv_files, read.csv, header = FALSE)

names(data_list) <- tools::file_path_sans_ext(basename(csv_files))

# Function to replace NaN values with 1 in a data frame
replace_nan_with_1 <- function(df) {
  # Apply is.nan() to each column individually and replace NaN with 1
  df[] <- lapply(df, function(x) { x[is.nan(x)] <- 1; return(x) })
  return(df)
}

# Apply the function to each data frame in the list
data_list <- lapply(data_list, replace_nan_with_1)

# Calculate the average of each df to find an average whole brain connectivity value
averages <- lapply(data_list, function(df) mean(as.numeric(unlist(df)), na.rm = TRUE))

write.csv(averages, "whole_brain_connectivity.csv", row.names = FALSE)

# Transpose the data frame
wholebrain <- as.data.frame(t(averages))

# Store original column names in a new column called 'ID'
wholebrain$ID <- colnames(wholebrain)

# Reorder columns so ID comes first
wholebrain <- wholebrain[, c(ncol(wholebrain), 1:(ncol(wholebrain)-1))]

#Rename column V1
wholebrain <- wholebrain %>%
  rename(wholebrain_fc = V1)

# Load in CDA grouping information
membership <- read.csv("cda_results.csv")

#Join FC and CDA grouping information
wholebrain <- wholebrain %>%
  left_join(membership %>% select(names, membership), by = "ID")

write.csv(wholebrain, "wholebrain_membership.csv", row.names = FALSE)

# Fit a multinomial logistic regression model to predict membership from FC
# A multinomial regression was used because the CDA returned 4 groups
wholebrain$membership <- factor(wholebrain$membership, ordered = FALSE)
wholebrain$membership <- relevel(wholebrain$membership, ref = "1")

regression <- multinom(membership ~ wholebrain_fc, data = wholebrain)

# View the model summary
summary(regression)

