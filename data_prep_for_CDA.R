###CDA
# This code was meant to run a community detection analysis (CDA) on a behavioral data set.
# It also preps this data set for the actual running of the CDA. 

#Load libraries
library(mice)
library(dplyr)
library(ggplot2)
library(reshape2)

#Load data
master <- read.csv("master_for_imputation.csv")


######################################
### IMPUTATION
#Find percent missing data from the dataset
percent_missing <- function(df) {
  # calculate the percentage of missing data for each column
  missing_percentages <- sapply(df, function(x) {
    sum(is.na(x)) / length(x) * 100  # % of missing values for each column
  })
  
  # convert the result to a dataframe
  missing_df <- data.frame(
    Variable = names(missing_percentages),  # column names
    Percent_Missing = missing_percentages   # % of missing values
  )
  
  return(missing_df)  # return the dataframe
}

missing_df = percent_missing(master)
write.csv(missing_df, "perc_missing_data_by_task.csv", row.names = FALSE)

#Find out percent missing data bu subject
percent_missing_by_id <- function(df, id_col = "ID") {
  # Ensure the ID column exists
  if (!(id_col %in% names(df))) {
    stop(paste("Column", id_col, "not found in the dataframe"))
  }
  
  # Remove the ID column from the data for missing calculation
  data_without_id <- df[ , !(names(df) %in% id_col)]
  
  # Calculate percent missing per row (excluding the ID column)
  missing_percentages <- apply(data_without_id, 1, function(row) {
    sum(is.na(row)) / length(row) * 100
  })
  
  # Create a dataframe with ID and percent missing
  missing_df <- data.frame(
    ID = df[[id_col]],
    Percent_Missing = missing_percentages
  )
  
  return(missing_df)
}

pwa_missing_df = percent_missing_by_id(master)
write.csv(pwa_missing_df, "perc_missing_data_by_pwa.csv", row.names = FALSE)

#Impute missing values
imputed <- mice(master,m=5,maxit=50,meth='pmm',seed=500)

imputed_data1 <- complete(imputed, action = 1)
imputed_data2 <- complete(imputed, action = 2)
imputed_data3 <- complete(imputed, action = 3)
imputed_data4 <- complete(imputed, action = 4)
imputed_data5 <- complete(imputed, action = 5)

# Export CSVs
write.csv(imputed_data1, "imputed_data1.csv", row.names = FALSE)
write.csv(imputed_data2, "imputed_data2.csv", row.names = FALSE)
write.csv(imputed_data3, "imputed_data3.csv", row.names = FALSE)
write.csv(imputed_data4, "imputed_data4.csv", row.names = FALSE)
write.csv(imputed_data5, "imputed_data5.csv", row.names = FALSE)

#get summary stats
summary(master)
summary(imputed_data1)
summary(imputed_data2)
summary(imputed_data3)
summary(imputed_data4)
summary(imputed_data5)


######################################
### POOL DATA
#Remove ID from Imputed data
imputed_data1$ID <- NULL
imputed_data2$ID <- NULL
imputed_data3$ID <- NULL
imputed_data4$ID <- NULL
imputed_data5$ID <- NULL

# Reduce the imputed datasets into one dataframe
dfs <- list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5)

# Calculate element wise average
dataframe_avg <- Reduce("+", dfs) / length(dfs)


######################################
### NORMALIZATION
dataframe <- as.data.frame(scale(dataframe_avg))

write.csv(dataframe , "imputed_pooled_normalized.csv", row.names = FALSE)

######################################
### CORRELATE
# Define tasks for correlation
correlation_tasks <- c("colorshape_congruent","colorshape_incongruent","digit_comp",
                       "fish_flanker_congruent","fish_flanker_incongruent",
                       "geometric_inclusion_included","geometric_inclusion_notincluded",
                       "geometric_matching_matching","geometric_matching_notmatching",
                       "lowlevel_complex1","lowlevel_complex2","lowlevel_complex3",
                       "lowlevel_complex4","lowlevel_pure1","lowlevel_pure2","lowlevel_pure3",
                       "lowlevel_pure4","MEC_emot_comprehension_sad",
                       "MEC_emot_comprehension_angry","MEC_emot_comprehension_happy",
                       "MEC_ling_comprehension_Question","MEC_ling_comprehension_Order",
                       "MEC_ling_comprehension_Statement","mental_calculation_multiplication",
                       "metasyntax_grammatical","metasyntax_ungrammatical","music_good",
                       "music_sour","PEPSC_comp_animal","PEPSC_comp_color","reading_numbers",
                       "rules_and_principles_principles","syntax_aud_active",
                       "syntax_aud_filler","syntax_aud_SE_clefts","syntax_aud_SRC",
                       "syntax_aud_passive","syntax_aud_OE_clefts","syntax_aud_ORC",
                       "syntax_vis_active","syntax_vis_filler","syntax_vis_SE_clefts",
                       "syntax_vis_SRC","syntax_vis_passive","syntax_vis_OE_clefts",
                       "syntax_vis_ORC","TOM_long_RUK__FILLER","TOM_long_RUK__MC",
                       "TOM_long_RUK__TB","TOM_long_RUK__FB","TOM_RK__FILLER",
                       "TOM_RK__TB_MC","TOM_RK__FB","writing_numbers",
                       "written_operations_multiplication","written_operations_subtraction",
                       "rcba_total_perc","bnt_total_perc","palpa_8_total_perc",
                       "palpa25_total_perc", "rbans_IMMEDIATEMEMORY_total",
                       "rbans_VISUOSPATIAL_total","rbans_LANGUAGE_total",
                       "rbans_ATTENTION_total","rbans_DELAYEDMEMORY_total",
                       "wms_visualreproduction_total","wms_logicalmemory_total",
                       "wms_verbalpaired_total","wms_symbol_span_total","symbol_cancel_perc",
                       "papt_picture_perc")

dataframe <- dataframe %>% select(correlation_tasks)

#Run correlations
dataframe_corr <- cor(dataframe[, correlation_tasks])
dataframe_corr <- as.data.frame(dataframe_corr)

write.csv(dataframe_corr, "corr_matrix.csv", row.names = TRUE)

# Reshape for visualization 
dataframe_corr_long <- melt(dataframe_corr,
                            varnames = c("colorshape_congruent","colorshape_incongruent","digit_comp",
                                         "fish_flanker_congruent","fish_flanker_incongruent",
                                         "geometric_inclusion_included","geometric_inclusion_notincluded",
                                         "geometric_matching_matching","geometric_matching_notmatching",
                                         "lowlevel_complex1","lowlevel_complex2","lowlevel_complex3",
                                         "lowlevel_complex4","lowlevel_pure1","lowlevel_pure2",
                                         "lowlevel_pure3","lowlevel_pure4","MEC_emot_comprehension_sad",
                                         "MEC_emot_comprehension_angry","MEC_emot_comprehension_happy",     
                                         "MEC_ling_comprehension_Question","MEC_ling_comprehension_Order",     
                                         "MEC_ling_comprehension_Statement","mental_calculation_multiplication",
                                         "metasyntax_grammatical","metasyntax_ungrammatical","music_good",
                                         "music_sour","PEPSC_comp_animal","PEPSC_comp_color","reading_numbers",
                                         "rules_and_principles_principles", "syntax_aud_active","syntax_aud_filler",                
                                         "syntax_aud_SE_clefts","syntax_aud_SRC","syntax_aud_passive","syntax_aud_OE_clefts",             "syntax_aud_ORC","syntax_vis_active",           
                                         "syntax_vis_filler","syntax_vis_SE_clefts","syntax_vis_SRC",
                                         "syntax_vis_passive","syntax_vis_OE_clefts","syntax_vis_ORC",          
                                         "TOM_long_RUK__FILLER","TOM_long_RUK__MC","TOM_long_RUK__TB",
                                         "TOM_long_RUK__FB","TOM_RK__FILLER","TOM_RK__TB_MC","TOM_RK__FB",
                                         "writing_numbers","written_operations_multiplication",
                                         "written_operations_subtraction","rcba_total_perc",
                                         "bnt_total_perc","palpa_8_total_perc","palpa25_total_perc",
                                         "rbans_IMMEDIATEMEMORY_total","rbans_VISUOSPATIAL_total",
                                         "rbans_LANGUAGE_total","rbans_ATTENTION_total",          
                                         "rbans_DELAYEDMEMORY_total","wms_visualreproduction_total",  
                                         "wms_logicalmemory_total","wms_verbalpaired_total",
                                         "wms_symbol_span_total","symbol_cancel_perc",            
                                         "papt_picture_perc"),value.name = "Correlation")

corr_df <- as.data.frame(dataframe_corr)
corr_df$X <- rownames(dataframe_corr)

names(corr_df) <- gsub(" \\.\\.\\. ", " - ", names(corr_df))

df_long <- melt(corr_df, id.vars = "X", variable.name = "Task2", value.name = "Correlation")

# Reorder Tasks
task_order <- corr_df$X
df_long$X <- factor(df_long$X, levels = task_order)
df_long$Task2 <- factor(df_long$Task2, levels = task_order)
df_long$Task2 <- df_long$X

# Remove self-correlates
df_long <- df_long[df_long$X != df_long$Task2, ]

# Remove duplicate pairs
df_long <- df_long[!duplicated(t(apply(df_long[, c("X", "Task2")], 1, sort))), ]

# Visualize Data
myplot <- ggplot(df_long, aes(x = X, y = Task2, fill = Correlation)) + geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
 scale_y_discrete(limits = rev(levels(df_long$Task2))) +  # Flip y-axis
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text.x = element_text(angle = 90, size = 4.5),
    axis.text.y = element_text(angle = 0, size = 5),
    panel.grid = element_blank()
  )

ggsave("matrix.pdf", plot = myplot, width = 9, height = 8, units = "in")


######################################
### THRESHOLD
# Function to return variable and row names with correlations >= threshold
tasks_above_threshold <- function(mat, threshold) {
  thresh_value <- threshold / 100
  results <- which(abs(mat) >= thresh_value, arr.ind = TRUE)
  
  # Create a data frame of matching variable and row names
  data.frame(
    Row = rownames(mat)[results[, 1]],
    Column = colnames(mat)[results[, 2]],
    Correlation = mat[results]
  )
}

# Apply threshold function and name the results
threshold <- c(70)

diag(dataframe_corr) <- NA

# Get variable names meeting each threshold
average_df_tasks <- lapply(threshold, function(t) tasks_above_threshold(dataframe_corr, t))
names(average_df_tasks) <- paste0(threshold, "%")

# Function to remove duplicate unordered pairs
remove_duplicate_pairs <- function(df) {
  # Create a consistent ordering for the pairs
  df$pair_id <- apply(df[c("Row", "Column")], 1, function(x) paste(sort(x), collapse = "_"))
  
  # Keep only the first occurrence of each pair
  df <- df[!duplicated(df$pair_id), ]
  
  return(df)
}

average_df_tasks <- lapply(average_df_tasks, remove_duplicate_pairs)

# Write to CSV
threshold_70 <- average_df_tasks[['70%']]
write.csv(threshold_70, "threshold_70.csv", row.names = FALSE)
