library(tidyverse)
library(dplyr)

#this code produces the data used in Figure 5 (with the base figure itself created in Excel, and then beautified in Affinity)
#read in master file
list <- read_csv("supplementary table 5.csv")

#separate file into pre-event and during event IDs
before<-dplyr::filter(list, during_event == "before")
during<-dplyr::filter(list, during_event == "yes")

#subset to just the reason columns and get counts of each column
list2<-dplyr::select(list, 1,3,7,8,12,13,14,15,16,17,20)

#remove original values of before event records. This is because, for records that had already been identified by that taxon’s expert assessor before the expert ID 
#Blitz period, we asked each assessor to review their identifications; if satisfied they were still correct, these records were scored as ‘species ID correct’. 
list3<-list2
rows_to_modify <- 1:2287
cols_to_modify <- c(5,6,7,8,9,10)
list3[rows_to_modify, cols_to_modify] <- NA

# Calculate sums where 'expert_id_rank' = 'coarser than species'
sums_coarser <- colSums(list3[list3$expert_id_rank == 'coarser than species', -1] == 1, na.rm = TRUE)

# Calculate sums where 'expert_id_rank' = 'species or finer'
sums_species <- colSums(list3[list3$expert_id_rank == 'species or finer', -1] == 1, na.rm = TRUE)

# Create dataframes for each set of sums
sums_coarser_df <- data.frame(column_names = names(sums_coarser), count_of_ones = as.vector(sums_coarser))
sums_species_df <- data.frame(column_names = names(sums_species), count_of_ones = as.vector(sums_species))

#here is a summary of how the categories across those two DFs get collapsed into the 5 categories in Figure 5:
# 'Species ID correct' in figure = correct_during_event + confirmed_id + own_observation_correct from sums_species_df
# 'Refined to species' in figure = refined_id from sums_species_df
# 'Refined, still coarse' in figure = refined_id from sums_coarser_df
# 'Coarse ID correct' in figure = confirmed_id + correct_during event from sums_coarser_df
# 'Misidentified' in figure = corrected_id from sums_species_df and corrected_id + coarsened + wrong_taxon from sums_coarser_df


#the above code was for all taxa combined. Now we'll create a function to run the same code for each of the 15 different taxa
calculate_sums <- function(data, taxon_value) {
  subset_data <- data[data$taxon == taxon_value, ]
  
  sums_coarser <- colSums(subset_data[subset_data$expert_id_rank == 'coarser than species', -1] == 1, na.rm = TRUE)
  sums_species <- colSums(subset_data[subset_data$expert_id_rank == 'species or finer', -1] == 1, na.rm = TRUE)
  
  sums_coarser_df <- data.frame(column_names = names(sums_coarser), count_of_ones = as.vector(sums_coarser))
  sums_species_df <- data.frame(column_names = names(sums_species), count_of_ones = as.vector(sums_species))
  
  return(list(sums_coarser_df = sums_coarser_df, sums_species_df = sums_species_df))
}


unique_taxon_values <- unique(list3$taxon)

results <- lapply(unique_taxon_values, function(taxon_value) {
  calculate_sums(list3, taxon_value)
})

#print results in console
for (i in seq_along(unique_taxon_values)) {
  taxon_value <- unique_taxon_values[i]
  cat("Taxon:", taxon_value, "\n")
  
  cat("Sums for 'coarser than species':\n")
  print(results[[i]]$sums_coarser_df)
  
  cat("Sums for 'species or finer':\n")
  print(results[[i]]$sums_species_df)
  
  cat("\n")
}


