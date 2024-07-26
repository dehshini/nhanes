# load all data
setwd("/Users/dehshini/code/R/nhanes")

library(tidyverse)
library(haven)
library(data.table)

# Function to load data files from a directory
load_data_files <- function(directory) {
    # List all files in the directory
    files <- list.files(directory, full.names = TRUE)

    # Load each file into a list
    data_list <- lapply(files, read_xpt)

    # Set names of the list elements to the filenames (without extension)
    names(data_list) <- tools::file_path_sans_ext(basename(files))

    # Join all dataframes by common columns (using full_join to preserve all rows and columns)
    joined_data <- Reduce(function(x, y) full_join(x, y, by = "SEQN"), data_list)

    # convert the joined data to a data table
    setDT(joined_data)

    # return the joined data
    return(joined_data)
}

# Main directory containing NHANES cycle folders
main_directory <- "/Users/dehshini/code/R/nhanes/data"

# List all cycle folders
cycle_folders <- list.dirs(main_directory, full.names = TRUE, recursive = FALSE)

# Iterate through each cycle folder
for (cycle_folder in cycle_folders) {

    # Extract the cycle name from the folder name
    cycle_name <- basename(cycle_folder)

    # Load data files in the current cycle folder
    cycle_data <- load_data_files(cycle_folder)

    # Save the cycle data to an object
    assign(paste0("nhanes", cycle_name), cycle_data)

}

# Save all data frames to a single RData file
#save(list = ls(), file = "nhanes_data.RData")


# create a list to hold the nhanes dataframes
nhanes_list <- list(nhanes01_02, nhanes03_04, nhanes05_06, nhanes07_08, nhanes09_10, nhanes11_12, nhanes13_14, nhanes15_16, nhanes17_18)


# create new MEC weights for each cycle, total 9 cycles

for (cycle in nhanes_list) {
    # create new weights, 9 cycles
    cycle[, WTMEC9YR := WTMEC2YR/9]
}


