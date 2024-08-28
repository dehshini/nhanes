########################################################

# create an output directory
#dir.create("out", showWarnings = FALSE)

# DUPLICATE THE DATAFRAME LIST
nhanes_list2 <- nhanes_list

# bind all rows in nhanes list into one dataframe
nhanes_all <- rbindlist(nhanes_list, fill = TRUE)

# convert HSD010 7 and 9 to NA in the combined dataframe
nhanes_all$HSD010 <- ifelse(nhanes_all$HSD010 %in% c(7, 9), NA, nhanes_all$HSD010)

# convert HSD010 7 and 9 to NA for the individual dataframes
for (i in 1:length(nhanes_list2)) {
    nhanes_list2[[i]]$HSD010 <- ifelse(nhanes_list2[[i]]$HSD010 %in% c(7, 9), NA, nhanes_list2[[i]]$HSD010)
}

#################################################

# exclude those whose age is less than 20
# for (i in 1:length(nhanes_list2)) {
#     nhanes_list2[[i]] <- nhanes_list2[[i]][!is.na(RIDAGEYR) & RIDAGEYR >= 20]
# }

# # keep only those with self reported diabetes
# for (i in 1:length(nhanes_list2)) {
#     nhanes_list2[[i]] <- nhanes_list2[[i]][DIQ010 == 1]
# }


# # keep only those with data on SRH
# for (i in 1:length(nhanes_list2)) {
#     nhanes_list2[[i]] <- nhanes_list2[[i]][!is.na(HSD010)]
# }
