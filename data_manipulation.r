########################################################

# create an output directory
#dir.create("out", showWarnings = FALSE)

# DUPLICATE THE DATAFRAME LIST and work with that
nhanes_list2 <- nhanes_list

# bind all rows in nhanes list into one dataframe
#nhanes_all <- rbindlist(nhanes_list, fill = TRUE)

# manipulate the combined dataframe
# convert HSD010(SRH) 7 and 9 to NA
nhanes_all$HSD010 <- ifelse(
    nhanes_all$HSD010 %in% c(7, 9), NA, nhanes_all$HSD010
)

# manipulate individual dataframes in nhanes_list 2
# convert HSD010 7 and 9 to NA
# for (i in 1:length(nhanes_list2)) {
#     nhanes_list2[[i]]$HSD010 <- ifelse(
#         nhanes_list2[[i]]$HSD010 %in% c(7, 9), NA, nhanes_list2[[i]]$HSD010
#     )
# }

#################################################

# use nhanes_list2 to create analytic datasets
# duplicate before modifying
# nhanes_list3 <- nhanes_list2

# # exclude those whose age is less than 20
# for (i in 1:length(nhanes_list3)) {
#     nhanes_list3[[i]] <- nhanes_list3[[i]][!is.na(RIDAGEYR) & RIDAGEYR >= 20]
# }

# # # keep only those with self reported diabetes
# for (i in 1:length(nhanes_list3)) {
#     nhanes_list3[[i]] <- nhanes_list3[[i]][DIQ010 == 1]
# }


# # # keep only those with data on SRH
# for (i in 1:length(nhanes_list3)) {
#     nhanes_list3[[i]] <- nhanes_list3[[i]][!is.na(HSD010)]
# }

# combine the dataframes into one
# nhanes_analytic <- rbindlist(nhanes_list3, fill = TRUE)


# # exclude those whose age is less than 20
# nhanes_analytic <- subset(nhanes_all, !is.na(RIDAGEYR) & RIDAGEYR >= 20)
# nrow(nhanes_analytic)
# # # # keep only those with self reported diabetes
# nhanes_analytic <- subset(nhanes_analytic, DIQ010 == 1)
# nrow(nhanes_analytic)
# # # # keep only those with data on SRH
# nhanes_analytic <- subset(nhanes_analytic, !is.na(HSD010))
# nrow(nhanes_analytic)

# # keep only the needed variables
# nhanes_analytic_small <- nhanes_analytic[, c(
#     "SEQN",
#     "RIDAGEYR",
#     "DIQ010",
#     "HSD010",
#     "AGEGROUP",
#     "FEMALE",
#     "EDULEVEL",
#     "insurance",
#     "RACE",
#     "FAM_INCOME",
#     "BMXBMI",
#     "BMICAT",
#     "current_smoker",
#     "HBA1C",
#     "HBA1C_CAT",
#     "diabetes_duration",
#     "SBP",
#     "DBP",
#     "hypertension",
#     "LBXTC",
#     "hypercholesterolemia",
#     "CVD",
#     "CKD",
#     "low_srh",
#     "insurance"
# )]


# remove NAs from variables that have missing < 2%
# nhanes_analytic_small <- nhanes_analytic[, lapply(.SD, function(x) ifelse(sum(is.na(x)) < 0.02*nrow(nhanes_analytic), x, NA)), .SDcols = names(nhanes_analytic)]

