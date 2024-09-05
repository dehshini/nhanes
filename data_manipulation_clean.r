# create an output directory
dir.create("out", showWarnings = FALSE)


# manipulate the combined dataframe
# convert HSD010(SRH) 7 and 9 to NA
nhanes_all$HSD010 <- ifelse(
    nhanes_all$HSD010 %in% c(7, 9), NA, nhanes_all$HSD010
)

# convert HSD010 7 and 9 to NA for each dataframe in list
for (i in 1:length(nhanes_list)) {
    nhanes_list[[i]]$HSD010 <- ifelse(
        nhanes_list[[i]]$HSD010 %in% c(7, 9), NA,
        nhanes_list[[i]]$HSD010
    )
}

# # exclude those whose age is less than 20
nhanes_analytic <- subset(nhanes_all, !is.na(RIDAGEYR) & RIDAGEYR >= 20)
nrow(nhanes_analytic)
# # # keep only those with self reported diabetes
nhanes_analytic <- subset(nhanes_analytic, DIQ010 == 1)
nrow(nhanes_analytic)
# # # keep only those with data on SRH
nhanes_analytic <- subset(nhanes_analytic, !is.na(HSD010))
nrow(nhanes_analytic)


# keep only the needed variables
nhanes_analytic_small <- nhanes_analytic[, c(
    "SEQN",
    "RIDAGEYR",
    "DIQ010",
    "HSD010",
    "AGEGROUP",
    "FEMALE",
    "EDULEVEL",
    "RACE",
    "FAM_INCOME",
    "BMICAT",
    "current_smoker",
    "HBA1C_CAT",
    "diabetes_duration",
    "hypertension",
    "hypercholesterolemia",
    "CVD",
    "CKD",
    "low_srh",
    "insurance"
)]

# missing proportions
plot_missing(nhanes_analytic_small)
missing_data <- profile_missing(nhanes_analytic_small)
write.csv(missing_data, file = "./out/missing_data_before.csv")


# exclude those who have missing data on hypercholesterolemia
nhanes_analytic <- subset(nhanes_analytic, !is.na(hypercholesterolemia))
nrow(nhanes_analytic)
# exclude those who have missing data on hypertension
nhanes_analytic <- subset(nhanes_analytic, !is.na(hypertension))
nrow(nhanes_analytic)
# exclude those who have missing data on diabetes duration
nhanes_analytic <- subset(nhanes_analytic, !is.na(diabetes_duration))
nrow(nhanes_analytic)
# exclude those who have missing data on CVD
nhanes_analytic <- subset(nhanes_analytic, !is.na(CVD))
nrow(nhanes_analytic)
# exclude those who have missing data on insurance
nhanes_analytic <- subset(nhanes_analytic, !is.na(insurance))
nrow(nhanes_analytic)
# exclude those who have missing data on education
nhanes_analytic <- subset(nhanes_analytic, !is.na(EDULEVEL))
nrow(nhanes_analytic)


# rerun nhanes analytic small.
nhanes_analytic_small <- nhanes_analytic[, c(
    "SEQN",
    "RIDAGEYR",
    "DIQ010",
    "HSD010",
    "AGEGROUP",
    "FEMALE",
    "EDULEVEL",
    "RACE",
    "FAM_INCOME",
    "BMICAT",
    "current_smoker",
    "HBA1C_CAT",
    "diabetes_duration",
    "hypertension",
    "hypercholesterolemia",
    "CVD",
    "CKD",
    "low_srh",
    "insurance"
)]

summary(nhanes_analytic_small)
# total n
nrow(nhanes_analytic_small)
# number of SRH, 0=high, 1=low
table(nhanes_analytic_small$low_srh, useNA = "ifany")

# check missing proportions now
plot_missing(nhanes_analytic_small)
missing_data <- profile_missing(nhanes_analytic_small)
write.csv(missing_data, file = "./out/missing_data_after.csv")

