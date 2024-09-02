# create an output directory
# dir.create("out", showWarnings = FALSE)

# DUPLICATE THE DATAFRAME LIST and work with that
nhanes_list2 <- nhanes_list

# manipulate the combined dataframe
# convert HSD010(SRH) 7 and 9 to NA
nhanes_all$HSD010 <- ifelse(
    nhanes_all$HSD010 %in% c(7, 9), NA, nhanes_all$HSD010
)

# # exclude those whose age is less than 20
nhanes_analytic <- subset(nhanes_all, !is.na(RIDAGEYR) & RIDAGEYR >= 20)
nrow(nhanes_analytic)
# # # keep only those with self reported diabetes
nhanes_analytic <- subset(nhanes_analytic, DIQ010 == 1)
nrow(nhanes_analytic)
# # # keep only those with data on SRH
nhanes_analytic <- subset(nhanes_analytic, !is.na(HSD010))
nrow(nhanes_analytic)

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
    "BMXBMI",
    "BMICAT",
    "current_smoker",
    "HBA1C",
    "HBA1C_CAT",
    "diabetes_duration",
    "SBP",
    "DBP",
    "hypertension",
    "LBXTC",
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

# missing proportions
plot_missing(nhanes_analytic_small)
profile_missing(nhanes_analytic_small)

# remove NAs from variables that have missing < 2%

# for (var in names(nhanes_analytic_small)) {
#     # Calculate the percentage of NA values for the current variable
#     na_percentage <- nhanes_analytic_small[, mean(is.na(get(var)))]
#     # If the percentage of NAs is less than 2%, remove rows with NA
#     if (na_percentage < 0.02) {
#         nhanes_analytic_small <- nhanes_analytic_small[!is.na(get(var))]
#     }
# }
