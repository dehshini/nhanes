# create cycle number variable
nhanes_all$cycle_num <- as.numeric(as.factor(nhanes_all$SDDSRVYR))

table(nhanes_all$cycle_num)

# create survey design object
trend_design <- svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTMEC9YR,
    nest = TRUE,
    data = nhanes_all
)

# create subset of interest
trend_design <- subset(
    trend_design,
    RIDAGEYR >= 20 & DIQ010 == 1 & !is.na(HSD010) &
        !is.na(hypercholesterolemia) &
        !is.na(hypertension) &
        !is.na(diabetes_duration) &
        !is.na(CVD) &
        !is.na(insurance) &
        !is.na(EDULEVEL)
)

# Logistic regression to test for trend
p_trend <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design,
        family = quasibinomial()
    )
)



# logistic regression to test for trend by age

# age < 65, estimate 0.00508, p value = 0.817
p_lt65 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(), 
        subset = RIDAGEYR < 65
    )
)

# age >= 65, estimate -0.07731, p value = 0.00166
p_gt65 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(), 
        subset = RIDAGEYR >= 65
    )
)



# logistic regression to test for trend by sex
p_female <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(), 
        subset = FEMALE == 1
    )
)
# female, estimate -0.03298, p value = 0.157

p_male <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(), 
        subset = FEMALE == 0
    )
)
# male, estimate -0.01649, p value = 0.517960



# logistic regression to test for trend by race

p_white <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = RACE == 1
    )
)
# white, estimate -0.0476, p value = 0.0586

p_black <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = RACE == 2
    )
)
# black, estimate -0.03679, p value = 0.128

p_hispanic <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = RACE == 3
    )
)
# hispanic, estimate 0.03433, p value = 0.256

p_other <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = RACE == 4
    )
)
# other, estimate 0.00418, p value = 0.939



# logistic regression to test for trend by family income

p_poor <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = !is.na(INDFMPIR) & INDFMPIR < 1
    )
)
# below threshold, estimate 0.02931, p value = 0.413

p_nonpoor <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = !is.na(INDFMPIR) & INDFMPIR >= 1
    )
)
# above threshold, estimate -0.03646, p value = 0.05691


# logistic regression to test for trend by insurance

p_insured <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = insurance == 1 | insurance == 2
    )
)
# insured, estimate -0.03333, p value = 0.05463

p_uninsured <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = insurance == 0
    )
)
# uninsured, estimate 0.049, p value = 0.281



# logistic regression to test for trend by education
p_lt_highschool <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = EDULEVEL == 1
    )
)
# less than high school, estimate -0.01069, p value = 0.6871

p_highschool <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = EDULEVEL == 2
    )
)
# high school, estimate -0.009274, p value = 0.769

p_college <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = EDULEVEL == 3
    )
)
# some college, estimate -0.01064, p value = 0.677


# logistic regression to test for trend by hba1c
p_lt7 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = !is.na(HBA1C) & HBA1C < 7
    )
)
# hba1c, estimate -0.05161, p value = 0.0310

p_7to9 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = !is.na(HBA1C) & HBA1C >= 7 & HBA1C <= 9
    )
)
# hba1c, estimate -0.02803, p value = 0.291

p_gt9 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = !is.na(HBA1C) & HBA1C > 9
    )
)
# hba1c, estimate 0.05453, p value = 0.186



############################################################
# 4 year cycles regression
############################################################

nhanes_all2 <- rbindlist(new_nhanes_list, use.names = TRUE, fill = TRUE)
nhanes_all2$cycle_num <- as.numeric(as.factor(nhanes_all2$SDDSRVYR))
table(nhanes_all2$cycle_num, useNA = "ifany")

# create survey design object
trend_design2 <- svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTMEC4YR,
    nest = TRUE,
    data = nhanes_all2
)

# create subset of interest
trend_design2 <- subset(
    trend_design2,
    RIDAGEYR >= 20 & DIQ010 == 1 & !is.na(HSD010) &
        !is.na(hypercholesterolemia) &
        !is.na(hypertension) &
        !is.na(diabetes_duration) &
        !is.na(CVD) &
        !is.na(insurance) &
        !is.na(EDULEVEL)
)

# logistic regression to test for trend
p_trend.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2,
        family = quasibinomial
    )
)

# logistic regression to test for trend by age

# age < 65, estimate 0.00508, p value = 0.817
p_lt65.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = RIDAGEYR < 65
    )
)

# age >= 65, estimate -0.07731, p value = 0.00166
p_gt65.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = RIDAGEYR >= 65
    )
)


# logistic regression to test for trend by sex
p_male.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = FEMALE == 0
    )
)

# female, estimate 0.00508, p value = 0.817
p_female.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = FEMALE == 1
    )
)
# male, estimate -0.01649, p value = 0.517960

# logistic regression to test for trend by race

p_white.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = RACE == 1
    )
)
# white, estimate -0.0476, p value = 0.0586
p_black.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = RACE == 2
    )
)
# black, estimate -0.01649, p value = 0.517960
p_hispanic.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = RACE == 3
    )
)
# hispanic, estimate -0.01649, p value = 0.517960

p_trend_other.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = RACE == 4
    )
)
# other, estimate -0.01649, p value = 0.517960


# logistic regression to test for trend by income

p_poor.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = !is.na(INDFMPIR) & INDFMPIR < 1
    )
)

p_nonpoor.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = !is.na(INDFMPIR) & INDFMPIR >= 1
    )
)


# logistic regression to test for trend by insurance
p_insured.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = insurance == 1 | insurance == 2
    )
)

p_uninsured.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = insurance == 0
    )
)

# logistic regression to test for trend by education
p_lt_highschool.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = EDULEVEL == 1
    )
)
p_highschool.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = EDULEVEL == 2
    )
)
p_college.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = EDULEVEL == 3
    )
)

# logistic regression to test for trend by CKD
# p_ckd.2 <- tidy(
#     svyglm(
#         low_srh ~ cycle_num,
#         design = trend_design2, family = quasibinomial(),
#         subset = CKD == 1
#     )
# )
# p_nckd.2 <- tidy(
#     svyglm(
#         low_srh ~ cycle_num,
#         design = trend_design2, family = quasibinomial(),
#         subset = CKD == 0
#     )
# )

# logistic regression to test for trend by hba1c
p_lt7.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = !is.na(HBA1C) & HBA1C < 7
    )
)
p_7to9.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = !is.na(HBA1C) & HBA1C >= 7 & HBA1C <= 9
    )
)
p_gt9.2 <- tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = !is.na(HBA1C) & HBA1C > 9
    )
)



###########################################################
# test of proportions
###########################################################

analyze_csv <- function(file_path) {
    # Read the CSV file
    df <- read.csv(file_path)
    
    # Extract the first and last rows
    first_row <- df[1, ]
    last_row <- df[nrow(df), ]
        
    # Perform the test of proportions
    prop_test <- prop.test(
        x = c(first_row$Proportion, last_row$Proportion),
        n = c(first_row$Weighted_Count.low_srh, last_row$Weighted_Count.low_srh),
        correct = FALSE
    )
    
    # Extract results
    test_results <- tidy(prop_test)
    
    # Create a results dataframe
    results <- data.frame(
        Variable = first_row[1, ncol(df)],
        First_Survey = first_row$X,
        First_Proportion = first_row$Proportion,
        Last_Survey = last_row$X,
        Last_Proportion = last_row$Proportion,
        #Chi_square = test_results$statistic,
        P_Value = test_results$p.value
    )
    
    return(results)
}

# Function to analyze proportions using survey design
analyze_csv2 <- function(df, weight, cycle, proportion) {
    # Ensure the variables are in the correct format
    df[[weight_var]] <- as.numeric(df[[weight_var]])
    df[[cycle]] <- as.factor(df[[cycle_var]])
    df[[proportion_var]] <- as.numeric(df[[proportion_var]])

    # Define the survey design
    svy_design <- svydesign(
        id = ~1, # assuming no clusters, use the appropriate clustering variable if you have one
        weights = df[[weight_var]],
        data = df
    )

    # Subset the design for the first and last cycle
    first_cycle <- levels(df[[cycle_var]])[1]
    last_cycle <- levels(df[[cycle_var]])[length(levels(df[[cycle_var]]))]

    # Create subsets for the first and last cycles
    svy_first <- subset(svy_design, df[[cycle_var]] == first_cycle)
    svy_last <- subset(svy_design, df[[cycle_var]] == last_cycle)

    # Estimate the proportion for the first cycle
    prop_first <- svymean(~ I(df[[proportion_var]]), svy_first)

    # Estimate the proportion for the last cycle
    prop_last <- svymean(~ I(df[[proportion_var]]), svy_last)

    # Combine the two cycles into one design for comparison
    combined_design <- svydesign(
        id = ~1,
        weights = df[[weight_var]],
        strata = df[[cycle_var]], # stratify by cycle
        data = df
    )

    # Perform a Wald test to compare the two proportions
    wald_test <- svychisq(~ df[[cycle_var]] + I(df[[proportion_var]]), combined_design)

    # Output results
    list(
        first_cycle_prop = prop_first,
        last_cycle_prop = prop_last,
        test = wald_test
    )
}





# sex
print(analyze_csv2("./out/lowsrh_female.csv"))
print(analyze_csv2("./out/lowsrh_male.csv"))
# age
print(analyze_csv("./out/lowsrh_gt65.csv"))
print(analyze_csv("./out/lowsrh_lt65.csv"))
# race
print(analyze_csv("./out/lowsrh_white.csv"))
print(analyze_csv("./out/lowsrh_black.csv"))
print(analyze_csv("./out/lowsrh_hispanic.csv"))
# insurance
print(analyze_csv("./out/lowsrh_insured.csv"))
print(analyze_csv("./out/lowsrh_noninsured.csv"))
# education
print(analyze_csv("./out/lowsrh_belowhighschool.csv"))
print(analyze_csv("./out/lowsrh_highschool.csv"))
print(analyze_csv("./out/lowsrh_abovehighschool.csv"))
# CKD
# print(analyze_csv("./out/lowsrh_ckd.csv"))
# print(analyze_csv("./out/lowsrh_nonckd.csv"))
# hba1c
print(analyze_csv("./out/lowsrh_lt7.csv"))
print(analyze_csv("./out/lowsrh_7to9.csv"))
print(analyze_csv("./out/lowsrh_gt9.csv"))
