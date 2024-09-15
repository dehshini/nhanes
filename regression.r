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
tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design,
        family = quasibinomial()
    )
)



# logistic regression to test for trend by age

# age < 65, estimate 0.00508, p value = 0.817
tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(), 
        subset = RIDAGEYR < 65
    )
)

# age >= 65, estimate -0.07731, p value = 0.00166
tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(), 
        subset = RIDAGEYR >= 65
    )
)



# logistic regression to test for trend by sex
tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(), 
        subset = FEMALE == 1
    )
)
# female, estimate -0.03298, p value = 0.157

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(), 
        subset = FEMALE == 0
    )
)
# male, estimate -0.01649, p value = 0.517960



# logistic regression to test for trend by race

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = RACE == 1
    )
)
# white, estimate -0.0476, p value = 0.0586

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = RACE == 2
    )
)
# black, estimate -0.03679, p value = 0.128

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = RACE == 3
    )
)
# hispanic, estimate 0.03433, p value = 0.256

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = RACE == 4
    )
)
# other, estimate 0.00418, p value = 0.939



# logistic regression to test for trend by family income

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = INDFMPIR < 1 & !is.na(INDFMPIR)
    )
)
# below threshold, estimate 0.02931, p value = 0.413

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = INDFMPIR >= 1 & !is.na(INDFMPIR)
    )
)
# above threshold, estimate -0.03646, p value = 0.05691


# logistic regression to test for trend by insurance

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = insurance == 1 | insurance == 2
    )
)
# insured, estimate -0.03333, p value = 0.05463

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = insurance == 0
    )
)
# uninsured, estimate 0.049, p value = 0.281


# logistic regression to test for trend by education
tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = EDULEVEL == 1
    )
)
# less than high school, estimate -0.01069, p value = 0.6871

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = EDULEVEL == 2
    )
)
# high school, estimate -0.009274, p value = 0.769

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = EDULEVEL == 3
    )
)
# some college, estimate -0.01064, p value = 0.677


# logistic regression to test for trend by hba1c
tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = !is.na(HBA1C) & HBA1C < 7
    )
)
# hba1c, estimate -0.05161, p value = 0.0310

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = !is.na(HBA1C) & HBA1C >= 7 & HBA1C <= 9
    )
)
# hba1c, estimate -0.02803, p value = 0.291

tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design, family = quasibinomial(),
        subset = !is.na(HBA1C) & HBA1C > 9
    )
)
# hba1c, estimate 0.05453, p value = 0.186


###########################################################
# test of proportions
###########################################################

analyze_csv <- function(file_path) {
    # Read the CSV file
    df <- read.csv(file_path)

    # Extract the first and last rows
    first_row <- df[1, ]
    last_row <- df[nrow(df), ]

    # Calculate counts for prop.test
    # count1 <- round(first_row$Unweighted_Count * first_row$Proportion / 100)
    # count2 <- round(last_row$Unweighted_Count * last_row$Proportion / 100)

    # Perform the test of proportions
    prop_test <- prop.test(
        x = c(first_row$Proportion, last_row$Proportion),
        n = c(first_row$Unweighted_Count, last_row$Unweighted_Count),
        correct = FALSE
    )

    # Extract results
    test_results <- tidy(prop_test)

    # Create a results dataframe
    results <- data.frame(
        Variable = first_row[1, ncol(df)],
        First_Survey = first_row$X,
        Last_Survey = last_row$X,
        First_Proportion = first_row$Proportion,
        Last_Proportion = last_row$Proportion,
        Chi_square = test_results$statistic,
        P_Value = test_results$p.value
    )

    return(results)
}

print(analyze_csv("./out/lowsrh_female.csv"))
print(analyze_csv("./out/lowsrh_male.csv"))
print(analyze_csv("./out/lowsrh_gt65.csv"))
print(analyze_csv("./out/lowsrh_lt65.csv"))



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
tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2,
        family = quasibinomial
    )
)

# logistic regression to test for trend by age

# age < 65, estimate 0.00508, p value = 0.817
tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = RIDAGEYR < 65
    )
)

# age >= 65, estimate -0.07731, p value = 0.00166
tidy(
    svyglm(
        low_srh ~ cycle_num,
        design = trend_design2, family = quasibinomial(),
        subset = RIDAGEYR >= 65
    )
)
