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


# Load required package
library(tidyverse)

# Function to test two weighted proportions from a CSV file
test_weighted_proportions <- function(file_path, row1, row2) {
    # Read the CSV file
    df <- read.csv(file_path)

    # Check if the row numbers are valid
    if (row1 > nrow(df) || row2 > nrow(df)) {
        stop("Row numbers exceed the number of rows in the data.")
    }

    # Extract the first and second row for comparison
    first_row <- df[row1, ]
    second_row <- df[row2, ]

    # Extract proportions and weighted sample sizes
    x <- c(first_row$Proportion/100, second_row$Proportion/100)
    n <- c(first_row$Unweighted_Count, second_row$Unweighted_Count)

    # Check that proportions are less than their respective sample sizes
    if (any(x > n)) {
        stop("Proportions must be less than or equal to sample sizes.")
    }

    # Perform the Chi-square test using actual sample sizes
    prop_test <- prop.test(x = round(x * n), n = n, correct = FALSE)

    # Prepare results as a data frame
    results <- data.frame(
        Row_Comparison = paste(first_row[1], "vs", second_row[1]),
        First_Proportion = first_row$Proportion,
        #First_Weighted_Count = first_row$Weighted_Count,
        Second_Proportion = second_row$Proportion,
        #Second_Weighted_Count = second_row$Weighted_Count,
        #Chi_Square_Stat = prop_test$statistic,
        P_Value = prop_test$p.value
    )

    return(results)
}

# sex
test_weighted_proportions("./out/lowsrh_female.csv", 1, 9)
test_weighted_proportions("./out/lowsrh_male.csv", 1, 9)

# age
test_weighted_proportions("./out/lowsrh_gt65.csv", 1, 9)
test_weighted_proportions("./out/lowsrh_lt65.csv", 1, 9)

# race
test_weighted_proportions("./out/lowsrh_white.csv", 1, 9)
test_weighted_proportions("./out/lowsrh_black.csv", 1, 9)
test_weighted_proportions("./out/lowsrh_hispanic.csv", 1, 9) 


## 4 year cycles

# sex
test_weighted_proportions("./out/lowsrh_female_2.csv", 1, 5)
test_weighted_proportions("./out/lowsrh_male_2.csv", 1, 5)

# age
test_weighted_proportions("./out/lowsrh_gt65_2.csv", 1, 5)
test_weighted_proportions("./out/lowsrh_lt65_2.csv", 1, 5)

# race
test_weighted_proportions("./out/lowsrh_white_2.csv", 1, 5)
test_weighted_proportions("./out/lowsrh_black_2.csv", 1, 5)
test_weighted_proportions("./out/lowsrh_hispanic_2.csv", 1, 5)

# income
test_weighted_proportions("./out/lowsrh_poverty_2.csv", 1, 5)
test_weighted_proportions("./out/lowsrh_no_poverty_2.csv", 1, 5)

# insurance
test_weighted_proportions("./out/lowsrh_insured_2.csv", 1, 5)
test_weighted_proportions("./out/lowsrh_uninsured_2.csv", 1, 5)

# education
test_weighted_proportions("./out/lowsrh_highschool_2.csv", 1, 5)
test_weighted_proportions("./out/lowsrh_college_2.csv", 1, 5)
test_weighted_proportions("./out/lowsrh_belowhighschool_2.csv", 1, 5)
