library(data.table)
library(survey)
library(gtsummary)

########################################################

# WEIGHTED CHARACTERISTICS OF PARTICIPANTS

# DUPLICATE THE DATAFRAME LIST
nhanes_list2 <- nhanes_list

# create a table of weighted baseline characteristics

# bind all rows in nhanes list into one dataframe
nhanes_all <- rbindlist(nhanes_list2, fill = TRUE)


head(nhanes_all$WTMEC9YR)

tail(nhanes_all$WTMEC9YR)


# categorize some variables

# Define the breakpoints and labels for BMI categories
bmi_breaks <- c(-Inf, 18.5, 24.9, 29.9, Inf) # Breakpoints for categories
bmi_labels <- c("Underweight", "Normal", "Overweight", "Obese") # Corresponding labels

# Categorize BMI
nhanes_all[, BMICAT := cut(BMXBMI, breaks = bmi_breaks, labels = bmi_labels, right = FALSE)]

# calculate SBP and DBP 
nhanes_all[, DBP := (BPXDI2 + BPXDI3) / 2]
nhanes_all[, SBP := (BPXSY2 + BPXSY3) / 2]

# marital status
# 1=married/livingwithpartner, 2=widowed/divorced/separated, 3=nevermarried

nhanes_all[, MARITAL := ifelse(DMDMARTL %in% c(1, 6), 1, 
        ifelse(DMDMARTL %in% c(2, 3, 4), 2, 
        ifelse(DMDMARTL == 5, 3, NA)))
]


# education level
# 1=less than high school, 2=high school/equivalent, 3=above high school/equivalent 
nhanes_all[, EDULEVEL := ifelse(DMDEDUC2 %in% c(1, 2), 1,
        ifelse(DMDEDUC2 == 3, 2, 
        ifelse(DMDEDUC2 %in% c(4, 5), 3, NA)))
]

#age >= 60
nhanes_all[, AGE60 := ifelse(RIDAGEYR >= 60, 1, 0)]

# race
# 1=white, 2=black, 3=mexican american, 4=other
nhanes_all[, RACE := ifelse(RIDRETH1 == 3, 1,
        ifelse(RIDRETH1 == 4, 2,
        ifelse(RIDRETH1 == 1, 3,
        ifelse(RIDRETH1 %in% c(2, 5), 4, NA))))
]

# smoke
nhanes_all[, SMOKE100 := ifelse(SMQ020 == 1, 1,
        ifelse(SMQ020 == 2, 0, NA))
]

nhanes_all[, SMOKENOW := ifelse(SMQ040 %in% c(1, 2), 1, 0)]

# duration of diabetes
# get current age and subtract age of diabetes diagnosis
nhanes_all[, DIAB_DUR := RIDAGEYR - DID040]

# retinopathy
nhanes_all[, RETINOPATHY := ifelse(DIQ080 == 1, 1, 0)]

# CVD = stroke MCQ160f, heart attack MCQ160e, angina MCQ160d, coronary heart disease MCQ160c
nhanes_all[, CVD := ifelse(MCQ160F == 1 | MCQ160E == 1 | MCQ160D == 1 | MCQ160C == 1, 1, 0)]


# CKD


# income poverty ratio
# 
PIR_breaks <- c(-Inf, 0.99, 2.99, Inf)
PIR_labels <- c("< 1", "1 to <3", "3 or more")
nhanes_all[, PIR := cut(INDFMPIR, breaks = PIR_breaks, labels = PIR_labels, right = FALSE)]



# create survey design object for all data

weighted_nhanes_all <- svydesign(
    id = ~SDMVPSU, 
    strata = ~SDMVSTRA, 
    weights = ~WTMEC9YR, 
    nest = TRUE,
    data = nhanes_all
)

# get the subset of interest: adults aged 20 and over with self-reported diabetes

analyze1 <- subset(weighted_nhanes_all, RIDAGEYR >= 20 & DIQ010 == 1)

# variables_needed <- c(
#     "RIDAGEYR", "RIAGENDR", "RIDRETH1", "DMDEDUC2",
#     "DMDMARTL", "SMQ020",
#     # labs
#     "LBDTCSI", "LBDTRSI", "LBDLDLSI", 
#     #body measures
#     "BMXBMI", "BPXOSY2", "BPXOSY3", "BPXODI2", "BPXODI3", "LBXGH"
# )



# convert categorical variables to factors
#cols to be converted to factors
# cols_to_factor <- c(
#     "RIDAGEYR", "RIAGENDR", "RIDRETH1", "DMDEDUC2",
#     "DMDMARTL", "SMQ020",
#     # labs
#     "LBDTCSI", "LBDTRSI", "LBDLDLSI", 
#     #body measures
#     "BMXBMI", "BPXOSY2"
# )



# create table label names
table1labels <- list(
    RIDAGEYR ~ "Age",
    AGE60 ~ "Age >= 60",
    RIAGENDR ~ "Gender",
    #RIDRETH1 ~ "Race",
    #DMDEDUC2 ~ "Education",
    #DMDMARTL ~ "Marital Status",
    #SMQ020 ~ "Smoked > 100 cigarettetes in life",
    LBDTCSI ~ "Total Cholesterol",
    LBDTRSI ~ "Triglycerides",
    LBDLDLSI ~ "Low Density Lipoprotein Cholesterol",
    BMXBMI ~ "Body Mass Index", 
    BMICAT ~ "Body Mass Index Category",
    MARITAL ~ "Marital Status",
    EDULEVEL ~ "Education Level",
    RACE ~ "Race", 
    SBP ~ "Systolic Blood Pressure",
    DBP ~ "Diastolic Blood Pressure",
    LBXGH ~ "Glycated Hemoglobin",
    #LBXGLUSI ~ "Two hour Glucose (OGTT)"
    #LBDGLUSI ~ "fasting glucose",
    SMOKE ~ "Smoked > 100 cigarettetes in life", 
    DIAB_DUR ~ "Duration of Diabetes",
    RETINOPATHY ~ "Retinopathy",
    CVD ~ "CVD", 
    PIR ~ "Poverty Income Ratio"
)



## create table
tbl_svysummary(
    analyze1,
    include = c(RIDAGEYR, AGE60, RIAGENDR, RACE, MARITAL, EDULEVEL,
                SMOKE, LBDTCSI, LBDTRSI, LBDLDLSI, BMXBMI, BMICAT,
                SBP, DBP, LBXGH, SMOKE, DIAB_DUR, RETINOPATHY, CVD, PIR
            ),
    statistic = list(
        all_continuous() ~ "{mean} ({mean.std.error})",
        all_categorical() ~ "{n_unweighted} ({p}%)"
    ),
    digits = list(
        all_continuous() ~ c(2, 2),
        all_categorical() ~ c(0, 1)
    ),
    missing = "no",
    #sort = list(everything() ~ "frequency"),
    label = table1labels
)  %>% 
    modify_header(
        list()
    ) %>% 
    add_ci()


#######################################################



# create a new column for each dataframe in the list called high_srh.
# if the SRH is less than or equal to 3, set high_srh to 1, otherwise 0

for (i in 1:length(nhanes_list2)) {
    nhanes_list2[[i]]$high_srh <- ifelse(nhanes_list2[[i]]$HSD010 <= 3, 1, 0)
}


############ SRH analysis by age, unweighted ##############

#create function to filter age and SRH
# filter out rows with missing age and SRH, keep age >= 18

filter_age_srh <- function(data) {
    # Filter rows to keep only those with non-missing age and SRH

    filtered_data <- data[!is.na(RIDAGEYR) & !is.na(HSD010)]

    # filter to keep only those with age >= 18

    filtered_data <- filtered_data[RIDAGEYR >= 18]

    return(filtered_data)
}


# use the function on the nhanes list
nhanes_list_by_age <- lapply(nhanes_list, filter_age_srh)

# rename the list, adding "a" to the name

names(nhanes_list_by_age) <- c("nhanes01_02a", "nhanes03_04a", "nhanes05_06a", "nhanes07_08a", "nhanes09_10a", "nhanes11_12a", "nhanes13_14a", "nhanes15_16a", "nhanes17_18a")


# create a new column for each dataframe in the list called high_srh.
# if the SRH is less than or equal to 3, set high_srh to 1, otherwise 0

for (i in 1:length(nhanes_list_by_age)) {
    nhanes_list_by_age[[i]]$high_srh <- ifelse(nhanes_list_by_age[[i]]$HSD010 <= 3, 1, 0)
}

# for each dataframe in the list, calculate the proportion of high SRH

# Function to calculate the proportion of 'yes' (1) for high_srh
calculate_prop_highsrh <- function(dt) {
    return(dt[, mean(high_srh, na.rm = TRUE)])
}

# apply the function to each dataframe in the list
proportion_highsrh <- lapply(nhanes_list_by_age, calculate_prop_highsrh)

print(proportion_highsrh)

# these proportions are for all adults >= 18 years for each cycle. 
# note: this is not weighted. 

# lets do a weighted analysis by age and SRH
# create survey design object for each dataframe



########### SRH analysis by age, weighted ##############
library(survey)

# create a new column for each dataframe in the list called high_srh.
# if the SRH is less than or equal to 3, set high_srh to 1, otherwise 0

for (i in 1:length(nhanes_list)) {
    nhanes_list[[i]]$high_srh <- ifelse(nhanes_list[[i]]$HSD010 <= 3, 1, 0)
}


#function to create survey design object

create_survey_design <- function(data) {
    survey_design <- svydesign(
        id = ~SDMVPSU, 
        strata = ~SDMVSTRA, 
        weights = ~WTINT2YR, 
        nest = TRUE,
        data = data
    )
    return(survey_design)
}

# apply the function to each dataframe in the list

weighted_nhanes_list <- lapply(nhanes_list, create_survey_design)

# rename the list, adding "w" to the name

names(weighted_nhanes_list) <- c("nhanes01_02w", "nhanes03_04w", "nhanes05_06w", "nhanes07_08w", "nhanes09_10w", "nhanes11_12w", "nhanes13_14w", "nhanes15_16w", "nhanes17_18w")


# subset the dataframes to only include rows with non-missing age, age >= 18 and SRH

for (i in 1:length(weighted_nhanes_list)) {
    weighted_nhanes_list[[i]] <- subset(weighted_nhanes_list[[i]], !is.na(RIDAGEYR) & !is.na(HSD010) & RIDAGEYR >= 18)
}

# calculate the proportion of 'yes' (1) for high_srh
# Function to calculate the weighted proportion of 'yes' (1) for high_srh

calculate_svy_prop <- function(design, x) {
    ## put the variable of interest in a formula
    form <- as.formula(paste0("~", x))
    ## only keep the TRUE column of counts from svytable
    weighted_counts <- svytable(form, design)[[2]]
    ## calculate proportions (multiply by 100 to get percentages)
    weighted_props <- svyciprop(form, design, na.rm = TRUE) * 100
    ## extract the confidence intervals and multiply to get percentages
    weighted_confint <- confint(weighted_props) * 100
    ## use svymean to calculate design effect and only keep the TRUE column
    design_eff <- deff(svymean(form, design, na.rm = TRUE, deff = TRUE))[[TRUE]]

    ## combine in to one data frame
    full_table <- cbind(
        "Variable" = x,
        "Count" = weighted_counts,
        "Proportion" = weighted_props,
        weighted_confint,
        "Design effect" = design_eff
    )

    ## return table as a dataframe
    full_table <- data.frame(full_table,
        ## remove the variable names from rows (is a separate column now)
        row.names = NULL
    )

    ## change numerics back to numeric
    full_table[, 2:6] <- as.numeric(full_table[, 2:6])

    ## return dataframe
    full_table
}

# apply the function to each dataframe in the list
proportion_highsrh_weighted <- lapply(weighted_nhanes_list, calculate_svy_prop, "high_srh")

print(proportion_highsrh_weighted)



# these proportions are the overall for all adults >= 18 years for each cycle.

# lets do a weighted analysis by age (>65 and less than 65) and SRH

svymean( ~ high_srh , weighted_nhanes_list$`nhanes01_02w`)

svyby(~high_srh, ~RIDAGEYR>=65, weighted_nhanes_list$`nhanes01_02w`, svymean)

# weighted analysis by gender
svyby(~high_srh, ~RIAGENDR, weighted_nhanes_list$`nhanes01_02w`, svymean)
lapply(weighted_nhanes_list, svyby(~high_srh, ~RIAGENDR, svymean))
