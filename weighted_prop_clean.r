########### SRH analysis by age and sex, weighted ##############

# function to create survey design object from 2 year cycle
create_survey_design <- function(data) {
    survey_design <- svydesign(
        id = ~SDMVPSU,
        strata = ~SDMVSTRA,
        weights = ~WTMEC2YR,
        nest = TRUE,
        data = data
    )
    return(survey_design)
}

# function to get summary statistics
getSummary <- function(varformula, byformula, design) {
    # Get mean, stderr, and unweighted sample size
    c <- svyby(varformula, byformula, design, unwtd.count, na.rm.by = TRUE)
    p <- svyby(varformula, byformula, design, svymean, na.rm.by = TRUE)
    outSum <- left_join(select(c, -se), p)
    outSum
}

# Function to calculate the weighted proportion for srh
calculate_svy_prop <- function(design, x) {
    # Put the variable of interest in a formula
    form <- as.formula(paste0("~", x))

    # Calculate weighted proportions (multiply by 100 to get percentages)
    weighted_props <- svyciprop(form, design, na.rm = TRUE) * 100

    # Extract the confidence intervals and multiply to get percentages
    Lower_CI <- confint(weighted_props)[1] * 100
    Upper_CI <- confint(weighted_props)[2] * 100

    unweighted_counts <- table(design$variables[[x]])[2]
    weighted_counts <- svytotal(form, design, na.rm = TRUE)

    # get the standard error
    se <- SE(weighted_props) * 100

    # Combine into one data frame
    full_table <- data.frame(
        "Unweighted_Count" = unweighted_counts,
        "Weighted_Count" = weighted_counts,
        "Proportion" = as.numeric(weighted_props),
        "Lower_CI" = Lower_CI,
        "Upper_CI" = Upper_CI,
        "SE" = se
    )
    ## return dataframe
    full_table
}


# create survey design object for COMBINED data
# note: we are using WTMEC9YR instead of WTMEC2YR

weighted_nhanes_all <- svydesign(
    id = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTMEC9YR,
    nest = TRUE,
    data = nhanes_all
)

# get the subset of interest: 
# adults aged 20 and over with self-reported diabetes
analyze1 <- subset(
    weighted_nhanes_all,
    RIDAGEYR >= 20 & DIQ010 == 1 & !is.na(HSD010) 
)

analyze1 <- subset(
    analyze1,
    !is.na(hypercholesterolemia) &
    !is.na(hypertension) &
    !is.na(diabetes_duration) &
    !is.na(CVD) &
    !is.na(insurance) &
    !is.na(EDULEVEL)
)

# create survey design objects for each 2 year cycle
weighted_nhanes_list <- lapply(nhanes_list, create_survey_design)

# rename the list, adding "w" to the names
names(weighted_nhanes_list) <- c(
    "nhanes01_02w", "nhanes03_04w", "nhanes05_06w",
    "nhanes07_08w","nhanes09_10w", "nhanes11_12w",
    "nhanes13_14w", "nhanes15_16w", "nhanes17_18w"
)

# subset the dataframes to only include 
# rows with age >= 20 and SRH
for (i in 1:length(weighted_nhanes_list)) {
    weighted_nhanes_list[[i]] <- subset(
        weighted_nhanes_list[[i]], 
        RIDAGEYR >= 20 & DIQ010 == 1 & !is.na(HSD010)
    )
}


# subset the dataframes to only include rows with non-missing select variables
for (i in 1:length(weighted_nhanes_list)) {
    weighted_nhanes_list[[i]] <- subset(
        weighted_nhanes_list[[i]],
        !is.na(hypercholesterolemia) &
        !is.na(hypertension) &
        !is.na(diabetes_duration) &
        !is.na(CVD) &
        !is.na(insurance) &
        !is.na(EDULEVEL)
    )
}



############################################################
# Calculate the proportions for low SRH

# srh proportions for each cycle
proportion_srh <- do.call(
    rbind, lapply(
        weighted_nhanes_list, function(design) {
            svymean(
                ~ factor(HSD010),
                design = design,
                na.rm = TRUE
            ) * 100
        }
    )
)
proportion_srh <- as.data.frame(proportion_srh)

# rename the columns
colnames(proportion_srh) <- c(
    "Excellent", "Very Good", "Good", "Fair", "Poor"
)
# save as excel file
write.xlsx(
    proportion_srh, "./out/srh_distribution.xlsx",
    rowNames = TRUE
)
write.csv(proportion_srh, file = "./out/srh_distribution.csv")


proportion_lowsrh_weighted <- do.call(
    rbind, lapply(
        weighted_nhanes_list, 
        calculate_svy_prop,
        "low_srh"
    )
)
write.csv(proportion_lowsrh_weighted, file = "./out/lowsrh_summary.csv")

###########################
# BEGIN STRATIFIED ANALYSIS
############################

#####################
# by AGE GROUP, less than 65 and >=65
#####################
lowsrh_gt65 <- lapply(weighted_nhanes_list, subset, RIDAGEYR >= 65)
# calculate the proportion of low_srh
prop_lowsrh_gt65 <- do.call(
    rbind, lapply(lowsrh_gt65, calculate_svy_prop, "low_srh")
)
# add a column for age group
prop_lowsrh_gt65$Age <- ">=65"
write.csv(prop_lowsrh_gt65, file = "./out/lowsrh_gt65.csv")


lowsrh_lt65 <- lapply(weighted_nhanes_list, subset, RIDAGEYR < 65)
# calculate the proportion of low_srh
prop_lowsrh_lt65 <- do.call(
    rbind, lapply(lowsrh_lt65, calculate_svy_prop, "low_srh")
)
# add a column for age group
prop_lowsrh_lt65$Age <- "<65"
write.csv(prop_lowsrh_lt65, file = "./out/lowsrh_lt65.csv")

####################
# by SEX, male and female
####################
lowsrh_male <- lapply(weighted_nhanes_list, subset, RIAGENDR == 1)
# calculate the proportion of low_srh
prop_lowsrh_male <- do.call(
    rbind, lapply(lowsrh_male, calculate_svy_prop, "low_srh")
)
# add a column for sex
prop_lowsrh_male$Sex <- "Male"
write.csv(prop_lowsrh_male, file = "./out/lowsrh_male.csv")


lowsrh_female <- lapply(weighted_nhanes_list, subset, RIAGENDR == 2)
# calculate the proportion of low_srh
prop_lowsrh_female <- do.call(
    rbind, lapply(lowsrh_female, calculate_svy_prop, "low_srh")
)
# add a column for sex
prop_lowsrh_female$Sex <- "Female"
write.csv(prop_lowsrh_female, file = "./out/lowsrh_female.csv")

#####################
# by ETHNICITY, white, black, hispanic, other
#####################
lowsrh_white <- lapply(weighted_nhanes_list, subset, RACE == 1)
# calculate the proportion of low_srh
prop_lowsrh_white <- do.call(
    rbind, lapply(lowsrh_white, calculate_svy_prop, "low_srh")
)
# add a column for ethnicity
prop_lowsrh_white$Ethnicity <- "White"
write.csv(prop_lowsrh_white, file = "./out/lowsrh_white.csv")

lowsrh_black <- lapply(weighted_nhanes_list, subset, RACE == 2)
# calculate the proportion of low_srh
prop_lowsrh_black <- do.call(
    rbind, lapply(lowsrh_black, calculate_svy_prop, "low_srh")
)
# add a column for ethnicity
prop_lowsrh_black$Ethnicity <- "Black"
write.csv(prop_lowsrh_black, file = "./out/lowsrh_black.csv")


lowsrh_hispanic <- lapply(weighted_nhanes_list, subset, RACE == 3)
# calculate the proportion of low_srh
prop_lowsrh_hispanic <- do.call(
    rbind, lapply(lowsrh_hispanic, calculate_svy_prop, "low_srh")
)
# add a column for ethnicity
prop_lowsrh_hispanic$Ethnicity <- "Hispanic"
write.csv(prop_lowsrh_hispanic, file = "./out/lowsrh_hispanic.csv")


lowsrh_other <- lapply(weighted_nhanes_list, subset, RACE == 4)
# calculate the proportion of low_srh
prop_lowsrh_other <- do.call(
    rbind, lapply(lowsrh_other, calculate_svy_prop, "low_srh")
)
# add a column for ethnicity
prop_lowsrh_other$Ethnicity <- "Other"
write.csv(prop_lowsrh_other, file = "./out/lowsrh_other.csv")


###########
# by CKD, yes and no
###########
lowsrh_ckd <- lapply(weighted_nhanes_list, subset, CKD == 1)
# calculate the proportion of low_srh
prop_lowsrh_ckd <- do.call(
    rbind, lapply(lowsrh_ckd, calculate_svy_prop, "low_srh")
)
# add a column for CKD
prop_lowsrh_ckd$CKD <- "CKD"
write.csv(prop_lowsrh_ckd, file = "./out/lowsrh_ckd.csv")

lowsrh_nonckd <- lapply(weighted_nhanes_list, subset, CKD == 0)
# calculate the proportion of low_srh
prop_lowsrh_nonckd <- do.call(
    rbind, lapply(lowsrh_nonckd, calculate_svy_prop, "low_srh")
)
# add a column for CKD
prop_lowsrh_nonckd$CKD <- "Non-CKD"
write.csv(prop_lowsrh_nonckd, file = "./out/lowsrh_nonckd.csv")


###############
# by INCOME, below and above poverty threshold
###############
lowsrh_poverty <- lapply(weighted_nhanes_list, subset, INDFMPIR < 1)
# calculate the proportion of low_srh
prop_lowsrh_poverty <- do.call(
    rbind, lapply(lowsrh_poverty, calculate_svy_prop, "low_srh")
)
# add a column for poverty
prop_lowsrh_poverty$Poverty <- "Below-threshold"
write.csv(prop_lowsrh_poverty, file = "./out/lowsrh_poverty.csv")


lowsrh_nonpoverty <- lapply(weighted_nhanes_list, subset, INDFMPIR >= 1)
# calculate the proportion of low_srh
prop_lowsrh_nonpoverty <- do.call(
    rbind, lapply(lowsrh_nonpoverty, calculate_svy_prop, "low_srh")
)
# add a column for poverty
prop_lowsrh_nonpoverty$Poverty <- "Above-threshold"
write.csv(prop_lowsrh_nonpoverty, file = "./out/lowsrh_nonpoverty.csv")


#######################
# by INSURANCE STATUS, non-insured and insured
#######################
lowsrh_noninsured <- lapply(weighted_nhanes_list, subset, insurance == 0)
# calculate the proportion of low_srh
prop_lowsrh_noninsured <- do.call(
    rbind, lapply(lowsrh_noninsured, calculate_svy_prop, "low_srh")
)
# add a column for insurance
prop_lowsrh_noninsured$Insurance <- "Non-insured"
write.csv(prop_lowsrh_noninsured, file = "./out/lowsrh_noninsured.csv")

lowsrh_insured <- lapply(
    weighted_nhanes_list, subset, insurance == 1 | insurance == 2
)
# calculate the proportion of low_srh
prop_lowsrh_insured <- do.call(
    rbind, lapply(lowsrh_insured, calculate_svy_prop, "low_srh")
)
# add a column for insurance
prop_lowsrh_insured$Insurance <- "Insured"
write.csv(prop_lowsrh_insured, file = "./out/lowsrh_insured.csv")


########################
# by EDUCATION, 
########################
lowsrh_belowhighschool <- lapply(weighted_nhanes_list, subset, EDULEVEL == 1)
# calculate the proportion of low_srh
prop_lowsrh_belowhighschool <- do.call(
    rbind, lapply(lowsrh_belowhighschool, calculate_svy_prop, "low_srh")
)
# add a column for education
prop_lowsrh_belowhighschool$Education <- "Less than high school"
write.csv(prop_lowsrh_belowhighschool, file = "./out/lowsrh_belowhighschool.csv")

lowsrh_highschool <- lapply(weighted_nhanes_list, subset, EDULEVEL == 2)
# calculate the proportion of low_srh
prop_lowsrh_highschool <- do.call(
    rbind, lapply(lowsrh_highschool, calculate_svy_prop, "low_srh")
)
# add a column for education
prop_lowsrh_highschool$Education <- "High school"
write.csv(prop_lowsrh_highschool, file = "./out/lowsrh_highschool.csv")

lowsrh_abovehighschool <- lapply(weighted_nhanes_list, subset, EDULEVEL == 3)
# calculate the proportion of low_srh
prop_lowsrh_abovehighschool <- do.call(
    rbind, lapply(lowsrh_abovehighschool, calculate_svy_prop, "low_srh")
)
# add a column for education
prop_lowsrh_abovehighschool$Education <- "college"
write.csv(prop_lowsrh_abovehighschool, file = "./out/lowsrh_abovehighschool.csv")


#####################
# by HBA1C, <7, 7-9, >9
#####################
lowsrh_lt7 <- lapply(weighted_nhanes_list, subset, HBA1C < 7)
# calculate the proportion of low_srh
prop_lowsrh_lt7 <- do.call(
    rbind, lapply(lowsrh_lt7, calculate_svy_prop, "low_srh")
)
# add a column for HBA1C
prop_lowsrh_lt7$HBA1C <- "<7"
write.csv(prop_lowsrh_lt7, file = "./out/lowsrh_lt7.csv")

lowsrh_7to9 <- lapply(weighted_nhanes_list, subset, HBA1C >= 7 & HBA1C <= 9)
# calculate the proportion of low_srh
prop_lowsrh_7to9 <- do.call(
    rbind, lapply(lowsrh_7to9, calculate_svy_prop, "low_srh")
)
# add a column for HBA1C
prop_lowsrh_7to9$HBA1C <- "7-9"
write.csv(prop_lowsrh_7to9, file = "./out/lowsrh_7to9.csv")

lowsrh_gt9 <- lapply(weighted_nhanes_list, subset, HBA1C > 9)
# calculate the proportion of low_srh
prop_lowsrh_gt9 <- do.call(
    rbind, lapply(lowsrh_gt9, calculate_svy_prop, "low_srh")
)
# add a column for HBA1C
prop_lowsrh_gt9$HBA1C <- ">9"
write.csv(prop_lowsrh_gt9, file = "./out/lowsrh_gt9.csv")




############################################################
# CALCULATE THE PROPORTION OF HIGH_SRH

proportion_highsrh_weighted <- do.call(
    rbind, lapply(
        weighted_nhanes_list, calculate_svy_prop, "high_srh"
    )
)
write.csv(
    proportion_highsrh_weighted,
    file = "./out/proportion_highsrh_weighted.csv"
)

################################################
# Stratified analysis
################################################

#####################
# by AGE GROUP, less than 65 and >=65
#####################

highsrh_lt65 <- lapply(
    weighted_nhanes_list, subset, RIDAGEYR >= 65
)
# calculate the proportion of high_srh
prop_highsrh_lt65 <- do.call(
    rbind, lapply(highsrh_lt65, calculate_svy_prop, "high_srh")
)
# add a column for age
prop_highsrh_lt65$Age <- ">=65"
write.csv(prop_highsrh_lt65, file = "./out/highsrh_gt65.csv")


highsrh_gt65 <- lapply(
    weighted_nhanes_list, subset, RIDAGEYR < 65
)
# calculate the proportion of high_srh
prop_highsrh_gt65 <- do.call(
    rbind, lapply(highsrh_gt65, calculate_svy_prop, "high_srh")
)
# add a column for age
prop_highsrh_gt65$Age <- "<65"
write.csv(prop_highsrh_gt65, file = "./out/highsrh_lt65.csv")


####################
# by SEX, male and female
####################


highsrh_male <- lapply(
    weighted_nhanes_list, subset, RIAGENDR == 1
)
# calculate the proportion of high_srh
prop_highsrh_male <- do.call(
    rbind, lapply(highsrh_male, calculate_svy_prop, "high_srh")
)
# add a column for sex
prop_highsrh_male$Sex <- "Male"
write.csv(prop_highsrh_male, file = "./out/highsrh_male.csv")


highsrh_female <- lapply(
    weighted_nhanes_list, subset, RIAGENDR == 2
)
# calculate the proportion of high_srh
prop_highsrh_female <- do.call(
    rbind, lapply(highsrh_female, calculate_svy_prop, "high_srh")
)
# add a column for sex
prop_highsrh_female$Sex <- "Female"
write.csv(prop_highsrh_female, file = "./out/highsrh_female.csv")

#####################
# by ETHNICITY, white, black, hispanic, other
#####################

highsrh_white <- lapply(
    weighted_nhanes_list, subset, RACE == 1
)
# calculate the proportion of high_srh
prop_highsrh_white <- do.call(
    rbind, lapply(highsrh_white, calculate_svy_prop, "high_srh")
)
# add a column for race
prop_highsrh_white$Race <- "NH White"
write.csv(prop_highsrh_white, file = "./out/highsrh_white.csv")


highsrh_black <- lapply(
    weighted_nhanes_list, subset, RACE == 2
)
# calculate the proportion of high_srh
prop_highsrh_black <- do.call(
    rbind, lapply(highsrh_black, calculate_svy_prop, "high_srh")
)
# add a column for race
prop_highsrh_black$Race <- "NH Black"
write.csv(prop_highsrh_black, file = "./out/highsrh_black.csv")


highsrh_hispanic <- lapply(
    weighted_nhanes_list, subset, RACE == 3
)
# calculate the proportion of high_srh
prop_highsrh_hispanic <- do.call(
    rbind, lapply(highsrh_hispanic, calculate_svy_prop, "high_srh")
)
# add a column for race
prop_highsrh_hispanic$Race <- "Hispanic"
write.csv(prop_highsrh_hispanic, file = "./out/highsrh_hispanic.csv")


highsrh_other <- lapply(
    weighted_nhanes_list, subset, RACE == 4
)
# calculate the proportion of high_srh
prop_highsrh_other <- do.call(
    rbind, lapply(highsrh_other, calculate_svy_prop, "high_srh")
)
# add a column for race
prop_highsrh_other$Race <- "Other"
write.csv(prop_highsrh_other, file = "./out/highsrh_other.csv")


###########
# by CKD, yes and no
###########

highsrh_ckd <- lapply(
    weighted_nhanes_list, subset, CKD == 1
)
# calculate the proportion of high_srh
prop_highsrh_ckd <- do.call(
    rbind, lapply(highsrh_ckd, calculate_svy_prop, "high_srh")
)
# add a column for CKD
prop_highsrh_ckd$CKD <- "CKD"
write.csv(prop_highsrh_ckd, file = "./out/highsrh_ckd.csv")


highsrh_nonckd <- lapply(
    weighted_nhanes_list, subset, CKD == 0
)
# calculate the proportion of high_srh
prop_highsrh_nonckd <- do.call(
    rbind, lapply(highsrh_nonckd, calculate_svy_prop, "high_srh")
)
# add a column for CKD
prop_highsrh_nonckd$CKD <- "No CKD"
write.csv(prop_highsrh_nonckd, file = "./out/highsrh_nonckd.csv")

###############
# by INCOME, below and above poverty threshold
###############

highsrh_poverty <- lapply(
    weighted_nhanes_list, subset, INDFMPIR < 1
)
# calculate the proportion of high_srh
prop_highsrh_poverty <- do.call(
    rbind, lapply(highsrh_poverty, calculate_svy_prop, "high_srh")
)
# add a column for income
prop_highsrh_poverty$Income <- "Below poverty threshold"
write.csv(prop_highsrh_poverty, file = "./out/highsrh_poverty.csv")


highsrh_nonpoverty <- lapply(
    weighted_nhanes_list, subset, INDFMPIR >= 1
)
# calculate the proportion of high_srh
prop_highsrh_nonpoverty <- do.call(
    rbind, lapply(highsrh_nonpoverty, calculate_svy_prop, "high_srh")
)
# add a column for income
prop_highsrh_nonpoverty$Income <- "Above poverty threshold"
write.csv(prop_highsrh_nonpoverty, file = "./out/highsrh_nonpoverty.csv")


#######################
# by INSURANCE STATUS, non-insured and insured
#######################

highsrh_noninsured <- lapply(
    weighted_nhanes_list, subset, insurance == 0
)
# calculate the proportion of high_srh
prop_highsrh_noninsured <- do.call(
    rbind, lapply(highsrh_noninsured, calculate_svy_prop, "high_srh")
)
# add a column for insurance
prop_highsrh_noninsured$Insurance <- "Non-insured"
write.csv(prop_highsrh_noninsured, file = "./out/highsrh_noninsured.csv")


highsrh_insured <- lapply(
    weighted_nhanes_list, subset, insurance == 1 | insurance == 2
)
# calculate the proportion of high_srh
prop_highsrh_insured <- do.call(
    rbind, lapply(highsrh_insured, calculate_svy_prop, "high_srh")
)
# add a column for insurance
prop_highsrh_insured$Insurance <- "Insured"
write.csv(
    prop_highsrh_insured,
    file = "./out/highsrh_insured.csv"
)



########################
# by EDUCATION,
########################

highsrh_belowhighschool <- lapply(
    weighted_nhanes_list, subset, EDULEVEL == 1
)
# calculate the proportion of high_srh
prop_highsrh_belowhighschool <- do.call(
    rbind, lapply(highsrh_belowhighschool, calculate_svy_prop, "high_srh")
)
# add a column for education
prop_highsrh_belowhighschool$Education <- "Less than high school"
write.csv(
    prop_highsrh_belowhighschool,
    file = "./out/highsrh_belowhighschool.csv"
)


highsrh_highschool <- lapply(
    weighted_nhanes_list, subset, EDULEVEL == 2
)
# calculate the proportion of high_srh
prop_highsrh_highschool <- do.call(
    rbind, lapply(highsrh_highschool, calculate_svy_prop, "high_srh")
)
# add a column for education
prop_highsrh_highschool$Education <- "High school"
write.csv(prop_highsrh_highschool, file = "./out/highsrh_highschool.csv")


highsrh_abovehighschool <- lapply(
    weighted_nhanes_list, subset, EDULEVEL == 3
)
# calculate the proportion of high_srh
prop_highsrh_abovehighschool <- do.call(
    rbind, lapply(highsrh_abovehighschool, calculate_svy_prop, "high_srh")
)
# add a column for education
prop_highsrh_abovehighschool$Education <- "Above high school"
write.csv(
    prop_highsrh_abovehighschool,
    file = "./out/highsrh_abovehighschool.csv"
)


#####################
# by HBA1C, <7, 7-9, >9
#####################


highsrh_lt7 <- lapply(weighted_nhanes_list, subset, HBA1C < 7)
# calculate the proportion of high_srh
prop_highsrh_lt7 <- do.call(
    rbind, lapply(highsrh_lt7, calculate_svy_prop, "high_srh")
)
# add a column for hba1c
prop_highsrh_lt7$HBA1C <- "<7"
write.csv(prop_highsrh_lt7, file = "./out/highsrh_lt7.csv")


highsrh_7to9 <- lapply(weighted_nhanes_list, subset, HBA1C >= 7 & HBA1C <= 9)
# calculate the proportion of high_srh
prop_highsrh_7to9 <- do.call(
    rbind, lapply(highsrh_7to9, calculate_svy_prop, "high_srh")
)
# add a column for hba1c
prop_highsrh_7to9$HBA1C <- "7-9"
write.csv(prop_highsrh_7to9, file = "./out/highsrh_7to9.csv")


highsrh_gt9 <- lapply(weighted_nhanes_list, subset, HBA1C > 9)
# calculate the proportion of high_srh
prop_highsrh_gt9 <- do.call(
    rbind, lapply(highsrh_gt9, calculate_svy_prop, "high_srh")
)
# add a column for hba1c
prop_highsrh_gt9$HBA1C <- ">9"
write.csv(prop_highsrh_gt9, file = "./out/highsrh_gt9.csv")

