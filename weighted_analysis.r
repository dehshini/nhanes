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


# create survey design object for COMBINED data
# note: we are using WTMEC9YR instead of WTMEC2YR

weighted_nhanes_all <- svydesign(
    id = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTMEC9YR,
    nest = TRUE,
    data = nhanes_all
)

# get the subset of interest: adults aged 20 and over with self-reported diabetes
analyze1 <- subset(weighted_nhanes_all, RIDAGEYR >= 20 & DIQ010 == 1 & !is.na(HSD010))


# Define a function to call svymean and unweighted count
getSummary <- function(varformula, byformula, design) {
    # Get mean, stderr, and unweighted sample size
    c <- svyby(varformula, byformula, design, unwtd.count, na.rm.by = TRUE)
    p <- svyby(varformula, byformula, design, svymean, na.rm.by = TRUE)
    outSum <- left_join(select(c, -se), p)
    outSum
}

# try the function
getSummary(~high_srh, ~RIAGENDR, analyze1)

################################################################################
# CREATE WEIGHTED DATA SETS FOR EACH YEAR GROUP
# apply the function to each cycle dataframe in the list

weighted_nhanes_list <- lapply(nhanes_list2, create_survey_design)

# rename the list, adding "w" to the name
names(weighted_nhanes_list) <- c("nhanes01_02w", "nhanes03_04w", "nhanes05_06w", "nhanes07_08w", "nhanes09_10w", "nhanes11_12w", "nhanes13_14w", "nhanes15_16w", "nhanes17_18w")

# duplicate the weighted list before modifying
weighted_nhanes_list2 <- weighted_nhanes_list

# subset the dataframes to only include rows with non-missing age, age >= 20 and SRH

for (i in 1:length(weighted_nhanes_list)) {
    weighted_nhanes_list[[i]] <- subset(weighted_nhanes_list[[i]], RIDAGEYR >= 20 & DIQ010 == 1 & !is.na(HSD010))
}

#########################################################################
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
    
    # get the standard error
    se <- SE(weighted_props) * 100
    
    #design_eff <- deff(svymean(form, design, na.rm = TRUE, deff = TRUE))[[TRUE]]
    
    # Combine into one data frame
    full_table <- data.frame(
        "Unweighted Count" = unweighted_counts,
        "Proportion" = as.numeric(weighted_props),
        "Lower_CI" = Lower_CI,
        "Upper_CI" = Upper_CI,
        "SE" = se
    )
    ## return dataframe
    full_table
}

################################################################################


# high srh. excellent and very good
proportion_highsrh_weighted <- do.call(rbind, lapply(weighted_nhanes_list, calculate_svy_prop, "high_srh"))
#rename the columns
colnames(proportion_highsrh_weighted) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
write.csv(proportion_highsrh_weighted, file = "./out/highsrh_summary.csv")


#low srh. fair and poor
proportion_lowsrh_weighted <- do.call(rbind, lapply(weighted_nhanes_list, calculate_svy_prop, "low_srh"))
#rename the columns
colnames(proportion_lowsrh_weighted) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
write.csv(proportion_lowsrh_weighted, file = "./out/lowsrh_summary.csv")


# srh proportion for each cycle
#subset the dataframes 
proportion_srh <- do.call(rbind, lapply(weighted_nhanes_list, function(design) {
    svymean(~ factor(HSD010), design = design, na.rm = TRUE) * 100
}))
proportion_srh <- as.data.frame(proportion_srh)
# save as excel file
write.xlsx(proportion_srh, "./out/srh_distribution.xlsx", rowNames = TRUE)
write.csv(proportion_srh, file = "./out/srh_distribution1.csv")


###########################
# BEGIN STRATIFIED ANALYSIS
################################################################
# calculate the proportion of 'yes' (1) for high_srh by age group, <65 and >=65

# subset the dataframes to only include rows with non-missing age, age >= 65 and SRH
highsrh_gt65 <- lapply(weighted_nhanes_list, subset, RIDAGEYR >= 65)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_gt65 <- do.call(rbind, lapply(highsrh_gt65, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_gt65) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_gt65, file = "./out/highsrh_gt65.csv")


# subset the dataframes to only include rows with non-missing age, age < 65 and SRH
highsrh_lt65 <- lapply(weighted_nhanes_list, subset, RIDAGEYR < 65)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_lt65 <- do.call(rbind, lapply(highsrh_lt65, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_lt65) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_lt65, file = "./out/highsrh_lt65.csv")





##############################################################################
# calculate the proportion of 'yes' (1) for high_srh by sex

# subset the dataframes to only include rows with non-missing sex and SRH
highsrh_male <- lapply(weighted_nhanes_list, subset, RIAGENDR == 1)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_male <- do.call(rbind, lapply(highsrh_male, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_male) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_male, file = "./out/highsrh_male.csv")


highsrh_female <- lapply(weighted_nhanes_list, subset, RIAGENDR == 2)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_female <- do.call(rbind, lapply(highsrh_female, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_female) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_female, file = "./out/highsrh_female.csv")



#################################################################
# calculate the proportion of 'yes' (1) for high_srh by race/ethnicity

# subset the dataframes to only include rows with non-missing race and SRH
highsrh_white <- lapply(weighted_nhanes_list, subset, RACE == 1)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_white <- do.call(rbind, lapply(highsrh_white, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_white) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_white, file = "./out/highsrh_white.csv")


highsrh_black <- lapply(weighted_nhanes_list, subset, RACE == 2)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_black <- do.call(rbind, lapply(highsrh_black, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_black) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_black, file = "./out/highsrh_black.csv")


highsrh_hispanic <- lapply(weighted_nhanes_list, subset, RACE == 3)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_hispanic <- do.call(rbind, lapply(highsrh_hispanic, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_hispanic) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_hispanic, file = "./out/highsrh_hispanic.csv")


highsrh_other <- lapply(weighted_nhanes_list, subset, RACE == 4)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_other <- do.call(rbind, lapply(highsrh_other, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_other) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_other, file = "./out/highsrh_other.csv")



#################################
# calculate the proportion of 'yes' (1) for high_srh by CKD

# subset the dataframes to only include rows with non-missing CKD and SRH
highsrh_ckd <- lapply(weighted_nhanes_list, subset, CKD == 1)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_ckd <- do.call(rbind, lapply(highsrh_ckd, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_ckd) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_ckd, file = "./out/highsrh_ckd.csv")


highsrh_nonckd <- lapply(weighted_nhanes_list, subset, CKD == 0)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_nonckd <- do.call(rbind, lapply(highsrh_nonckd, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_nonckd) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_nonckd, file = "./out/highsrh_nonckd.csv")



#################################
# by poverty status

highsrh_poverty <- lapply(weighted_nhanes_list, subset, INDFMPIR < 1)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_poverty <- do.call(rbind, lapply(highsrh_poverty, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_poverty) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_poverty, file = "./out/highsrh_poverty.csv")


highsrh_nonpoverty <- lapply(weighted_nhanes_list, subset, INDFMPIR >= 1)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_nonpoverty <- do.call(rbind, lapply(highsrh_nonpoverty, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_nonpoverty) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_nonpoverty, file = "./out/highsrh_nonpoverty.csv")



#################################
# by insurance status


highsrh_noninsured <- lapply(weighted_nhanes_list, subset, INSURANCE == 0)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_noninsured <- do.call(rbind, lapply(highsrh_noninsured, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_noninsured) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_noninsured, file = "./out/highsrh_noninsured.csv")


highsrh_priv_insurance <- lapply(weighted_nhanes_list, subset, INSURANCE == 1)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_priv_insurance <- do.call(rbind, lapply(highsrh_priv_insurance, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_priv_insurance) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_priv_insurance, file = "./out/highsrh_priv_insurance.csv")


highsrh_pub_insurance <- lapply(weighted_nhanes_list, subset, INSURANCE == 2)
# calculate the proportion of 'yes' (1) for high_srh
prop_highsrh_pub_insurance <- do.call(rbind, lapply(highsrh_pub_insurance, calculate_svy_prop, "high_srh"))
colnames(prop_highsrh_pub_insurance) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_highsrh_pub_insurance, file = "./out/highsrh_pub_insurance.csv")




#################################################################
################### BEGIN LOW SRH, STRATIFIED ###############################
#################################################################

# calculate the proportion of 'yes' (1) for low_srh by age group

# subset the dataframes
lowsrh_gt65 <- lapply(weighted_nhanes_list, subset, RIDAGEYR >= 65)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_gt65 <- do.call(rbind, lapply(lowsrh_gt65, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_gt65) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_gt65, file = "./out/lowsrh_gt65.csv")

lowsrh_lt65 <- lapply(weighted_nhanes_list, subset, RIDAGEYR < 65)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_lt65 <- do.call(rbind, lapply(lowsrh_lt65, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_lt65) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_lt65, file = "./out/lowsrh_lt65.csv")


###############################################################
## calculate the proportion of 'yes' (1) for low_srh by sex
lowsrh_male <- lapply(weighted_nhanes_list, subset, RIAGENDR == 1)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_male <- do.call(rbind, lapply(lowsrh_male, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_male) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_male, file = "./out/lowsrh_male.csv")


lowsrh_female <- lapply(weighted_nhanes_list, subset, RIAGENDR == 2)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_female <- do.call(rbind, lapply(lowsrh_female, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_female) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_female, file = "./out/lowsrh_female.csv")


#############################################################
### by race/ethnicity
lowsrh_white <- lapply(weighted_nhanes_list, subset, RACE == 1)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_white <- do.call(rbind, lapply(lowsrh_white, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_white) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_white, file = "./out/lowsrh_white.csv")


lowsrh_black <- lapply(weighted_nhanes_list, subset, RACE == 2)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_black <- do.call(rbind, lapply(lowsrh_black, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_black) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_black, file = "./out/lowsrh_black.csv")


lowsrh_hispanic <- lapply(weighted_nhanes_list, subset, RACE == 3)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_hispanic <- do.call(rbind, lapply(lowsrh_hispanic, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_hispanic) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_hispanic, file = "./out/lowsrh_hispanic.csv")

lowsrh_other <- lapply(weighted_nhanes_list, subset, RACE == 4)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_other <- do.call(rbind, lapply(lowsrh_other, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_other) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_other, file = "./out/lowsrh_other.csv")


##########################################################
## by CKD

lowsrh_ckd <- lapply(weighted_nhanes_list, subset, CKD == 1)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_ckd <- do.call(rbind, lapply(lowsrh_ckd, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_ckd) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_ckd, file = "./out/lowsrh_ckd.csv")


lowsrh_nonckd <- lapply(weighted_nhanes_list, subset, CKD == 0)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_nonckd <- do.call(rbind, lapply(lowsrh_nonckd, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_nonckd) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_nonckd, file = "./out/lowsrh_nonckd.csv")


#################################################################
# by insurance status
lowsrh_noninsured <- lapply(weighted_nhanes_list, subset, INSURANCE == 0)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_noninsured <- do.call(rbind, lapply(lowsrh_noninsured, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_noninsured) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_noninsured, file = "./out/lowsrh_noninsured.csv")


lowsrh_priv_insurance <- lapply(weighted_nhanes_list, subset, INSURANCE == 1)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_priv_insurance <- do.call(rbind, lapply(lowsrh_priv_insurance, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_priv_insurance) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_priv_insurance, file = "./out/lowsrh_priv_insurance.csv")


lowsrh_pub_insurance <- lapply(weighted_nhanes_list, subset, INSURANCE == 2)
# calculate the proportion of 'yes' (1) for low_srh
prop_lowsrh_pub_insurance <- do.call(rbind, lapply(lowsrh_pub_insurance, calculate_svy_prop, "low_srh"))
colnames(prop_lowsrh_pub_insurance) <- c("Unweighted_count", "Proportion", "Lower", "Upper", "SE")
# save to csv
write.csv(prop_lowsrh_pub_insurance, file = "./out/lowsrh_pub_insurance.csv")


#############################################################
################### END LOW SRH, STRATIFIED ###############################
################################################################

## analyze for nhanes all
analyze1 %>% 
    svyby(~low_srh, ~RACE, design = ., svymean, na.rm = TRUE)

analyze1 %>%
    svyby(~low_srh, ~INSURANCE, design = ., svymean, na.rm = TRUE)

svymean(~RIDAGEYR, design = analyze1, method = "mean")
svymean(~RACE, design = analyze1, method = "quantile")
prop.table((svytable((~RACE), design = analyze1)))
prop.table(svyby(~RACE, ~low_srh, design = analyze1, svytable))
