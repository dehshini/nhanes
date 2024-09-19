# load("~/Desktop/nhanes.RDATA")

new_nhanes_list <- list()

names(nhanes_list) <- c(
  "01/02", "03/04", "05/06",
  "07/08", "09/10", "11/12",
  "13/14", "15/16", "17/18"
)

# create 4 year weights for 03/04
nhanes_list[[2]] <- nhanes_list[[2]] %>% 
    mutate(
        WTMEC4YR = WTMEC2YR / 2
    )

# Combine 2-year cycles into 4-year cycles, leave the last cycle 2017-2018 unchanged
for (i in seq(1, length(nhanes_list) - 1, by = 2)) {
  # Combine two consecutive cycles (i and i+1)
  combined_cycle <- rbind(nhanes_list[[i]], nhanes_list[[i + 1]], fill = T)
  # Name the combined cycle (e.g., 2001-2004, 2005-2008, etc.)
  cycle_name <- paste0(names(nhanes_list)[i], "-", names(nhanes_list)[i + 1])
  # Add the combined cycle to the new list
  new_nhanes_list[[cycle_name]] <- combined_cycle
}

# Add the last cycle (2017-2018) to the new list unchanged
new_nhanes_list[["2017-2018"]] <- nhanes_list[[length(nhanes_list)]]


# Create 4-year weights for each combined cycle in new_nhanes_list
for (cycle_name in names(new_nhanes_list)) {
    # Check if the 4-year weight column does not already exist
    if (!("WTMEC4YR" %in% colnames(new_nhanes_list[[cycle_name]]))) {
        # Check if the 2-year weight column exists
        if ("WTMEC2YR" %in% colnames(new_nhanes_list[[cycle_name]])) {
            # Create the 4-year weight by dividing the 2-year weight by 2
            new_nhanes_list[[cycle_name]] <- new_nhanes_list[[cycle_name]] %>%
                mutate(WTMEC4YR = WTMEC2YR / 2)
        } else {
            warning(paste("2-year weight (WTMEC2YR) not found for cycle:", cycle_name))
        }
    } else {
        message(paste("4-year weight (WTMEC4YR) already exists for cycle:", cycle_name, ", skipping!"))
    }
}

# subset the dataframes to only include 
# rows with age >= 20 and SRH
for (i in 1:length(new_nhanes_list)) {
    new_nhanes_list[[i]] <- subset(
        new_nhanes_list[[i]], 
        RIDAGEYR >= 20 & DIQ010 == 1 & !is.na(HSD010)
    )
}


# subset the dataframes to only include rows with non-missing select variables
for (i in 1:length(new_nhanes_list)) {
    new_nhanes_list[[i]] <- subset(
        new_nhanes_list[[i]],
        !is.na(hypercholesterolemia) &
            !is.na(hypertension) &
            !is.na(diabetes_duration) &
            !is.na(CVD) &
            !is.na(insurance) &
            !is.na(EDULEVEL)
    )
}


# Create survey design objects for each combined cycle
weighted_4y_list <- list()
for (cycle_name in names(new_nhanes_list)) {
    weighted_4y_list[[cycle_name]] <- svydesign(
        id = ~SDMVPSU, # Primary Sampling Units
        strata = ~SDMVSTRA, # Stratification variable
        weights = ~WTMEC4YR, # Use 4-year weights
        data = new_nhanes_list[[cycle_name]], # Use the combined cycle data
        nest = TRUE
    )
}


############################################################
# Calculate the proportions for low SRH

# srh proportions for each cycle
proportion_srh2 <- do.call(
    rbind, lapply(
        weighted_4y_list, function(design) {
            svymean(
                ~ factor(HSD010),
                design = design,
                na.rm = TRUE
            ) * 100
        }
    )
)
proportion_srh2 <- as.data.frame(proportion_srh2)

# rename the columns
colnames(proportion_srh2) <- c(
    "Excellent", "Very Good", "Good", "Fair", "Poor"
)
# save as excel file
write.xlsx(
    proportion_srh2, "./out/srh_distribution2.xlsx",
    rowNames = TRUE
)
write.csv(proportion_srh2, file = "./out/srh_distribution2.csv")


proportion_lowsrh_weighted2 <- do.call(
    rbind, lapply(
        weighted_4y_list, 
        calculate_svy_prop,
        "low_srh"
    )
)
write.csv(proportion_lowsrh_weighted2, file = "./out/lowsrh_summary2.csv")

###########################
# BEGIN STRATIFIED ANALYSIS
############################

#####################
# by AGE GROUP, less than 65 and >=65
#####################
lowsrh_gt65.2 <- lapply(weighted_4y_list, subset, RIDAGEYR >= 65)
# calculate the proportion of low_srh
prop_lowsrh_gt65.2 <- do.call(
    rbind, lapply(lowsrh_gt65.2, calculate_svy_prop, "low_srh")
)
# add a column for age group
prop_lowsrh_gt65.2$Age <- ">=65"
write.csv(prop_lowsrh_gt65.2, file = "./out/lowsrh_gt65_2.csv")


lowsrh_lt65.2 <- lapply(weighted_4y_list, subset, RIDAGEYR < 65)
# calculate the proportion of low_srh
prop_lowsrh_lt65.2 <- do.call(
    rbind, lapply(lowsrh_lt65.2, calculate_svy_prop, "low_srh")
)
# add a column for age group
prop_lowsrh_lt65.2$Age <- "<65"
write.csv(prop_lowsrh_lt65.2, file = "./out/lowsrh_lt65_2.csv")


####################
# by SEX, male and female
####################
lowsrh_male.2 <- lapply(weighted_4y_list, subset, RIAGENDR == 1)
# calculate the proportion of low_srh
prop_lowsrh_male.2 <- do.call(
    rbind, lapply(lowsrh_male.2, calculate_svy_prop, "low_srh")
)
# add a column for sex
prop_lowsrh_male.2$Sex <- "Male"
write.csv(prop_lowsrh_male.2, file = "./out/lowsrh_male_2.csv")


lowsrh_female.2 <- lapply(weighted_4y_list, subset, RIAGENDR == 2)
# calculate the proportion of low_srh
prop_lowsrh_female.2 <- do.call(
    rbind, lapply(lowsrh_female.2, calculate_svy_prop, "low_srh")
)
# add a column for sex
prop_lowsrh_female.2$Sex <- "Female"
write.csv(prop_lowsrh_female.2, file = "./out/lowsrh_female_2.csv")



#####################
# by ETHNICITY, white, black, hispanic, other
#####################
lowsrh_white.2 <- lapply(weighted_4y_list, subset, RACE == 1)
# calculate the proportion of low_srh
prop_lowsrh_white.2 <- do.call(
    rbind, lapply(lowsrh_white.2, calculate_svy_prop, "low_srh")
)
# add a column for ethnicity
prop_lowsrh_white.2$Ethnicity <- "White"
write.csv(prop_lowsrh_white.2, file = "./out/lowsrh_white_2.csv")

lowsrh_black.2 <- lapply(weighted_4y_list, subset, RACE == 2)
# calculate the proportion of low_srh
prop_lowsrh_black.2 <- do.call(
    rbind, lapply(lowsrh_black.2, calculate_svy_prop, "low_srh")
)
# add a column for ethnicity
prop_lowsrh_black.2$Ethnicity <- "Black"
write.csv(prop_lowsrh_black.2, file = "./out/lowsrh_black_2.csv")


lowsrh_hispanic.2 <- lapply(weighted_4y_list, subset, RACE == 3)
# calculate the proportion of low_srh
prop_lowsrh_hispanic.2 <- do.call(
    rbind, lapply(lowsrh_hispanic.2, calculate_svy_prop, "low_srh")
)
# add a column for ethnicity
prop_lowsrh_hispanic.2$Ethnicity <- "Hispanic"
write.csv(prop_lowsrh_hispanic.2, file = "./out/lowsrh_hispanic_2.csv")


lowsrh_other.2 <- lapply(weighted_4y_list, subset, RACE == 4)
# calculate the proportion of low_srh
prop_lowsrh_other.2 <- do.call(
    rbind, lapply(lowsrh_other.2, calculate_svy_prop, "low_srh")
)
# add a column for ethnicity
prop_lowsrh_other.2$Ethnicity <- "Other"
write.csv(prop_lowsrh_other.2, file = "./out/lowsrh_other_2.csv")


###############
# by INCOME, below and above poverty threshold
###############

lowsrh_poverty.2 <- lapply(weighted_4y_list, subset, INDFMPIR < 1)
# calculate the proportion of low_srh
prop_lowsrh_poverty.2 <- do.call(
    rbind, lapply(lowsrh_poverty.2, calculate_svy_prop, "low_srh")
)
# add a column for income
prop_lowsrh_poverty.2$Income <- "Below poverty threshold"
write.csv(prop_lowsrh_poverty.2, file = "./out/lowsrh_poverty_2.csv")

lowsrh_no_poverty.2 <- lapply(weighted_4y_list, subset, INDFMPIR >= 1)
# calculate the proportion of low_srh
prop_lowsrh_no_poverty.2 <- do.call(
    rbind, lapply(lowsrh_no_poverty.2, calculate_svy_prop, "low_srh")
)
# add a column for income
prop_lowsrh_no_poverty.2$Income <- "Above poverty threshold"
write.csv(prop_lowsrh_no_poverty.2, file = "./out/lowsrh_no_poverty_2.csv")


#######################
# by INSURANCE STATUS, non-insured and insured
#######################

lowsrh_insured.2 <- lapply(weighted_4y_list, subset, insurance == 0)
# calculate the proportion of low_srh
prop_lowsrh_insured.2 <- do.call(
    rbind, lapply(lowsrh_insured.2, calculate_svy_prop, "low_srh")
)
# add a column for insurance
prop_lowsrh_insured.2$Insurance <- "Insured"
write.csv(prop_lowsrh_insured.2, file = "./out/lowsrh_insured_2.csv")

lowsrh_uninsured.2 <- lapply(
    weighted_4y_list, subset, insurance == 1 | insurance == 2
)
# calculate the proportion of low_srh
prop_lowsrh_uninsured.2 <- do.call(
    rbind, lapply(lowsrh_uninsured.2, calculate_svy_prop, "low_srh")
)
# add a column for insurance
prop_lowsrh_uninsured.2$Insurance <- "Uninsured"
write.csv(prop_lowsrh_uninsured.2, file = "./out/lowsrh_uninsured_2.csv")


########################
# by EDUCATION,
########################

lowsrh_belowhighschool.2 <- lapply(weighted_4y_list, subset, EDULEVEL == 1)
# calculate the proportion of low_srh
prop_lowsrh_belowhighschool.2 <- do.call(
    rbind, lapply(lowsrh_belowhighschool.2, calculate_svy_prop, "low_srh")
)
# add a column for education
prop_lowsrh_belowhighschool.2$Education <- "Below high school"
write.csv(prop_lowsrh_belowhighschool.2, file = "./out/lowsrh_belowhighschool_2.csv")

lowsrh_highschool.2 <- lapply(weighted_4y_list, subset, EDULEVEL == 2)
# calculate the proportion of low_srh
prop_lowsrh_highschool.2 <- do.call(
    rbind, lapply(lowsrh_highschool.2, calculate_svy_prop, "low_srh")
)
# add a column for education
prop_lowsrh_highschool.2$Education <- "High school"
write.csv(prop_lowsrh_highschool.2, file = "./out/lowsrh_highschool_2.csv")

lowsrh_college.2 <- lapply(weighted_4y_list, subset, EDULEVEL == 3)
# calculate the proportion of low_srh
prop_lowsrh_college.2 <- do.call(
    rbind, lapply(lowsrh_college.2, calculate_svy_prop, "low_srh")
)
# add a column for education
prop_lowsrh_college.2$Education <- "College"
write.csv(prop_lowsrh_college.2, file = "./out/lowsrh_college_2.csv")


#####################
# by HBA1C, <7, 7-9, >9
#####################


lowsrh_lt7.2 <- lapply(weighted_4y_list, subset, HBA1C < 7)
# calculate the proportion of low_srh
prop_lowsrh_lt7.2 <- do.call(
    rbind, lapply(lowsrh_lt7.2, calculate_svy_prop, "low_srh")
)
# add a column for HBA1C
prop_lowsrh_lt7.2$HBA1C <- "<7"
write.csv(prop_lowsrh_lt7.2, file = "./out/lowsrh_lt7_2.csv")

lowsrh_7to9.2 <- lapply(weighted_4y_list, subset, HBA1C >= 7 & HBA1C <= 9)
# calculate the proportion of low_srh
prop_lowsrh_7to9.2 <- do.call(
    rbind, lapply(lowsrh_7to9.2, calculate_svy_prop, "low_srh")
)
# add a column for HBA1C
prop_lowsrh_7to9.2$HBA1C <- "7-9"
write.csv(prop_lowsrh_7to9.2, file = "./out/lowsrh_7to9_2.csv")

lowsrh_gt9.2 <- lapply(weighted_4y_list, subset, HBA1C > 9)
# calculate the proportion of low_srh
prop_lowsrh_gt9.2 <- do.call(
    rbind, lapply(lowsrh_gt9.2, calculate_svy_prop, "low_srh")
)
# add a column for HBA1C
prop_lowsrh_gt9.2$HBA1C <- ">9"
write.csv(prop_lowsrh_gt9.2, file = "./out/lowsrh_gt9_2.csv")


#######################################################
