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
# adults aged 20 and over with self-reported diabetes, and have data on srh
analyze1 <- subset(
    weighted_nhanes_all, 
    RIDAGEYR >= 20 & DIQ010 == 1 & !is.na(HSD010)
)

# Initialize an empty list to store the results
results_list <- list()
#### summary of proportions
for (cycle in unique(nhanes_all$SDDSRVYR)) {
    cycle_design <- subset(analyze1, SDDSRVYR == cycle)
    prop_data <- calculate_svy_prop(cycle_design, "low_srh")
    prop_data$Cycle <- cycle # Add cycle information
    results_list[[as.character(cycle)]] <- prop_data
}
# Combine the results into a single dataframe
lowsrh_summary <- rbindlist(results_list, use.names = TRUE, fill = TRUE)

# rename cycles and save to csv
lowsrh_summary$Cycle <- c("01-02", "03-04", "05-06", "07-08", "09-10", "11-12", "13-14", "15-16", "17-18")
write.csv(, file = "./out/lowsrh_summary.csv")


# Race
results_list <- list()
# Loop through each race/ethnicity group and apply the calculate_svy_prop function
for (race in unique(nhanes_all$RACE)) {
    race_design <- subset(analyze1, RACE == race)
    prop_data <- calculate_svy_prop(race_design, "low_srh")
    prop_data$Race_Ethnicity <- race # Add race/ethnicity information
    results_list[[as.character(race)]] <- prop_data
}
# Combine the results into a single dataframe
race_summary <- rbindlist(results_list, use.names = TRUE, fill = TRUE)

