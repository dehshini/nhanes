########### SRH analysis by age, weighted ##############
library(survey)

# create a new column for each dataframe in the list called high_srh.
# if the SRH is less than or equal to 3, set high_srh to 1, otherwise 0

for (i in 1:length(nhanes_list)) {
    nhanes_list[[i]]$high_srh <- ifelse(nhanes_list[[i]]$HSD010 <= 3, 1, 0)
}


# function to create survey design object

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
