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
        message(paste("4-year weight (WTMEC4YR) already exists for cycle:", cycle_name))
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
# TREND GRAPH
######################################################

xlabels.2 <- c(
    "2001-2004",
    "2005-2008",
    "2009-2012",
    "2013-2016",
    "2017-2018"
)

# load srh summary data
srh_summary.2 <- fread("./out/lowsrh_summary2.csv")

# rename the first column
colnames(srh_summary.2)[1] <- "cycle"

# create the plot
ggplot(
    data = srh_summary.2,
    aes(x = cycle, y = Proportion)
) +
    geom_line(
        aes(group = 1),
        linewidth = 1
    ) +
    geom_point(
        size = 3.5
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI
        ),
        width = 0.1
    ) +
    scale_x_discrete(
        label = xlabels.2
    ) +
    scale_y_continuous(
        limits = c(0, 60)
    ) +
    labs(
        x = "Cycle",
        y = "Proportion (%)",
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes"
    ) +
    theme_general





######################################################
# TREND BY AGE GRAPH

plot_gt65.2 <- fread("./out/lowsrh_gt65_2.csv")
plot_lt65.2 <- fread("./out/lowsrh_lt65_2.csv")

# rename the first column
colnames(plot_gt65.2)[1] <- "cycle"
colnames(plot_lt65.2)[1] <- "cycle"

# combine the data
ageplot.2 <- rbind(plot_gt65.2, plot_lt65.2)

# age P values
age_ptrend.2 <- c("< 65 years, P trend ", ">= 65 years, P trend ")

# create the plot
ggplot(
    data = ageplot.2,
    aes(x = cycle, y = Proportion, color = Age)
) +
    geom_line(
        aes(group = Age, linetype = Age),
        linewidth = 1
    ) +
    geom_point(
        aes(color = Age, shape = Age),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Age,
            linetype = Age
        ),
        width = 0.3,
        position = position_dodge(width = 0.3)
    ) +
    labs(
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes, by Age Group",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Age Group",
        linetype = "Age Group",
        shape = "Age Group"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels.2) +
    ylim(0, 60) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = age_ptrend.2) +
    scale_linetype_discrete(labels = age_ptrend.2) +
    scale_shape_discrete(labels = age_ptrend.2) +
    theme(
        legend.position = c(0.8, 0.8) # Position inside the plot area
    )

# END TREND BY AGE GRAPH
######################################################



######################################################
# TREND BY SEX GRAPH

# load the data
plot_male.2 <- fread("./out/lowsrh_male_2.csv")
plot_female.2 <- fread("./out/lowsrh_female_2.csv")

# rename the first column
colnames(plot_male.2)[1] <- "cycle"
colnames(plot_female.2)[1] <- "cycle"

# combine the data
sexplot.2 <- rbind(plot_male.2, plot_female.2)

# sex P values
sex_ptrend.2 <- c("Female, P trend ", "Male, P trend ")

# create the plot
ggplot(
    data = sexplot.2,
    aes(x = cycle, y = Proportion, color = Sex)
) +
    geom_line(
        aes(group = Sex, linetype = Sex),
        linewidth = 1
    ) +
    geom_point(
        aes(color = Sex, shape = Sex),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Sex,
            linetype = Sex
        ),
        width = 0.3,
        position = position_dodge(width = 0.3)
    ) +
    labs(
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes, by Sex",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Sex",
        linetype = "Sex",
        shape = "Sex"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 60)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = sex_ptrend.2) +
    scale_linetype_discrete(labels = sex_ptrend.2) +
    scale_shape_discrete(labels = sex_ptrend.2) +
    theme(
        legend.position = c(0.8, 0.9) # Position inside the plot area
    )

# END TREND BY SEX GRAPH
######################################################


#################################
# TREND BY RACE AND ETHNICITY

# load the data
plot_white.2 <- fread("./out/lowsrh_white_2.csv")
plot_black.2 <- fread("./out/lowsrh_black_2.csv")
plot_hispanic.2 <- fread("./out/lowsrh_hispanic_2.csv")


# rename the first column
colnames(plot_white.2)[1] <- "cycle"
colnames(plot_black.2)[1] <- "cycle"
colnames(plot_hispanic.2)[1] <- "cycle"

# combine the data
raceplot.2 <- rbind(plot_white.2, plot_black.2, plot_hispanic.2)
head(raceplot.2, 10)


# race P values
race_ptrend.2 <- c("NH Black, P trend ", "Hispanic, P trend ", "NH White, P trend ")

# create the plot
ggplot(
    data = raceplot.2,
    aes(x = cycle, y = Proportion, color = Ethnicity)
) +
    geom_line(
        aes(group = Ethnicity, linetype = Ethnicity),
        linewidth = 1
    ) +
    geom_point(
        aes(color = Ethnicity, shape = Ethnicity),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Ethnicity,
            linetype = Ethnicity
        ),
        width = 0.3,
        position = position_dodge(width = 0.3)
    ) +
    labs(
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes, by Race/Ethnicity",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Ethnicity",
        linetype = "Ethnicity",
        shape = "Ethnicity"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 80)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = race_ptrend.2) +
    scale_linetype_discrete(labels = race_ptrend.2) +
    scale_shape_discrete(labels = race_ptrend.2) +
    theme(
        legend.position = c(0.8, 0.9) # Position inside the plot area
    )


# END TREND BY RACE AND ETHNICITY
######################################################



#######################################################
# TREND GRAPH BY FAMILY INCOME/POVERTY
#######################################################


# load the data
plot_poverty.2 <- fread("./out/lowsrh_poverty_2.csv")
plot_nonpoverty.2 <- fread("./out/lowsrh_no_poverty_2.csv")

# rename the first column
colnames(plot_poverty.2)[1] <- "cycle"
colnames(plot_nonpoverty.2)[1] <- "cycle"

# combine the data
incomeplot.2 <- rbind(plot_poverty.2, plot_nonpoverty.2)
head(incomeplot.2, 10)


# income P values
income_ptrend.2 <- c("Above Poverty threshold, P trend ", "Below Poverty threshold, P trend ")

# create the plot
ggplot(
    data = incomeplot.2,
    aes(x = cycle, y = Proportion, color = Income)
) +
    geom_line(
        aes(group = Income, linetype = Income),
        linewidth = 1
    ) +
    geom_point(
        aes(color = Income, shape = Income),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Income,
            linetype = Income
        ),
        width = 0.3,
        position = position_dodge(width = 0.3)
    ) +
    labs(
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes, by Family Income",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Family Income",
        linetype = "Family Income",
        shape = "Family Income"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = income_ptrend.2) +
    scale_linetype_discrete(labels = income_ptrend.2) +
    scale_shape_discrete(labels = income_ptrend.2) +
    theme(
        legend.position = c(0.8, 0.9) # Position inside the plot area
    )

#######################################################
# TREND GRAPH BY INSURANCE
#######################################################


# load the data
plot_insurance.2 <- fread("./out/lowsrh_insured_2.csv")
plot_uninsured.2 <- fread("./out/lowsrh_uninsured_2.csv")


# rename the first column
colnames(plot_insurance.2)[1] <- "cycle"
colnames(plot_uninsured.2)[1] <- "cycle"

# combine the data
insuranceplot.2 <- rbind(plot_insurance.2, plot_uninsured.2)
head(insuranceplot.2, 10)

# insurance P values
insurance_ptrend.2 <- c(
    "Uninsured, P trend ",
    "Insured (Private/Public), P trend "
)

# create the plot
ggplot(
    data = insuranceplot.2,
    aes(x = cycle, y = Proportion, color = Insurance)
) +
    geom_line(
        aes(group = Insurance, linetype = Insurance),
        linewidth = 1
    ) +
    geom_point(
        aes(color = Insurance, shape = Insurance),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Insurance,
            linetype = Insurance
        ),
        width = 0.3,
        position = position_dodge(width = 0.3)
    ) +
    labs(
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes, by Insurance",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Insurance",
        linetype = "Insurance",
        shape = "Insurance"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = insurance_ptrend.2) +
    scale_linetype_discrete(labels = insurance_ptrend.2) +
    scale_shape_discrete(labels = insurance_ptrend.2) +
    theme(
        legend.position = c(0.8, 0.9) # Position inside the plot area
    )

# END
#######################################################


#######################################################
# TREND GRAPH BY EDUCATION


# load the data
plot_education.2 <- fread("./out/lowsrh_highschool_2.csv")
plot_education1.2 <- fread("./out/lowsrh_belowhighschool_2.csv")
plot_education2.2 <- fread("./out/lowsrh_college_2.csv")

# rename the first column
colnames(plot_education.2)[1] <- "cycle"
colnames(plot_education1.2)[1] <- "cycle"
colnames(plot_education2.2)[1] <- "cycle"

# combine the data
educationplot.2 <- rbind(plot_education.2, plot_education1.2, plot_education2.2)
head(educationplot, 10)

ptrend_education.2 <- c(
    "Less than High School, P trend ",
    "Above High School, P trend ",
    "High School/equivalent, P trend "
)

# create the plot
ggplot(
    data = educationplot.2,
    aes(x = cycle, y = Proportion, color = Education)
) +
    geom_line(
        aes(group = Education, linetype = Education),
        linewidth = 1
    ) +
    geom_point(
        aes(color = Education, shape = Education),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Education,
            linetype = Education
        ),
        width = 0.3,
        position = position_dodge(width = 0.3)
    ) +
    labs(
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes, by Education",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Education",
        linetype = "Education",
        shape = "Education"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = ptrend_education.2) +
    scale_linetype_discrete(labels = ptrend_education.2) +
    scale_shape_discrete(labels = ptrend_education.2) +
    theme(
        legend.position = c(0.8, 0.9) # Position inside the plot area
    )


# END
#######################################################

#######################################################
# TREND GRAPH BY HBA1C

# load the data
plot_hba1c1.2 <- fread("./out/lowsrh_lt7_2.csv")
plot_hba1c2.2 <- fread("./out/lowsrh_7to9_2.csv")
plot_hba1c3.2 <- fread("./out/lowsrh_gt9_2.csv")

# rename the first column
colnames(plot_hba1c1.2)[1] <- "cycle"
colnames(plot_hba1c2.2)[1] <- "cycle"
colnames(plot_hba1c3.2)[1] <- "cycle"

# combine the data
hba1cplot.2 <- rbind(plot_hba1c1.2, plot_hba1c2.2, plot_hba1c3.2)
head(hba1cplot.2, 10)

ptrend_hba1c.2 <- c(
    "HBA1C < 7, P trend 0.031",
    "HBA1C > 9, P trend 0.186",
    "HBA1C 7-9, P trend 0.291"
)

# create the plot
ggplot(
    data = hba1cplot.2,
    aes(x = cycle, y = Proportion, color = HBA1C)
) +
    geom_line(
        aes(group = HBA1C, linetype = HBA1C),
        linewidth = 1
    ) +
    geom_point(
        aes(color = HBA1C, shape = HBA1C),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = HBA1C,
            linetype = HBA1C
        ),
        width = 0.3,
        position = position_dodge(width = 0.3)
    ) +
    labs(
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes, by HBA1C",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "HBA1C",
        linetype = "HBA1C",
        shape = "HBA1C"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = ptrend_hba1c.2) +
    scale_linetype_discrete(labels = ptrend_hba1c.2) +
    scale_shape_discrete(labels = ptrend_hba1c.2) +
    theme(legend.justification = c(0.5, 1), legend.position = c(0.8, 1))


###################################################
## REGRESSION ANALYSIS
#################################################

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


# Function to perform weighted chi-squared test for two specific cycles over any variable
perform_survey_chisq <- function(
    survey_design, 
    cycle1, 
    cycle2, 
    subset_condition, 
    test_variable
) {
    # Subset the survey design object based on the cycles and additional conditions
    subset_design <- subset(survey_design, cycle %in% c(cycle1, cycle2) & subset_condition)

    # Create a formula dynamically based on the test variable
    formula <- as.formula(paste0("~cycle + ", test_variable))

    # Perform the chi-squared test
    chisq_test <- svychisq(formula, design = subset_design)

    # Return the chi-squared test result
    return(chisq_test)
}


?svychisq

