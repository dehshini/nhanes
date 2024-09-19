#######################################################
# TREND GRAPH

# load srh summary 2
plot_data <- fread("./out/proportion_highsrh_weighted.csv")
head(plot_data)

# rename the first column
colnames(plot_data)[1] <- "cycle"

# create the plot
ggplot(plot_data, aes(x = cycle, y = Proportion)) +
    geom_line(
        aes(group = 1),
        linewidth = 1,
        color = "#000000"
    ) + # Line for trend
    geom_point(
        color = "#2d03ff",
        size = 3.5
    ) + # Points for each cycle
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI
        ),
        width = 0.1,
        color = "#999999",
        alpha = 0.7
    ) + # Error bars
    labs(
        title = "Trend of high SRH Among US Adults with Self-Reported Diabetes",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    ylim(0, 80) +
    annotate(
        geom = "text",
        x = 8, y = 55,
        label = paste("P trend", round(p_trend[[2, 5]], 3)),
        size = 8
    )

######################################################
# TREND BY AGE GRAPH

# load the data
plot_gt65 <- fread("./out/highsrh_gt65.csv")
plot_lt65 <- fread("./out/highsrh_lt65.csv")
# rename the first column
colnames(plot_gt65)[1] <- "cycle"
colnames(plot_lt65)[1] <- "cycle"

# combine the data
ageplot <- rbind(plot_gt65, plot_lt65)

# add the p values
ptrend_age <- c(
    "<65 P trend 0.817",
    ">=65 P trend 0.002"
)

# create the plot
ggplot(
    ageplot,
    aes(x = cycle, y = Proportion, color = Age)
) +
    geom_line(
        aes(group = Age, linetype = Age),
        linewidth = 1,
        position = position_dodge(width = 0.2)
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
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of High SRH Among US Adults
        with Self-Reported Diabetes, by Age",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Age",
        linetype = "Age",
        shape = "Age"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_y_continuous(
        breaks = seq(0, 90, 20),
        limits = c(0, 90)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = ptrend_age) +
    scale_linetype_discrete(labels = ptrend_age) +
    scale_shape_discrete(labels = ptrend_age) +
    theme(legend.position = c(0.8, 0.2))


# by sex
plot_male <- fread("./out/highsrh_male.csv")
plot_female <- fread("./out/highsrh_female.csv")

# rename the first column
colnames(plot_male)[1] <- "cycle"
colnames(plot_female)[1] <- "cycle"

# combine the data
sexplot <- rbind(plot_male, plot_female)    

ptrend_sex <- c("Female, P trend 0.157", "Male, P trend 0.518")

# create the plot
ggplot(
    data = sexplot,
    aes(x = cycle, y = Proportion, color = Sex)
) +
    geom_line(
        aes(group = Sex, linetype = Sex),
        linewidth = 1,
        position = position_dodge(width = 0.2)
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
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of High SRH Among US Adults
        with Self-Reported Diabetes, by Sex",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Sex",
        linetype = "Sex",
        shape = "Sex"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_y_continuous(
        breaks = seq(0, 100, 20),
        limits = c(20, 100)
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_color_discrete(labels = ptrend_sex) +
    scale_linetype_discrete(labels = ptrend_sex) +
    scale_shape_discrete(labels = ptrend_sex) +
    theme(legend.justification = c(0.5, 0.1), legend.position = c(0.8, 0.1))


# by race

# load the data
plot_white <- fread("./out/highsrh_white.csv")
plot_black <- fread("./out/highsrh_black.csv")
plot_hispanic <- fread("./out/highsrh_hispanic.csv")

# rename the first column
colnames(plot_white)[1] <- "cycle"
colnames(plot_black)[1] <- "cycle"
colnames(plot_hispanic)[1] <- "cycle"

# combine the data
raceplot <- rbind(plot_white, plot_black, plot_hispanic)
head(raceplot, 10)
ptrend_race <- c(
    "Non-Hispanic White, P trend 0.059",
    "Non-Hispanic Black, P trend 0.128",
    "Hispanic, P trend 0.256"
)

# create the plot
ggplot(
    data = raceplot,
    aes(x = cycle, y = Proportion, color = Race)
) +
    geom_line(
        aes(group = Race, linetype = Race),
        linewidth = 1,
        position = position_dodge(width = 0.2)
    ) +
    geom_point(
        aes(color = Race, shape = Race),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Race,
            linetype = Race
        ),
        width = 0.3,
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of High SRH Among US Adults
        with Self-Reported Diabetes, by Race/Ethnicity",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Race",
        linetype = "Race",
        shape = "Race"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_y_continuous(
        breaks = seq(0, 100, 20),
        limits = c(0, 100)
    ) +
    # scale_color_brewer(palette = "Set1", breaks = c("NH White", "NH Black", "Hispanic")) +
    # scale_color_discrete(labels = ptrend_race, breaks = c("NH White", "Hispanic", "NH Black")) +
    # scale_linetype_discrete(labels = ptrend_race, breaks = c("NH White", "NH Black", "Hispanic")) +
    # scale_shape_discrete(labels = ptrend_race, breaks = c("NH White", "NH Black", "Hispanic")) +
    theme(legend.position = c(0.7, 0.2))


# by family income
# load the data
plot_poverty <- fread("./out/highsrh_poverty.csv")
plot_nonpoverty <- fread("./out/highsrh_nonpoverty.csv")


# rename the first column
colnames(plot_poverty)[1] <- "cycle"
colnames(plot_nonpoverty)[1] <- "cycle"

# combine the data
povertyplot <- rbind(plot_poverty, plot_nonpoverty)
head(povertyplot, 10)

ptrend_income <- c(
    "Above Poverty threshold, P trend 0.057",
    "Below Poverty threshold, P trend 0.413"
)

# create the plot
ggplot(
    data = povertyplot,
    aes(x = cycle, y = Proportion, color = Income)
) +
    geom_line(
        aes(group = Income, linetype = Income),
        linewidth = 1,
        position = position_dodge(width = 0.2)
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
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of High SRH Among US Adults
        with Self-Reported Diabetes, by Family Income",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Family Income",
        linetype = "Family Income",
        shape = "Family Income"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 90)
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_color_discrete(labels = ptrend_income) +
    scale_linetype_discrete(labels = ptrend_income) +
    scale_shape_discrete(labels = ptrend_income) +
    theme(legend.justification = c(0.6, 1), legend.position = c(0.8, 0.2))



# by insurance

# load the data
plot_insurance <- fread("./out/highsrh_noninsured.csv")
plot_insurance1 <- fread("./out/highsrh_insured.csv")


# rename the first column
colnames(plot_insurance)[1] <- "cycle"
colnames(plot_insurance1)[1] <- "cycle"


# combine the data
insuranceplot <- rbind(plot_insurance, plot_insurance1)
head(insuranceplot, 10)


ptrend_insurance <- c(
    "Insured (Private/Public), P trend 0.055",
    "Uninsured, P trend 0.281"
)

# create the plot
ggplot(
    data = insuranceplot,
    aes(x = cycle, y = Proportion, color = Insurance)
) +
    geom_line(
        aes(group = Insurance, linetype = Insurance),
        linewidth = 1,
        position = position_dodge(width = 0.2)
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
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of High SRH Among US Adults
        with Self-Reported Diabetes, by Insurance Status",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Insurance",
        linetype = "Insurance",
        shape = "Insurance"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 100)
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_color_discrete(labels = ptrend_insurance) +
    scale_linetype_discrete(labels = ptrend_insurance) +
    scale_shape_discrete(labels = ptrend_insurance) +
    theme(legend.position = c(0.7, 0.15))











############################################################
# 4 YEAR PLOTS
############################################################

# by age group

# load the data
plot_gt65.2 <- fread("./out/highsrh_gt65_2.csv")
plot_lt65.2 <- fread("./out/highsrh_lt65_2.csv")

# rename the first column
colnames(plot_gt65.2)[1] <- "cycle"
colnames(plot_lt65.2)[1] <- "cycle"

# combine the data
ageplot.2 <- rbind(plot_gt65.2, plot_lt65.2)

# age P values
age_ptrend.2 <- c("< 65 years, P trend 0.821", ">= 65 years, P trend 0.002")

# create the plot
ggplot(
    data = ageplot.2,
    aes(x = cycle, y = Proportion, color = Age)
) +
    geom_line(
        aes(group = Age, linetype = Age),
        linewidth = 1,
        position = position_dodge(width = 0.2)
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
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of high SRH Among US Adults with Self-Reported Diabetes, by Age Group",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Age Group",
        linetype = "Age Group",
        shape = "Age Group",
    ) +
    theme_general +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        breaks = seq(0, 100, 20),
        limits = c(0, 80)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = age_ptrend.2) +
    scale_linetype_discrete(labels = age_ptrend.2) +
    scale_shape_discrete(labels = age_ptrend.2) +
    theme(
        legend.position = c(0.8, 0.2) # Position inside the plot area
    )

# END TREND BY AGE GRAPH
######################################################


######################################################
# TREND BY SEX GRAPH

# load the data
plot_male.2 <- fread("./out/highsrh_male_2.csv")
plot_female.2 <- fread("./out/highsrh_female_2.csv")

# rename the first column
colnames(plot_male.2)[1] <- "cycle"
colnames(plot_female.2)[1] <- "cycle"

# combine the data
sexplot.2 <- rbind(plot_male.2, plot_female.2)

# sex P values
sex_ptrend.2 <- c("Female, P trend 0.168", "Male, P trend 0.509")

# create the plot
ggplot(
    data = sexplot.2,
    aes(x = cycle, y = Proportion, color = Sex)
) +
    geom_line(
        aes(group = Sex, linetype = Sex),
        linewidth = 1,
        position = position_dodge(width = 0.2)
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
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of high SRH Among US Adults with Self-Reported Diabetes, by Sex",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Sex",
        linetype = "Sex",
        shape = "Sex"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(20, 80)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = sex_ptrend.2) +
    scale_linetype_discrete(labels = sex_ptrend.2) +
    scale_shape_discrete(labels = sex_ptrend.2) +
    theme(
        legend.position = c(0.8, 0.2) # Position inside the plot area
    )

# END TREND BY SEX GRAPH
######################################################


#################################
# TREND BY RACE AND ETHNICITY

# load the data
plot_white.2 <- fread("./out/highsrh_white_2.csv")
plot_black.2 <- fread("./out/highsrh_black_2.csv")
plot_hispanic.2 <- fread("./out/highsrh_hispanic_2.csv")


# rename the first column
colnames(plot_white.2)[1] <- "cycle"
colnames(plot_black.2)[1] <- "cycle"
colnames(plot_hispanic.2)[1] <- "cycle"

# combine the data
raceplot.2 <- rbind(plot_white.2, plot_black.2, plot_hispanic.2)
head(raceplot.2, 10)


# race P values
ptrend_race.2 <- c("NH Black, P trend 0.120", "Hispanic, P trend 0.260", "NH White, P trend 0.058")

# create the plot
ggplot(
    data = raceplot.2,
    aes(x = cycle, y = Proportion, color = Ethnicity)
) +
    geom_line(
        aes(group = Ethnicity, linetype = Ethnicity),
        linewidth = 1,
        position = position_dodge(width = 0.2)
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
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of high SRH Among US Adults with Self-Reported Diabetes, by Race/Ethnicity",
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
    scale_color_discrete(labels = ptrend_race.2) +
    scale_linetype_discrete(labels = ptrend_race.2) +
    scale_shape_discrete(labels = ptrend_race.2) +
    theme(
        legend.position = c(0.8, 0.2) # Position inside the plot area
    )



#######################################################
# TREND GRAPH BY FAMILY INCOME/POVERTY
#######################################################


# load the data
plot_poverty.2 <- fread("./out/highsrh_poverty_2.csv")
plot_nonpoverty.2 <- fread("./out/highsrh_no_poverty_2.csv")

# rename the first column
colnames(plot_poverty.2)[1] <- "cycle"
colnames(plot_nonpoverty.2)[1] <- "cycle"

# combine the data
incomeplot.2 <- rbind(plot_poverty.2, plot_nonpoverty.2)
head(incomeplot.2, 10)


# income P values
income_ptrend.2 <- c("Above Poverty threshold, P trend 0.062", "Below Poverty threshold, P trend 0.414")

# create the plot
ggplot(
    data = incomeplot.2,
    aes(x = cycle, y = Proportion, color = Income)
) +
    geom_line(
        aes(group = Income, linetype = Income),
        linewidth = 1,
        position = position_dodge(width = 0.2)
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
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of high SRH Among US Adults with Self-Reported Diabetes, by Family Income",
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
        legend.position = c(0.7, 0.2) # Position inside the plot area
    )




#######################################################
# TREND GRAPH BY INSURANCE
#######################################################


# load the data
plot_insurance.2 <- fread("./out/highsrh_insured_2.csv")
plot_uninsured.2 <- fread("./out/highsrh_uninsured_2.csv")


# rename the first column
colnames(plot_insurance.2)[1] <- "cycle"
colnames(plot_uninsured.2)[1] <- "cycle"

# combine the data
insuranceplot.2 <- rbind(plot_insurance.2, plot_uninsured.2)
head(insuranceplot.2, 10)

# insurance P values
insurance_ptrend.2 <- c(
    "Uninsured, P trend 0.259",
    "Insured (Private/Public), P trend 0.054"
)

# create the plot
ggplot(
    data = insuranceplot.2,
    aes(x = cycle, y = Proportion, color = Insurance)
) +
    geom_line(
        aes(group = Insurance, linetype = Insurance),
        linewidth = 1,
        position = position_dodge(width = 0.2)
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
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of high SRH Among US Adults
        with Self-Reported Diabetes, by Insurance",
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
        legend.position = c(0.7, 0.1) # Position inside the plot area
    )

# END
#######################################################




#######################################################
# TREND GRAPH BY EDUCATION


# load the data
plot_education.2 <- fread("./out/highsrh_highschool_2.csv")
plot_education1.2 <- fread("./out/highsrh_belowhighschool_2.csv")
plot_education2.2 <- fread("./out/highsrh_college_2.csv")

# rename the first column
colnames(plot_education.2)[1] <- "cycle"
colnames(plot_education1.2)[1] <- "cycle"
colnames(plot_education2.2)[1] <- "cycle"

# combine the data
educationplot.2 <- rbind(plot_education.2, plot_education1.2, plot_education2.2)
head(educationplot.2, 10)

ptrend_education.2 <- c(
    "Less than High School, P trend 0.783",
    "Above High School, P trend 0.662",
    "High School/equivalent, P trend 0.736"
)

# create the plot
ggplot(
    data = educationplot.2,
    aes(x = cycle, y = Proportion, color = Education)
) +
    geom_line(
        aes(group = Education, linetype = Education),
        linewidth = 1,
        position = position_dodge(width = 0.2)
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
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of high SRH Among US Adults
        with Self-Reported Diabetes, by Education",
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
        legend.position = c(0.7, 0.15) # Position inside the plot area
    )


# END
#######################################################



#######################################################
# TREND GRAPH BY HBA1C

# load the data
plot_hba1c1.2 <- fread("./out/highsrh_lt7_2.csv")
plot_hba1c2.2 <- fread("./out/highsrh_7to9_2.csv")
plot_hba1c3.2 <- fread("./out/highsrh_gt9_2.csv")

# rename the first column
colnames(plot_hba1c1.2)[1] <- "cycle"
colnames(plot_hba1c2.2)[1] <- "cycle"
colnames(plot_hba1c3.2)[1] <- "cycle"

# combine the data
hba1cplot.2 <- rbind(plot_hba1c1.2, plot_hba1c2.2, plot_hba1c3.2)
head(hba1cplot.2, 10)

ptrend_hba1c.2 <- c(
    "HBA1C < 7, P trend 0.036",
    "HBA1C > 9, P trend 0.169",
    "HBA1C 7-9, P trend 0.280"
)

# create the plot
ggplot(
    data = hba1cplot.2,
    aes(x = cycle, y = Proportion, color = HBA1C)
) +
    geom_line(
        aes(group = HBA1C, linetype = HBA1C),
        linewidth = 1,
        position = position_dodge(width = 0.2)
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
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "Trend of high SRH Among US Adults with Self-Reported Diabetes, by HBA1C",
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
    theme(legend.position = c(0.7, 0.2))


