############################################################
# 4 YEAR PLOTS
############################################################


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
    theme_general +
    annotate(
        geom = "text",
        x = 5,
        y = 60,
        label = paste("P = ", round(p_trend.2[[2, 5]], 3)),
        size = 8
    )



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
g_age.2 <- ggplot(
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
g_sex.2 <- ggplot(
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
g_race.2 <- ggplot(
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


