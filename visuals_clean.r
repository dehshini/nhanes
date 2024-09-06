# create directory for figures/tables
# dir.create("figures", showWarnings = FALSE)

table1_labels <- list(
    RIDAGEYR ~ "Age (years), mean",
    AGEGROUP ~ "Age Group, %",
    FEMALE ~ "Female, %",
    EDULEVEL ~ "Education, %",
    insurance ~ "Health Insurance, %",
    RACE ~ "Race/Ethnicity, %",
    FAM_INCOME ~ "Family Income, %",
    BMXBMI ~ "BMI (kg/m2)",
    BMICAT ~ "BMI Category",
    current_smoker ~ "Current Smoker, self-reported or cotinine > 10ng/ml, %",
    HBA1C ~ "HbA1c (%), mean",
    HBA1C_CAT ~ "HbA1c Categories, %",
    diabetes_duration_cat ~ "Time since diabetes diagnosis, %",
    hypertension ~ "Hypertension, %",
    hypercholesterolemia ~ "Hypercholesterolemia",
    CVD ~ "Cardiovascular Disease",
    CKD ~ "Chronic Kidney Disease"
)

#### table 1 using low srh
table1low <- tbl_svysummary(
    analyze1,
    include = c(
        RIDAGEYR, AGEGROUP, FEMALE, RACE, FAM_INCOME, EDULEVEL,
        insurance,
        BMXBMI, BMICAT, current_smoker,
        HBA1C, HBA1C_CAT,
        diabetes_duration_cat,
        hypertension, hypercholesterolemia, CVD, CKD
    ),
    by = low_srh,
    statistic = list(
        all_continuous() ~ "{mean} ({mean.std.error})",
        all_categorical() ~ "{p} ({p.std.error})"
    ),
    digits = list(
        all_continuous() ~ c(1, 1),
        all_categorical() ~ c(1, 4),
        HBA1C ~ c(1, 2)
    ),
    missing = "ifany",
    missing_text = "Missing",
    missing_stat = "{p_miss}",
    label = table1_labels
)

table1low <- table1low %>% 
    add_overall(
        col_label = "Overall \nMean or % (SE)"
    ) %>%
    modify_header(
        stat_1 = "**High, N = {n} **",
        stat_2 = "**Low, N = {n} **"
    )
    modify_spanning_header(
        c("stat_1", "stat_2") ~ "**Self-Reported Health Status**"
    ) %>%
    add_p(
        test = list(
            all_continuous() ~ "svy.t.test",
            all_categorical() ~ "svy.chisq.test"
        )
    )

table1low %>%
    as_gt() %>% 
    tab_header(
        title = "Characteristics of US Adults with Diabetes,
                Overall and by Self-Reported Health Status."
    ) %>% 
    gt::gtsave("./figures/table1low0905.docx")


# END TABLE
#######################################################

####################
# PLOT CUSTOMIZATION

# Custom X-axis labels
xlabels <- c("01-02", "03-04", "05-06",
             "07-08", "09-10", "11-12",
             "13-14", "15-16", "17-18"
        )

# build a theme for the plot
theme_general <- theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = 20)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(plot.caption = element_text(hjust = 0.5)) +
    theme(axis.title = element_text(size = 15)) +
    theme(axis.text = element_text(size = 14)) +
    # add a legend
    # theme(legend.position = "top") +
    theme(legend.title = element_text(size = 13)) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.key.size = unit(1, "cm"))

# END PLOT CUSTOMIZATION
########################


#######################################################
# TREND GRAPH

# load srh summary 1
plot_data <- fread("./out/lowsrh_summary.csv")
head(plot_data)
str(plot_data)

# rename the first column
colnames(plot_data)[1] <- "cycle"



# Create the trends graph with error bars
ggplot(plot_data, aes(x = cycle, y = Proportion)) +
    geom_line(
        aes(group = 1),
        linewidth = 1.5,
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
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    ylim(0, 60) +
    annotate("text", x = 8, y = 60, label = paste("P trend = 0.0919"), size = 6)


# END TREND GRAPH
######################################################



######################################################
# TREND BY AGE GRAPH

# load the data
plot_gt65 <- fread("./out/lowsrh_gt65.csv")
plot_lt65 <- fread("./out/lowsrh_lt65.csv")

# rename the first column
colnames(plot_gt65)[1] <- "cycle"
colnames(plot_lt65)[1] <- "cycle"

head(plot_gt65)
head(plot_lt65)

# combine the data
ageplot <- rbind(plot_gt65, plot_lt65)
head(ageplot, 10)

# create the plot
ggplot(
    data = ageplot,
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
        title = "Trend of Low SRH Among US Adults 
            with Self-Reported Diabetes, by Age Group",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Age Group",
        linetype = "Age Group",
        shape = "Age Group"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 70),
    ) +
    scale_color_brewer(palette = "Dark2")


# END TREND BY AGE GRAPH
######################################################



######################################################
# TREND BY SEX GRAPH

#load the data
plot_male <- fread("./out/lowsrh_male.csv")
plot_female <- fread("./out/lowsrh_female.csv")

# rename the first column
colnames(plot_male)[1] <- "cycle"
colnames(plot_female)[1] <- "cycle"


# combine the data
sexplot <- rbind(plot_male, plot_female)
head(sexplot, 10)

# create the plot
ggplot(
    data = sexplot,
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
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Sex",
        linetype = "Sex",
        shape = "Sex"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 70)
    ) +
    scale_color_brewer(palette = "Set1")


# END TREND BY SEX GRAPH
######################################################



#################################
# TREND BY RACE AND ETHNICITY

# load the data
plot_white <- fread("./out/lowsrh_white.csv")
plot_black <- fread("./out/lowsrh_black.csv")
plot_hispanic <- fread("./out/lowsrh_hispanic.csv")


# rename the first column
colnames(plot_white)[1] <- "cycle"
colnames(plot_black)[1] <- "cycle"
colnames(plot_hispanic)[1] <- "cycle"


# combine the data
raceplot <- rbind(plot_white, plot_black, plot_hispanic)
head(raceplot, 10)


# create the plot
ggplot(
    data = raceplot,
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
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Ethnicity",
        linetype = "Ethnicity",
        shape = "Ethnicity"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 70)
    ) +
    scale_color_brewer(palette = "Set1")


# END TREND BY RACE AND ETHNICITY
######################################



#######################################################
# TREND GRAPH BY FAMILY INCOME/POVERTY
#######################################################


# load the data
plot_poverty <- fread("./out/lowsrh_poverty.csv")
plot_nonpoverty <- fread("./out/lowsrh_nonpoverty.csv")


# rename the first column
colnames(plot_poverty)[1] <- "cycle"
colnames(plot_nonpoverty)[1] <- "cycle"

# combine the data
povertyplot <- rbind(plot_poverty, plot_nonpoverty)
head(povertyplot, 10)


# create the plot
ggplot(
    data = povertyplot,
    aes(x = cycle, y = Proportion, color = Poverty)
) +
    geom_line(
        aes(group = Poverty, linetype = Poverty),
        linewidth = 1
    ) +
    geom_point(
        aes(color = Poverty, shape = Poverty),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Poverty,
            linetype = Poverty
        ),
        width = 0.3,
        position = position_dodge(width = 0.3)
    ) +
    labs(
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes",
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
        limits = c(0, 100)
    ) +
    scale_color_brewer(palette = "Set1")


# END TREND BY FAMILY INCOME/POVERTY
######################################



#######################################################
# TREND GRAPH BY INSURANCE
#######################################################


# load the data
plot_insurance <- fread("./out/lowsrh_noninsured.csv")
plot_insurance1 <- fread("./out/lowsrh_insured.csv")


# rename the first column
colnames(plot_insurance)[1] <- "cycle"
colnames(plot_insurance1)[1] <- "cycle"


# combine the data
insuranceplot <- rbind(plot_insurance, plot_insurance1)
head(insuranceplot, 10)


# create the plot
ggplot(
    data = insuranceplot,
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
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes",
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
    scale_color_brewer(palette = "Set1")


# END TREND BY INSURANCE
######################################


#######################################################
# TREND GRAPH BY EDUCATION
######################################################