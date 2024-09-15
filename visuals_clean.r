# create directory for figures/tables
dir.create("figures", showWarnings = FALSE)

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
) %>% 
add_overall(
    col_label = "Overall\nMean or % (SE)"
) %>%
modify_header(
    stat_1 = "**High**",
    stat_2 = "**Low**"
) %>%
add_p(
    test = list(
        all_continuous() ~ "svy.t.test",
        all_categorical() ~ "svy.chisq.test"
    )
)

table1low %>%
    as_gt() %>%
    tab_source_note(source_note = "Data Source:  NHANES 2001-2018") %>%
    tab_stubhead(label = "Self-Reported Health Status") %>%
    tab_spanner(
        label = "Self-Reported Health Status",
        columns = c(stat_1, stat_2)
    ) %>%
    tab_options(
        table.font.size = px(10),
        table.font.color = "black",
        table.border.bottom.style = "none",
        table.border.bottom.color = "black",
        table.border.top.color = "black",
        column_labels.border.bottom.color = "black"
    ) %>%
    tab_header(
        title = "Characteristics of US Adults with Diabetes,
                Overall and by Self-Reported Health Status."
    ) %>%
    gtsave("./figures/table1low0909.docx")


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
    theme(plot.title = element_text(size = 22)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(plot.caption = element_text(hjust = 0.5)) +
    theme(axis.title = element_text(size = 18)) +
    theme(axis.text = element_text(size = 18)) +
    # add a legend
    # theme(legend.position = "top") +
    theme(legend.title = element_text(size = 20)) +
    theme(legend.text = element_text(size = 20)) +
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
        title = "Trend of Low SRH Among US Adults with Self-Reported Diabetes",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    ylim(0, 60) +
    annotate("text", x = 8, y = 55, label = paste("P trend 0.0919"), size = 8)

# save the plot
#ggsave("./figures/trend_lowsrh.png", width = 10, height = 6)

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

# age P values
age_ptrend <- c("< 65 years, P trend 0.817", ">= 65 years, P trend 0.002")

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
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = age_ptrend) +
    scale_linetype_discrete(labels = age_ptrend) +
    scale_shape_discrete(labels = age_ptrend) +
    theme(legend.justification = c(0.5, 1), legend.position = c(0.8, 1))

# save the plot
ggsave("./figures/trend_lowsrh_age.png", width = 1536, height = 992, units = "px")


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

ptrend_sex <- c("Female, P trend 0.157", "Male, P trend 0.518")

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
        title = "Trend of Low SRH Among US Adults 
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
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 70)
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_color_discrete(labels = ptrend_sex) +
    scale_linetype_discrete(labels = ptrend_sex) +
    scale_shape_discrete(labels = ptrend_sex) +
    theme(legend.justification = c(0.5, 1), legend.position = c(0.8, 1))


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

race_ptrend <- c(
    "Non-Hispanic White, P trend 0.059",
    "Non-Hispanic Black, P trend 0.128", 
    "Hispanic, P trend 0.256"
)

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
        title = "Trend of Low SRH Among US Adults 
        with Self-Reported Diabetes, by Race/Ethnicity",
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
        limits = c(0, 100)
    ) +
    scale_color_brewer(palette = "Set1", breaks = c("White", "Black", "Hispanic")) +
    scale_color_discrete(labels = race_ptrend, breaks = c("White", "Black", "Hispanic")) +
    scale_linetype_discrete(labels = race_ptrend, breaks = c("White", "Black", "Hispanic")) +
    scale_shape_discrete(labels = race_ptrend, breaks = c("White", "Black", "Hispanic"))+  
    theme(legend.justification = c(0.5, 1), legend.position = c(0.8, 1))

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

ptrend_income <- c(
    "Above Poverty threshold, P trend 0.057", 
    "Below Poverty threshold, P trend 0.413"
)

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
        title = "Trend of Low SRH Among US Adults 
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
    theme(legend.justification = c(0.6, 1), legend.position = c(0.8, 1))


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
    # geom_smooth(
    #     method = "glm",
    #     se = FALSE,
    #     aes(group = Insurance, alpha = 0.2),
    #     linewidth = 0.7,
    #     show.legend = FALSE   # Set line thickness of regression lines
    # ) +
    labs(
        title = "Trend of Low SRH Among US Adults 
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
    theme(legend.justification = c(0.6, 1), legend.position = c(0.8, 1))


# END TREND BY INSURANCE
######################################


#######################################################
# TREND GRAPH BY EDUCATION
######################################################


# load the data
plot_education <- fread("./out/lowsrh_abovehighschool.csv")
plot_education1 <- fread("./out/lowsrh_belowhighschool.csv")
plot_education2 <- fread("./out/lowsrh_highschool.csv")

# rename the first column
colnames(plot_education)[1] <- "cycle"
colnames(plot_education1)[1] <- "cycle"
colnames(plot_education2)[1] <- "cycle"

# combine the data
educationplot <- rbind(plot_education, plot_education1, plot_education2)
head(educationplot, 10)

ptrend_education <- c(
    "Above High School, P trend 0.677", 
    "High School/equivalent, P trend 0.769", 
    "Less than High School, P trend 0.687"
)

# create the plot
ggplot(
    data = educationplot,
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
    # geom_smooth(
    #     method = "glm",
    #     se = FALSE,
    #     aes(group = Education, alpha = 0.5),
    #     linewidth = 0.7,
    #     show.legend = FALSE
    # ) +
    labs(
        title = "Trend of Low SRH Among US Adults 
        with Self-Reported Diabetes, by Education",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Education",
        linetype = "Education",
        shape = "Education"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 100)
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_color_discrete(labels = ptrend_education) +
    scale_linetype_discrete(labels = ptrend_education) +
    scale_shape_discrete(labels = ptrend_education) +
    theme(legend.justification = c(0.6, 1), legend.position = c(0.8, 1))


# END TREND BY EDUCATION
######################################


#######################################################
# TREND GRAPH BY HBA1C
######################################################


# load the data
plot_hba1c <- fread("./out/lowsrh_lt7.csv")
plot_hba1c1 <- fread("./out/lowsrh_7to9.csv")
plot_hba1c2 <- fread("./out/lowsrh_gt9.csv")

# rename the first column
colnames(plot_hba1c)[1] <- "cycle"
colnames(plot_hba1c1)[1] <- "cycle"
colnames(plot_hba1c2)[1] <- "cycle"

# combine the data
hba1cplot <- rbind(plot_hba1c, plot_hba1c1, plot_hba1c2)
head(hba1cplot, 10)

ptrend_hba1c <- c(
    "HBA1C < 7, P trend 0.031",
    "HBA1C 7-9, P trend 0.291",
    "HBA1C > 9, P trend 0.186"
)

# create the plot
ggplot(
    data = hba1cplot,
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
    # geom_smooth(
    #     method = "glm",
    #     se = FALSE,
    #     aes(group = HBA1C, alpha = 0.5),
    #     linewidth = 0.7,
    #     show.legend = FALSE
    # ) +
    labs(
        title = "Trend of Low SRH Among US Adults 
        with Self-Reported Diabetes, by HBA1C",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "HBA1C",
        linetype = "HBA1C",
        shape = "HBA1C"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 85)
    ) +
    scale_color_brewer(palette = "Set1", breaks = c("<7", "7-9", ">9")) +
    scale_color_discrete(labels = ptrend_hba1c, breaks = c("<7", "7-9", ">9")) +
    scale_linetype_discrete(labels = ptrend_hba1c, breaks = c("<7", "7-9", ">9")) +
    scale_shape_discrete(labels = ptrend_hba1c, breaks = c("<7", "7-9", ">9")) +
    theme(legend.justification = c(0.5, 1), legend.position = c(0.8, 1))
