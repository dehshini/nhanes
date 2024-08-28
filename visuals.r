# create directory for figures/tables
# dir.create("figures_tables", showWarnings = FALSE)


## TABLE
#############################################################################
# create table label names
# table1labels <- list(
#     RIDAGEYR ~ "Age (years)",
#     AGE65 ~ "Age >= 65",
#     RIAGENDR ~ "Gender",
#     LBDTCSI ~ "Total Cholesterol (mmol/L)",
#     LBDTRSI ~ "Triglycerides (mmol/L)",
#     LBDLDLSI ~ "Low Density Lipoprotein Cholesterol (mmol/L)",
#     BMXBMI ~ "Body Mass Index (kg/m2)",
#     BMICAT ~ "Body Mass Index Category",
#     MARITAL ~ "Marital Status",
#     EDULEVEL ~ "Education Level",
#     RACE ~ "Race",
#     SBP ~ "Systolic Blood Pressure (mmHg)",
#     DBP ~ "Diastolic Blood Pressure (mmHg)",
#     LBXGH ~ "Glycated Hemoglobin (%)",
#     # LBXGLUSI ~ "Two hour Glucose (OGTT)"
#     # LBDGLUSI ~ "fasting glucose",
#     SMOKE ~ "Smoking Status",
#     DIAB_DUR ~ "Duration of Diabetes (years)",
#     RETINOPATHY ~ "Retinopathy",
#     ASCVD ~ "ASCVD",
#     HEART_FAIL ~ "Heart Failure",
#     CKD ~ "CKD",
#     PIR ~ "Poverty Income Ratio"
# )

#NEW TABLE1 LABELS
newtable1_labels <- list(
    RIDAGEYR ~ "Age (years), mean",
    AGEGROUP ~ "Age Group, %",
    FEMALE ~ "Female, %",
    EDULEVEL ~ "Education, %",
    INSURANCE ~ "Health Insurance, %",
    RACE ~ "Race/Ethnicity, %",
    FAM_INCOME ~ "Family Income, %",
    BMXBMI ~ "BMI (kg/m2)",
    BMICAT ~ "BMI Category",
    current_smoker ~ "Current Smoker, self-reported or cotinine > 10ng/ml, %",
    HBA1C ~ "HbA1c (%), mean",
    HBA1C_CAT ~ "HbA1c Categories, %",
    DIAB_DUR_CAT ~ "Time since diabetes diagnosis, %",
    hypertension ~ "Hypertension, %",
    hypercholesterolemia ~ "Hypercholesterolemia",
    CVD ~ "Cardiovascular Disease",
    CKD ~ "Chronic Kidney Disease"
)



## create table
# table1 <- tbl_svysummary(
#     analyze1,
#     include = c(
#         RIDAGEYR, AGE65, RIAGENDR, RACE, MARITAL, EDULEVEL,
#         SMOKE, LBDTCSI, LBDTRSI, LBDLDLSI, BMXBMI, BMICAT,
#         SBP, DBP, LBXGH, SMOKE, 
#         DIAB_DUR, 
#         RETINOPATHY, ASCVD, HEART_FAIL, CKD, PIR
#     ),
#     by = high_srh,
#     statistic = list(
#         all_continuous() ~ "{mean} ({mean.std.error})",
#         all_categorical() ~ "{p}% ({p.std.error})",
#         DIAB_DUR ~ "{median} ({p25}, {p75})"
#     ),
#     digits = list(
#         all_continuous() ~ c(1, 1),
#         all_categorical() ~ c(1, 2)
#     ),
#     missing = "no",
#     missing_text = "Missing",
#     # missing_stat = "{N_obs}",
#     label = table1labels
# ) %>%
#     add_overall(
#         col_label = "**Overall**, N = {N_unweighted}"
#     ) %>%
#     modify_header(
#         label = "**Variable**",
#         all_stat_cols() ~ "**N = {n_unweighted}**"
#     ) %>%
#     #add_stat_label(
#     #    location = "column"
#     #) %>%
#     modify_spanning_header(
#         c("stat_1", "stat_2") ~ "**High SRH**"
#     ) %>%
#     add_p() %>%
#     modify_caption(
#         "**Table 1.** Characteristics of US Adults with Diabetes, Overall and by High SRH status."
#     ) %>%
#     modify_footnote(
#         all_stat_cols() ~ "Mean (standard error), Proportion (%) (standard error), Median (p25, p75) for diabetes duration",
#     ) %>%
#     as_gt()
    #gt::gtsave("/Users/dehshini/code/R/nhanes/out/table1.docx")

#table1 %>% gt::gtsave("./out/table1.docx")

# NEW TABLE1
table1n <- tbl_svysummary(
    analyze1,
    include = c(
        RIDAGEYR, AGEGROUP, FEMALE, RACE, FAM_INCOME, EDULEVEL,
        INSURANCE,
        BMXBMI, BMICAT, current_smoker,
        HBA1C, HBA1C_CAT,
        DIAB_DUR_CAT,
        hypertension, hypercholesterolemia, CVD, CKD 
    ),
    by = high_srh,
    statistic = list(
        all_continuous() ~ "{mean} ({mean.std.error})",
        all_categorical() ~ "{p} ({p.std.error})"
    ),
    digits = list(
        all_continuous() ~ c(1, 1),
        all_categorical() ~ c(1, 3)
    ),
    missing = "ifany",
    missing_text = "Missing",
    missing_stat = "{p_miss}",
    label = newtable1_labels
) %>%
    add_overall(
        col_label = "Overall\nMean or % (SE)"
    ) %>%
    modify_spanning_header(
        c("stat_1", "stat_2") ~ "**High SRH**"
    ) %>%
    add_p(
        test = list(
            all_continuous() ~ "svy.t.test",
            all_categorical() ~ "svy.chisq.test"
        )
    )
#    modify_caption(
#        caption = "Table 1. Characteristics of US Adults with Diabetes, Overall and by High SRH status. N = {N_unweighted}"
#    )
#    modify_footnote(
#        all_stat_cols() ~ "Mean (standard error), Proportion (%) (standard error), Median (p25, p75) for diabetes duration",
#    )
#    as_gt()
# gt::gtsave("/Users/dehshini/code/R/nhanes/out/table1.docx")

table1n %>%
    as_gt() %>%
    tab_header(title = "Table 1. Characteristics of US Adults with Diabetes, Overall and by High SRH status")
    #gt::gtsave("./figures_tables/table1n0824.docx")


#### table 1 using low srh
table1low <- tbl_svysummary(
    analyze1,
    include = c(
        RIDAGEYR, AGEGROUP, FEMALE, RACE, FAM_INCOME, EDULEVEL,
        INSURANCE,
        BMXBMI, BMICAT, current_smoker,
        HBA1C, HBA1C_CAT,
        DIAB_DUR_CAT,
        hypertension, hypercholesterolemia, CVD, CKD
    ),
    by = low_srh,
    statistic = list(
        all_continuous() ~ "{mean} ({mean.std.error})",
        all_categorical() ~ "{p} ({p.std.error})"
    ),
    digits = list(
        all_continuous() ~ c(1, 1),
        all_categorical() ~ c(1, 2)
    ),
    missing = "ifany",
    missing_text = "Missing",
    missing_stat = "{p_miss}",
    label = newtable1_labels
) %>%
    add_overall(
        col_label = "Overall\nMean or % (SE)"
    ) %>%
    modify_spanning_header(
        c("stat_1", "stat_2") ~ "**Low SRH**"
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
        title = "Characteristics of US Adults with Diabetes, Overall and by Low SRH status."
    )
    # gt::gtsave("./figures_tables/table1low0824.docx")
    

# END TABLE
#######################################################


####################
# PLOT CUSTOMIZATION

# Custom X-axis labels
xlabels <- c("01-02", "03-04", "05-06", "07-08", "09-10", "11-12", "13-14", "15-16", "17-18")

# build a theme for the plot
theme_general <- theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
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
plot_data <- fread("./out/highsrh_summary.csv")
head(plot_data)
str(plot_data)

# rename the first column
colnames(plot_data)[1] <- "cycle"



# Create the trends graph with error bars
ggplot(plot_data, aes(x = cycle, y = Proportion)) +
    geom_line(group = 1, color = "#292930", linewidth = 1, linetype = "dashed") + # Line for trend
    geom_point(color = "#2d03ff", size = 2) + # Points for each cycle
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1, color = "#999999", alpha = 0.7) + # Error bars
    labs(
        title = "Trends of High SRH Among US Adults with Self-Reported Diabetes",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    ylim(0, 100)



# END TREND GRAPH
######################################################


######################################################
# TREND BY AGE GRAPH

# load the data
plot_gt65 <- fread("./out/highsrh_gt65.csv")
plot_lt65 <- fread("./out/highsrh_lt65.csv")

# rename the first column
colnames(plot_gt65)[1] <- "cycle"
colnames(plot_lt65)[1] <- "cycle"

# Convert the cycle column to a factor
plot_gt65[, cycle := factor(cycle)]
plot_lt65[, cycle := factor(cycle)]

head(plot_gt65)
head(plot_lt65)


# create the plot
ggplot() +
    geom_line(data = plot_gt65, aes(x = cycle, y = Proportion, color = "gt65"), group = 1, linewidth = 1, linetype = "dashed") + # Line for trend
    geom_point(data = plot_gt65, aes(x = cycle, y = Proportion, color = "gt65"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_gt65, aes(x = cycle, ymin = Lower, ymax = Upper), width = 0.1, color = "#999999", alpha = 0.8) + # Error bars
    geom_line(data = plot_lt65, aes(x = cycle, y = Proportion, color = "lt65"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_lt65, aes(x = cycle, y = Proportion, color = "lt65"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_lt65, aes(x = cycle, ymin = Lower, ymax = Upper), width = 0.1, color = "#999999", alpha = 0.8) + # Error bars
    labs(
        title = "Trends of High SRH Among US Adults with Self-Reported Diabetes by Age Group",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    ) +
    theme_general +
    scale_color_manual(
        name = "Age Group",
        values = c("gt65" = "#c92601", "lt65" = "#2d03ff"),
        labels = c("gt65" = "Age >= 65", "lt65" = "Age < 65")
    ) +
    guides(color = guide_legend(override.aes = list(linetype = c(2, 1), shape = c(NA, NA), size = c(1.5, 1.5)))) +
    scale_x_discrete(label = xlabels) +
    ylim(0, 100)


# END TREND BY AGE GRAPH
######################################################


######################################################
# TREND BY SEX GRAPH

#load the data
plot_male <- fread("./out/highsrh_male.csv")
plot_female <- fread("./out/highsrh_female.csv")

# rename the first column
colnames(plot_male)[1] <- "cycle"
colnames(plot_female)[1] <- "cycle"


# Convert the cycle column to a factor
plot_male[, cycle := factor(cycle)]
plot_female[, cycle := factor(cycle)]

head(plot_male)
head(plot_female)


# create the plot
ggplot() +
    geom_line(data = plot_male, aes(x = cycle, y = Proportion, color = "male"), group = 1, linewidth = 1, linetype = "dashed") + # Line for trend
    geom_point(data = plot_male, aes(x = cycle, y = Proportion, color = "male"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_male, aes(x = cycle, ymin = Lower, ymax = Upper), width = 0.2, color = "#0401d2", alpha = 0.7) + # Error bars
    geom_line(data = plot_female, aes(x = cycle, y = Proportion, color = "female"), group = 1, linewidth = 1, linetype = "dotted") + # Line for trend
    geom_point(data = plot_female, aes(x = cycle, y = Proportion, color = "female"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_female, aes(x = cycle, ymin = Lower, ymax = Upper), width = 0.2, color = "#ad0000", alpha = 0.7) + # Error bars
    labs(
        title = "Trends of High SRH Among US Adults with Self-Reported Diabetes by Sex",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    ) +
    theme_general +
    scale_color_manual(
        name = "Sex",
        values = c("male" = "#2d03ff", "female" = "#c92601"),
        labels = c("male" = "Male", "female" = "Female")
    ) +
    scale_x_discrete(label = xlabels) +
    ylim(0, 100)

# END TREND BY SEX GRAPH
######################################################



#################################
# TREND BY RACE AND ETHNICITY

# load the data
plot_other <- fread("./out/srh_other.csv")
plot_black <- fread("./out/srh_black.csv")
plot_hispanic <- fread("./out/srh_hispanic.csv")
plot_white <- fread("./out/srh_white.csv")


# rename the first column
colnames(plot_other)[1] <- "cycle"
colnames(plot_black)[1] <- "cycle"
colnames(plot_hispanic)[1] <- "cycle"
colnames(plot_white)[1] <- "cycle"


# Convert the cycle column to a factor
plot_other[, cycle := factor(cycle)]
plot_black[, cycle := factor(cycle)]
plot_hispanic[, cycle := factor(cycle)]
plot_white[, cycle := factor(cycle)]


# create the plot
ggplot() +
    geom_line(data = plot_other, aes(x = cycle, y = Proportion, color = "other"), group = 1, linewidth = 1, linetype = "dashed") + # Line for trend
    geom_point(data = plot_other, aes(x = cycle, y = Proportion, color = "other"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_other, aes(x = cycle, ymin = Lower, ymax = Upper), width = 0.2, color = "#ad0000", alpha = 0.7) + # Error bars
    geom_line(data = plot_black, aes(x = cycle, y = Proportion, color = "blck"), group = 1, linewidth = 1, linetype = "dotted") + # Line for trend
    geom_point(data = plot_black, aes(x = cycle, y = Proportion, color = "blck"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_black, aes(x = cycle, ymin = Lower, ymax = Upper), width = 0.2, color = "#ad0000", alpha = 0.7) + # Error bars
    geom_line(data = plot_hispanic, aes(x = cycle, y = Proportion, color = "hispanic"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_hispanic, aes(x = cycle, y = Proportion, color = "hispanic"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_hispanic, aes(x = cycle, ymin = Lower, ymax = Upper), width = 0.2, color = "#ad0000", alpha = 0.7) + # Error bars
    geom_line(data = plot_white, aes(x = cycle, y = Proportion, color = "whte"), group = 1, linewidth = 1, linetype = "dashed-dotted") + # Line for trend
    geom_point(data = plot_white, aes(x = cycle, y = Proportion, color = "whte"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_white, aes(x = cycle, ymin = Lower, ymax = Upper), width = 0.2, color = "#ad0000", alpha = 0.7) + # Error bars
    labs(
        title = "Trends of High SRH Among US Adults with Self-Reported Diabetes by Race and Ethnicity",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    ) +
    theme_general +
    scale_color_manual(
        name = "Race and Ethnicity",
        values = c("other" = "#0401d2", "blck" = "#000000", "hispanic" = "#ad0000", "whte" = "#00ee30"),
        labels = c("other" = "Other", "blck" = "Black", "hispanic" = "Hispanic", "whte" = "White")
    ) +
    scale_x_discrete(label = xlabels) +
    ylim(0, 100)


# END TREND BY RACE AND ETHNICITY
###################################

## distribution of each SRH status. create stacked bar graph
# load the data
plot_data2 <- fread("./out/srh_distribution.csv")

ggplot() +
    geom_bar(data = plot_data, aes(x = cycle, y = Proportion, fill = SRH), position = "fill") +
    labs(
        title = "Distribution of SRH Among US Adults with Self-Reported Diabetes",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_fill_manual(
        name = "SRH",
        values = c("#0401d2", "#ad0000", "#00ee30"),
        labels = c("Poor", "Fair", "Good")



#try, remove na values
nhanes_all[, .(N = .N, Mean_age = mean(RIDAGEYR), SE = sd(RIDAGEYR)/sqrt(.N)), by = .(high_srh)]
nhanes_all[, .(N = .N, Mean_BMI = mean(BMXBMI, na.rm = TRUE), SD = sd(BMXBMI, na.rm = TRUE)/sqrt(sum(!is.na(BMXBMI))))]
nhanes_all[, .(N = .N, Mean_HBA1C = mean(HBA1C), SD = sd(HBA1C)/sqrt(.N))]



#########################################################
# STACKED BAR GRAPH OF THE STATUSES
########################################################

# load the data
stackedbar_data <- fread("./out/srh_distribution.csv")
# rename columns
colnames(stackedbar_data) <- c("cycle", "excellent", "verygood", "good", "fair", "poor")
head(stackedbar_data)

# convert from wide to long
stackedbar_data1 <- melt(stackedbar_data, id.vars = "cycle")
head(stackedbar_data1, 18)

# create stacked bar graph

ggplot(stackedbar_data1, aes(x = cycle, y = value, fill = variable)) +
    geom_bar(position = "fill", stat = "identity") +
    labs(
        title = "SRH Distribution Among US Adults with Self-Reported Diabetes",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        fill = "SRH Status"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_fill_brewer(
        palette = "RdYlGn"
    ) +
    scale_y_continuous(labels = scales::percent)

#save the plot
ggsave("./out/srh_distribution.png", width = 10, height = 6)

# END STACKED BAR GRAPH OF THE STATUSES
#######################################