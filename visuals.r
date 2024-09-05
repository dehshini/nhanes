# create directory for figures/tables
# dir.create("figures_tables", showWarnings = FALSE)


## TABLE
#############################################################################
#NEW TABLE1 LABELS
newtable1_labels <- list(
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

# NEW TABLE1
table1n <- tbl_svysummary(
    analyze1,
    include = c(
        RIDAGEYR, AGEGROUP, FEMALE, RACE, FAM_INCOME, EDULEVEL,
        insurance,
        BMXBMI, BMICAT, current_smoker,
        HBA1C, HBA1C_CAT,
        diabetes_duration_cat,
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

table1n %>%
    as_gt() %>%
    tab_header(title = "Table 1. Characteristics of US Adults with Diabetes, Overall and by High SRH status")
    #gt::gtsave("./figures_tables/table1n0824.docx")


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
        all_continuous() ~ c(1, 3),
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
plot_data <- fread("./out/lowsrh_summary.csv")
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
        title = "Trends of Low SRH Among US Adults with Self-Reported Diabetes",
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
plot_4065 <- fread("./out/highsrh_4065.csv")
plot_lt40 <- fread("./out/highsrh_lt40.csv")

# rename the first column
colnames(plot_gt65)[1] <- "cycle"
colnames(plot_lt40)[1] <- "cycle"
colnames(plot_4065)[1] <- "cycle"

# Convert the cycle column to a factor
plot_gt65[, cycle := factor(cycle)]
plot_lt40[, cycle := factor(cycle)]
plot_4065[, cycle := factor(cycle)]

head(plot_gt65)
head(plot_lt40)
head(plot_4065)


# create the plot
ggplot() +
    geom_line(data = plot_gt65, aes(x = cycle, y = Proportion, color = ">= 65"), group = 1, linewidth = 1, linetype = "dashed") + # Line for trend
    geom_point(data = plot_gt65, aes(x = cycle, y = Proportion, color = ">= 65"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_gt65, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.1, color = "#999999", alpha = 0.8) + # Error bars

    geom_line(data = plot_4065, aes(x = cycle, y = Proportion, color = "40 - 64"), group = 1, linewidth = 1, linetype = "dotted") + # Line for trend
    geom_point(data = plot_4065, aes(x = cycle, y = Proportion, color = "40 - 64"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_4065, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.1, color = "#999999", alpha = 0.8) + # Error bars

    geom_line(data = plot_lt40, aes(x = cycle, y = Proportion, color = "20 - 39"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_lt40, aes(x = cycle, y = Proportion, color = "20 - 39"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_lt40, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.1, color = "#999999", alpha = 0.8) + # Error bars

    labs(
        title = "Trends of High SRH Among US Adults with Self-Reported Diabetes by Age Group",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    ) +
    theme_general +
    scale_color_brewer(palette = "Set1") +
    # guides(color = guide_legend(override.aes = list(linetype = c(2, 1), shape = c(NA, NA), size = c(1.5, 1.5)))) +
    scale_x_discrete(label = xlabels) +
    ylim(0, 100) +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100), 
        labels = c("0", "20", "40", "60", "80", "100"), 
        limits = c(0, 100),
        minor_breaks = c(10, 30, 50, 70, 90)
    )


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
    geom_errorbar(data = plot_male, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#0401d2", alpha = 0.7) + # Error bars

    geom_line(data = plot_female, aes(x = cycle, y = Proportion, color = "female"), group = 1, linewidth = 1, linetype = "dotted") + # Line for trend
    geom_point(data = plot_female, aes(x = cycle, y = Proportion, color = "female"), size = 2) + # Points for each cycle
    geom_errorbar(data = plot_female, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#ad0000", alpha = 0.7) + # Error bars

    labs(
        title = "Trends of High SRH Among US Adults with Self-Reported Diabetes by Sex",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    ) +
    theme_general +
    scale_color_brewer(palette = "Set1") +
    scale_x_discrete(label = xlabels) +
    ylim(0, 100) +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 100),
        minor_breaks = c(10, 30, 50, 70, 90)
    )


# END TREND BY SEX GRAPH
######################################################



#################################
# TREND BY RACE AND ETHNICITY

# load the data
plot_other <- fread("./out/highsrh_other.csv")
plot_black <- fread("./out/highsrh_black.csv")
plot_hispanic <- fread("./out/highsrh_hispanic.csv")
plot_white <- fread("./out/highsrh_white.csv")


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
    geom_line(data = plot_other, aes(x = cycle, y = Proportion, color = "Other"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_other, aes(x = cycle, y = Proportion, color = "Other", shape = Race), size = 4) + # Points for each cycle
    geom_errorbar(data = plot_other, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#000000", alpha = 0.5) + # Error bars

    geom_line(data = plot_black, aes(x = cycle, y = Proportion, color = "NH Black"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_black, aes(x = cycle, y = Proportion, color = "NH Black", shape = Race), size = 4) + # Points for each cycle
    geom_errorbar(data = plot_black, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#000000", alpha = 0.7) + # Error bars

    geom_line(data = plot_hispanic, aes(x = cycle, y = Proportion, color = "Hispanic"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_hispanic, aes(x = cycle, y = Proportion, color = "Hispanic", shape = Race), size = 4) + # Points for each cycle
    geom_errorbar(data = plot_hispanic, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#000000", alpha = 0.7) + # Error bars

    geom_line(data = plot_white, aes(x = cycle, y = Proportion, color = "NH White"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_white, aes(x = cycle, y = Proportion, color = "NH White", shape = Race), size = 4) + # Points for each cycle
    geom_errorbar(data = plot_white, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#000000", alpha = 0.7) + # Error bars

    labs(
        title = "Trends of High SRH Among US Adults with Self-Reported Diabetes by Race/Ethnicity",
        x = "NHANES Cycle",
        y = "Proportion"
    ) +
    theme_general +
    scale_color_brewer(palette = "Set1") +
    scale_x_discrete(label = xlabels) +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100")
    ) +
    scale_shape_manual(values = c(15, 16, 17, 18)) +
    facet_wrap(~ factor(Race, levels = c("NH White", "NH Black", "Hispanic", "Other")), ncol = 2) +
    theme(strip.text = element_text(size = 15)) +
    theme(legend.position = "none")


# save the plot
ggsave("./out/highsrh_race_ethnicity.png", width = 16, height = 9, dpi = 300)

# END TREND BY RACE AND ETHNICITY
###################################



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
        y = "Proportion",
        fill = "SRH Status"
    ) +
    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_fill_brewer(
        palette = "RdYlGn",
        direction = -1
    ) +
    scale_y_continuous(
        labels = scales::label_percent(),
        breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
    )

#save the plot
ggsave("./out/srh_distribution.png", width = 10, height = 6)

# END STACKED BAR GRAPH OF THE STATUSES
#######################################


#######################################################
# TREND GRAPH BY CKD
#######################################################

# load the data
plot_ckd <- fread("./out/highsrh_ckd.csv")
plot_nonckd <- fread("./out/highsrh_nonckd.csv")

# rename the first column
colnames(plot_ckd)[1] <- "cycle"
colnames(plot_nonckd)[1] <- "cycle"

# Convert the cycle column to a factor
plot_ckd[, cycle := factor(cycle)]
plot_nonckd[, cycle := factor(cycle)]

head(plot_nonckd)
head(plot_ckd)


# create the plot

ggplot() +
    geom_line(data = plot_ckd, aes(x = cycle, y = Proportion, color = "CKD"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_ckd, aes(x = cycle, y = Proportion, color = "CKD", shape = CKD), size = 4) + # Points for each cycle
    geom_errorbar(data = plot_ckd, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#000000", alpha = 0.5) + # Error bars

    geom_line(data = plot_nonckd, aes(x = cycle, y = Proportion, color = "No-CKD"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_nonckd, aes(x = cycle, y = Proportion, color = "No-CKD", shape = CKD), size = 4) + # Points for each cycle
    geom_errorbar(data = plot_nonckd, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#000000", alpha = 0.5) + # Error bars

    theme_general + 
    scale_x_discrete(label = xlabels) +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 100),
        minor_breaks = c(10, 30, 50, 70, 90)
    ) +

    labs(
        title = "Trends of High SRH Among US Adults with Self-Reported Diabetes",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    ) +

    scale_shape_manual(values = c(15, 17), labels = c("CKD", "No-CKD"))

# save the plot
ggsave("./out/highsrh_ckd.png", width = 10, height = 6)

#######################################################
# END TREND GRAPH BY CKD
#######################################################




#######################################################
# TREND GRAPH BY FAMILY INCOME/POVERTY
#######################################################


# load the data
plot_poverty <- fread("./out/highsrh_poverty.csv")
plot_nonpoverty <- fread("./out/highsrh_nonpoverty.csv")


# rename the first column
colnames(plot_poverty)[1] <- "cycle"
colnames(plot_nonpoverty)[1] <- "cycle"


# Convert the cycle column to a factor
plot_poverty[, cycle := factor(cycle)]
plot_nonpoverty[, cycle := factor(cycle)]

head(plot_nonpoverty)
head(plot_poverty)


# create the plot

ggplot() +
    geom_line(data = plot_poverty, aes(x = cycle, y = Proportion, color = "Poverty"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_poverty, aes(x = cycle, y = Proportion, color = "Poverty"), size = 4) + # Points for each cycle
    geom_errorbar(data = plot_poverty, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#000000", alpha = 0.5) + # Error bars

    geom_line(data = plot_nonpoverty, aes(x = cycle, y = Proportion, color = "Non-Poverty"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_nonpoverty, aes(x = cycle, y = Proportion, color = "Non-Poverty"), size = 4) + # Points for each cycle
    geom_errorbar(data = plot_nonpoverty, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#000000", alpha = 0.5) + # Error bars

    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 100),
        minor_breaks = c(10, 30, 50, 70, 90)
    ) +
    labs(
        title = "Trends of High SRH Among US Adults with Self-Reported Diabetes",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    )

    #scale_shape_manual(values = c(15, 17), labels = c("Below Poverty Threshold", "At/Above Poverty Threshold"))

# save the plot
ggsave("./out/highsrh_poverty.png", width = 12, height = 6)

#######################################################
# END TREND GRAPH BY FAMILY INCOME/POVERTY
#######################################################


#######################################################
# TREND GRAPH BY INSURANCE
#######################################################


# load the data
plot_insurance <- fread("./out/highsrh_noninsured.csv")
plot_insurance1 <- fread("./out/highsrh_priv_insurance.csv")
plot_insurance2 <- fread("./out/highsrh_pub_insurance.csv")


# rename the first column
colnames(plot_insurance)[1] <- "cycle"
colnames(plot_insurance1)[1] <- "cycle"
colnames(plot_insurance2)[1] <- "cycle"

# Convert the cycle column to a factor
plot_insurance[, cycle := factor(cycle)]
plot_insurance1[, cycle := factor(cycle)]
plot_insurance2[, cycle := factor(cycle)]

head(plot_insurance)
head(plot_insurance1)
head(plot_insurance2)

# create the plot

ggplot() +
    geom_line(data = plot_insurance, aes(x = cycle, y = Proportion, color = "Non-Insured"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_insurance, aes(x = cycle, y = Proportion, color = "Non-Insured"), size = 4) + # Points for each cycle
    geom_errorbar(data = plot_insurance, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#000000", alpha = 0.5) + # Error bars

    geom_line(data = plot_insurance1, aes(x = cycle, y = Proportion, color = "Private Insurance"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_insurance1, aes(x = cycle, y = Proportion, color = "Private Insurance"), size = 4) + # Points for each cycle
    geom_errorbar(data = plot_insurance1, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#000000", alpha = 0.5) + # Error bars

    geom_line(data = plot_insurance2, aes(x = cycle, y = Proportion, color = "Public Insurance"), group = 1, linewidth = 1, linetype = "solid") + # Line for trend
    geom_point(data = plot_insurance2, aes(x = cycle, y = Proportion, color = "Public Insurance"), size = 4) + # Points for each cycle
    geom_errorbar(data = plot_insurance2, aes(x = cycle, ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "#000000", alpha = 0.5) + # Error bars

    theme_general +
    scale_x_discrete(label = xlabels) +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c("0", "20", "40", "60", "80", "100"),
        limits = c(0, 100),
        minor_breaks = c(10, 30, 50, 70, 90)
    ) +
    labs(
        title = "Trends of High SRH Among US Adults with Self-Reported Diabetes by Insurance Status",
        x = "NHANES Cycle",
        y = "Proportion (%)"
    )

# save the plot
ggsave("./out/highsrh_insurance.png", width = 14, height = 6)


#######################################################
# END TREND GRAPH BY INSURANCE
#######################################################