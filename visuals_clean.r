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
        title = "Characteristics of US Adults with Diabetes, Overall and by Self-Reported Health Status."
    )
# gt::gtsave("./figures_tables/table1low0824.docx")


# END TABLE
#######################################################
