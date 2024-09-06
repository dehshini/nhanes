# create cycle number variable
nhanes_all$cycle_num <- as.numeric(as.factor(nhanes_all$SDDSRVYR))

table(nhanes_all$cycle_num)

# create survey design object
trend_design <- svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~WTMEC9YR,
    nest = TRUE,
    data = nhanes_all
)

# create subset of interest
trend_design <- subset(
    trend_design,
    RIDAGEYR >= 20 & DIQ010 == 1 & !is.na(HSD010) &
        !is.na(hypercholesterolemia) &
        !is.na(hypertension) &
        !is.na(diabetes_duration) &
        !is.na(CVD) &
        !is.na(insurance) &
        !is.na(EDULEVEL)
)

# Logistic regression to test for trend
trend_model <- svyglm(
    low_srh ~ cycle_num,
    design = trend_design,
    family = quasibinomial()
)
# View summary of the model to check for linear trend
summary(trend_model)$coefficients
View(trend_model)

# logistic regression to test for trend by age
trend_model_age <- svyglm(
    low_srh ~ factor(RIDAGEYR < 65) + factor(RIDAGEYR >= 65) + cycle_num,
    design = trend_design,
    family = quasibinomial()
)
# View summary of the model to check for linear trend
summary(trend_model_age)$coefficient
View