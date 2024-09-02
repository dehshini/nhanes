# load all data
setwd("/Users/dehshini/code/R/nhanes")
# load("nhanes_data.RDATA")

library(tidyverse)
library(haven)
library(data.table)
library(survey)
library(gtsummary)
library(transplantr)
library(openxlsx)
library(gt)
library(tableone)
library(DataExplorer)
library(Publish)


# Function to load data files from a directory
load_data_files <- function(directory) {
    # List all files in the directory
    files <- list.files(directory, full.names = TRUE)

    # Load each file into a list
    data_list <- lapply(files, read_xpt)

    # Set names of the list elements to the filenames (without extension)
    names(data_list) <- tools::file_path_sans_ext(basename(files))

    # Join all data frames in the list
    # use the file that begins with "DEMO" as the first data frame
    joined_data <- data_list[[grep("^DEMO", names(data_list))]]

    # Loop through the remaining data frames and add them to the joined data
    for (i in seq_along(data_list)[-grep("^DEMO", names(data_list))]) {
        joined_data <- left_join(joined_data, data_list[[i]])
    }

    # convert the joined data to a data table
    setDT(joined_data)

    # return the joined data
    return(joined_data)
}

# Main directory containing NHANES cycle folders
main_directory <- "./data"

# List all cycle folders
cycle_folders <- list.dirs(
    main_directory, 
    full.names = TRUE, 
    recursive = FALSE
)

# Iterate through each cycle folder
for (cycle_folder in cycle_folders) {
    # Extract the cycle name from the folder name
    cycle_name <- basename(cycle_folder)

    # Load data files in the current cycle folder
    cycle_data <- load_data_files(cycle_folder)

    # Save the cycle data to an object
    assign(paste0("nhanes", cycle_name), cycle_data)
}

# Save all data frames to a single RData file
# save(list = ls(), file = "nhanes_data.RData")


# create a list to hold the nhanes dataframes
nhanes_list <- list(
    nhanes01_02, nhanes03_04, nhanes05_06, nhanes07_08, nhanes09_10,
    nhanes11_12, nhanes13_14, nhanes15_16, nhanes17_18
)

# bind all rows in nhanes list into one dataframe
nhanes_all <- rbindlist(nhanes_list, fill = TRUE)


# income poverty ratio
PIR_breaks <- c(-Inf, 0.99, 2.99, Inf)
PIR_labels <- c("< 1", "1 to <3", "3 or more")

# create derived variables

# create new weights, 9 cycles
nhanes_all[, WTMEC9YR := WTMEC2YR / 9]

# create BMI categories
nhanes_all[, BMICAT := cut(
    BMXBMI,
    breaks = c(-Inf, 24.99, 29.99, Inf),
    labels = c("<25", "25 to <30", ">30"),
    right = FALSE
)]

# create PIR categories
nhanes_all[, PIR_cat := cut(
    INDFMPIR, 
    breaks = PIR_breaks, 
    labels = PIR_labels
)]

# create age groups
nhanes_all[, AGEGROUP := cut(
    RIDAGEYR,
    breaks = c(0, 19.99, 39.99, 64.99, Inf),
    labels = c("0-19yeas", "20-39yrs", "40-64yrs", ">=65years"),
    right = FALSE
)]

# female
nhanes_all[, FEMALE := ifelse(RIAGENDR == 2, 1, 0)]

# create marital status
nhanes_all[, MARITAL := ifelse(DMDMARTL %in% c(1, 6), 1,
        ifelse(DMDMARTL %in% c(2, 3, 4), 2,
            ifelse(DMDMARTL == 5, 3, NA)
        )
    )
]

# education level
nhanes_all[, EDULEVEL := ifelse(DMDEDUC2 %in% c(1, 2), 1,
    ifelse(DMDEDUC2 == 3, 2,
        ifelse(DMDEDUC2 %in% c(4, 5), 3, NA)
    )
)]

# race/ethnicity
nhanes_all[, RACE := ifelse(RIDRETH1 == 3, 1,
    ifelse(RIDRETH1 == 4, 2,
        ifelse(RIDRETH1 %in% c(1, 2), 3,
            ifelse(RIDRETH1 %in% c(5), 4, NA)
        )
    )
)]

# smoking status, 1 = current smoker, 2 = former smoker, 3 = never smoker
nhanes_all[, SMOKE := ifelse(SMQ040 %in% c(1, 2), 1,
    ifelse(SMQ020 == 1 & !SMQ040 %in% c(1, 2), 2,
        ifelse(SMQ020 == 2, 3, NA)
    )
)]

# current smoker
nhanes_all[, current_smoker := ifelse(SMOKE == 1 | LBXCOT > 10, 1, 0)]

# hba1c and hba1c category
nhanes_all[, HBA1C := LBXGH]
nhanes_all[, HBA1C_CAT := cut(
    HBA1C,
    breaks = c(-Inf, 6.99, 7.99, 8.99, Inf),
    labels = c("<7%", "7-8%", "8-9%", ">9%"),
    right = FALSE
)]

# poverty status
nhanes_all[, FAM_INCOME := cut(
    INDFMPIR, 
    breaks = c(-Inf, 0.99, Inf), 
    labels = c("Below poverty threshold", "Above or at poverty threshold"), 
    right = FALSE)
]

# eGFR(ckd_epi equation), sex and ethnicity(black/non-black) needed
nhanes_all[, sex := ifelse(RIAGENDR == 1, "M", "F")]
nhanes_all[, ethnicity := ifelse(RACE == 2, "black", "non-black")]
nhanes_all[, eGFR := ckd_epi(
    creat = LBDSCRSI, 
    age = RIDAGEYR, 
    sex = sex, 
    ethnicity = ethnicity
)]

# calculate albumin creat ratio, acr = urine albumin/urine creatinine *100
nhanes_all[, acr := (URXUMA / URXUCR) * 100]

# define ckd, 1 = CKD, 0 = non-CKD
nhanes_all[, CKD := ifelse(eGFR < 60 | acr >= 30, 1,
    ifelse(eGFR >= 60 | acr < 30, 0, NA)
)]

# ASCVD, includes CHD, angina, heart attack, stroke
nhanes_all[, ASCVD := ifelse(
    MCQ160F == 1 | MCQ160E == 1 | MCQ160D == 1 | MCQ160C == 1, 1, 0
)]

nhanes_all[, RETINOPATHY := ifelse(DIQ080 == 1, 1, 0)]

nhanes_all[, HEART_FAIL := ifelse(MCQ160B == 1, 1, 0)]

# high and low SRH
nhanes_all[, high_srh := ifelse(HSD010 <= 3, 1, 0)]
nhanes_all[, low_srh := ifelse(HSD010 %in% c(4, 5), 1, 0)]

# diabetes duration
# convert large values to NA
nhanes_all[, DID040Q := ifelse(
    DID040Q == 99999 | DID040Q == 77777, NA, DID040Q
)]
nhanes_all[, DID040 := ifelse(
    DID040 == 999 | DID040 == 666, NA, DID040
)]
# age diagnosed. this is held in different variables
nhanes_all[, age_diabts := DID040Q]
nhanes_all[, age_diabts := ifelse(
    is.na(age_diabts), DID040, age_diabts
)]

# diabetes duration and categories
nhanes_all[, diabetes_duration := RIDAGEYR - age_diabts]
nhanes_all[, diabetes_duration_cat := cut(
    diabetes_duration,
    breaks = c(-Inf, 4.99, 14.99, Inf), 
    labels = c("0 to <5 years", "5 to <15 years", ">=15 years"),
    right = FALSE
)]

# blood pressure
nhanes_all[, SBP := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")
]

nhanes_all[, DBP := rowMeans(.SD, na.rm = TRUE),
    .SDcols = c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
]
# convert 0s to NA
nhanes_all[, SBP := ifelse(SBP == 0, NA, SBP)]
nhanes_all[, DBP := ifelse(DBP == 0, NA, DBP)]

# BP meds
nhanes_all[, bpmeds := BPQ040A]

nhanes_all[, bpmeds := ifelse(
    is.na(bpmeds) & (BPQ010 == 5 | BPQ020 == 2), 0, 
    ifelse(BPQ040A == 9, NA, 
        ifelse(BPQ040A == 2, 0, BPQ040A))
)]

# hypertension
nhanes_all[, hypertension := ifelse(
    SBP >= 130 | DBP >= 80 | bpmeds == 1, 1, 0
)]

# cholesterol meds
nhanes_all[, cholmeds := ifelse(
    BPQ090D == 1, 1,
    ifelse(BPQ090D %in% c(2, 7, 9), 0, NA)
)]

# hypercholesterolemia
nhanes_all[, hypercholesterolemia := fcase(
    !is.na(LBXTC) & LBXTC >= 240, 1,
    !is.na(cholmeds) & cholmeds == 1, 1,
    is.na(LBXTC) & is.na(cholmeds), NA_real_,
    default = 0
)]

# CVD, includes CHD, angina, heart attack, stroke, heart failure
nhanes_all[, CVD := ifelse(ASCVD == 1 | HEART_FAIL == 1, 1, 0)]

# insurance status, 0=private, 1=public, 2=uninsured
nhanes_all[, insurance := fcase(
    HIQ031A == 14 | HID030A == 1, 0,
    HIQ031B == 15 | HIQ031C == 16 | HIQ031D == 17 |
        HIQ031E == 18 | HIQ031F == 19 | HIQ031G == 20 |
        HIQ031H == 21 | HIQ031I == 22 |
        HID030B == 1 | HID030C == 1 | HID030D == 1, 1,
    HIQ011 == 2 | HID010 == 2, 2,
    default = NA_real_
)]
