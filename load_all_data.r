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
cycle_folders <- list.dirs(main_directory, full.names = TRUE, recursive = FALSE)

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



# Define the breakpoints and labels for BMI categories
bmi_breaks <- c(-Inf, 18.5, 24.9, 29.9, Inf) # Breakpoints for categories
bmi_labels <- c("<2", "Normal", "Overweight", "Obese") # Corresponding labels

# income poverty ratio
PIR_breaks <- c(-Inf, 0.99, 2.99, Inf)
PIR_labels <- c("< 1", "1 to <3", "3 or more")

# NEW
for (cycle in nhanes_list) {
    # create new weights, 9 cycles
    cycle[, WTMEC9YR := WTMEC2YR / 9]

    # create derived variables
    # age group, 1 = 20-39, 2= 40-64, 3 = 65+
    cycle[, AGEGROUP := cut(
        RIDAGEYR, breaks = c(0, 19.99, 39.99, 64.99, Inf),
        labels = c("0-19yeas", "20-39yrs", "40-64yrs", ">=65years"),
        right = FALSE
        )
    ]
    
    # female
    cycle[, FEMALE := ifelse(RIAGENDR == 2, 1, 0)]

    # blood pressure, use any of 4 measurements or take average

    cycle[, c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4") :=
        lapply(.SD, function(x) fifelse(x == 0, NA_real_, x)),
    .SDcols = c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
    ]
    
    cycle[, 
        SBP := rowMeans(.SD, na.rm = TRUE), 
        .SDcols = c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")
    ]
    cycle[, 
        DBP := rowMeans(.SD, na.rm = TRUE), 
        .SDcols = c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")
    ]
    # convert 0s to NA
    cycle[, SBP := ifelse(SBP == 0, NA, SBP)]
    cycle[, DBP := ifelse(DBP == 0, NA, DBP)]
    
    cycle[, BMICAT := cut(
        BMXBMI, 
        breaks = c(-Inf, 24.99, 29.99, Inf), 
        labels = c("<25", "25 to <30", ">30"), 
        right = FALSE
        )
    ]
    cycle[, MARITAL := ifelse(DMDMARTL %in% c(1, 6), 1,
        ifelse(DMDMARTL %in% c(2, 3, 4), 2,
            ifelse(DMDMARTL == 5, 3, NA)
        )
    )]
    cycle[, EDULEVEL := ifelse(DMDEDUC2 %in% c(1, 2), 1,
        ifelse(DMDEDUC2 == 3, 2,
            ifelse(DMDEDUC2 %in% c(4, 5), 3, NA)
        )
    )]
    cycle[, RACE := ifelse(RIDRETH1 == 3, 1,
        ifelse(RIDRETH1 == 4, 2,
            ifelse(RIDRETH1 %in% c(1, 2), 3,
                ifelse(RIDRETH1 %in% c(5), 4, NA)
            )
        )
    )]

    cycle[, SMOKE := ifelse(SMQ040 %in% c(1, 2), 1,
        ifelse(SMQ020 == 1 & !SMQ040 %in% c(1, 2), 2,
            ifelse(SMQ020 == 2, 3, NA)
        )
    )]
    cycle[, current_smoker := ifelse(SMOKE == 1 | LBXCOT > 10, 1, 0)]

    # hba1c
    cycle[, HBA1C := LBXGH]
    # categorize hba1c
    cycle[, 
    HBA1C_CAT := cut(
        HBA1C, 
        breaks = c(-Inf, 6.99, 7.99, 8.99, Inf), 
        labels = c("<7%", "7-8%", "8-9%", ">9%"), 
        right = FALSE
        )
    ]

    # diabetes duration is held in different variables

    # Check if DID040Q exists, if not, use DID040 (first 2 cycles use DID040Q)
    # replace extreme values with NA first
    if ("DID040Q" %in% names(cycle)) {
        cycle[, DID040Q := ifelse(
            DID040Q == 99999 | DID040Q == 77777, NA, DID040Q)
        ]
        cycle[, DIAB_DUR := (RIDAGEYR - DID040Q)]
    } else {
        cycle[, DID040 := ifelse(DID040 == 999 | DID040 == 777, NA, DID040)]
        cycle[, DIAB_DUR := (RIDAGEYR - DID040)]
    }
    # categorize diabetes duration
    cycle[, DIAB_DUR_CAT := cut(
        DIAB_DUR, 
        breaks = c(-Inf, 4.99, 14.99, Inf), 
        labels = c("0 to <5 years", "5 to <15 years", ">=15 years"), 
        right = FALSE
        )
    ]

    # RETINOPATHY
    cycle[, RETINOPATHY := ifelse(DIQ080 == 1, 1, 0)]

    # ASCVD
    cycle[, ASCVD := ifelse(
        MCQ160F == 1 | MCQ160E == 1 | MCQ160D == 1 | MCQ160C == 1, 1, 0)
    ]

    # heart failure
    cycle[, HEART_FAIL := ifelse(MCQ160B == 1, 1, 0)]

    # income poverty ratio
    cycle[, PIR := cut(
        INDFMPIR, 
        breaks = PIR_breaks, 
        labels = PIR_labels, 
        right = FALSE)
    ]
    cycle[, FAM_INCOME := cut(INDFMPIR, breaks = c(-Inf, 0.99, Inf), labels = c("Below poverty threshold", "Above or at poverty threshold"), right = FALSE)]
    # convert sex to M/F
    cycle[, sex := ifelse(RIAGENDR == 1, "M", "F")]
    # convert ethnicity to black/nonblack
    cycle[, ethnicity := ifelse(RACE == 2, "black", "non-black")]
    cycle[, eGFR := ckd_epi(creat = LBDSCRSI, age = RIDAGEYR, sex = sex, ethnicity = ethnicity)]
    # calculate albumin creat ratio, acr = urxuma/urxucr *100
    cycle[, acr := (URXUMA / URXUCR) * 100]
    # define ckd
    cycle[, CKD := ifelse(eGFR < 60 | acr >= 30, 1,
        ifelse(eGFR >= 60 | acr < 30, 0, NA)
    )]
    cycle[, high_srh := ifelse(HSD010 <= 3, 1, 0)]
    cycle[, low_srh := ifelse(HSD010 %in% c(4, 5), 1, 0)]

    # bp meds
    # cycle[, bpmeds := BPQ040A]
    # cycle[, bpmeds := ifelse(
    #     is.na(bpmeds) & (BPQ010 == 5 | BPQ020 == 2), 0, 
    #     ifelse(BPQ040A == 9, NA, 
    #            ifelse(BPQ040A == 2, 0, BPQ040A))
    #     )
    # ]
    # hypertension
    # cycle[, hypertension := ifelse(
    #     SBP >= 130 | DBP >= 80 | bpmeds == 1, 1, 0
    # )]


    # cholesterol meds
    cycle[, cholmeds := ifelse(
        BPQ090D == 1, 1,
        ifelse(BPQ090D %in% c(2, 7, 9), 0, NA)
    )]

    cycle[, hypercholesterolemia := fcase(
    !is.na(LBXTC) & LBXTC >= 240, 1,
    !is.na(cholmeds) & cholmeds == 1, 1,
    is.na(LBXTC) & is.na(cholmeds), NA_real_,
    default = 0
    )]

    # CVD
    cycle[, CVD := ifelse(ASCVD == 1 | HEART_FAIL == 1, 1, 0)]
}


# insurance status, 0=no insurance, 1=private, 2=public

for (cycle in nhanes_list) {
    # Check if the relevant variables exist
    has_HIQ011 <- "HIQ011" %in% names(cycle) & "HIQ031G" %in% names(cycle)
    has_HID010 <- "HID010" %in% names(cycle)
    has_HIQ011butnotG <- "HIQ011" %in% names(cycle) & !("HIQ031G" %in% names(cycle))

    if (has_HIQ011) {
        cycle[, INSURANCE := fcase(
            HIQ011 == 2, 0,
            HIQ031G == 20 | HIQ031H == 21 | HIQ031I == 22 |
                HIQ031B == 15 | HIQ031C == 16 | HIQ031D == 17 |
                HIQ031E == 18 | HIQ031F == 19, 2,
            HIQ011 == 1 & HIQ031A == 14, 1,
            default = NA_real_
        )]
        print("Processed with HIQ011")
    } else if (has_HID010) {
        cycle[, INSURANCE := fcase(
            HID010 == 2, 0,
            HID030B == 1 | HID030C == 1 | HID030D == 1, 2,
            HID010 == 1 & HID030A == 1, 1,
            default = NA_real_
        )]
        print("Processed with HID010")
    } else if (has_HIQ011butnotG) {
        cycle[, INSURANCE := fcase(
            HIQ011 == 2, 0,
            HIQ031H == 21 | HIQ031I == 22 |
                HIQ031B == 15 | HIQ031C == 16 | HIQ031D == 17 |
                HIQ031E == 18 | HIQ031F == 19, 2,
            HIQ011 == 1 & HIQ031A == 14, 1,
            default = NA_real_
        )]
        print("Processed with HIQ011 but without HIQ031G")
    } else {
        print("Could not find HIQ011 or HID010 in the data")
    }

    # Check the distribution of the INSURANCE variable
    print(table(cycle$INSURANCE, useNA = "ifany"))
}

