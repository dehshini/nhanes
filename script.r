setwd("/Users/dehshini/code/R/nhanes")

library(tidyverse)
library(data.table)
library(haven)
library(survey)
library(tableone)
library(gt)
library(gtsummary)

# nhanes 2017-2020
# download the files

# read the demo data
demo17_20 <- read_xpt("./data/17_20/demo.xpt")

# read the body measure data
body17_20 <- read_xpt("./data/17_20/bm.xpt")

# merge bm with demo
nhanes17_20 <- left_join(demo17_20, body17_20, by = "SEQN")

# get the cholesterol data
hdl17_20 <- read_xpt("./data/17_20/hdl_cholesterol.xpt")
ldl17_20 <- read_xpt("./data/17_20/ldl_cholesterol.xpt")
tchol17_20 <- read_xpt("./data/17_20/total_cholesterol.xpt")

# merge it with nhanes
nhanes17_20 <- left_join(nhanes17_20, hdl17_20, by = "SEQN")
nhanes17_20 <- left_join(nhanes17_20, ldl17_20, by = "SEQN")
nhanes17_20 <- left_join(nhanes17_20, tchol17_20, by = "SEQN")

# get the bp
bp <- read_xpt("./data/17_20/blood_pressure.xpt")

# for each patient, get the mean systolic and diastolic
# using the 2nd and 3rd readings
bp_sub <- bp %>%
    group_by(SEQN) %>%
    summarise(
        mean_sys = (BPXOSY2 + BPXOSY3) / 2,
        mean_dia = (BPXODI2 + BPXODI3) / 2
    )

# merge BP with nhanes
nhanes2020 <- left_join(nhanes17_20, bp_sub, by = "SEQN")

# cholesterol questionnaire data
bpchol <- read_xpt("./data/17_20/q_bp_chol.xpt")

#merge
nhanes17_20 <- left_join(nhanes17_20, bpchol, by = "SEQN")

# load diabetes questionnaire
diabetes <- read_xpt("./data/17_20/q_diabetes.xpt")

# merge it with nhanes
nhanes17_20 <- left_join(nhanes17_20, diabetes, by = "SEQN")

# load the medical data questionnaire
meds <- read_xpt("./data/17_20/q_medical.xpt")

# merge it with nhanes
nhanes17_20 <- left_join(nhanes17_20, meds, by = "SEQN")

# load the smoking data
smoking <- read_xpt("./data/17_20/q_smoking.xpt")

# merge it with nhanes
nhanes17_20 <- left_join(nhanes17_20, smoking, by = "SEQN")

# load income data
income <- read_xpt("./data/17_20/q_income.xpt")

# merge it with nhanes
nhanes17_20 <- left_join(nhanes17_20, income, by = "SEQN")

# general health
genhealth <- read_xpt("./data/17_20/q_genhealth.xpt")

# merge it with nhanes
nhanes17_20 <- left_join(nhanes17_20, genhealth, by = "SEQN")


#########################################################
# nhanes 2015-2016
# download the files

# read the demo data
demo15 <- read_xpt("./data/15_16/DEMO_I.xpt")

# read the body measure data
body15 <- read_xpt("./data/15_16/BMX_I.xpt")

# merge bm with demo
nhanes15_16 <- left_join(demo15, body15, by = "SEQN")

#bp questionnaire
bp <- read_xpt("./data/15_16/BPQ_I.XPT")

#merge
nhanes15_16 <- left_join(nhanes15_16, bp, by = "SEQN")

#CVD questionnaire
cvd <- read_xpt("./data/15_16/CDQ_I.XPT")

#merge
nhanes15_16 <- left_join(nhanes15_16, cvd, by = "SEQN")

# current health status
genhealth <- read_xpt("./data/15_16/HSQ_I.XPT")

#merge
nhanes15_16 <- left_join(nhanes15_16, genhealth, by = "SEQN")

# bp
bp <- read_xpt("./data/15_16/BPX_I.XPT")

#merge
nhanes15_16 <- left_join(nhanes15_16, bp, by = "SEQN")

# get the cholesterol data
hdl <- read_xpt("./data/15_16/HDL_I.XPT")
ldl <- read_xpt("./data/15_16/TRIGLY_I.XPT")
tchol <- read_xpt("./data/15_16/TCHOL_I.XPT")

# merge it with nhanes
nhanes15_16 <- left_join(nhanes15_16, hdl, by = "SEQN")
nhanes15_16 <- left_join(nhanes15_16, ldl, by = "SEQN")
nhanes15_16 <- left_join(nhanes15_16, tchol, by = "SEQN")



#########################################################
# nhanes 2013-2014
# download the files

# read the demo data
demo13 <- read_xpt("./data/13_14/DEMO_H.XPT")

# read the body measure data
body13 <- read_xpt("./data/13_14/BMX_H.XPT")

# merge bm with demo
nhanes13_14 <- left_join(demo13, body13, by = "SEQN")

# bp
bp <- read_xpt("./data/13_14/BPX_H.XPT")

#merge
nhanes13_14 <- left_join(nhanes13_14, bp, by = "SEQN")

# get the cholesterol data
hdl <- read_xpt("./data/13_14/HDL_H.XPT")
ldl <- read_xpt("./data/13_14/TRIGLY_H.XPT")
tchol <- read_xpt("./data/13_14/TCHOL_H.XPT")

# merge it with nhanes
nhanes13_14 <- left_join(nhanes13_14, hdl, by = "SEQN")
nhanes13_14 <- left_join(nhanes13_14, ldl, by = "SEQN")
nhanes13_14 <- left_join(nhanes13_14, tchol, by = "SEQN")

####
# get BP questionnaire
bpchol <- read_xpt("./data/13_14/BPQ_H.XPT")

#merge
nhanes13_14 <- left_join(nhanes13_14, bpchol, by = "SEQN")

# load diabetes questionnaire
diabetes <- read_xpt("./data/13_14/DIQ_H.XPT")

# merge it with nhanes
nhanes13_14 <- left_join(nhanes13_14, diabetes, by = "SEQN")

# load the smoking data
smoking <- read_xpt("./data/13_14/SMQ_H.XPT")

# merge it with nhanes
nhanes13_14 <- left_join(nhanes13_14, smoking, by = "SEQN")

# load income data
income <- read_xpt("./data/13_14/INQ_H.XPT")

# merge it with nhanes
nhanes13_14 <- left_join(nhanes13_14, income, by = "SEQN")

# general health
genhealth <- read_xpt("./data/13_14/HSQ_H.XPT")

#merge
nhanes13_14 <- left_join(nhanes13_14, genhealth, by = "SEQN")

# load cardiovascular questionnaire
cvd <- read_xpt("./data/13_14/CDQ_H.XPT")

# merge it with nhanes
nhanes13_14 <- left_join(nhanes13_14, cvd, by = "SEQN")

# load
