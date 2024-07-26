#' ------------------------------------------------------------------------------------------------------------- 

#' Example R code to replicate NCHS Data Brief No.303, Figures 1                                                  
#' Prevalence of Depression Among Adults Aged 20 and Over: United States, 2013-2016                             
                                                                                                              
#' Brody DJ, Pratt LA, Hughes JP. Prevalence of Depression Among Adults Aged 20 and Over: United                
#' States, 2013-2016. NCHS Data Brief. No 303. Hyattsville, MD: National Center for Health Statistics. 2018.                     
                                                                                                              
#' Available at: https://www.cdc.gov/nchs/products/databriefs/db303.htm                                         

#' ------------------------------------------------------------------------------------------------------------  

# Load survey and dplyr packages
#+ message = FALSE, warning=FALSE
library(dplyr)
library(survey)
#'
options(survey.lonely.psu='adjust')

# Display Version Information
cat("R package versions:\n")
for (p in c("base", "survey","dplyr")) { 
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}

#' # Data preparation
# Download & Read SAS Transport Files
# Demographic (DEMO)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DEMO_H.XPT", tf <- tempfile(), mode="wb")
DEMO_H <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","SDMVSTRA","SDMVPSU","WTMEC2YR")]
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.XPT", tf <- tempfile(), mode="wb")
DEMO_I <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","SDMVSTRA","SDMVPSU","WTMEC2YR")]

# Mental Health - Depression Screener (DPQ) 
download.file("http://wwwn.cdc.gov/nchs/nhanes/2013-2014/DPQ_H.XPT", tf <- tempfile(), mode="wb")
DPQ_H <- foreign::read.xport(tf)
download.file("http://wwwn.cdc.gov/nchs/nhanes/2015-2016/DPQ_I.XPT", tf <- tempfile(), mode="wb")
DPQ_I <- foreign::read.xport(tf)

# Append Files
DEMO <- bind_rows(DEMO_H, DEMO_I)
DPQ <- bind_rows(DPQ_H, DPQ_I)

# Merge DEMO and DPQ files and create derived variables

One <- left_join(DEMO, DPQ, by="SEQN") %>%
  # Set 7=Refused and 9=Don't Know To Missing for variables DPQ010 thru DPQ090 ##
  mutate_at(vars(DPQ010:DPQ090), ~ifelse(. >=7, NA, .)) %>%
  mutate(. , 
         # create indicator for overall summary
         one = 1,
         # Create depression score as sum of variables DPQ010 -- DPQ090
         Depression.Score = rowSums(select(. , DPQ010:DPQ090)),
         # Create depression indicator as binary 0/100 variable. (is missing if Depression.Score is missing)
         Depression= ifelse(Depression.Score >=10, 100, 0), 
         # Create factor variables
         Gender = factor(RIAGENDR, labels=c("Men", "Women")),
         Age.Group = cut(RIDAGEYR, breaks=c(-Inf,19,39,59,Inf),labels=c("Under 20", "20-39","40-59","60 and over")),
         # Generate 4-year MEC weight (Divide weight by 2 because we are appending 2 survey cycles)                                                             
         # Note: using the MEC Exam Weights (WTMEC2YR), per the analytic notes on the               
         #       Mental Health - Depression Screener (DPQ_H) documentation                          
         WTMEC4YR = WTMEC2YR/2 ,
         # Define indicator for analysis population of interest: adults aged 20 and over with a valid depression score
         inAnalysis= (RIDAGEYR >= 20 & !is.na(Depression.Score))
         ) %>% 
  # drop DPQ variables
  select(., -starts_with("DPQ"))

#' ## Define survey design 
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC4YR, nest=TRUE)
                    
# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score 
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)

#' ## Analysis
# Define a function to call svymean and unweighted count
getSummary <- function(varformula, byformula, design){
  # Get mean, stderr, and unweighted sample size
  c <- svyby(varformula, byformula, design, unwtd.count ) 
  p <- svyby(varformula, byformula, design, svymean ) 
  outSum <- left_join(select(c,-se), p) 
  outSum
}

#' ### Calculate prevalence of depression overall, by gender, by age group, and by age and gender
#' Adults
getSummary(~Depression, ~one, NHANES)
#' By sex
getSummary(~Depression, ~Gender, NHANES)
#' By age
getSummary(~Depression, ~Age.Group, NHANES)
#' By sex and age
getSummary(~Depression, ~Gender + Age.Group, NHANES)

#' ### Compare Prevalence Between Men And Women
svyttest(Depression~Gender, NHANES)$p.value %>% as.numeric 
svyttest(Depression~Gender, subset(NHANES, Age.Group=="20-39"))$p.value %>% as.numeric
svyttest(Depression~Gender, subset(NHANES, Age.Group=="40-59"))$p.value %>% as.numeric
svyttest(Depression~Gender, subset(NHANES, Age.Group=="60 and over"))$p.value %>% as.numeric


#' ### Pairwise t-testing by age groups for total, men, and women
#' Differences by age group, among all adults
# Full output from svyttest command
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="20-39" | Age.Group=="40-59"))
# Displaying p-values only
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="20-39" | Age.Group=="40-59"))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="20-39" | Age.Group=="60 and over"))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="40-59" | Age.Group=="60 and over"))$p.value %>% as.numeric
#' Differences by age group, among men
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Men" & (Age.Group=="20-39" | Age.Group=="40-59")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Men" & (Age.Group=="20-39" | Age.Group=="60 and over")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Men" & (Age.Group=="40-59" | Age.Group=="60 and over")))$p.value %>% as.numeric
#' Differences by age group, among women
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Women" & (Age.Group=="20-39" | Age.Group=="40-59")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Women" & (Age.Group=="20-39" | Age.Group=="60 and over")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Women" & (Age.Group=="40-59" | Age.Group=="60 and over")))$p.value %>% as.numeric



