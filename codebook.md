# Workflow
* begin with "load data files script". this load the nhanes data from the directory by cycle. merges them using a full join. converts the data to data tables.

* go to the "data manipulation" script. 
do srh analysis by age.

* 

* 



## Variables  
--

### DEMOGRAPHY  
SEQN = UNIQUE ID  
SDMVSTRA = STRATUM  
SDMVPSU = PSU  
RIAGENDR = GENDER  
RIDAGEYR = AGE IN YEARS  
RIDRETH1 = RACE  
DMDEDUC2 - Education level - Adults 20+ 
WTINTPRP - Full sample interview weight  
WTMECPRP - Full sample MEC exam weight  
SDMVPSU - Masked variance pseudo-PSU  
SDMVSTRA - Masked variance pseudo-stratum  
INDFMPIR - Ratio of family income to poverty   

## CHOLESTEROL  
WTSAFPRP - Fasting Subsample Weight  
LBDTCSI = TOTAL CHOLESTEROL MMOL/L  
LBXSCH = SAME IN MG/DL  
LBDHDD = HDL CHOLEST MG/DL  
LBDHDDSI = HDL CHOLEST MMOL/L   
LBXTR - Triglyceride (mg/dL)  
LBDTRSI - Triglyceride (mmol/L)  
LBDLDL - LDL-Cholesterol, Friedewald (mg/dL)  
LBDLDLSI - LDL-Cholesterol, Friedewald (mmol/L)  
LBDLDLM - LDL-Cholesterol, Martin-Hopkins (mg/dL)  
LBDLDMSI - LDL-Cholesterol, Martin-Hopkins (mmol/L)  
LBDLDLN - LDL-Cholesterol, NIH equation 2 (mg/dL)  
LBDLDNSI - LDL-Cholesterol, NIH equation 2 (mmol/L)  

## BLOOD PRESSURE  
BPAOARM - Arm selected - oscillometric  
BPAOCSZ - Coded cuff size - oscillometric  
BPXOSY1 - Systolic - 1st oscillometric reading  
BPXODI1 - Diastolic - 1st oscillometric reading  
BPXOSY2 - Systolic - 2nd oscillometric reading  
BPXODI2 - Diastolic - 2nd oscillometric reading  
BPXOSY3 - Systolic - 3rd oscillometric reading  
BPXODI3 - Diastolic - 3rd oscillometric reading  
BPXOPLS1 - Pulse - 1st oscillometric reading  
BPXOPLS2 - Pulse - 2nd oscillometric reading  
BPXOPLS3 - Pulse - 3rd oscillometric reading  

## BODY MEASURES  
BMDSTATS - Body Measures Component Status Code  
BMXWT - Weight (kg)  
BMIWT - Weight Comment  
BMXHT - Standing Height (cm)  
BMIHT - Standing Height Comment  
BMXBMI - Body Mass Index (kg/m**2)  
BMDBMIC - BMI Category - Children/Youth   
BMXWAIST - Waist Circumference (cm)  
BMIWAIST - Waist Circumference Comment  
BMXHIP - Hip Circumference (cm)  
BMIHIP - Hip Circumference Comment  

## QUESTIONNAIRE  

### BP and Cholesterol  
BPQ020 - Ever told you had high blood pressure  
BPQ030 - Told had high blood pressure - 2+ times  
BPD035 - Age told had hypertension  
BPQ040A - Taking prescription for hypertension  
BPQ050A - Now taking prescribed medicine for HBP  
BPQ080 - Doctor told you - high cholesterol level  
BPQ060 - Ever had blood cholesterol checked  
BPQ070 - When blood cholesterol last checked  
BPQ090D - Told to take prescriptn for cholesterol  
BPQ100D - Now taking prescribed medicine  

### Cardiovascular health
CDQ001 - SP ever had pain or discomfort in chest
CDQ002 - SP get it walking uphill or in a hurry
CDQ003 - During an ordinary pace on level ground
CDQ003A - CHECK ITEM
CDQ004 - If so does SP continue or slow down
CDQ005 - Does standing relieve pain/discomfort
CDQ006 - How soon is the pain relieved
CDQ009A - Pain in right arm
CDQ009B - Pain in right chest
CDQ009C - Pain in neck
CDQ009D - Pain in upper sternum
CDQ009E - Pain in lower sternum
CDQ009F - Pain in left chest
CDQ009G - Pain in left arm
CDQ009H - Pain in epigastric area
CDQ008 - Severe pain in chest more than half hour
CDQ010 - Shortness of breath on stairs/inclines  

### Diabetes
SEQN - Respondent sequence number
DIQ010 - Doctor told you have diabetes
DID040 - Age when first told you had diabetes
DIQ159 - CHECK ITEM
DIQ160 - Ever told you have prediabetes
DIQ180 - Had blood tested past three years
DIQ050 - Taking insulin now
DID060 - How long taking insulin
DIQ060U - Unit of measure (month/year)
DIQ065 - CHECK ITEM
DIQ070 - Take diabetic pills to lower blood sugar
DIQ229 - CHECK ITEM
DIQ230 - How long ago saw a diabetes specialist
DIQ240 - Is there one Dr you see for diabetes
DID250 - Past year how many times seen doctor
DID260 - How often check blood for glucose/sugar
DIQ260U - Unit of measure (day/week/month/year)
DIQ275 - Past year Dr checked for A1C
DIQ280 - What was your last A1C level
DIQ291 - What does Dr say A1C should be
DIQ295 - CHECK ITEM
DIQ300S - What was your recent SBP
DIQ300D - What was your recent DBP
DID310S - What does Dr say SBP should be
DID310D - What does Dr say DBP should be
DID320 - What was most recent LDL number
DID330 - What does Dr say LDL should be
DID341 - Past year times Dr check feet for sores
DID350 - How often do you check your feet
DIQ350U - Unit of measure (day/week/month/year)
DIQ360 - Last time had pupils dilated for exam

### Medical conditions  
MCQ160b - Ever told had congestive heart failure
MCD180b - Age when told you had heart failure
MCQ160c - Ever told you had coronary heart disease
MCD180c - Age when told had coronary heart disease
MCQ160d - Ever told you had angina/angina pectoris
MCD180d - Age when told you had angina pectoris
MCQ160e - Ever told you had heart attack
MCD180e - Age when told you had heart attack
MCQ160f - Ever told you had a stroke
MCD180F - Age when told you had a stroke

### Current health status
SEQN - Respondent sequence number
HSD010 - General health condition
HSQ500 - SP have head cold or chest cold
HSQ510 - SP have stomach or intestinal illness?
HSQ520 - SP have flu, pneumonia, ear infection?
HSQ571 - SP donated blood in past 12 months?
HSQ580 - How long ago was last blood donation?
HSQ590 - Blood ever tested for HIV virus?
HSAQUEX - Source of Health Status Data


### Prescription Medications  

### Smoking  
SMQ020 - Smoked at least 100 cigarettes in life

### Income  
SEQN - Respondent sequence number
INDFMMPI - Family monthly poverty level index
INDFMMPC - Family monthly poverty level category

### Health insurance
SEQN - Respondent sequence number
HIQ011 - Covered by health insurance
HIQ032A - Covered by private insurance
HIQ032B - Covered by Medicare
HIQ032C - Covered by Medi-Gap
HIQ032D - Covered by Medicaid
HIQ032E - Covered by CHIP
HIQ032H - Covered by state-sponsored health plan
HIQ032I - Covered by other government insurance
HIQ032J - Not covered by any insurance
HID259 - CHECK ITEM
HIQ260 - Have Medicare?
HIQ105 - Insurance card available or not
HIQ269 - CHECK ITEM
HIQ270 - Do plans cover prescriptions?
HIQ210 - Time when no insurance in past year?



### Variables needed
 
##### Demographic factors: 
age, sex, race/ethnicity, education, income to poverty ratio?? INDFMPIR.

##### Clinical factors: 
duration of diabetes, Age when first told you had diabetes: DID040
presence of complications (e.g., retinopathy, neuropathy, cardiovascular disease, hypertension, CKD (eGFR<60/acr>=30), ).
had retinopathy DIQ080
Take diabetic pills to lower blood sugar DIQ070
Taking insulin now DIQ050


##### Behavioral factors: 
smoking status, physical activity, diet.

##### Psychosocial factors: 
depression, social support.
Food insecurity

#### Labs/measures
cholesterol, triglycerides, HbA1c(LBXGH), 
SBP, DBP, BMI, 
