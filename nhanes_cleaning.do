*** data cleaning ***
               
*Number of months taking insulin, converted to years
                replace diq060q=diq060q/12 if diq060u==1 //1999-2000 
                                replace diq060q=. if diq060q==99999
                replace did060q=did060q/12 if diq060u==1 //2001-2004
                replace did060=did060/12 if diq060u==1 //2005-2016
                                replace did060=. if did060==999
                                replace did060=.04 if did060==666 //less than 1 month
                               
*Calculate number of years on insulin
gen yrs_insulin=min(diq060q,did060q,did060)
 
*Calculate age when started insulin
gen age_insulin=ridageyr-yrs_insulin
 
*Replace age when diagnosed with diabetes to missing for the below values
                replace diq040q=. if diq040q==99999|diq040q==77777
                replace did040=. if did040==999|did040==666
 
gen age_diabts=diq040q
                replace age_diabts=did040q if missing(age_diabts)
                replace age_diabts=did040 if missing(age_diabts)
replace age_diabts=ridageyr if age_diabts>ridageyr & !missing(age_diabts)
 
gen time_diabts=ridageyr-age_diabts
 
gen time_diabts_cat=0 if time_diabts<5
                replace time_diabts_cat=1 if time_diabts>=5 & time_diabts<15
                replace time_diabts_cat=2 if time_diabts>=15 & !missing(time_diabts)
               
               
*** more variables ---------------------------
                gen hdl=lbdhdd
                                replace hdl=lbxhdd if missing(hdl)
                                replace hdl=lbdhdl if missing(hdl)
                               
                gen race=1 if ridreth1==3
                                replace race=2 if ridreth1==4
                                replace race=3 if ridreth1==1|ridreth1==2
                                replace race=4 if ridreth1==5
                                label define race_lbl 1 "Non-Hispanic White" 2 "Non-Hispanic Black" 3 "Hispanic" 4 "Other"
                                label value race race_lbl
                label var race "Race"
               
                gen black=(ridreth1==4)
                                label define black_lbl 1 "Non-Hispanic Black" 0 "Non-Black"
                                label value black black_lbl
                label var black "Black"
                gen female=riagendr==2
                                label var female "Femle"
                *Age
                                gen age_gt65=0 if ridageyr<65
                                                replace age_gt65=1 if ridageyr>=65
                                               
                                label define age_gt65_lbl 0 "Age<65" 1 "Age {&ge}65"
                                label value age_gt65 age_gt65_lbl
                               
                                tab age_gt65
                                label var age_gt65 "Age>=65"
                               
                                gen age_grp=0 if ridageyr<=44
                                                replace age_grp=1 if ridageyr>44 & ridageyr<=64
                                                replace age_grp=2 if ridagey>64
                                               
                                label define age_grp_lbl 0 "Age 20-44" 1 "Age 45-64" 2 "Age {&ge}65"
                                label value age_grp age_grp_lbl
                               
                               
                *PIR
                gen pir_calc=0 if indfmpir<1
                                replace pir_calc=1 if indfmpir>=1 & !missing(indfmpir)
                               
                                label define pir_calc_lbl 0 "Below poverty threshold" 1 ">=1 times poverty threshold"
                                label value pir_calc pir_calc_lbl
                               
                                label var pir_calc "PIR"
                                egen pir_calc_vers2=cut(indfmpir), at(0 0.5,1,1.25,2,5.5)
                                tabstat indfmpir, by(pir_calc_vers2) stat(n min max)
               
                *Education
                gen education=0 if dmdeduc2==1
                                replace education=1 if dmdeduc2==2|dmdeduc2==3
                                replace education=2 if dmdeduc2==4|dmdeduc2==5
                               
                                label define education_lbl 0 "<High school" 1 "High school degree" 2 "> High school degree"
                                label value education education_lbl
                                label var education "Education"
                *Health insurance
                gen hlth_ins=0 if hiq031a==14|hid030a==1
                                replace hlth_ins=1 if hiq031b==15|hiq031c==16|hiq031d==17|hiq031e==18|hiq031f==19|hiq031g==20|hiq031h==21|hiq031i==22
                                                replace hlth_ins=1 if hid030b==1|hid030c==1|hid030d==1
                                replace hlth_ins=2 if hiq011==2|hid010==2
                               
                                label define hlth_ins_lbl 0 "Private insurance" 1 "Public insurance" 2 "No insurance"
                                label value hlth_ins hlth_ins_lbl
                                tab hlth_ins
                               
                                label var hlth_ins "Health insurance"
                *Prevalent CVD
                foreach v of varlist mcq160c mcq160d mcq160e mcq160f {
                                tab `v' sddsrvyr
                }
               
                gen prvcvd = .
                                replace prvcvd = 1 if (mcq160c==1 | mcq160f==1 | mcq160b==1)
                                replace prvcvd = 0 if (mcq160c==2 & mcq160f==2 & mcq160b==2)
                label var prvcvd "CVD, self-report"
 
                tab prvcvd sddsrvyr
 
                *smoking
               
                gen smoking=0 if smq020!=7 & smq020!=9 & smq020!=.
                                replace smoking=1 if smq040==1|smq040==2
                                replace smoking=1 if lbxcot>10 & !missing(lbxcot)
                tab smoking sddsrvyr
                *Serum creatinine
                gen scr_stnd=lbdscrsi/88.4
                label variable scr_stnd "Creatinine (mg/dL)"
               
                replace scr_stnd=(1.013*scr_stnd)+0.147 if sddsrvyr==1
                replace scr_stnd=(0.978*scr_stnd)-0.016 if sddsrvyr==4
               
                *eGFR (CKD-EPI)
                gen egfr_CKDEPI_stnd = 166 * (0.993)^ridageyr * (scr_stnd/ 0.7)^(-0.329) if riagendr==2 & black==1 & scr_stnd<=0.7
                                replace egfr_CKDEPI_stnd = 166 * (0.993)^ridageyr * (scr_stnd/ 0.7)^(-1.209) if riagendr==2 & black==1 & scr_stnd>0.7
                                replace egfr_CKDEPI_stnd = 163 * (0.993)^ridageyr * (scr_stnd/ 0.9)^(-0.411) if riagendr==1 & black==1 & scr_stnd<=0.9
                                replace egfr_CKDEPI_stnd = 163 * (0.993)^ridageyr * (scr_stnd/ 0.9)^(-1.209) if riagendr==1 & black==1 & scr_stnd>0.9
                                replace egfr_CKDEPI_stnd = 144 * (0.993)^ridageyr * (scr_stnd / 0.7)^(-0.329) if riagendr==2 & black==0 & scr_stnd<=0.7
                                replace egfr_CKDEPI_stnd = 144 * (0.993)^ridageyr * (scr_stnd / 0.7)^(-1.209) if riagendr==2 & black==0 & scr_stnd>0.7
                                replace egfr_CKDEPI_stnd = 141 * (0.993)^ridageyr * (scr_stnd / 0.9)^(-0.411) if riagendr==1 & black==0 & scr_stnd<=0.9
                                replace egfr_CKDEPI_stnd = 141 * (0.993)^ridageyr * (scr_stnd / 0.9)^(-1.209) if riagendr==1 & black==0 & scr_stnd>0.9
                                replace egfr_CKDEPI_stnd = . if scr_stnd==.
                label variable egfr_CKDEPI_stnd "CKD-EPI eGFR"
 
                tabstat egfr_CKDEPI_stnd, by(sddsrvyr) stat(n min max mean sd)
               
                *ACR
                gen cr_adj=(1.02*sqrt(urxucr)-0.36)^2 if urxucr<75
                                replace cr_adj=(1.05*sqrt(urxucr)-0.74)^2 if urxucr>=75 & urxucr<250
                                replace cr_adj=(1.01*sqrt(urxucr)-0.1)^2 if urxucr>=250 & urxucr!=.
                                replace cr_adj=urxucr if sddsrvyr>=5
                gen acr=100*(urxuma/cr_adj)
               
                gen ckd=0 if !missing(egfr_CKDEPI_stnd) & !missing(acr)
                                replace ckd=1 if (egfr_CKDEPI_stnd<60)|(acr>=30 & !missing(acr))
                                label var ckd "CKD"
                *Antidiabetes medication
                                gen diab_pills=diq070
                                                replace diab_pills=did070 if sddsrvyr==4|sddsrvyr==5
                                               
                                gen antidiab_meds=0 if diq050!=1 & diab_pills!=1
                                                replace antidiab_meds=1 if diq050==1|diab_pills==1
                                tab antidiab_meds sddsrvyr,m  
 
gen mec18yr=(2/9)*wtmec4yr if sddsrvyr==1|sddsrvyr==2 /* for 1999-2002, 4/14=2/7 */
                replace mec18yr = (1/9) * wtmec2yr if inlist(sddsrvyr,3,4,5,6,7,8,9,10) /* for 2003-2012 */
gen mec20yr=(2/10)*wtmec4yr if sddsrvyr==1|sddsrvyr==2 /* for 1999-2002, 4/14=2/7 */
                replace mec20yr = (1/10) * wtmec2yr if inlist(sddsrvyr,3,4,5,6,7,8,9,10) /* for 2003-2012 */                         
 
 
gen glyc_control=0 if a1c_eq_round>=7 & !missing(a1c_eq_round)
                replace glyc_control=1 if a1c_eq_round<7
               
                label define glyc_control_lbl 0 "HbA1c>=7%" 1 "HbA1c<7%"
                label value glyc_control glyc_control_lbl
               
gen glyc_control_9=0 if a1c_eq_round<=9
                replace glyc_control_9=1 if a1c_eq_round>9 & !missing(a1c_eq_round)
               
                label define glyc_control9_lbl 0 "HbA1c<=9%" 1 "HbA1c>9%"
                label value glyc_control_9 glyc_control9_lbl        
               
*EYE EXAM
replace diq360=. if diq360==7|diq360==9
 
gen eye_exam=1 if diq360<=2
                replace eye_exam=0 if missing(eye_exam) & diq360!=.
               
*FOOT EXAM
tab did340 sddsrvyr
                replace did340=. if did340==7777|did340==9999
tab did341 sddsrvyr
                replace did341=. if did341==7777|did341==9999
               
gen foot_exam=1 if (did340>=1 & !missing(did340))|(did341>=1 & !missing(did341))
                replace foot_exam=0 if missing(foot_exam) & (did340==0|did341==0)
               
*Hosp overnight
replace hud080=. if hud080==99999|hud080==99
 
gen hosp_2nights=1 if hud080>=2 & !missing(hud080)
                replace hosp_2nights=1 if huq080>=2 & !missing(huq080)
                replace hosp_2nights=0 if hosp_2nights!=1 & (hud080==1|huq070==2)|(huq080==1|hud070==2)|(hud080==1|huq071==2)
 
*seen a doctor for diabetes, how many times
                replace did250=. if did250==7777|did250==9999
 
                *>=2 times
                gen doc_twotimes=0 if !missing(diq240)
                                replace doc_twotimes=1 if diq240==1 & did250>=2 & !missing(did250)
 
*HbA1c in past 12 months
                tab did270 //2005-08
                replace did270=. if did270==666|did270==777|did270==999
               
                tab diq275
                replace diq275=. if diq275==7|diq275==9
               
                gen hba1c_1yr=0 if !missing(diq275)|!missing(did270)
                                replace hba1c_1yr=1 if diq275==1|(did270>=1 &!missing(did270))
 
*blood cholesterol test in last year
                replace bpq060=. if bpq060==7|bpq060==9
                replace bpq070=. if bpq070==7|bpq070==9
               
                gen cholest_1yr=0 if !missing(bpq060)|!missing(bpq070)
                                replace cholest_1yr=1 if bpq060==1 & bpq070==1
                                replace cholest_1yr=1 if bpq070==1 & inlist(sddsrvyr,7,8,9,10) // adjust for bpq060 skip in year 2011-2016
 
*Comprehensive care
gen comp_care_doc=1 if doc_twotimes==1
gen comp_care_a1c=1 if hba1c_1yr==1
gen comp_care_eye=1 if eye_exam==1
gen comp_care_foot=1 if foot_exam==1
gen comp_care_cholest=1 if cholest_1yr==1
 
egen comp_care=rowtotal(comp_care_doc comp_care_a1c comp_care_eye comp_care_foot comp_care_cholest)
 
gen comp_care_5=0 if !missing(comp_care) & sddsrvyr>=4
                replace comp_care_5=1 if comp_care==5 & sddsrvyr>=4
               
foreach v of varlist comp_care_doc comp_care_a1c comp_care_eye comp_care_foot comp_care_cholest {
tab `v' sddsrvyr if sddsrvyr>=4
}
 
capture drop sddsrvyr_6yr
gen sddsrvyr_6yr=.
   replace sddsrvyr_6yr =0 if inlist(sddsrvyr,1,2,3)
   replace sddsrvyr_6yr =1 if inlist(sddsrvyr,4,5,6)
   replace sddsrvyr_6yr =2 if inlist(sddsrvyr,7,8,9)
  
capture drop weight_mec_6yr                  
                gen weight_mec_6yr=(2/3) * wtmec4yr if (sddsrvyr==1 | sddsrvyr==2)
                replace weight_mec_6yr=(1/3) * wtmec2yr if (sddsrvyr==3)
                replace weight_mec_6yr=(1/3) * wtmec2yr if (sddsrvyr==4 | sddsrvyr==5 | sddsrvyr==6)
                replace weight_mec_6yr=(1/3) * wtmec2yr if (sddsrvyr==7 | sddsrvyr==8 | sddsrvyr==9)
                               
replace insulin=1 if diq050==1
 
replace drug_therapy=0 if insulin==1
 
gen therapy=0 if drug_therapy==0|(drug_therapy>3 & !missing(drug_therapy))
                replace therapy=1 if drug_therapy==1
                replace therapy=2 if drug_therapy==2
                replace therapy=3 if drug_therapy==3
                replace therapy=4 if insulin==1
               
label define drug_tx 0 "No therapy" 1 "Monotherapy" 2 "Dual therapy" 3 "Triple therapy" 4 "Insulin"
label value therapy drug_tx
tab therapy sddsrvyr,m
 
gen monotherapy=0 if medication==1
                replace monotherapy=1 if drug_therapy==1
               
gen dual_therapy=0 if medication==1
                replace dual_therapy=1 if drug_therapy==2
               
gen triple_therapy=0 if medication==1
                replace triple_therapy=1 if drug_therapy==3
               
replace insulin=0 if missing(insulin)
 
gen sddsrvyr_9902=1 if sddsrvyr<=2
 
gen mec12yr=(1/6) * wtmec2yr if inlist(sddsrvyr,4,5,6,7,8,9) /* for 2005-2016 */
gen mec14yr=(1/7) * wtmec2yr if inlist(sddsrvyr,4,5,6,7,8,9,10) /* for 2005-2018 */
 
*treatment control
                gen hdl_control=0 if !missing(hdl)
                                replace hdl_control=1 if hdl>=40 & !missing(hdl) & female==0
                                replace hdl_control=1 if hdl>=50 & !missing(hdl) & female==1
 
                gen ldl_control=0 if !missing(lbdldl) & prvcvd==1 //different weights for AM group. At 2020 July 9th:LDL data is not available for 2017-18 data due to fasting sample not available now
                                replace ldl_control=1 if lbdldl<70 & prvcvd==1
               
*bpq090d
 
foreach v of varlist bpxdi1 bpxdi2 bpxdi3 bpxdi4 {
                replace `v'=. if `v'==0
}
 
summ bpxsy1 bpxsy2 bpxsy3 bpxsy4
summ bpxdi1 bpxdi2 bpxdi3 bpxdi4
 
foreach v of varlist bpxdi1 bpxdi2 bpxdi3 bpxdi4 {
                replace `v'=. if `v'==0
}
 
egen avg_sbp=rmean(bpxsy1 bpxsy2 bpxsy3 bpxsy4)
egen avg_dbp=rmean(bpxdi1 bpxdi2 bpxdi3 bpxdi4)
 
 
gen bp_ada=0 if !missing(avg_sbp) & !missing(avg_dbp)
                replace bp_ada=1 if avg_sbp<140 & avg_dbp<90
 
gen bp_acc=0 if !missing(avg_sbp) & !missing(avg_dbp)
                replace bp_acc=1 if avg_sbp<130 & avg_dbp<80
               
gen fast_wght_6yr=(1/3) * wtsaf2yr if (sddsrvyr==4 | sddsrvyr==5 | sddsrvyr==6)
                                replace fast_wght_6yr=(1/3)*wtsaf2yr if (sddsrvyr==7 | sddsrvyr==8 | sddsrvyr==9)
 
gen cholest_med=0 if !missing(bpq090d)
                replace cholest_med=1 if bpq090d==1
               
replace bpq090d=. if bpq090d==9
 
gen bmi_grp=0 if bmxbmi<25
                replace bmi_grp=1 if bmxbmi>=25 & bmxbmi<30
                replace bmi_grp=2 if bmxbmi>=30 & !missing(bmxbmi)
               
label define bmi 0 "<25" 1 "25-<30" 2 "{&ge} 30"
label value bmi_grp bmi
               
gen q=0.0112*(1.059^(age_diabts-55))*(0.525^female)*(0.39^0)*(1.35^smoking)*(1.183^(a1c_eq_round-6.72))*(1.088^((avg_sbp-135.7)/10))*(3.845^(ln(lbxtc/hdl)-1.59))
gen ukpds=100*(1-exp((-q*-1.1192764)/(1-1.078)))
drop q
 
gen bp_med=bpq040a
                replace bp_med=0 if missing(bp_med) & (bpq010==5|bpq020==2)
                replace bp_med=. if bpq040a==9
                recode bp_med (2=0)
 
gen no_bpmed=0 if bp_med==1
                replace no_bpmed=1 if bp_med==0
               
 
gen frs=(2.32888*ln(ridageyr)) + (1.20904*ln(lbxtc)) + (-0.70833*ln(hdl)) + ((2.82263-(no_bpmed*0.06106))*ln(avg_sbp)) + (0.52873*smoking) +(0.69154*1) if female==1
replace frs=(3.06117*ln(ridageyr))+(1.1237*ln(lbxtc))+(-0.93263*ln(hdl))+((1.99881-(no_bpmed*0.06578))*ln(avg_sbp))+(0.65451*smoking)+(0.57367*1) if female==0
 
replace frs=1-0.95012^(exp(frs-26.1931)) if female==1
replace frs=1-0.88936^(exp(frs-23.9802)) if female==0
 
replace frs=frs*100
 
gen frs_cat=0 if frs<10
                replace frs_cat=1 if frs>=10 & frs<20
                replace frs_cat=2 if frs>=20 & !missing(frs)
               
 
gen ukpds_cat=0 if ukpds<10
                replace ukpds_cat=1 if ukpds>=10 & ukpds<20
                replace ukpds_cat=2 if ukpds>=20 & !missing(ukpds)
               
label var age_grp "Age group"
label var bmi_grp "BMI group"
label var education "Education"
label var hlth_ins "Health insurance"
label var time_diabts_cat "Time since diabetes dx"
label var ukpds_cat "UKPDS"
label var frs_cat "FRS"