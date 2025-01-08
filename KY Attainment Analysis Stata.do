clear
* KY Attainment Analysis

// Set macros for file paths
local attain "G:\My Drive\ky attainment\"

// Set working directory
cd "`attain'"

// Set log file
set logtype text
capture log close
log using "KY Attainment Log", replace

// Load ACS Microdata
use "`attain'ky_attainment_data.dta"

/*
    - This analysis examines the impact of Kentucky's 60x30 statewide attainment goal, implemented in 2016, on associate's degree or higher attainment using a Difference-in-Differences (DiD) framework.
	- Outcome variable: Percentage of the population aged 25 to 64 years old with an associate's degree or higher.
    - Kentucky is the treatment group, and states with no statewide attainment goals are the control group.
    - Analysis includes:
      - Descriptive statistics and simple DiD estimates.
      - Multiple regression specifications with covariates and fixed effects.
      - Subsample analysis and sensitivity tests.
*/

// Step 1: Descriptive Analysis for Treatment and Control Groups

// Treatment group pre-policy
summ adhigher if statefip == 21 & year <= 2015 [fweight=perwt]
gen treat_pre_ad = r(mean)

// Treatment group post-policy
summ adhigher if statefip == 21 & year >= 2016 [fweight=perwt]
gen treat_post_ad = r(mean)

// Difference in treatment group pre and post-policy
gen treat_postpre_ad = treat_post_ad - treat_pre_ad
noi disp treat_postpre_ad[1]

// Control group pre-policy
summ adhigher if statefip != 21 & year <= 2015 [fweight=perwt]
gen cont_pre_ad = r(mean)

// Control group post-policy
summ adhigher if statefip != 21 & year >= 2016 [fweight=perwt]
gen cont_post_ad = r(mean)

// Difference in control group pre and post-policy
gen cont_postpre_ad = cont_post_ad - cont_pre_ad
noi disp cont_postpre_ad[1]

// Unadjusted Difference-in-Differences Estimate
gen did_unadj_ad = treat_postpre_ad - cont_postpre_ad
noi disp did_unadj_ad[1]  // Simple DiD Estimate

// Step 2: Simple Regression Estimates Without Covariates
gen stateKY = statefip == 21
gen postpolicy = year >= 2016
gen stateKYXpostpolicy = stateKY * postpolicy

reg adhigher i.stateKY##i.postpolicy [fweight=perwt]
// Interpretation: The coefficient for the interaction term (stateKY#postpolicy) equals the simple DiD estimate (~0.0054 or 0.54%).

// Step 3: Visualizing Treatment and Control Group Trends
label variable adhigher "Associate Degree or Higher"

bysort year: egen adhigher_control = mean(adhigher) if statefip != 21
bysort year: egen adhigher_treat = mean(adhigher) if statefip == 21

twoway (line adhigher_treat year, xline(2016)) (line adhigher_control year), ///
    ytitle("Associate Degree and Higher") ///
    legend(order(1 "Kentucky" 2 "Control Group")) ///
    note("Source: IPUMS USA - University of Minnesota - www.ipums.org", size(small)) ///
    title("Figure 7", size(medium))
// A copy of this graph will be added to GitHub

// Step 4: Regression Estimates With Covariates

reg adhigher i.stateKY##i.postpolicy black asian hispanic otherrace female metrodummy ftotinc age [fweight=perwt]
// Interpretation: The DiD coefficient increases to ~0.00995 (~1%) and is statistically significant (t-score = 41.34).

// Step 5: Main Specification Regression with Fixed Effects and Demographic Interaction Terms

reg adhigher i.stateKY##i.postpolicy black asian hispanic otherrace female metrodummy ftotinc age ///
    i.stateKY##i.postpolicy##i.black i.stateKY##i.postpolicy##i.asian i.stateKY##i.postpolicy##i.hispanic ///
    i.stateKY##i.postpolicy##i.otherrace i.year i.statefip [fweight=perwt]
// Interpretation: The DiD estimate for the main specification of the model (~0.01238, t=46.98) suggests that the application of treatment has increased the probability that a person in Kentucky will have an associate degree or higher by 1.24 percent compared to the control group of states.

// Step 6: Subsample Analysis
// Estimate main specification for specific age cohorts. 
// Here, we show results for the 35-39 age cohort, which had the largest effect size.
preserve
keep if age >= 35 & age <= 39
reg adhigher i.stateKY##i.postpolicy black asian hispanic otherrace female metrodummy ftotinc age ///
    i.stateKY##i.postpolicy##i.black i.stateKY##i.postpolicy##i.asian i.stateKY##i.postpolicy##i.hispanic ///
    i.stateKY##i.postpolicy##i.otherrace i.year i.statefip [fweight=perwt]
restore
// Interpretation: For the 35-39 age cohort, the DiD estimate is ~0.0296 (~3%), the largest observed effect across all age cohorts (t = 39.21).

// Step 7: Sensitivity Tests

// Test Parallel Trends Assumption with Interaction Terms 
// We test the core DiD assumption of pre-treatment parallel trends statistically by including interaction terms for stateKY and year indicator variables in the model.

reg adhigher black asian hispanic otherrace female metrodummy ftotinc age ///
    i.stateKY##i.y2010 i.stateKY##i.y2011 i.stateKY##i.y2012 i.stateKY##i.y2013 i.stateKY##i.y2014 i.stateKY##i.y2015 ///
    i.stateKY##i.y2017 i.stateKY##i.y2018 i.stateKY##i.y2019 i.stateKY##i.postpolicy##i.black ///
    i.stateKY##i.postpolicy##i.asian i.stateKY##i.postpolicy##i.hispanic i.stateKY##i.postpolicy##i.otherrace [fweight=perwt]
// Interpretation: Statistically significant coefficients for pre-treatment interaction terms suggest possible violations of the parallel trends assumption.

// Excluding California and New York from the Control Group
// New York and California are states with large populations and demographic characteristics that differ from Kentucky. Removing them tests the robustness of the model.

preserve
drop if statefip == 06  // Remove California
reg adhigher i.stateKY##i.postpolicy black asian hispanic otherrace female metrodummy ftotinc age ///
    i.stateKY##i.postpolicy##i.black i.stateKY##i.postpolicy##i.asian i.stateKY##i.postpolicy##i.hispanic ///
    i.stateKY##i.postpolicy##i.otherrace i.year i.statefip [fweight=perwt]
restore

preserve
drop if statefip == 36  // Remove New York
reg adhigher i.stateKY##i.postpolicy black asian hispanic otherrace female metrodummy ftotinc age ///
    i.stateKY##i.postpolicy##i.black i.stateKY##i.postpolicy##i.asian i.stateKY##i.postpolicy##i.hispanic ///
    i.stateKY##i.postpolicy##i.otherrace i.year i.statefip [fweight=perwt]
restore

preserve
drop if statefip == 06 | statefip == 36  // Remove Both California and New York
reg adhigher i.stateKY##i.postpolicy black asian hispanic otherrace female metrodummy ftotinc age ///
    i.stateKY##i.postpolicy##i.black i.stateKY##i.postpolicy##i.asian i.stateKY##i.postpolicy##i.hispanic ///
    i.stateKY##i.postpolicy##i.otherrace i.year i.statefip [fweight=perwt]
restore
// Interpretation: The DiD coefficient remains positive and statistically significant in all cases, ranging from ~0.0087 to ~0.014.
log close
