# KY Attainment Analysis in R

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Set file paths
attain_path <- "G:/My Drive/ky attainment/"

# Load ACS Microdata
acs_data <- read.csv(paste0(attain_path, "ky_attainment_data.csv"))

# Step 1: Descriptive Analysis for Treatment and Control Groups

# Treatment group pre-policy
treat_pre_ad <- acs_data %>%
  filter(statefip == "kentucky", year <= 2015) %>%
  summarise(mean_adhigher = weighted.mean(adhigher, perwt, na.rm = TRUE)) %>%
  pull(mean_adhigher)

# Treatment group post-policy
treat_post_ad <- acs_data %>%
  filter(statefip == "kentucky", year >= 2016) %>%
  summarise(mean_adhigher = weighted.mean(adhigher, perwt, na.rm = TRUE)) %>%
  pull(mean_adhigher)

# Difference in treatment group pre and post-policy
treat_postpre_ad <- treat_post_ad - treat_pre_ad
cat("Treatment Pre-Post Difference:", treat_postpre_ad, "\n")

# Control group pre-policy
cont_pre_ad <- acs_data %>%
  filter(statefip != "kentucky", year <= 2015) %>%
  summarise(mean_adhigher = weighted.mean(adhigher, perwt, na.rm = TRUE)) %>%
  pull(mean_adhigher)

# Control group post-policy
cont_post_ad <- acs_data %>%
  filter(statefip != "kentucky", year >= 2016) %>%
  summarise(mean_adhigher = weighted.mean(adhigher, perwt, na.rm = TRUE)) %>%
  pull(mean_adhigher)

# Difference in control group pre and post-policy
cont_postpre_ad <- cont_post_ad - cont_pre_ad
cat("Control Pre-Post Difference:", cont_postpre_ad, "\n")

# Unadjusted Difference-in-Differences Estimate
did_unadj_ad <- treat_postpre_ad - cont_postpre_ad
cat("Unadjusted DiD Estimate:", did_unadj_ad, "\n")

# Step 2: Simple Regression Estimates Without Covariates
acs_data <- acs_data %>%
  mutate(
    stateKY = ifelse(statefip == "kentucky", 1, 0),
    postpolicy = ifelse(year >= 2016, 1, 0),
    stateKYXpostpolicy = stateKY * postpolicy
  )

simple_model <- lm(adhigher ~ stateKY * postpolicy, data = acs_data, weights = perwt)
summary(simple_model)

# Step 3: Visualizing Treatment and Control Group Trends
trend_data <- acs_data %>%
  mutate(group = ifelse(statefip == "kentucky", "Kentucky", "Control Group")) %>%
  group_by(year, group) %>%
  summarise(
    mean_adhigher = weighted.mean(adhigher, perwt, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(trend_data, aes(x = year, y = mean_adhigher, color = group)) +
  geom_line() +
  geom_vline(xintercept = 2016, linetype = "dashed") +
  labs(
    title = "Trends in Associate Degree or Higher Attainment",
    x = "Year",
    y = "Mean Associate Degree or Higher",
    color = "Group"
  ) +
  theme_minimal()

# Step 4: Regression Estimates With Covariates
covariate_model <- lm(
  adhigher ~ stateKY * postpolicy + black + asian + hispanic + otherrace +
    female + metrodummy + ftotinc + age,
  data = acs_data,
  weights = perwt
)
summary(covariate_model)

# Step 5: Main Specification Regression with Fixed Effects and Demographic Interaction Terms

# Convert indicator variables into factors
acs_data <- acs_data %>%
  mutate(
    year = as.factor(year),
    statefip = as.factor(statefip),
  )

# set reference levels 
acs_data <- acs_data %>%
  mutate(
    year = relevel(year, ref = "2010"), 
    statefip = relevel(statefip, ref = "california")
  )

main_model <- lm(
  adhigher ~ stateKY * postpolicy + # Main treatment effects 
    black + asian + hispanic + otherrace + # Covariates 
    female + metrodummy + ftotinc + age + 
    stateKY * postpolicy * black + stateKY * postpolicy * asian + # Race interaction terms 
    stateKY * postpolicy * hispanic + stateKY * postpolicy * otherrace +
    year + statefip,  # Fixed effects
  data = acs_data,
  weights = perwt
)
summary(main_model)

# Step 6: Subsample Analysis
subsample_35_39 <- acs_data %>%
  filter(age >= 35 & age <= 39)

subsample_model <- lm(
  adhigher ~ stateKY * postpolicy +
    black + asian + hispanic + otherrace +
    female + metrodummy + ftotinc + age +
    stateKY * postpolicy * black + stateKY * postpolicy * asian +
    stateKY * postpolicy * hispanic + stateKY * postpolicy * otherrace +
    year + statefip,
  data = subsample_35_39,
  weights = perwt
)
summary(subsample_model)


# Step 7: Sensitivity Tests

# Parallel Trends Assumption
parallel_model <- lm(
  adhigher ~ black + asian + hispanic + otherrace + female + metrodummy +
    ftotinc + age +
    stateKY * y2010 + stateKY * y2011 + stateKY * y2012 + stateKY * y2013 +
    stateKY * y2014 + stateKY * y2015 + stateKY * y2017 + stateKY * y2018 +
    stateKY * y2019 +
    stateKY * postpolicy * black + stateKY * postpolicy * asian +
    stateKY * postpolicy * hispanic + stateKY * postpolicy * otherrace,
  data = acs_data,
  weights = perwt
)
summary(parallel_model)


# Exclude California, New York, and Both

# Filter out California
acs_data_no_ca <- acs_data %>% 
  filter(statefip != "california") %>%
  mutate(statefip = relevel(as.factor(statefip), ref = "delaware"))  # Set Delaware as reference

# Filter out New York
acs_data_no_ny <- acs_data %>% 
  filter(statefip != "new york") %>%
  mutate(statefip = relevel(as.factor(statefip), ref = "california"))  # Set California as reference

# Filter out both California and New York
acs_data_no_ca_ny <- acs_data %>% 
  filter(statefip != "california" & statefip != "new york") %>%
  mutate(statefip = relevel(as.factor(statefip), ref = "delaware"))  # Set Delaware as reference

# Model excluding California
model_no_ca <- lm(
  adhigher ~ stateKY * postpolicy +  # Main treatment effects
    black + asian + hispanic + otherrace +  # Covariates
    female + metrodummy + ftotinc + age +
    stateKY * postpolicy * black + stateKY * postpolicy * asian +  # Race interaction terms
    stateKY * postpolicy * hispanic + stateKY * postpolicy * otherrace +
    year + statefip,  # Fixed effects
  data = acs_data_no_ca,
  weights = perwt
)
summary(model_no_ca)

# Model excluding New York
model_no_ny <- lm(
  adhigher ~ stateKY * postpolicy +  # Main treatment effects
    black + asian + hispanic + otherrace +  # Covariates
    female + metrodummy + ftotinc + age +
    stateKY * postpolicy * black + stateKY * postpolicy * asian +  # Race interaction terms
    stateKY * postpolicy * hispanic + stateKY * postpolicy * otherrace +
    year + statefip,  # Fixed effects
  data = acs_data_no_ny,
  weights = perwt
)
summary(model_no_ny)

# Model excluding both California and New York
model_no_ca_ny <- lm(
  adhigher ~ stateKY * postpolicy +  # Main treatment effects
    black + asian + hispanic + otherrace +  # Covariates
    female + metrodummy + ftotinc + age +
    stateKY * postpolicy * black + stateKY * postpolicy * asian +  # Race interaction terms
    stateKY * postpolicy * hispanic + stateKY * postpolicy * otherrace +
    year + statefip,  # Fixed effects
  data = acs_data_no_ca_ny,
  weights = perwt
)
summary(model_no_ca_ny)
