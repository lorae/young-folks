# logit-regression.R
#
# The purpose of this script is to predict whether an individual lives with their
# parents based on a number of predictor variables

# input:

# output:

# ----- Step 0: Load packages ----- #
library("duckdb")
library("dplyr")

# ----- Step 1: Source helper functions ----- #

devtools::load_all("../dataduck")

# ----- Step 2: Import data ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")

ipums_relate <- tbl(con, "ipums_relationships") 

# ----- Step 3: Some data quality pre-checks ----- #
# GRADEATT: grade level attending. This variable is 0 if the individual is not
# currently in school, and encoded based on the level of school they're in. I'd be
# surprised to see at GRADEATT less than 5 (which indicates grades 9-12) among
# 18+ year-olds. In 2022, it appears that there were only 500 respondents who were 
# encoded at less than high school, and all 500 of those observations were individuals
# in grades 5 through 8.
# This variable appears like it's encoded reasonably.
ipums_relate |> filter(YEAR == 2022 & AGE >= 18) |> count(GRADEATT)

# EDUC: educational attainment. I am surprised to see such a large number of over-18s
# who have no educational attainment (0). 
ipums_relate |> filter(YEAR == 2022 & AGE >= 18) |> count(EDUC) |> print(n = 100)

# Disabilites: 
ipums_relate |> count(DIFFREM) # cognitive. Note that the NA (0) values are for individuals < age 5
ipums_relate |> count(DIFFPHYS) # physical. Note that the NA (0) values are for individuals < age 5
ipums_relate |> count(DIFFMOB) # independent living. Note that the NA (0) values are for individuals < age 16
ipums_relate |> count(DIFFCARE) # self-care (dressing/bathing). Note that the NA (0) values are for individuals < age 5
ipums_relate |> count(DIFFSENS) # hearing or vision. All individuals: Note that there are no NA values.


# ----- Step 4: Prepare variables for regression ---- #
# The ipums_for_logit data frame will be a data frame with added columns that are
# encoded in a binary fashion for use in a logit model.
ipums_for_logit <- ipums_relate |>
  mutate(
    in_school = GRADEATT != 0, # TRUE if individual currently in school
    hs_diploma = EDUC %in% c(6, 7, 8, 9, 10, 11), # TRUE if individual attained HS diploma
    bach_diploma = EDUC %in% c(10, 11), # TRUE if individual attained 4-year degree
    is_emp = (EMPSTAT == 1), # TRUE if individual currently employed
    is_female = (SEX == 2), # TRUE if female, FALSE if male
    is_aapi = (RACE_ETH_bucket == "AAPI"),
    is_aian = (RACE_ETH_bucket == "AIAN"),
    is_black = (RACE_ETH_bucket == "Black"),
    is_hispanic = (RACE_ETH_bucket == "Hispanic"),
    is_multiracial = (RACE_ETH_bucket == "Multiracial"),
    is_otherrace = (RACE_ETH_bucket == "Other"),
    # @hite is the excluded race category to avoid multicollinearity issues
    is_married = (MARST %in% c(1, 2)), # TRUE if married
    native_born = (BPL < 100), # TRUE if born in the United States
    dis_cognitive = (DIFFREM == 2), # TRUE if cognitive disability
    dis_physical = (DIFFPHYS == 2), # TRUE if physical disability
    dis_independent = (DIFFMOB == 2), # TRUE if independent living disability
    dis_selfcare = (DIFFCARE == 2), # TRUE if self-care disability
    dis_hearingvision = (DIFFSENS == 2), # TRUE if vision or hearing disability
    # Outcome variables
    cohabit = cohabit_bin %in% c(1, 2, 3), # TRUE if lives with parent
    cohabit_dependent = (cohabit_bin == 3), # TRUE if lives with and depends on parent
    cohabit_not_provider = cohabit_bin %in% c(2,3), # TRUE if lives with and does not provide support to parent
    cohabit_provider = (cohabit_bin == 1) # TRUE if lives with and provides financial support to parent
    )

# ----- Step 5: Run some more sanity checks ----- #
# Note that these are crude estimates because they don't account for the needed
# weights to make the numbers representative of the population (PERWT). These simple
# calculations check that there aren't any glaring issues with the data.

# Function which takes a summary TRUE-FALSE table and outputs what proportion of 
# observations are true. Assumes col 1 contains TRUE and FALSE only.
prop_true <- function(tf_table) {
  
  # using base R because the name of the first column varies
  num_true <- tf_table[tf_table[[1]] == TRUE, "n"] |>
    as.numeric()
  num_false <- tf_table[tf_table[[1]] == FALSE, "n"] |>
    as.numeric()

  prop_true <- num_true / (num_true + num_false)
  
  return(prop_true)
  }

# Fraction of over 25 year olds in the US in 2022 with a high school diploma
ipums_for_logit |> filter(YEAR == 2022 & AGE >= 25) |> count(hs_diploma) |> collect() |> prop_true()

# Fraction of over 25 year olds in the US in 2022 with a 4-year bachelor's diploma
ipums_for_logit |> filter(YEAR == 2022 & AGE >= 25) |> count(bach_diploma) |> collect() |> prop_true()

# Fraction of 18 - 65 year old population in 2022 that is employed
ipums_for_logit |> filter(YEAR == 2022 & AGE >= 18 & AGE <= 65) |> count(is_emp) |> collect() |> prop_true()

# Race
ipums_for_logit |> filter(YEAR == 2022) |> count(is_aapi) |> collect() |> prop_true()
ipums_for_logit |> filter(YEAR == 2022) |> count(is_aian) |> collect() |> prop_true()
ipums_for_logit |> filter(YEAR == 2022) |> count(is_black) |> collect() |> prop_true()
ipums_for_logit |> filter(YEAR == 2022) |> count(is_hispanic) |> collect() |> prop_true()
ipums_for_logit |> filter(YEAR == 2022) |> count(is_multiracial) |> collect() |> prop_true()
ipums_for_logit |> filter(YEAR == 2022) |> count(is_otherrace) |> collect() |> prop_true()

# Married
ipums_for_logit |> filter(YEAR == 2022 & AGE >= 25) |> count(is_married) |> collect() |> prop_true()

# Native born
ipums_for_logit |> filter(YEAR == 2022) |> count(native_born) |> collect() |> prop_true()

# Disability
ipums_for_logit |> filter(YEAR == 2022) |> count(dis_cognitive) |> collect() |> prop_true()
ipums_for_logit |> filter(YEAR == 2022) |> count(dis_physical) |> collect() |> prop_true()
ipums_for_logit |> filter(YEAR == 2022) |> count(dis_independent) |> collect() |> prop_true()
ipums_for_logit |> filter(YEAR == 2022) |> count(dis_selfcare) |> collect() |> prop_true()
ipums_for_logit |> filter(YEAR == 2022) |> count(dis_hearingvision) |> collect() |> prop_true()

# ----- Step 6: Tun logit regressions on 2022 data ----- #

# MODEL 1: 2022, ages 18-40 (not in institution), living with parent
logit01 <- glm(cohabit ~ 
                in_school + 
                hs_diploma +
                bach_diploma +
                is_emp +
                is_female +
                AGE +
                is_aapi +
                is_aian +
                is_black +
                is_hispanic +
                is_multiracial +
                is_otherrace +
                is_married +
                native_born +
                dis_cognitive +
                dis_physical +
                dis_independent +
                dis_selfcare +
                dis_hearingvision, 
              data = ipums_for_logit |> filter(YEAR == 2022 & AGE >= 18 & AGE <= 40 & cohabit_bin != 9), 
              family = "binomial",
              weights = PERWT
)

# MODEL 1a: 2022, ages 18-24 (not in institution), living with parent
logit01a <- glm(cohabit ~ 
                in_school + 
                hs_diploma +
                bach_diploma +
                is_emp +
                is_female +
                AGE +
                is_aapi +
                is_aian +
                is_black +
                is_hispanic +
                is_multiracial +
                is_otherrace +
                is_married +
                native_born +
                dis_cognitive +
                dis_physical +
                dis_independent +
                dis_selfcare +
                dis_hearingvision, 
              data = ipums_for_logit |> filter(YEAR == 2022 & AGE >= 18 & AGE <= 24 & cohabit_bin != 9), 
              family = "binomial",
              weights = PERWT
)
# I have a complete separation problem here.

# MODEL 1b: 2022, ages 25-40 (not in institution), living with parent
logit01b <- glm(cohabit ~ 
                in_school + 
                hs_diploma +
                bach_diploma +
                is_emp +
                is_female +
                AGE +
                is_aapi +
                is_aian +
                is_black +
                is_hispanic +
                is_multiracial +
                is_otherrace +
                is_married +
                native_born +
                dis_cognitive +
                dis_physical +
                dis_independent +
                dis_selfcare +
                dis_hearingvision, 
              data = ipums_for_logit |> filter(YEAR == 2022 & AGE >= 25 & AGE <= 40 & cohabit_bin != 9), 
              family = "binomial"
              )

# MODEL 2: 2022, ages 18-40 (not in institution), not supporting the parent
logit02 <- glm(cohabit_not_provider ~ 
                in_school + 
                hs_diploma +
                bach_diploma +
                is_emp +
                is_female +
                AGE +
                is_aapi +
                is_aian +
                is_black +
                is_hispanic +
                is_multiracial +
                is_otherrace +
                is_married +
                native_born +
                dis_cognitive +
                dis_physical +
                dis_independent +
                dis_selfcare +
                dis_hearingvision, 
              data = ipums_for_logit |> filter(YEAR == 2022 & AGE >= 18 & AGE <= 40 & cohabit_bin != 9), 
              family = "binomial"
)

# MODEL 2a: 2022, ages 18-24 (not in institution), not supporting the parent
logit02a <- glm(cohabit_not_provider ~ 
                 in_school + 
                 hs_diploma +
                 bach_diploma +
                 is_emp +
                 is_female +
                 AGE +
                 is_aapi +
                 is_aian +
                 is_black +
                 is_hispanic +
                 is_multiracial +
                 is_otherrace +
                 is_married +
                 native_born +
                 dis_cognitive +
                 dis_physical +
                 dis_independent +
                 dis_selfcare +
                 dis_hearingvision, 
               data = ipums_for_logit |> filter(YEAR == 2022 & AGE >= 18 & AGE <= 24 & cohabit_bin != 9), 
               family = "binomial"
)

# MODEL 2: 2022, ages 25-40 (not in institution), not supporting the parent
logit02b <- glm(cohabit_not_provider ~ 
                 in_school + 
                 hs_diploma +
                 bach_diploma +
                 is_emp +
                 is_female +
                 AGE +
                 is_aapi +
                 is_aian +
                 is_black +
                 is_hispanic +
                 is_multiracial +
                 is_otherrace +
                 is_married +
                 native_born +
                 dis_cognitive +
                 dis_physical +
                 dis_independent +
                 dis_selfcare +
                 dis_hearingvision, 
               data = ipums_for_logit |> filter(YEAR == 2022 & AGE >= 25 & AGE <= 40 & cohabit_bin != 9), 
               family = "binomial"
)

# MODEL 3: 2022, ages 18-40 (not in institution), supporting the parent
logit03 <- glm(cohabit_provider ~ 
                 in_school + 
                 hs_diploma +
                 bach_diploma +
                 is_emp +
                 is_female +
                 AGE +
                 is_aapi +
                 is_aian +
                 is_black +
                 is_hispanic +
                 is_multiracial +
                 is_otherrace +
                 is_married +
                 native_born +
                 dis_cognitive +
                 dis_physical +
                 dis_independent +
                 dis_selfcare +
                 dis_hearingvision, 
               data = ipums_for_logit |> filter(YEAR == 2022 & AGE >= 18 & AGE <= 40 & cohabit_bin != 9), 
               family = "binomial"
)

# MODEL 3a: 2022, ages 18-24 (not in institution), supporting the parent
logit03a <- glm(cohabit_provider ~ 
                  in_school + 
                  hs_diploma +
                  bach_diploma +
                  is_emp +
                  is_female +
                  AGE +
                  is_aapi +
                  is_aian +
                  is_black +
                  is_hispanic +
                  is_multiracial +
                  is_otherrace +
                  is_married +
                  native_born +
                  dis_cognitive +
                  dis_physical +
                  dis_independent +
                  dis_selfcare +
                  dis_hearingvision, 
                data = ipums_for_logit |> filter(YEAR == 2022 & AGE >= 18 & AGE <= 24 & cohabit_bin != 9), 
                family = "binomial"
)

# MODEL 3: 2022, ages 25-40 (not in institution), not supporting the parent
logit03b <- glm(cohabit_provider ~ 
                  in_school + 
                  hs_diploma +
                  bach_diploma +
                  is_emp +
                  is_female +
                  AGE +
                  is_aapi +
                  is_aian +
                  is_black +
                  is_hispanic +
                  is_multiracial +
                  is_otherrace +
                  is_married +
                  native_born +
                  dis_cognitive +
                  dis_physical +
                  dis_independent +
                  dis_selfcare +
                  dis_hearingvision, 
                data = ipums_for_logit |> filter(YEAR == 2022 & AGE >= 25 & AGE <= 40 & cohabit_bin != 9), 
                family = "binomial"
)

# create a correlation matrix between the factors at play (except for age)
ipums_for_cov <- ipums_for_logit |> 
  filter(YEAR == 2022 & AGE >= 18 & AGE <= 40 & cohabit_bin != 9) |>
  select(
    in_school, hs_diploma, bach_diploma, is_emp, is_female, is_aapi, is_aian,
    is_black, is_hispanic, is_multiracial, is_otherrace, is_married, native_born,
    dis_cognitive, dis_physical, dis_independent, dis_selfcare, dis_hearingvision
  ) |>
  collect()

cov_matrix <- cor(ipums_for_cov, y = NULL, use = "everything", method = "pearson")

# ----- Step 7: Save results for use in Shiny app ----- #
# code here

