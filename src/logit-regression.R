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
# surprised to see at GRADEATT less than 5 (which indicates grades 9-12). In 2022,
# it appears that there were only 500 respondents who were currently in school, and 
# those folks were in grades 5 through 8.
ipums_relate |> filter(YEAR == 2022) |> filter(AGE >= 18) |> count(GRADEATT)

# EDUC: educational attainment. 
ipums_relate |> filter(YEAR == 2022) |> filter(AGE >= 18) |> count(EDUC) |> print(n = 100)

# Disabilites: 
ipums_relate |> count(DIFFREM) # cognitive. Note that the NA values are for individuals < age 5
ipums_relate |> count(DIFFPHYS) # physical. Note that the NA values are for individuals < age 5
ipums_relate |> count(DIFFMOB) # independent living. Note that the NA values are for individuals < age 16
ipums_relate |> count(DIFFCARE) # self-care (dressing/bathing). Note that the NA values are for individuals < age 5
ipums_relate |> count(DIFFSENS) # hearing or vision. All individuals.


# ----- Step 4: Prepare variables for regression ---- #
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
    # white is the excluded race category
    is_married = (MARST %in% c(1, 2)), # TRUE if married
    native_born = (BPL < 100), # TRUE if born in the United States
    dis_cognitive = (DIFFREM == 2), # TRUE if cognitive disability
    dis_physical = (DIFFPHYS == 2), # TRUE if physical disability
    dis_independent = (DIFFMOB == 2), # TRUE if independent living disability
    dis_selfcare = (DIFFCARE == 2), # TRUE if self-care disability
    dis_hearingvision = (DIFFSENS == 2), # TRUE if vision or hearing disability
    # Outcome variables
    cohabit = cohabit_bin %in% c(1, 2, 3), # TRUE if lives with parent
    cohabit_dependent = (cohabit_bin == 3),
    cohabit_provider = (cohabit_bin == 1),
    cohabit_not_provider = cohabit_bin %in% c(2,3)
    )

# Some more sanity checks
# Fraction of over 25 year olds in the US in 2022 with a high school diploma
hs_dipl_table <- ipums_for_logit |> filter(YEAR == 2022 & AGE >= 25) |> count(hs_diploma) |> collect()
with_dipl <- hs_dipl_table |> filter(hs_diploma == TRUE)|> pull(n)
wo_dipl <- hs_dipl_table |> filter(hs_diploma == FALSE)|> pull(n)
with_dipl / (with_dipl + wo_dipl)

# Fraction of over 25 year olds in the US in 2022 with a 4-year bachelor's diploma
bach_dipl_table <- ipums_for_logit |> filter(YEAR == 2022 & AGE >= 25) |> count(bach_diploma) |> collect()
with_dipl <- bach_dipl_table |> filter(bach_diploma == TRUE) |> pull(n)
wo_dipl <- bach_dipl_table |> filter(bach_diploma == FALSE)|> pull(n)
with_dipl / (with_dipl + wo_dipl)

# Fraction of 18 - 65 year old population in 2022 that is employed
ipums_for_logit |> filter(YEAR == 2022 & AGE >= 18 & AGE <= 65) |> count(is_emp)

# Race
ipums_for_logit |> count(is_aapi)
ipums_for_logit |> count(is_aian)
ipums_for_logit |> count(is_black)
ipums_for_logit |> count(is_hispanic)
ipums_for_logit |> count(is_multiracial)
ipums_for_logit |> count(is_otherrace)

# Married
ipums_for_logit |> filter(YEAR == 2022 & AGE >= 25) |> count(is_married)

# Native born
ipums_for_logit |> filter(YEAR == 2022) |> count(native_born)

# Disability
ipums_for_logit |> count(dis_cognitive)
ipums_for_logit |> count(dis_physical)
ipums_for_logit |> count(dis_independent)
ipums_for_logit |> count(dis_selfcare)
ipums_for_logit |> count(dis_hearingvision)

# ----- Step 4: Preliminary logit regression on 2022 data ----- #

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
              family = "binomial"
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
              family = "binomial"
)

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