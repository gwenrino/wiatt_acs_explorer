# Load required libraries
library(tidycensus)
library(tidyverse)
library(rsconnect)

census_api_key("INSERT KEY HERE")

## Function to process pulled data
process_acs_pull <- function(input_df) {
  
  # Remove margin of error columns
  cols_to_remove <- grep(pattern = ".[M]$", x = names(input_df), value = TRUE)
  output_df <- input_df %>%
    select(-all_of(cols_to_remove)) %>%
    rename(geoid = GEOID, name = NAME)
  
  # Remove E from remaining columns
  colnames(output_df)[-c(1:2)] = substr(colnames(output_df)[-c(1:2)], 1, nchar(colnames(output_df)[-c(1:2)])-1)
  
  return(output_df)
}

## Function to summarize tract data
summarize_tract_data <- function(input_df) {

  # transpose columns
  output_df <- as.data.frame(t(input_df %>% select(-geoid)))

  # replace column names
  colnames(output_df) <- output_df["name",]

  # other processing
  output_df <- output_df %>%
    rownames_to_column("Variable") %>% # make rownames into a column
    filter(Variable != "name") # remove "name" row

  output_df <- output_df %>%
    mutate_at(c(2:ncol(output_df)), as.numeric) # convert all but first column to numerics

  # calculate totals and percentages
  output_df <- output_df %>% 
    mutate(`Tract Total` = rowSums(select(., starts_with("Census"))),
           `Area Total` = sum(`Tract Total`),
           Percent = round(`Tract Total`/`Area Total`,3))

  return(output_df)
}

# Define a function to get age data for a specific state
get_age_data <- function(state_code) {
  age <- get_acs(geography = "tract",
                 variables = c(male_under_5 = "B01001_003",
                               male_5_to_9 = "B01001_004",
                               male_10_to_14 = "B01001_005",
                               male_15_to_17 = "B01001_006",
                               male_18_and_19 = "B01001_007",
                               male_20 = "B01001_008",
                               male_21 = "B01001_009",
                               male_22_to_24 = "B01001_010",
                               male_25_to_29 = "B01001_011",
                               male_30_to_34 = "B01001_012",
                               male_35_to_39 = "B01001_013",
                               male_40_to_44 = "B01001_014",
                               male_45_to_49 = "B01001_015",
                               male_50_to_54 = "B01001_016",
                               male_55_to_59 = "B01001_017",
                               male_60_and_61 = "B01001_018",
                               male_62_to_64 = "B01001_019",
                               male_65_and_66 = "B01001_020",
                               male_67_to_69 = "B01001_021",
                               male_70_to_74 = "B01001_022",
                               male_75_to_79 = "B01001_023",
                               male_80_to_84 = "B01001_024",
                               male_over_85 = "B01001_025",
                               female_under_5 = "B01001_027",
                               female_5_to_9 = "B01001_028",
                               female_10_to_14 = "B01001_029",
                               female_15_to_17 = "B01001_030",
                               female_18_and_19 = "B01001_031",
                               female_20 = "B01001_032",
                               female_21 = "B01001_033",
                               female_22_to_24 = "B01001_034",
                               female_25_to_29 = "B01001_035",
                               female_30_to_34 = "B01001_036",
                               female_35_to_39 = "B01001_037",
                               female_40_to_44 = "B01001_038",
                               female_45_to_49 = "B01001_039",
                               female_50_to_54 = "B01001_040",
                               female_55_to_59 = "B01001_041",
                               female_60_and_61 = "B01001_042",
                               female_62_to_64 = "B01001_043",
                               female_65_and_66 = "B01001_044",
                               female_67_to_69 = "B01001_045",
                               female_70_to_74 = "B01001_046",
                               female_75_to_79 = "B01001_047",
                               female_80_to_84 = "B01001_048",
                               female_over_85 = "B01001_049"),
                 state = state_code,
                 year = 2020,
                 output = "wide")
  
  return(age)
}

# Define a function to get sex data for a specific state
get_sex_data <- function(state_code) {
  sex <- get_acs(geography = "tract", 
                 variables = c(male = "B01001_002",
                               female = "B01001_026"),
                 state = state_code, 
                 year = 2020,
                 output = "wide")
  
  return(sex)
}

# Define a function to get race/ethnicity data for a specific state
get_race_ethnicity_data <- function(state_code) {
  race_ethnicity <- get_acs(geography = "tract", 
                            variables = c(Hispanic_or_Latino_any_race = "B03002_012",
                                          White_not_Latino = "B03002_003",
                                          Black_not_Latino = "B03002_004",
                                          AIAN_not_Latino = "B03002_005",
                                          Asian_not_Latino = "B03002_006",
                                          NHOPI_not_Latino = "B03002_007",
                                          Other_race_not_Latino = "B03002_008",
                                          Two_or_more_races_not_Latino = "B03002_009"),
                            state = state_code, 
                            year = 2020,
                            output = "wide")
}

# Define a function to get language data for a specific state
get_language_data <- function(state_code) {
  language <- get_acs(geography = "tract", 
                      variables = c(English = "C16001_002",
                                    Spanish = "C16001_003",
                                    French_Haitian_Cajun = "C16001_006",
                                    german_westgermanic = "C16001_009",
                                    russian_polish_slavic = "C16001_012",
                                    other_indo_european = "C16001_015",
                                    Korean = "C16001_018",
                                    Chinese = "C16001_021",
                                    Vietnamese = "C16001_024",
                                    Tagalog = "C16001_027",
                                    Other_Asian_Pac_Island = "C16001_030",
                                    Arabic = "C16001_033",
                                    Other_or_unspecified = "C16001_036"),
                      state = state_code, 
                      year = 2020,
                      output = "wide")
}

# Define a function to get income data for a specific state
get_income_data <- function(state_code) {
  income <- get_acs(geography = "tract", 
                    variables = c(income_less_than_10k = "B19001_002",
                                  income_10k_to_15k = "B19001_003",
                                  income_15k_to_20k = "B19001_004",
                                  income_20k_to_25k = "B19001_005",
                                  income_25k_to_30k = "B19001_006",
                                  income_30k_to_35k = "B19001_007",
                                  income_35k_to_40k = "B19001_008",
                                  income_40k_to_45k = "B19001_009",
                                  income_45k_to_50k = "B19001_010",
                                  income_50k_to_60k = "B19001_011",
                                  income_60k_to_75k = "B19001_012",
                                  income_75k_to_100k = "B19001_013",
                                  income_100k_to_125k = "B19001_014",
                                  income_125k_to_150k = "B19001_015",
                                  income_150k_to_200k = "B19001_016",
                                  income_over_200k = "B19001_017"),
                    state = state_code, 
                    year = 2020,
                    output = "wide")
}

# Define a function to get households with children data for a specific state
get_hh_w_children_data <- function(state_code) {
  hh_w_children <- get_acs(geography = "tract", 
                           variables = c(households_w_children = "B11005_002",
                                         households_wo_children = "B11005_011"),
                           state = state_code, 
                           year = 2020,
                           output = "wide")
}

# Define a function to get education data for a specific state
get_education_data <- function(state_code) {
  education <- get_acs(geography = "tract", 
                       variables = c(no_schooling = "B15003_002",
                                     nursery_school = "B15003_003",
                                     kindergarten = "B15003_004",
                                     grade1 = "B15003_005",
                                     grade2 = "B15003_006",
                                     grade3 = "B15003_007",
                                     grade4 = "B15003_008",
                                     grade5 = "B15003_009",
                                     grade6 = "B15003_010",
                                     grade7 = "B15003_011",
                                     grade8 = "B15003_012",
                                     grade9 = "B15003_013",
                                     grade10 = "B15003_014",
                                     grade11 = "B15003_015",
                                     grade12_no_diploma = "B15003_016",
                                     high_school_diploma = "B15003_017",
                                     ged = "B15003_018",
                                     less_than_1_year_college = "B15003_019",
                                     some_college_no_degree = "B15003_020",
                                     associates_degree = "B15003_021",
                                     bachelors_degree = "B15003_022",
                                     masters_degree = "B15003_023",
                                     professional_degree = "B15003_024",
                                     doctorate_degree = "B15003_025"),
                       state = state_code, 
                       year = 2020,
                       output = "wide")
}

# Define a function to get disability status data for a specific state
get_disability_status_data <- function(state_code) {
  disability <- get_acs(geography = "tract", 
                        variables = c(disability_male_18_to_34 = "B18101_010",
                                      no_disability_male_18_to_34 = "B18101_011",
                                      disability_male_35_to_64 = "B18101_013",
                                      no_disability_male_35_to_64 = "B18101_014",
                                      disability_male_65_to_74 = "B18101_016",
                                      no_disability_male_65_to_74 = "B18101_017",
                                      disability_male_over_75 = "B18101_019",
                                      no_disability_male_over_75 = "B18101_020",
                                      disability_female_18_to_34 = "B18101_029",
                                      no_disability_female_18_to_34 = "B18101_030",
                                      disability_female_35_to_64 = "B18101_032",
                                      no_disability_female_35_to_64 = "B18101_033",
                                      disability_female_65_to_74 = "B18101_035",
                                      no_disability_female_65_to_74 = "B18101_036",
                                      disability_female_over_75 = "B18101_038",
                                      no_disability_female_over_75 = "B18101_039"),
                        state = state_code, 
                        year = 2020,
                        output = "wide") 
}  

## Disability type ##

# Define a function to get disability type data for a specific state
get_hearing_data <- function(state_code) {
  hearing_disability <- get_acs(geography = "tract", 
                                variables = c(hearing_disability_male_18_to_34 = "B18102_010",
                                              no_hearing_disability_male_18_to_34 = "B18102_011",
                                              hearing_disability_male_35_to_64 = "B18102_013",
                                              no_hearing_disability_male_35_to_64 = "B18102_014",
                                              hearing_disability_male_65_to_74 = "B18102_016",
                                              no_hearing_disability_male_65_to_74 = "B18102_017",
                                              hearing_disability_male_over_75 = "B18102_019",
                                              no_hearing_disability_male_over_75 = "B18102_020",
                                              hearing_disability_female_18_to_34 = "B18102_029",
                                              no_hearing_disability_female_18_to_34 = "B18102_030",
                                              hearing_disability_female_35_to_64 = "B18102_032",
                                              no_hearing_disability_female_35_to_64 = "B18102_033",
                                              hearing_disability_female_65_to_74 = "B18102_035",
                                              no_hearing_disability_female_65_to_74 = "B18102_036",
                                              hearing_disability_over_75 = "B18102_038",
                                              no_hearing_disability_over_75 = "B18102_039"),
                                state = state_code, 
                                year = 2020,
                                output = "wide")
}

# Define a function to get vision disability data for a specific state
get_vision_data <- function(state_code) {
  vision_disability <- get_acs(geography = "tract", 
                               variables = c(vision_disability_male_18_to_34 = "B18103_010",
                                             no_vision_disability_male_18_to_34 = "B18103_011",
                                             vision_disability_male_35_to_64 = "B18103_013",
                                             no_vision_disability_male_35_to_64 = "B18103_014",
                                             vision_disability_male_65_to_74 = "B18103_016",
                                             no_vision_disability_male_65_to_74 = "B18103_017",
                                             vision_disability_male_over_75 = "B18103_019",
                                             no_vision_disability_male_over_75 = "B18103_020",
                                             vision_disability_female_18_to_34 = "B18103_029",
                                             no_vision_disability_female_18_to_34 = "B18103_030",
                                             vision_disability_female_35_to_64 = "B18103_032",
                                             no_vision_disability_female_35_to_64 = "B18103_033",
                                             vision_disability_female_65_to_74 = "B18103_035",
                                             no_vision_disability_female_65_to_74 = "B18103_036",
                                             vision_disability_female_over_75 = "B18103_038",
                                             no_vision_disability_female_over_75 = "B18103_039"),
                               state = state_code, 
                               year = 2020,
                               output = "wide")
}

# Define a function to get cognitive disability data for a specific state
get_cognitive_data <- function(state_code) {
  cognitive_disability <- get_acs(geography = "tract", 
                                  variables = c(cognitive_disability_male_w_18_to_34 = "B18104_007",
                                                no_cognitive_disability_male_w_18_to_34 = "B18104_008",
                                                cognitive_disability_male_35_to_64 = "B18104_010",
                                                no_cognitive_disability_male_35_to_64 = "B18104_011",
                                                cognitive_disability_male_65_to_74 = "B18104_013",
                                                no_cognitive_disability_male_65_to_74 = "B18104_014",
                                                cognitive_disability_male_over_75 = "B18104_016",
                                                no_cognitive_disability_male_over_75 = "B18104_017",
                                                cognitive_disability_female_18_to_34 = "B18104_023",
                                                no_cognitive_disability_female_18_to_34 = "B18104_024",
                                                cognitive_disability_female_35_to_64 = "B18104_026",
                                                no_cognitive_disability_female_35_to_64 = "B18104_027",
                                                cognitive_disability_female_65_to_74 = "B18104_029",
                                                no_cognitive_disability_female_65_to_74 = "B18104_030",
                                                cognitive_disability_female_over_75 = "B18104_032",
                                                no_cognitive_disability_female_over_75 = "B18104_033"),
                                  state = state_code, 
                                  year = 2020,
                                  output = "wide")
}

# Define a function to get ambulatory disability data for a specific state
get_ambulatory_data <- function(state_code) {
  ambulatory_disability <- get_acs(geography = "tract", 
                                   variables = c(ambulatory_disability_male_18_to_34 = "B18105_007",
                                                 no_ambulatory_disability_male_18_to_34 = "B18105_008",
                                                 ambulatory_disability_male_35_to_64 = "B18105_010",
                                                 no_ambulatory_disability_male_35_to_64 = "B18105_011",
                                                 ambulatory_disability_male_65_to_74 = "B18105_013",
                                                 no_ambulatory_disability_male_65_to_74 = "B18105_014",
                                                 ambulatory_disability_male_over_75 = "B18105_016",
                                                 no_ambulatory_disability_male_over_75 = "B18105_017",
                                                 ambulatory_disability_female_18_to_34 = "B18105_023",
                                                 no_ambulatory_disability_female_18_to_34 = "B18105_024",
                                                 ambulatory_disability_female_35_to_64 = "B18105_026",
                                                 no_ambulatory_disability_female_35_to_64 = "B18105_027",
                                                 ambulatory_disability_female_65_to_74 = "B18105_029",
                                                 no_ambulatory_disability_female_65_to_74 = "B18105_030",
                                                 ambulatory_disability_female_over_75 = "B18105_032",
                                                 no_ambulatory_disability_female_over_75 = "B18105_033"),
                                   state = state_code, 
                                   year = 2020,
                                   output = "wide")
}

# Define a function to get self care disability data for a specific state
get_self_care_data <- function(state_code) {
  self_care_disability <- get_acs(geography = "tract", 
                                  variables = c(self_care_disability_male_18_to_34 = "B18106_007",
                                                no_self_care_disability_male_18_to_34 = "B18106_008",
                                                self_care_disability_male_35_to_64 = "B18106_010",
                                                no_self_care_disability_male_35_to_64 = "B18106_011",
                                                self_care_disability_male_65_to_74 = "B18106_013",
                                                no_self_care_disability_male_65_to_74 = "B18106_014",
                                                self_care_disability_male_over_75 = "B18106_016",
                                                no_self_care_disability_male_over_75 = "B18106_017",
                                                self_care_disability_female_18_to_34 = "B18106_023",
                                                no_self_care_disability_female_18_to_34 = "B18106_024",
                                                self_care_disability_female_35_to_64 = "B18106_026",
                                                no_self_care_disability_female_35_to_64 = "B18106_027",
                                                self_care_disability_female_65_to_74 = "B18106_029",
                                                no_self_care_disability_female_65_to_74 = "B18106_030",
                                                self_care_disability_female_over_75 = "B18106_032",
                                                no_self_care_disability_female_over_75 = "B18106_033"),
                                  state = state_code, 
                                  year = 2020,
                                  output = "wide")
}

# Define a function to get independent living disability data for a specific state
get_independent_living_data <- function(state_code) {
  independent_living_disability <- get_acs(geography = "tract", 
                                           variables = c(independent_living_disability_male_18_to_34 = "B18107_004",
                                                         no_independent_living_disability_male_18_to_34 = "B18107_005",
                                                         independent_living_disability_male_35_to_64 = "B18107_007",
                                                         no_independent_living_disability_male_35_to_64 = "B18107_008",
                                                         independent_living_disability_male_65_to_74 = "B18107_010",
                                                         no_independent_living_disability_male_65_to_74 = "B18107_011",
                                                         independent_living_disability_male_over_75 = "B18107_013",
                                                         no_independent_living_disability_male_over_75 = "B18107_014",
                                                         independent_living_disability_female_18_to_34 = "B18107_017",
                                                         no_independent_living_disability_female_18_to_34 = "B18107_018",
                                                         independent_living_disability_female_35_to_64 = "B18107_020",
                                                         no_independent_living_disability_female_35_to_64 = "B18107_021",
                                                         independent_living_disability_female_65_to_74 = "B18107_023",
                                                         no_independent_living_disability_female_65_to_74 = "B18107_024",
                                                         independent_living_disability_female_over_75 = "B18107_026",
                                                         no_independent_living_disability_female_over_75 = "B18107_027"),
                                           state = state_code, 
                                           year = 2020,
                                           output = "wide")
  
}

# Define a function to get veteran status data for a specific state
get_veteran_data <- function(state_code) {
  veteran <- get_acs(geography = "tract", 
                     variables = c(veteran = "B21001_002",
                                   non_veteran = "B21001_003"),
                      state = state_code, 
                      year = 2020,
                      output = "wide")
}

# Define a function to get internet access data for a specific state
get_internet_data <- function(state_code) {
  internet_status <- get_acs(geography = "tract", 
                             variables = c(internet_subscription = "B28002_002",
                                           internet_no_subscription = "B28002_012",
                                           no_internet_access = "B28002_013"),
                             state = state_code, 
                             year = 2020,
                             output = "wide")
}