---
title: "Exploratory Analysis -Municipal Data "
author: "Meredith Rolfe, Isha Mahajan, Jason Wierzbowski"
date: "10/14/2021"
output: html_document
---


##### Municipal Survey Coastal Resilience Data 

## DATA CLEANING 

The data set we have chosen to start with was granted to us by Professor Marta Vicarelli who conducted a massive survey at the municipal level with the help of the Massachusetts Municipal Association. The survey was given to officials from 111 different municipalities. The original data set contains 157 observations of 157 variables. In preparation to explore the data set we removed an extra header row, removed a row missing the information on what municipality the row was from, and cut out the first 17 columns as they contained information that was not useful to us. We then recoded all of the columns as they were originally named Q1, Q2, etc. and were recoded to more accurately reflect what the questions were asking. The original survey also included all text responses so we recoded the text responses into numbered values to make the data easier to work with. After all of our original data cleaning we ended with 155 observations of 106 variables and were prepared to move on to factor analysis.

```{r}
library(tidyverse)
library(dplyr)
library(broom)
```

```{r}
library(readxl)
#read in data, remove first 17 columns to de-identify data and check if imported correctly 
MSCR <- read_excel("data/2021_10_01_Municipal Survey_Coastal Resilience.xlsx", 
    sheet = "Original_data")
MSCR <- subset(MSCR, select = -c(1:17))
MSCR <- MSCR[-c(1),] # Remove rows with questions in them
MSCR <- MSCR[-c(71),] # Remove Rows with Empty Municipalilty names 
```



```{r}
#Rename Variables in Tidy
MSCR <- MSCR %>% 
rename (consent_other = Q1,
        municipal_or_planning_other = Q2,
        municipal_or_planning_text_other = Q2_3_TEXT,
        municipality_name_other = Q3_1,
        planning_agency_name_other = Q4_1,
        first_last_name_other = Q5,
        role_other = Q6,
        email_other = Q7,
        
         climate_impact_current_impact = Q9,
         climate_impact_heat_waves = Q9_1,
         climate_impact_drought = Q9_2,
         climate_impact_wildfires = Q9_3,
         climate_impact_storms_and_winds = Q9_4,
         climate_impact_storm_surges = Q9_5,
         climate_impact_sea_level_rise = Q9_6,
         climate_impact_flooding = Q9_7,
         climate_impact_high_tide = Q9_8,
         climate_impact_marine_heat_waves = Q9_9,
         climate_impact_ocean_water_warming = Q9_10,
         climate_impact_ocean_acidification = Q9_11,
         climate_impact_deoxygenated_water = Q9_12,
         climate_impact_other = Q9_13,
         climate_impact_other_text = Q9_13_TEXT,
        
         health_air_quality = Q10_1, 
         health_vector_borne_diseases = Q10_2 ,
         health_related_illness = Q10_3,
         health_water_resource_degredation =  Q10_4,
         health_wastewater_mngmt_concern = Q10_5,
         health_waste_sites = Q10_6,
         health_water_habitat_degredation = Q10_7,
         health_tree_loss = Q10_8,
         health_algal_bacteria = Q10_9,
         health_species_loss = Q10_10,
         health_animal_out_migration = Q10_11,
         health_invasive_species = Q10_12,
         health_other = Q10_13, 
         health_other_text = Q10_13_TEXT, 
        
         infra_dam_seawall_damage = Q12_1,
         infra_ports_coastal_damage = Q12_2,
         infra_disconnected_roads = Q12_3,
         infra_public_transport_damage = Q12_4,
         infra_public_structure_damage = Q12_5,
         infra_historical_site_damage = Q12_6,
         infra_private_property_damage = Q12_7,
         infra_loss_beaches_coastal_property = Q12_8,
         infra_drainage_system_damage = Q12_9,
         infra_wastewater_treatment_damage = Q12_10,
         infra_water_supply_damage = Q12_11,
         infra_power_outages = Q12_12,
         infra_other = Q12_13,
         infra_other_text = Q12_13_TEXT,
        
          economic_additional_disaster_response = Q13_1,
          economic_additional_public_health_cost =Q13_2, 
          economic_productivity_loss = Q13_3, 
          economic_decreased_tax_revenue =  Q13_4,
          economic_property_insurance_difficulty = Q13_5,
          economic_decreased_prop_value= Q13_6,
          economic_increased_housing_insecurity = Q13_7,
          economic_increased_unemployement = Q13_8,
          economic_resident_out_migration = Q13_9,
          economic_resident_in_migration = Q13_10,
          economic_decreased_housing_availability = Q13_11,
          economic_other = Q13_12,
          economic_other_text = Q13_12_TEXT,
        
          industry_healthcare= Q14_1, 
          industry_municipal_services = Q14_2 ,
          industry_tourism = Q14_3,
          industry_hospitality_food = Q14_4,
          industry_real_estate = Q14_5,
          industry_wholesale_retail_trade = Q14_6,
          industry_prof_sci_tech_services = Q14_7,
          industry_manufacturing = Q14_8,
          industry_construction = Q14_9,
          industry_commercial_fishing = Q14_10,
          industry_agriculture = Q14_11,
          industry_other = Q14_12,
          industry_other_text = Q14_12_TEXT, 
        
          additional_thoughts_other = Q15 , 
          vulnerable_population_other = Q17, 
          vulnerable_population_text_other = Q17_8_TEXT, 
        
          resources_african_american = Q18_1, 
          resources_children = Q18_2, 
          resources_elderly = Q18_3 ,
          resources_immigrants = Q18_4,
          resources_low_income_res = Q18_5,
          resources_disabilities = Q18_6,
          resources_veterans = Q18_7,
          resources_other = Q18_8,
          resources_other_text = Q18_8_TEXT,
        
          data_health = Q19_1 ,
          data_employement = Q19_2 ,
          data_housing_security = Q19_3 ,
          data_food_security = Q19_4 ,
          data_other = Q19_5 ,
          data_other_text= Q19_5_TEXT, 
        
          climate_planning_priority_other = Q21, 
          climate_planning_staff_other = Q22, 
          climate_planning_staff_specified_other = Q23, 
        
          climate_strategies_education_projects = Q24_1,
          climate_strategies_risk_assesments = Q24_2,
          climate_strategies_zoning_planning =  Q24_3,
          climate_strategies_municipal_codes = Q24_4,
          climate_strategies_threatened_areas = Q24_5,
          climate_strategies_conservation_restrictions = Q24_6,
          climate_strategies_adaptive_management_capacity = Q24_7,
          climate_strategies_capital_improvement_plan = Q24_8,
          climate_strategies_redevelopment_plans = Q24_9,
          climate_strategies_collaboration = Q24_10,
          climate_strategies_other = Q24_11,
          climate_strategies_other_text = Q24_11_TEXT, 
        
          engineering_strategies_seawalls_barriers = Q25_1,
          engineering_strategies_dry_flood_proofings = Q25_2,
          engineering_strategies_wet_flood_proofing = Q25_3,
          engineering_strategies_weatherization_retrofitting = Q25_4, 
          engineering_strategies_wastewater_system_improvement = Q25_5, 
          engineering_strategies_drainage_system_improvement = Q25_6,
          engineering_strategies_resilence_power_systems  = Q25_7,
          engineering_strategies_resilience_telecommunications = Q25_8,
          engineering_strategies_other = Q25_9,
          engineering_strategies_other_text =  Q25_9_TEXT,
        
           green_strategies_geen_roofs = Q26_1 ,
           green_strategies_urban_greening =  Q26_2,
           green_strategies_nbs_stormwater =Q26_3,
           green_strategies_nbs_hie = Q26_4, 
           green_strategies_nbs_flooding = Q26_5, 
           green_strategies_nbs_erosion = Q26_6, 
           green_strategies_land_conservation = Q26_7,
           green_strategies_restorative_agriculture = Q26_8,
           green_strategies_other = Q26_9,
           green_strategies_other_text = Q26_9_TEXT,
           green_recovery_concept_other = Q27,
        
        government_funded_programs_other = Q28,
        government_funded_programs_other_text = Q28_13_TEXT,
        municipal_resources_other = Q30,
        municipal_resources_text = Q30_6_TEXT,
        data_information_other = Q31,
        data_information_text = Q31_9_TEXT, 
        data_needs_other = Q32,
        data_needs_other_text = Q32_6_TEXT,
        governance_and_coordination_other = Q33,
        governance_and_coordination_text = Q33_10_TEXT,
        data_information_2_other = Q35 ,
        data_information_2_other_text = Q35_9_TEXT,
        
          nccr_resources_technical_assistance = Q36_1,
          nccr_resources_webinars_conference = Q36_2,
          nccr_resources_workshops = Q36_3, 
          nccr_resources_training_sessions = Q36_4,
          nccr_resources_informative_videos = Q36_5,
          nccr_resources_mailing_lists =Q36_6,
          nccr_resources_partnerships_assistance = Q36_7,
          nccr_resources_web_platforms = Q36_8,
          nccr_resources_expert_testimony = Q36_9, 
          nccr_resources_legal_support = Q36_10,
          nccr_resources_other = Q36_11,
          nccr_resources_text = Q36_11_TEXT, 
         
         invitation_interests_other = Q37, 
          final_thoughts_other = Q38)
         
head(MSCR)
```

```{r}
other<- MSCR %>% 
  select(contains("other"), contains("text"))
```

```{r}
MSCR.clean <- MSCR %>% 
  select(-contains("other"), -contains("text"))
head(MSCR.clean)
```


```{r}
#Recode Columns
MSCR.clean<- MSCR.clean %>%
  mutate(climate_impact_current_impact = case_when(
    climate_impact_current_impact== "Minor" ~ 1,
    climate_impact_current_impact== "Moderate" ~ 2,
    climate_impact_current_impact== "Significant" ~ 3,
    climate_impact_current_impact== "Extreme" ~ 4
  ))
```


```{r}
#Impacts (climate_impacts: economic_impacts)
MSCR.clean <- MSCR.clean %>%
  mutate(across(2:48, ~case_when(
    .=="Not affected, but we anticipate problems in the future" ~ 1,
    .=="Not affected" ~ 2,
    .=="Mildly affected" ~ 3,
    .=="Strongly affected" ~ 4
  )))
```

```{r}
#Impacts (industrial)
MSCR.clean <- MSCR.clean %>%
  mutate(across(49:59, ~case_when(
    .=="None" ~ 1,
    .=="Minor" ~ 2,
    .=="Moderate" ~ 3,
    .=="Somewhat strong" ~ 4,
    .=="Very strong" ~ 5
  )))
```

```{r}
#resource dedication
MSCR.clean <- MSCR.clean %>%
  mutate(across(60:66, ~case_when(
    .=="Not applicable in our municipality/region" ~ 1,
    .=="No" ~ 2,
    .=="No but we plan to" ~ 3,
    .=="Yes" ~ 4
  )))
```

```{r}
#Local Data Use
MSCR.clean <- MSCR.clean %>%
  mutate(across(67:70, ~case_when(
    .=="No" ~ 1,
    .=="Yes" ~ 2,
    .=="Yes, and the data is seperated by race and ethnicity" ~ 3
  )))
```

```{r}
#Stratergies
MSCR.clean <- MSCR.clean %>%
  mutate(across(71:96, ~case_when(
    .=="No interest" ~ 1,
    .=="We would like to adopt this strategy" ~ 2,
    .=="We adopted this strategy" ~ 3
  )))
```

```{r}
#NCCR Services
MSCR.clean <- MSCR.clean %>%
  mutate(across(97:106, ~ case_when(
    .=="Not at all helpful" ~ 1,
    .=="Somewhat helpful" ~ 2,
    .=="Very helpful" ~ 3
  )))
```

```{r}
#load municipal names data
names <- read_csv("data/municipal_names.csv")
head(names)
```

