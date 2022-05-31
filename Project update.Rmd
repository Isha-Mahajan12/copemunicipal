---
title: "Exploratory Analysis -Municipal Data "
author: "Meredith Rolfe, Isha Mahajan, Jason Wierzbowski"
date: "10/14/2021"
output: html_document
---


##### Municipal Survey Coastal Resilience Data 

## DATA CLEANING 


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

```{r Data for FA}
#Group the questions
climate.impact <- select (MSCR.clean, contains ("climate_impact"))
health.environ.impact <- select(MSCR.clean, contains("health"))
infrastructure.impact <- select(MSCR.clean, contains("infra"))
economic.impact <- select(MSCR.clean, contains("economic"))
industry.impact <- select(MSCR.clean, contains("industry"))
vulner.resource <- select(MSCR.clean, contains ("resources"))
data.use <- select(MSCR.clean, contains("data"))
climate.strat <- select(MSCR.clean, contains("climate_strategies"))
engi.strat <- select(MSCR.clean, contains("engineering_strategies"))
green.strat <- select(MSCR.clean, contains("green_strategies"))
NCCR.resource <- select(MSCR.clean, contains("nccr_resources"))
```



### Functions

*P Values, Confidence Intervals, Alpha Scores and Correlations*

```{r Exploratory Stats}
library(GPArotation)
library(psych)
exploratory_stats <- function(data) {
  list(
  data_cor <- lowerCor(data), # correlation
  data_p <- corr.test(data, use = "pairwise.complete.obs")$p ,   #p-values
  data_ci <- corr.test(data, use= "pairwise.complete.obs")$ci ,  #confidence_intervals
  data_alpha <- alpha(data)#alpha scores 
  )
}
```

*Factor Analysis*

```{r Exploratory Factor Analysis}
efa_model <- function(name, data,i = 4){
  name <- fa(data)
  name.diagram <- fa.diagram(name)
  name.cor <- cor(data, use = "pairwise.complete.obs")
  name.ev <- eigen(name.cor)
  sigEV <- name.ev$values >=1 #Eigenvalues are used in an absence of theory to guide research. An eigenvalue of greater than 1 represents that the factor is meaningful.
  description<- describe(data)
  errordot <- error.dots (data)
  name.model <- fa(data, nfactors = i)
  name_list <- list(name$loadings, name.ev$values, name.model, name.diagram, description, errordot,scree(name.cor, factors = FALSE))
  return(name_list)
}
```

```{r}
fa.scores <- function(data, model, i){
  name<-deparse(substitute(data))
  temp<-factor.scores(data,model[[i]][[3]], impute = "median")$scores
  colnames(temp)<-paste(name, 1:(i), sep="_")
  temp
  }
```

  For most of our grouping, after careful examination we decided that 1 factor was the best option upon examining the alpha scores, proportional and cumulative variance, and the RMSEA. For groupings that we decided to use multiple factors, our explanation is below.

### Climate Impact 

```{r}
climate_impact_stat <- exploratory_stats(climate.impact) 
climate_impact_stat 
```

```{r} 
climate_impact_list<-map(1:4, ~efa_model(efa.impact, 
                 climate.impact, .))
climate_impact_list
```

  We decided to go with 3 loadings on climate impact. Although the alpha score is a bit higher that .8(.84), we still felt 3 loadings was appropriate as distinct groups of variables formed when compares to i = 1, 2, and 4. The first loading had high values storm surge(.75), sea level rise(.8), and high tide(.87) which seems to indicate coastal municipalities responses. The second loading has high values for marine heat wave(.67), warming ocean/water(.72), ocean acidification(.91), and de-oxygenated water(.61) which seems to indicate ... The third loading has high values for current impact(.49), heat wave(.57), and severe storm and high wind(.57) which seems to indicate a general impact factor.
  
```{r}
clim.im.scores3 <- fa.scores(climate.impact, climate_impact_list, 3)
```

### Health Environmental Impact 

```{r}
health_impact_stat <- exploratory_stats(health.environ.impact)
health_impact_stat
```

```{r}
health_impact_list<-map (1:4, ~efa_model (efa.impact, 
                 health.environ.impact, .))
health_impact_list
```

We decided to go with 3 loadings on health and environmental impact. The alpha score was under .8(.78) and we found that there were 3 distinct groups. The first loading had high values for species loss(.75) and animal out migration(.84) which could indicate a general impact factor. The second loading has high values for air quality concerns(.80), increase in vector born diseases(.5), and heat related illness(.79) which seems to indicate municipalities that are cities. The third loading has high values for wastewater management concerns(.83) and water habitat degradation(.43) which seems to represent coastal municipalities.

```{r}
health.scores3 <- fa.scores(health.environ.impact, health_impact_list, 3)
```

### Infrastructure Impact 

```{r}
infra_impact_stat <- exploratory_stats(infrastructure.impact)
infra_impact_stat
```

```{r}
infra_impact_list<-map(1:4, ~efa_model(efa.impact, 
                 infrastructure.impact, .))
infra_impact_list
```

```{r}
infra.scores1 <-fa.scores(infrastructure.impact, infra_impact_list, 1)
```

### Economic Impact 

```{r}
economic_impact_stat <- exploratory_stats(economic.impact)
economic_impact_stat
```

```{r}
econ_impact_list<-map(1:4, ~efa_model(efa.impact, 
                 economic.impact, .))
econ_impact_list
```

```{r}
econ.scores1 <-fa.scores(economic.impact, econ_impact_list, 1)

```

### Industry Impact ***( Needs further clarification)

```{r}
industry_impact_stat <- exploratory_stats(industry.impact)
industry_impact_stat
```

```{r}
indus_impact_list<-map(1:4, ~efa_model(efa.impact, 
                 industry.impact, .))
indus_impact_list
```

```{r}
indus.scores1 <-fa.scores(industry.impact, indus_impact_list, 1)
```

### Vulnerable Resources

```{r}
vulnerable_resource_stat <- exploratory_stats(vulner.resource)
vulnerable_resource_stat
```

```{r}
resource_impact_list<-map(1:4, ~efa_model(efa.impact, 
                 vulner.resource, .))
resource_impact_list
```

```{r}
resource.scores1 <-fa.scores(vulner.resource, resource_impact_list, 1)
```

### Data Usage 

```{r}
data_stat <- exploratory_stats(data.use)
data_stat
```

```{r}
data_impact_list<-map(1:4, ~efa_model(efa.impact, 
                 data.use, .))
data_impact_list
```

```{r}
data.scores1 <-fa.scores(data.use, data_impact_list, 1)
```

### Climate Stratergy 

```{r}
climate_stratergy_stat <- exploratory_stats(climate.strat)
climate_stratergy_stat
```

```{r}
clim_strat_list<-map(1:4, ~efa_model(efa.impact, 
                 climate.strat, .))
clim_strat_list
```

We decided to go with 3 loadings for climate strategies as the alpha score was .81 which was close enough that we felt distinct groups would form. The first loading has high values for creating adaptive management capacity(.74) and redevelopment plans(.73) which we believe to represent coastal municipalities. The second loading has high values for zoning planning(1) which may indicate a city municipalities. The third loading has high values for education projects(.64), risk assessment(.6), and collaboration(.56) which we believe represents a general impact factor.

```{r}
clim.strat.scores3 <- fa.scores(climate.strat, clim_strat_list, 3)
```

### Engineering Stratergy 

```{r}
engineering_stratergy_stat <- exploratory_stats(engi.strat)
engineering_stratergy_stat
```

```{r}
engi_strat_list<-map(1:4, ~efa_model(efa.impact, 
                 engi.strat, .))
engi_strat_list
```

```{r}
engi.strat.scores1 <- fa.scores(engi.strat, engi_strat_list, 1)
```

### Green Stratergy 

```{r}
green_stratergy_stat <- exploratory_stats(green.strat)
green_stratergy_stat
```

```{r}
green_strat_list<-map(1:4, ~efa_model(efa.impact, 
                 green.strat, .))
green_strat_list
```

We decided to go with 3 loadings for green strategies as the alpha score was .74 which told us that there may be valuable information in additional loadings. The first loading has high values for NBS to flooding(.86) and NBS to erosion(.83) which may indicate coastal municipalities. The second loading has high values for green roofs(.52), urban greening(.72), and NBS to HIE(.87) definetly indicating city municipalities. The third loading has high values for restorative agriculture(1) possibly indicating a general impact variable.

```{r}
green.strat.scores3 <- fa.scores(green.strat, green_strat_list, 3)
```
  
### NCCR Resources 

```{r}
nccr_resource_stat <- exploratory_stats(NCCR.resource)
nccr_resource_stat
```

```{r}
NCCR_resource_list<-map(1:4, ~efa_model(efa.impact, 
                 NCCR.resource, .))
NCCR_resource_list
```

```{r}
NCCR.resource.scores1 <-fa.scores(NCCR.resource, NCCR_resource_list, 1)
```

###FACTOR TABLE

```{r}
#combine factor scores with coastal and city data
final_data <- cbind(names, clim.im.scores3, health.scores3, infra.scores1, econ.scores1, indus.scores1, resource.scores1, data.scores1, clim.strat.scores3, engi.strat.scores1, green.strat.scores3, NCCR.resource.scores1 )
head(final_data)
```

```{r}
pop <- read.csv("data/tabular.b01001_population_by_age_gender_acs_m_2015-19.csv")

final_data <- final_data%>%
  left_join(pop, by = c("Muni names" = "municipal"))%>%
  select(colnames(final_data), mpop_p)
#proportion of male population in each municipality
```

```{r}
race <- read.csv("data/tabular.b03002_race_ethnicity_acs_m_2015-19.csv")

final_data <- final_data%>%
  left_join(race, by = c("Muni names" = "municipal"))%>%
  select(colnames(final_data), nh_p, nhwhi_p, nhaa_p, nhna_p, nhas_p, nhpi_p, nhoth_p, nhmlt_p, lat_p)

#proportion of non-hispanic, non-hispanic white, non-hispanic afican american, non-hispanic native american, non-hispanic asian, non-hispanic pacific islander, non-hispanic other, non-hispanic multiple races, latino
```

```{r}
tax <- read.csv("data/tabular.econ_municipal_taxes_revenue_m_2016.csv")

final_data <- final_data%>%
  left_join(tax, by = c("Muni names" = "municipal"))%>%
  select(colnames(final_data), tot_rev)

# total tax revenue of each municipality. 
```

```{r}
edu <- read.csv("data/tabular.b15002_educational_attainment_acs_m_2015-19.csv")

final_data <- final_data%>%
  left_join(edu, by = c("Muni names" = "municipal"))%>%
  select(colnames(final_data), nohs, hs, sc, assoc, ba, mast, prof, doc)

final_data <- final_data%>%
  mutate(higher_edu = 
  sum(ba+mast+prof+doc)/sum(nohs+hs+sc+assoc+ba+mast+prof+doc))%>%
  select(-nohs, -hs, -sc, -assoc, -ba, -mast, -prof, -doc)

#proportion of municipal population that hold a bachelors degree or greater. 
```
