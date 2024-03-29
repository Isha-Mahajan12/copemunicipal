---
title: "Data Cleaning for Bubbleplots- Initial Dimensionality of Data "
author: "Meredith Rolfe, Isha Mahajan, Jason Wierzbowski"
date: "10/14/2021"
---


### Data Cleaning


## Load Packages 

```{r}
library(tidyverse)
library(dplyr)
library(broom)
```


## Read Data 

```{r}
library(readxl)
#read in data, remove first 17 columns to de-identify data and check if imported correctly 
MSCR <- read_excel("data/2021_10_01_Municipal Survey_Coastal Resilience.xlsx", 
    sheet = "Original_data")
MSCR <- subset(MSCR, select = -c(1:17))
```

```{r}
#Create a vector adjusted for data frame
MSCR.names <- c("Consent", "Municipality or Planning Agency", "Municipality or Planning Agency-Other", "Muni Name", "Planning Agency Name", "Name", "Role", "Email", "Current Impact", "Heat Wave", "Drought", "Wildfire", "Severe Storm and High Wind", "Storm Surge", "Sea Level Rise", "Flooding", "High Tide", "Marine Heat Wave", "Warming Ocean/Water", "Ocean Acidification", "De-Oxygenated Water", "Other Impact", "Other Impact-Specified", "Air Quality Concerns", "Increase in Vector Born Diseases", "Heat Related Illness", "Water Resourse Degradation", "Wastewater Management Concerns", "Waste Sites", "Water Habitat Degradation", "Tree Loss", "Algal/Bacteria", "Species loss", "Animal Out Migration", "Introduction of Invasive Species", "Health/Environmental Impact-Other", "Health/Environmental Impact-Other Specified", "Dam/Seawall Damage", "Ports/Coastal Transport Damage", "Disconnected Roads", "Public Transport Damage", "Municipal/Public Structure Damage", "Historical Site Damage", "Private Property Damage", "Loss of Beaches/Coastal Property", "Drainage System Dmaage", "Wastewater Treatment Damage", "Water Supply Damage", "Power Outages", "Infrastructure Impact-Other", "Infrastructure Impact-Other Specified", "Additonal Disaster Response Cost", "Additional Public Health Cost", "Productivity Loss", "Decreased Tax Revenue", "Property Insurance Difficulty", "Decreased Property Value", "Increased Housing Insecurity", "Increased Unemployment", "Resident Out Migration", "Resident In Migration", "Decreased Housing Availability", "Economic Impact-Other", "Economic Impact-Other Specified", "Healthcare", "Municipal Services", "Tourism", "Hospitality/Food", "Real Estate", "Wholesale/Retail Trade", "Professional/Scientific/Technical Services", "Manufacturing", "Construction", "Commercial Fishing", "Agriculture", "Industry Impact-Other", "Industry Impact-Other Specified", "CC Impact-Addtional Thoughts", "Vulnerable Population", "Vulnerable Population-Other", "African American", "Children", "Elderly", "Immigrants", "Low Income Residents", "People with Disabilities", "Veterans", "Resource Dedication-Other", "Resource Dedication-Other Specified", "Health Data", "Employment Data", "Housing Security Data", "Food Security Data", "Local Data Use-Other", "Local Data Use-Other Specified", "Climate Planning Priority", "Climate Planning Staff", "Climate Planning Staff-Specified", "Education Projects", "Vulnerability/Risk Assessment", "Zoning/Planning Practices", "Changes to Municipal Codes", "Threatened Areas", "Conservation Restrictions", "Creating Adaptive Management Capacity", "Capital Improvement Plan", "Redevelopment Plans", "Collaboration"
                
, "Climate Strategies-Other", "Climate Strategies-Other Specified", "Tidal Barriers/Seawalls", "Dry Flood Proofing", "Wet Flood Proofing", "Weatherization/Retrofitting Buildings", "Improvement/Expansion of Wastewater Systems", "Improvement/Expansion of Drainage Systems", "Resilience of Power Systems", "Resilience of Telecommunication", "Engineered Infrastructure Strategy-Other", "Engineered Infrastructure Strategy-Other Specified", "Green Roofs", "Urban Greening", "NBS to Stormwater", "NBS to HIE", "NBS to Flooding", "NBS to Erosion", "Land Conservation", "Restorative Agriculture", "Green Infrastructure Solutions-Other", "Green Infrastructure Solutions-Other Specified", "Green Recovery Concept", "Government funded Programs", "Government Funded Programs-Other", "Municipal Resources", "Municipal Resources-Others", "D & I", "D & I-Other", "Data Needs", "Data Needs-Other", "Governance and Coordination", "Governance and Coordination-Other", "Data and Information", "Data and Information-Other", "Technical Assistance", "Webinars/Conferences", "Workshops", "Training Sessions", "Informative Videos", "Mailing Lists", "Partnership Assistance", "Web Platform", "Expert Testimony", "Legal Support", "NCCR Services-Other", "NCCR Services-Other Specified", "Invitation Interest", "Final Thoughts")
```


```{r}
#rename columns and remove first row from responses (topline)
colnames(MSCR) <- MSCR.names
MSCR <- MSCR[-c(1),]
MSCR <- MSCR[-c(71),]
MSCR.clean <- select(MSCR, 9:21, 23:34, 36:47, 49:59, 61:71, 75:86, 88, 91:100, 102:109, 111:118, 127:136)
MSCR.clean <- select(MSCR.clean, -37)
```


```{r,echo = FALSE}
#recode columns to numerical likert scale 
#Current Impact
MSCR.clean <- MSCR.clean %>%
  mutate(`Current Impact` = case_when(
    `Current Impact`== "Minor" ~ 1,
    `Current Impact`== "Moderate" ~ 2,
    `Current Impact`== "Significant" ~ 3,
    `Current Impact`== "Extreme" ~ 4
  ))
```


```{r}
#Impacts
MSCR.clean <- MSCR.clean %>%
  mutate(across(2:47, ~case_when(
    .=="Not affected, but we anticipate problems in the future" ~ 1,
    .=="Not affected" ~ 2,
    .=="Mildly affected" ~ 3,
    .=="Strongly affected" ~ 4
  )))
```

```{r}
#Industrial Impact
MSCR.clean <- MSCR.clean %>%
  mutate(across(48:58, ~case_when(
    .=="None" ~ 1,
    .=="Minor" ~ 2,
    .=="Moderate" ~ 3,
    .=="Somewhat strong" ~ 4,
    .=="Very strong" ~ 5
  )))
```

```{r}
#Resource dedication
MSCR.clean <- MSCR.clean %>%
  mutate(across(59:65, ~case_when(
    .=="Not applicable in our municipality/region" ~ 1,
    .=="No" ~ 2,
    .=="No but we plan to" ~ 3,
    .=="Yes" ~ 4
  )))
```

```{r}
#Local Data Use
MSCR.clean <- MSCR.clean %>%
  mutate(across(66:69, ~case_when(
    .=="No" ~ 1,
    .=="Yes" ~ 2,
    .=="Yes, and the data is seperated by race and ethnicity" ~ 3
  )))
```

```{r}
#Stratergies
MSCR.clean <- MSCR.clean %>%
  mutate(across(70:95, ~case_when(
    .=="No interest" ~ 1,
    .=="We would like to adopt this strategy" ~ 2,
    .=="We adopted this strategy" ~ 3
  )))
```

```{r}
#NCCR Services
MSCR.clean <- MSCR.clean %>%
  mutate(across(96:105, ~ case_when(
    .=="Not at all helpful" ~ 1,
    .=="Somewhat helpful" ~ 2,
    .=="Very helpful" ~ 3
  )))
```


Our final data frame consists of 139 columns and 157 rows. We did not recode the columns that had options - "check all that apply"


## Exploratory Data Analysis 

```{r}
#begin by looking at the summary of the dataset 
summary(MSCR)
```

We begin by asking the data how many responses were given by a municipality and planning agencies

```{r}
count <- MSCR %>% 
  count(`Municipality or Planning Agency`, `Municipality or Planning Agency-Other`)
head(count)
```
There were 139 responses by a municipality and 14 responses by a planning agency. 

WE EXPLORE THE SECTIONS OF THE DATA AND LEAVE OUT THE OTHER COLUMNS FOR NOW. 

#### SECTION 1 HAZARDS 

```{r}
library(ggthemes)
library(ggplot2)
library(extrafont)

#Create a wide dataset 
hazard_wide <- MSCR %>% 
  #select grouping variable Municipality/planning agency, and cols 10-21 that comprise of information on hazard threat
  select(`Municipality or Planning Agency`, 10:21) %>% 
   #pivot longer the dataset, select the columns and interchange names and values
   pivot_longer(
    cols =  `Heat Wave`:`De-Oxygenated Water`,
    names_to = "impact",
    values_to = "hazard"
  ) %>% 
  #group by hazard and impact to enable count 
  group_by(`hazard`, impact ) %>% 
  #count the number of respondents who responded different levels of threats  
  count(hazard) %>% 
  #recode variables 
  mutate(hazard = recode_factor(hazard, 
    `1` = "Not Affected \n But Anticipate Future Problems" ,
    `2` = "Not Affected" , 
    `3` = "Mildly Affected" ,
    `4` = "Strongly Affected")) %>% 
  filter(hazard != "NA") 
```


```{r}
#code for PLOT BUBBLEPLOT
bubbleplot <- ggplot(hazard_wide, aes(x = hazard, y= impact)) +
	geom_point(aes(size = n)) +
	theme_fivethirtyeight()+
    labs( x= "Impact", y = "Hazard",title = "Regional Affect of Hazards Related to Climate Change",caption="Graphic: Jason Wierzbowski\nIsha Akshita Mahajan\n Student,UMass Amherst\nSource: NORC") +
   scale_fill_manual(values = c("Not Affected \n But Anticipate Future Problems"= "#307DC4","Not Affected" ="#DB3030","Mildly Affected" = "#DFD58E","Strongly Affected"="#FDD5A5")) +
 theme(text=element_text (size = 12, hjust=0.5))
bubbleplot

```

*REORDER VARIABLES ON X AXIS AND ADD INLAND/COASTAL GROUPS, FIX Labels on Y Axis*

Next we move to the health section of the data set and asses the Health and Environmental Impacts of Climate Change on Municipalities of Massachusetts 

#### SECTION 2 HEALTH AND ENVIRONMENTAL IMPACTS OF CLIMATE CHANGE 

```{r}
health_wide <- MSCR %>% 
 select(`Municipality or Planning Agency`, 23:34)  %>% 
  pivot_longer(
    cols =  `Air Quality Concerns`:`Introduction of Invasive Species`,
    names_to = "impact",
    values_to = "health"
  ) %>% 
  group_by(`health`, impact ) %>% 
  count(health) %>% 
  mutate(health = recode_factor(health, 
    `1` = "Not Affected \n But Anticipate Future Problems",
    `2` = "Not affected" , 
    `3` = "Mildly affected" ,
    `4` = "Strongly affected")) %>% 
  filter(health != "NA") 
```

```{r}
  bubbleplot_health <- ggplot(health_wide, aes(x = health, y = impact, color = n)) +
	geom_point(aes(size = n)) +
	theme_fivethirtyeight()+
  labs( x= "Impact", y = "Health/Environmental Problem", title = "Regional Affect of Health and Environmental Damage Related to Climate Change",caption="Graphic: Jason Wierzbowski\nIsha Akshita Mahajan\n Student,UMass Amherst\nSource: NORC")+
  scale_fill_manual(values = c("Not Affected \n But Anticipate Future Problems"= "#307DC4","Not Affected" ="#DB3030","Mildly Affected" = "#DFD58E","Strongly Affected"="#FDD5A5")) +
 theme(text=element_text (size = 12, hjust=0.5))
  bubbleplot_health
```

*REORDER VARIABLES ON X AXIS AND ADD INLAND/COASTAL GROUPS, FIX Labels on Y Axis*

#### SECTION 3 INFRASTRUCTURE IMPACT

```{r}
infra_wide <- MSCR %>% 
 select(`Municipality or Planning Agency`, 36:47)  %>% 
  pivot_longer(
    cols =  `Dam/Seawall Damage`:`Power Outages`,
    names_to = "impact",
    values_to = "infrastructure"
  ) %>% 
  group_by(`infrastructure`, impact ) %>% 
  count(infrastructure) %>% 
  mutate(infrastructure = recode_factor(infrastructure, 
    `1` = "Not Affected \n But Anticipate Future Problems" ,
    `2` = "Not affected" , 
    `3` = "Mildly affected" ,
    `4` = "Strongly affected")) %>%  
  filter(infrastructure != "NA") 

```


```{r}
  bubbleplot_infra <- ggplot(infra_wide, aes(x = infrastructure, y = impact, color = n)) +
	geom_point(aes(size = n))+
	theme_fivethirtyeight()+
  labs(x= "Impact", y = "Infrastructure Problem", title = "Regional Affect on Infrastructure Damage Related to Climate Change",caption="Graphic: Jason Wierzbowski\nIsha Akshita Mahajan\n Student,UMass Amherst\nSource: NORC")+
  scale_fill_manual(values = c("Not Affected \n But Anticipate Future Problems"= "#307DC4","Not Affected" ="#DB3030","Mildly Affected" = "#DFD58E","Strongly Affected"="#FDD5A5")) +
 theme(text=element_text (size = 12, hjust=0.5))
  bubbleplot_infra
```

*REORDER VARIABLES ON X AXIS AND ADD INLAND/COASTAL GROUPS, FIX Labels on Y Axis*

#### SECTION 4 ECONOMIC IMPACT

```{r}
economic_wide <- MSCR %>% 
 select(`Municipality or Planning Agency`, 49:59)  %>% 
  pivot_longer(
    cols =  `Additonal Disaster Response Cost`:
      `Decreased Housing Availability`,
    names_to = "impact",
    values_to = "economic problem"
  ) %>% 
  group_by(`economic problem`, impact ) %>% 
  count(`economic problem`) %>% 
  mutate(`economic problem` = recode_factor(`economic problem`, 
    `1` = "Not Affected \n But Anticipate Future Problems",
    `2` = "Not affected" , 
    `3` = "Mildly affected" ,
    `4` = "Strongly affected")) %>% 
  filter(`economic problem` != "NA") 


```

```{r}
  bubbleplot_economic <- ggplot(economic_wide, aes(x = `economic problem`, y = impact, color = n)) +
	geom_point(aes(size = n)) +
	theme_fivethirtyeight()+
  labs(x= "Impact", y = "Economic Problem", title = "Regional Economic Impact Related to Climate Change",caption="Graphic: Jason Wierzbowski\nIsha Akshita Mahajan\n Student,UMass Amherst\nSource: NORC")+
  scale_fill_manual(values = c("Not Affected \n But Anticipate Future Problems"= "#307DC4","Not Affected" ="#DB3030","Mildly Affected" = "#DFD58E","Strongly Affected"="#FDD5A5")) +
 theme(text=element_text (size = 12, hjust=0.5))
 bubbleplot_economic
```

*REORDER VARIABLES ON X AXIS AND ADD INLAND/COASTAL GROUPS, FIX Labels on Y Axis*

### SECTION 5 INDUSTRY IMPACT

```{r}
industry_wide <- MSCR %>% 
 select(`Municipality or Planning Agency`, 61:71)  %>% 
  pivot_longer(
    cols =  `Healthcare`:
      `Agriculture`,
    names_to = "impact",
    values_to = "industry"
  ) %>% 
  group_by(`industry`, impact ) %>% 
  count(`industry`) %>% 
  mutate(`industry` = recode_factor(`industry`, 
    `1` = "Not Affected \n But Anticipate Future Problems" ,
    `2` = "Not affected" , 
    `3` = "Mildly affected" ,
    `4` = "Strongly affected")) %>% 
   filter(`industry` != "NA")

```

```{r}
  bubbleplot_industry <- ggplot(industry_wide, aes(x = `industry`, y = impact, color = n)) +
	geom_point(aes(size = n)) +
	theme_fivethirtyeight() +
  labs(x= "Impact", y = "Industry", title = "Industrial Impact Related to Climate Change",caption="Graphic: Jason Wierzbowski\nIsha Akshita Mahajan\n Student,UMass Amherst\nSource: NORC")+
  scale_fill_manual(values = c("Not Affected \n But Anticipate Future Problems"= "#307DC4","Not Affected" ="#DB3030","Mildly Affected" = "#DFD58E","Strongly Affected"="#FDD5A5")) +
 theme(text=element_text (size = 12, hjust=0.5))
 bubbleplot_industry
```

*REORDER VARIABLES ON X AXIS AND ADD INLAND/COASTAL GROUPS, FIX Labels on Y Axis*

### SECTION 6 RESOURCE DEDICATION

```{r}
resource_wide <- MSCR %>% 
 select(`Municipality or Planning Agency`, 75:81)  %>% 
  pivot_longer(
    cols =  `African American`:
      `Veterans`,
    names_to = "impact",
    values_to = "resource"
  ) %>% 
  group_by(`resource`, impact ) %>% 
  count(`resource`) %>% 
  mutate(`resource` = recode_factor(`resource`, 
    `1` = "Not Applicable to our \n Municipality or Region" ,
    `2` = "No" , 
    `3` = "No \n but we plan to " ,
    `4` = "Yes")) %>% 
   filter(`resource` != "NA")
```

```{r}
  bubbleplot_resource <- ggplot(resource_wide, aes(x = `resource`, y = impact, color = n)) +
	geom_point(aes(size = n)) +
	theme_fivethirtyeight()+
  labs(x= "Impact", y = "Resources", title = "Regional Resource Dedication",caption="Graphic: Jason Wierzbowski\nIsha Akshita Mahajan\n Student,UMass Amherst\nSource: NORC")+
  scale_fill_manual(values = c("Not Applicable to our \n Municipality or Region"= "#307DC4","No" ="#DB3030","No \n but we plan to" = "#DFD58E","Yes"="#FDD5A5")) +
 theme(text=element_text (size = 12, hjust=0.5))
  bubbleplot_resource
```
 
*REORDER VARIABLES ON X AXIS AND ADD INLAND/COASTAL GROUPS, FIX Labels on Y Axis*

### SECTION 7 LOCAL DATA USE
  
```{r}
local_data_wide <- MSCR %>% 
 select(`Municipality or Planning Agency`, 83:86)  %>% 
  pivot_longer(
    cols =  `Health Data`:
      `Food Security Data`,
    names_to = "impact",
    values_to = "data"
  ) %>% 
  group_by(`data`, impact ) %>% 
  count(`data`) %>% 
  mutate(`data` = recode_factor(`data`, 
    `1` = "No" ,
    `2` = "Yes" , 
    `3` = "Yes and Data is Seperated  \n by Race and Ethnicity")) %>% 
  filter(`data`!= "NA")

```

```{r}
bubbleplot_data <- ggplot(local_data_wide, aes(x = `data`, y = impact, color = n)) +
	geom_point(aes(size = n)) +
	theme_fivethirtyeight() +
    labs(x= "Impact", y = "Data ", title = "Regional Use of Data ",caption="Graphic: Jason Wierzbowski\nIsha Akshita Mahajan\n Student,UMass Amherst\nSource: NORC")+
  scale_fill_manual(values = c("No"= "#307DC4","Yes" ="#DB3030","Yes and the Data is Seperated  \n by Race and Ethnicity" = "#DFD58E")) +
 theme(text=element_text (size = 12, hjust=0.5))
bubbleplot_data
``` 
  
*REORDER VARIABLES ON X AXIS AND ADD INLAND/COASTAL GROUPS, FIX Labels on Y Axis*

### SECTION 8 CLIMATE STRATEGIES

```{r}
climstrat_wide <- MSCR %>% 
 select(`Municipality or Planning Agency`, 91:100)  %>% 
  pivot_longer(
    cols =  `Education Projects`:
      `Collaboration`,
    names_to = "impact",
    values_to = "climstrat"
  ) %>% 
  group_by(`climstrat`, impact ) %>% 
  count(`climstrat`) %>% 
  mutate(`climstrat` = recode_factor(`climstrat`, 
    `1` = "No interest" ,
    `2` = "We would like to adopt this strategy" , 
    `3` = "We adopted this strategy")) %>% 
  filter(`climstrat`!= "NA")

```

```{r}
bubbleplot_climstrat <- ggplot(climstrat_wide, aes(x = `climstrat`, y = impact, color = n)) +
	geom_point(aes(size = n)) +
	theme_fivethirtyeight() +
    labs(x= "Impact", y = "Climate Stratergy ", title = " Regional Climate Stratergies",caption="Graphic: Jason Wierzbowski\nIsha Akshita Mahajan\n Student,UMass Amherst\nSource: NORC")+
  scale_fill_manual(values = c("No interest"= "#307DC4","We would like to adopt this strategy" ="#DB3030","We adopted this strategy" = "#DFD58E")) +
 theme(text=element_text (size = 12, hjust=0.5))
  bubbleplot_climstrat
  
```
  
*REORDER VARIABLES ON X AXIS AND ADD INLAND/COASTAL GROUPS, FIX Labels on Y Axis*

### SECTION 9 ENGINEERED INFRASTRUCTURE STRATEGIES

```{r}
engistrat_wide <- MSCR %>% 
 select(`Municipality or Planning Agency`, 102:109)  %>% 
  pivot_longer(
    cols =  `Tidal Barriers/Seawalls`:
      `Resilience of Telecommunication`,
    names_to = "impact",
    values_to = "engistrat"
  ) %>% 
  group_by(`engistrat`, impact ) %>% 
  count(`engistrat`) %>% 
  mutate(`engistrat` = recode_factor(`engistrat`, 
    `1` = "No interest" ,
    `2` = "We would like to adopt this strategy" , 
    `3` = "We adopted this strategy")) %>% 
  filter(`engistrat`!= "NA")
  
```

```{r}
bubbleplot_engistrat <- ggplot(engistrat_wide, aes(x = `engistrat`, y = impact, color = n)) +
	geom_point(aes(size = n)) +
	theme_fivethirtyeight() +
  labs(x= "Impact", y = "Engineered Stratergy ", title = " Regional Engineered Stratergies",caption="Graphic: Jason Wierzbowski\nIsha Akshita Mahajan\n Student,UMass Amherst\nSource: NORC")+
  scale_fill_manual(values = c("No interest"= "#307DC4","We would like to adopt this strategy" ="#DB3030","We adopted this strategy" = "#DFD58E")) +
 theme(text=element_text (size = 12, hjust=0.5))
  bubbleplot_engistrat

```

*REORDER VARIABLES ON X AXIS AND ADD INLAND/COASTAL GROUPS, FIX Labels on Y Axis*

### SECTION 10 GREEN STRATEGIES

```{r}
greenstrat_wide <- MSCR %>% 
 select(`Municipality or Planning Agency`, 111:118)  %>% 
  pivot_longer(
    cols =  `Green Roofs`:
      `Restorative Agriculture`,
    names_to = "impact",
    values_to = "greenstrat"
  ) %>% 
  group_by(`greenstrat`, impact ) %>% 
  count(`greenstrat`) %>% 
  mutate(`greenstrat` = recode_factor(`greenstrat`, 
    `1` = "No interest" ,
    `2` = "We would like to adopt this strategy" , 
    `3` = "We adopted this strategy")) %>% 
   filter(`greenstrat`!= "NA")

```
  
  
```{r}
 bubbleplot_greenstrat <- ggplot(greenstrat_wide, aes(x = `greenstrat`, y = impact, color = n)) +
	geom_point(aes(size = n)) +
	theme_fivethirtyeight()+
  labs(x= "Impact", y = "Green Stratergy ", title = " Regional Green Stratergy Adoption",caption="Graphic: Jason Wierzbowski\nIsha Akshita Mahajan\n Student,UMass Amherst\nSource: NORC")+
  scale_fill_manual(values = c("No interest"= "#307DC4","We would like to adopt this strategy" ="#DB3030","We adopted this strategy" = "#DFD58E")) +
 theme(text=element_text (size = 12, hjust=0.5))
  bubbleplot_greenstrat
```

*REORDER VARIABLES ON X AXIS AND ADD INLAND/COASTAL GROUPS, FIX Labels on Y Axis*

### SECTION 11 NCCR ASSISTANCE

```{r}
NCCRstrat_wide <- MSCR %>% 
 select(`Municipality or Planning Agency`, 127:136)  %>% 
  pivot_longer(
    cols =  `Technical Assistance`:
      `Legal Support`,
    names_to = "impact",
    values_to = "NCCRstrat"
  ) %>% 
  group_by(`NCCRstrat`, impact ) %>% 
  count(`NCCRstrat`) %>% 
  mutate(`NCCRstrat` = recode_factor(`NCCRstrat`, 
    `1` = "Not at all helpful" ,
    `2` = "Somewhat helpful" , 
    `3` = "Very helpful")) %>% 
  filter(`NCCRstrat`!= "NA")

```

```{r}
bubbleplot_NCCRstrat <- ggplot(NCCRstrat_wide, aes(x = `NCCRstrat`, y = impact, color = n)) +
	geom_point(aes(size = n)) +
	theme_fivethirtyeight()+
  labs(x= "Impact", y = "NCCR Support", title = " NCCR Support",caption="Graphic: Jason Wierzbowski\nIsha Akshita Mahajan\n Student,UMass Amherst\nSource: NORC")+
  scale_fill_manual(values = c("Not helpful at all"= "#307DC4","Somewhat helpful" ="#DB3030","Very helpful" = "#DFD58E")) +
 theme(text=element_text (size = 12, hjust=0.5))
  bubbleplot_NCCRstrat
```

*REORDER VARIABLES ON X AXIS AND ADD INLAND/COASTAL GROUPS, FIX Labels on Y Axis*
