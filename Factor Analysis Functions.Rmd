---
title: "Factor Analysis on Data Segments"
author: "Meredith Rolfe, Isha Mahajan, Jason Wierzbowski"
date: "10/14/2021"
output: html_document
---

#### Load the data from Data Wrangling File

**Run chunks 14-303 from Data Wrangling File**

After our initial data cleaning, we formed groups based on the questions asked. We elected to remove questions that didn't contain much useful information or were generally not very useful for our analysis. We ended up working with 11 question groups which included questions about climate change impact, health impacts, infrastructure impacts, economic impacts, industry impacts, the use of resources to vulnerable populations, the availability of local data, climate change strategies, engineering strategies, green strategies, and the use of resources provided by the National Center of Coastal Resilience. We then created functions to extract useful measures that would help us decide how many factors to use for each question group. After careful examination we decided it would be best to use 1 factor for most question groups with exceptions for climate change impact, health impacts, climate strategies, and green strategies where we decided on 3 factors based on alpha scores and root mean squared error as a measure of goodness of fit. We then calculated the factor scores for each question group and combined it into one data set.

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

```{r}
jpeg('rplot.jpg', width = 800, height = 600, quality = 800)
plot <- scree(cor(climate.impact, use = "pairwise.complete.obs"), factors = FALSE)
dev.off()
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

We decided to go with 3 loadings on health and environmental impact. The alpha score was under .8(.78) and we found that there were 3 distinct groups. The first loading had high values for species loss(.75) and animal out migration(.84) which could indicate a general impact factor. The second loading has high values for air quality concerns(.80), increase in vector borne diseases(.5), and heat related illness(.79) which seems to indicate municipalities that are cities. The third loading has high values for wastewater management concerns(.83) and water habitat degradation(.43) which seems to represent coastal municipalities.

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

We decided to go with 3 loadings for climate strategies as the alpha score was .81 which was close enough that we felt distinct groups would form. The first loading has high values for creating adaptive management capacity(.74) and redevelopment plans(.73) which we believe to represent coastal municipalities. The second loading has high values for zoning planning(1) which may indicate city municipalities. The third loading has high values for education projects(.64), risk assessment(.6), and collaboration(.56) which we believe represents a general impact factor.

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

We decided to go with 3 loadings for green strategies as the alpha score was .74 which told us that there may be valuable information in additional loadings. The first loading has high values for NBS to flooding(.86) and NBS to erosion(.83) which may indicate coastal municipalities. The second loading has high values for green roofs(.52), urban greening(.72), and NBS to HIE(.87) definitely indicating city municipalities. The third loading has high values for restorative agriculture(1) possibly indicating a general impact variable.

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

#final_data <- final_data %>%
  #group_by(`Muni names`) %>% 
  #mutate(higher_edu = sum(ba+mast+prof+doc)/sum(nohs+hs+sc+assoc+ba+mast+prof+doc)) %>% 
  #select(-nohs, -hs, -sc, -assoc, -ba, -mast, -prof, -doc)
#proportion of municipal population that hold a bachelors degree or greater. 
```

```{r}
for (i in 1:nrow(final_data)) {
  final_data$higher_edu[i] = (final_data$ba[i] + final_data$mast[i] + final_data$prof[i] + final_data$doc[i])/(final_data$nohs[i] + final_data$hs[i] + final_data$sc[i] + final_data$assoc[i] + final_data$ba[i] + final_data$mast[i] + final_data$prof[i] + final_data$doc[i])
}

final_data <- final_data%>%
  select(-nohs, -hs, -sc, -assoc, -ba, -mast, -prof, -doc)
```


```{r}
pol <- read_csv("data/2020 Municipal Voting Data.csv")

final_data <- final_data%>%
  left_join(pol, by = c("Muni names" = "Town"))%>%
  select(colnames(final_data), Democratic)
```

Our final data set contains 155 observations of 39 variables. In addition to the factor scores we received, we also included binary variables for coastal municipalities and city municipalities. From those binary variables we created four more binary variables to indicate the four possible outcomes from combining the two original binary variables which included coastal city, coastal town, inland city, and inland town. After that we began to collect municipal level data from the U.S. Census Bureau. This data included male and female population proportions from each municipality, population proportions by race, the total tax revenue for each municipality, the proportion of the population that holds a bachelor's degree or higher, and the percentage of municipal populations that voted democratic and republican in the 2020 presidential election

```{r eval=FALSE, include=FALSE}
#inc <- read_csv("data/median household income.csv")
#inc <- na.omit(inc)

#inc$Town <- substr(inc$Town, 1, nchar(inc$Town)- 19)
#inc$Town <- gsub(" Center", "",inc$Town)
#inc$Town <- gsub(" Town", "",inc$Town)
#inc$Town <- trimws(inc$Town, which = c("right"))

#final_data <- final_data%>%
  #left_join(inc, by = c("Muni names" = "Town"))%>%
  #select(colnames(final_data), "Median Income")
```

