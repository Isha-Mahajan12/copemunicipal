# Copemunicipal

As part of an independent study with Dr. Meredith Rolfe, a team of two students performed a data analysis on [Survey Data] collected by the Massachusetts Municipal Association and the Northeast Center for Coastal Resilience. The survey targeted regional planners and members of local municipalities and aimed to understand the impacts of climate change on their communities. 

# Key Components of the project 
1. Stakeholder Analysis to get a better understanding of the Data from the PIs.
2. Data Cleaning and Exploratory Visualizations to visualize the structure of the data 
3. Performing Factor Analysis to reduce data dimensionality. From 157 variables, we removed qualitative assessments and reduced the data to 26 variables by looking at alpha scores, and root mean squared error of approximation (RMSEA) for goodness of fit. 
4. Added demographic data to municipalities by geolocation. Included data for education levels, voting preferences, median incomes, race geolocation of each municipality i.e. coastal, inland and type of municipality i.e. city or town. 
5. Ran K-Means clustering on scaled and unscaled data to search for clear clusters in the data based on their means/centroid properties. 

# libraries used
R: tidyverse, psych, factoExtra, pls, tidy census

# Results
Our analysis found that K=3 on scaled data had the least amount of overlap and when we looked back at the data we found that geographic locations were the key variables that were distinct from one another in this analysis.
