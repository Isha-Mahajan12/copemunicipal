---
title: "Municipal Data"
author: "Meredith Rolfe, Isha Mahajan, Jason Wierzbowski"
date: "10/14/2021"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(miniUI)
library(dplyr)
```

## Data sources

The data used here are downloaded from the Massachusetts Muncicipal Association (MMA) website, or the [MAPC datacommon](https://datacommon.mapc.org/gallery)

The MMA data is provided in pdf format, and tabulizer is needed to extract the table of data.

This first chunk is used to locate the columns in the pdf manually.

```{r}
area<-tabulizer::locate_areas("data/Forms-of-Municipal-Governnment_2020-21.pdf")
cols<-tabulizer::locate_areas("data/Forms-of-Municipal-Governnment_2020-21.pdf")
cols2<-tabulizer::locate_areas("data/Forms-of-Municipal-Governnment_2020-21.pdf")

```

Now we can extra all columns.

```{r}
mma.orig<-tabulizer::extract_tables("data/Forms-of-Municipal-Governnment_2020-21.pdf",
    guess=FALSE,
   area = list(area[[1]], area[[2]], area[[2]], area[[2]]),
   columns = list(c(310, 404, 465, 518),
                  c(135, 195, 248, 310, 404, 465, 518),
                  c(135, 195, 248, 310, 404, 465, 517),
                  c(135, 195, 248, 310, 404, 465, 518)))
mma.orig[[1]]<-mma.orig[[1]][-c(1,2),-1]

mma.names<-c("Community", "chiefOfficial", "policyBoard", "legislativeBody")

colnames(mma.orig[[1]])<-mma.names

for(i in 2:4){
mma.orig[[i]]<-rbind(mma.orig[[i]][-c(1,2),1:4], mma.orig[[i]][-c(1,2),5:8])
colnames(mma.orig[[i]])<-mma.names
}

```

Now do some additional cleaning to identify cities (in capitals) and pull out the dates of incorporation (in paranethesis) from the Community columns

```{r}
mma<-tibble(do.call(rbind.data.frame, mma.orig))%>%
  mutate(city = str_detect(Community, pattern = "^[[:upper:]]{3,}"),
         cityCharter = str_extract(Community, pattern = "\\([ABCDEF]\\)"),
         Community = str_remove(Community, pattern = "\\([ABCDEF]\\)"),
         policySize = str_extract(policyBoard, pattern = "\\([:digit:]\\)"),
         policyBoard = str_remove(policyBoard, pattern = "\\([:digit:]\\)"),
         legislativeSize = str_extract(legislativeBody, pattern = "\\([:digit:]+\\)"),
         legislativeBody = str_remove(legislativeBody, pattern = "\\([:digit:]+\\)"),
         homeRule = str_extract(Community, pattern = "\\([:digit:]{4}\\)"),
         Community = str_remove(Community, pattern = "\\([:digit:]{4}\\)"),
         policySize = str_remove_all(policySize, pattern = "[[:punct:]]"),
         legislativeSize = str_remove_all(legislativeSize, pattern = "[[:punct:]]"),
         homeRule = str_remove_all(homeRule, pattern = "[[:punct:]]"))%>%
  filter(!Community=="")

write.csv(mma, "mma_govtforms.csv")
```








