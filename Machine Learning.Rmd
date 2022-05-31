---
title: "Machine Learning on Final Data Set"
author: "Meredith Rolfe, Isha Mahajan, Jason Wierzbowski"
date: "10/14/2021"
output: html_document
---

**Run chunks 14-303 from Data Wrangling File**
**Run chunks 12-342 from Factor Analysis Functions File**

```{r}
library(pls)
library(factoextra)
library(ggthemes)

final_data <- na.omit(final_data)
```

##UNSUPERVISED LEARNING 

### K-Means 

We began our analysis by running K-mean models with number of clusters 3, 4 and 5. Given that we ran Factor Analysis prior to this clustering, we thought that running more than 5 clusters might be counter-productive as it would increase data-dimensionality. 


## K = 3 

With K= 3, the sizes of the clusters were 1, 11 and 143 variables respectively. 


```{r}
set.seed(007)
km3 <- kmeans(select(final_data, -"Muni names", -Coastal, -City), centers = 3)
km3$cluster
km3$size
```


```{r}
km3_viz <- fviz_cluster(km3, data = select(final_data, -"Muni names", -Coastal, -City),
             palette = c("#e60000", "#0080ff", "#00cc66"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_fivethirtyeight(), 
             title = "Cluster Plot K = 3", 
             caption = "Graphic: Jason Wierzbowski,Isha Mahajan 
             Source: Massachusetts Municipal Association")
print(km3_viz)

ggsave("km3_viz.jpg", device="jpg", width=8, height=8, units="in",dpi=300)
```


## K = 4


```{r}
km4 <- kmeans(select(final_data, -"Muni names", -Coastal, -City ), centers = 4)
km4$cluster
km3$size
```



```{r}
km4_viz <- fviz_cluster(km4, data = select(final_data, -"Muni names", -Coastal, -City),
             palette = c("#e60000", "#0080ff", "#00cc66","#ffcc00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_fivethirtyeight(), 
             title = "Cluster Plot K = 4", 
             caption = "Graphic: Jason Wierzbowski,Isha Mahajan 
             Source: Massachusetts Municipal Association")
km4_viz
ggsave("km4_viz.jpg", device="jpg", width=8, height=8, units="in",dpi=300)
```

```{r}
km5 <- kmeans(select(final_data, -"Muni names", -Coastal, -City ), centers = 5)
km5$cluster
plot(km5$cluster)
```


```{r}
km5_viz <- fviz_cluster(km5, data = select(final_data, -"Muni names", -Coastal, -City),
             palette = c("#e60000", "#0080ff", "#00cc66","#ffcc00", "#9966ff"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_fivethirtyeight(), 
             title = "Cluster Plot K = 5", 
             caption = "Graphic: Jason Wierzbowski,Isha Mahajan 
             Source: Massachusetts Municipal Association")
km5_viz
ggsave("km5_viz.jpg", device="jpg", width=8, height=8, units="in",dpi=300)
```


```{r}
set.seed(007)
final_s <- scale(select(final_data, -"Muni names", -Coastal, -City))
km3s <- kmeans(final_s, centers = 3)

cluster <- final_data %>% 
mutate(cluster =km3s$cluster) %>% 
group_by(cluster) %>% 
arrange(cluster) %>% 
relocate(cluster)
```


### K-MEANS  with Scaled data 

```{r}
km3s_viz <- fviz_cluster(km3s, data = select(final_data, -"Muni names", -Coastal, -City),
             palette = c("#e60000", "#0080ff", "#00cc66"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_fivethirtyeight(), 
             title = "Cluster Plot K = 3 (Scaled)", 
             caption = "Graphic: Jason Wierzbowski,Isha Mahajan 
             Source: Massachusetts Municipal Association")
km3s_viz
ggsave("km3s_viz.jpg", device="jpg", width=8, height=8, units="in",dpi=300)
```


```{r}
final_4 <- scale(select(final_data, -"Muni names", -Coastal, -City))
km4s <- kmeans(final_s, centers = 4)
km4s$size

cluster_4 <- final_data %>% 
mutate(cluster =km4s$cluster) %>% 
group_by(cluster) %>% 
arrange(cluster) %>% 
relocate(cluster)
```


```{r}
km4s_viz <- fviz_cluster(km4s, data = select(final_data, -"Muni names", -Coastal, -City),
             palette = c("#e60000", "#0080ff", "#00cc66","#ffcc00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_fivethirtyeight(), 
             title = "Cluster Plot K = 4 (Scaled)", 
             caption = "Graphic: Jason Wierzbowski,Isha Mahajan 
             Source: Massachusetts Municipal Association")
km4s_viz
ggsave("km4s_viz.jpg", device="jpg", width=8, height=8, units="in",dpi=300)
```


```{r}
final_5 <- scale(select(final_data, -"Muni names", -Coastal, -City))
km5s <- kmeans(final_s, centers = 5)
km5s$size

cluster_5 <- final_data %>% 
mutate(cluster =km4s$cluster) %>% 
group_by(cluster) %>% 
arrange(cluster) %>% 
relocate(cluster)
```


```{r}
km5s_viz <- fviz_cluster(km5s, data = select(final_data, -"Muni names", -Coastal, -City),
             palette = c("#e60000", "#0080ff", "#00cc66","#ffcc00", "#9966ff"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_fivethirtyeight(), 
             title = "Cluster Plot K = 5 (Scaled)" , 
             caption = "Graphic: Jason Wierzbowski,Isha Mahajan 
             Source: Massachusetts Municipal Association")
km5s_viz
ggsave("km5s_viz.jpg", device="jpg", width=8, height=8, units="in",dpi=300)
```

### Heirarchical Clustering

## Method = Complete

```{r}
df <- scale(select(final_data, -"Muni names", -Coastal, -City))
# Hierarchical clustering
res.hc.complete <- hclust(dist(df), method = "complete")
res.hc.complete$labels = final_data$`Muni names`
# Default plot
fviz_dend(res.hc.complete)
fviz_dend(res.hc.complete, color_labels_by_k = TRUE)
# Don't color labels, add rectangles
fviz_dend(res.hc.complete,
 color_labels_by_k = FALSE, rect = TRUE)
```


## Method = Average

```{r}
# Hierarchical clustering
res.hc.avg <- hclust(dist(df), method = "average")
res.hc.avg$labels = final_data$`Muni names`
# Default plot
fviz_dend(res.hc.avg)
fviz_dend(res.hc.avg, cex = 0.5, color_labels_by_k = TRUE)
# Don't color labels, add rectangles
fviz_dend(res.hc.avg, cex = 0.5,
 color_labels_by_k = FALSE, rect = TRUE)
```


## Method = Euclidean

```{r}
# Hierarchical clustering
res.hc.euc <- hclust(dist(df), method = "centroid")
res.hc.euc$labels = final_data$`Muni names`
# Default plot
fviz_dend(res.hc.euc)
fviz_dend(res.hc.euc, cex = 0.5)
# Don't color labels, add rectangles
fviz_dend(res.hc.euc, cex = 0.5, rect = TRUE)

```

##SUPERVISED LEARNING 

```{r}

```



