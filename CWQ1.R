library(tidyverse)
library(readxl)
library(NbClust)
library(knitr)
library(tidymodels)
library(flexclust)
library(funtimes)
library(factoextra)
theme_set(theme_light())

rm(list = ls())

# Read in the original excel datafile


data.vehicles.original <- read_excel("D:/EDUCATION/IIT/SECOND YEAR/SECOND SEMESTER/MACHINE LEARNING/Coursework 01/vehicles.xlsx") %>%
  janitor::clean_names() %>%
  #https://www.rdocumentation.org/packages/janitor/versions/1.2.0/topics/clean_names
  mutate(class = as_factor(class))
# Get a birds eye view of how the dataset looks like
summary(data.vehicles.original)
length(data.vehicles.original)

data.vehicles.original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'van'")

data.vehicles.original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'bus'")


data.vehicles.original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class Saab")



data.vehicles.original %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for Opel class")




data.vehicles.bus = data.vehicles.original %>%
  filter(class == "bus") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .85)))))


data.vehicles.van = data.vehicles.original %>%
  filter(class == "van") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))


data.vehicles.opel = data.vehicles.original %>%
  filter(class == "opel") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))


data.vehicles.saab = data.vehicles.original %>%
  filter(class == "saab") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))


new.data = bind_rows(list(data.vehicles.bus,data.vehicles.opel,data.vehicles.saab,data.vehicles.van)) %>%
  arrange(samples)



print(new.data)



new.data %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers class: 'bus'")


##new.data %>%
  ##pivot_longer(2:19,names_to = "labels") %>%
  ##filter(class == "bus") %>%
  ##mutate(class = fct_reorder(class,value,median)) %>%
  ##ggplot(aes(class, value, fill = reorder(labels,value))) +
  ##geom_boxplot(outlier.shape = NA) +
  ##labs(title = "Transformed Outliers class: 'bus'")





new.data %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: saab")

new.data %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for Van class")

new.data %>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Transformed Outliers for class: opel")

##data.rm.out <- new.data[!new.data %in% boxplot.stats(ff)$out]
##length(ff)-length(data.rm.out)



# Remove the sample name and the class name. Both of these will be remove so that only n

#Numerical data is left for the algorithm.

final_data_points = new.data %>%
  
  select(-samples, -class)

# Now that we have the "vehicles_data_points" dataset, scaling is performed

scaled.veh.data = final_data_points %>%
  
  mutate(across(everything(), scale))



set.seed(123)


#Using kMeans Algorithm for the k taken randomly (Manually)
rand.result_1<- kmeans(scaled.veh.data,3) #aplly k-means algorithm with no. of centroids(k)=3
rand.result_1$size # gives no. of records in each cluster

rand.result_1$centers

rand.result_1$cluster
rand.result_1$means

table(rand.result_1$cluster, new.data$class)
fviz_cluster(rand.result_1, data = scaled.veh.data)





# Perform the kmeans using the NbClust function

# Use Euclidean for distance

euclidean.cluster = NbClust(scaled.veh.data,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")





# Use manhattan for distance

manhattan.cluster = NbClust(scaled.veh.data,distance="manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")


# Elbow method
fviz_nbclust(scaled.veh.data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#k = 2:10
#set.seed(42)
#WSS = sapply(k, function(k) {kmeans(scaled.veh.data, centers=k)$tot.withinss})
#plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")

# Silhouette method
fviz_nbclust(scaled.veh.data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
#fviz_nbclust(scaled.veh.data, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  #labs(subtitle = "Gap statistic method")


#Using kMeans Algorithm for the k=2 calculated by majority of methods
major_result<- kmeans(scaled.veh.data,2) #aplly k-means algorithm with no. of centroids(k)=3
major_result$size # gives no. of records in each cluster

major_result$centers

major_result$cluster


table(major_result$cluster, new.data$class)
fviz_cluster(major_result, data = scaled.veh.data)

#Using kMeans Algorithm for the k calculated by elbow method
elbow.result<- kmeans(scaled.veh.data,4) #aplly k-means algorithm with no. of centroids(k)=3
elbow.result$size # gives no. of records in each cluster

elbow.result$centers

elbow.result$cluster


table(elbow.result$cluster, new.data$class)
fviz_cluster(elbow.result, data = scaled.veh.data)

#Using kMeans Algorithm for the k calculated by gap statistic method
stat.result<- kmeans(scaled.veh.data,10) #aplly k-means algorithm with no. of centroids(k)=3
stat.result$size # gives no. of records in each cluster

stat.result$centers

stat.result$cluster


table(stat.result$cluster, new.data$class)
fviz_cluster(stat.result, data = scaled.veh.data)