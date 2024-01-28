library(dplyr)
library(tidyr)
library(factoextra)
library(caret)
###1### prepare
#read csv
Data_2018 <- read.csv("archive/2018_Financial_Data.csv", header = TRUE)
#test# check columns
unique(Data_2018$Sector)
group_by(Data_2018, Sector) %>% summarise(count=n())
# select columns (6 indicator (Ng & Khor, 2017))
temp_2018 <- subset(Data_2018, select = c("X", 
                                          "assetTurnover",
                                          "cashRatio",
                                          "debtRatio",
                                          "returnOnEquity",
                                          "dividendYield",
                                          "priceEarningsToGrowthRatio",
                                          "Sector",
                                          "Class"))
#join top 500
top_500 <- read.csv("constituents_csv.csv", header = TRUE)
top_500 <- select(top_500, -3)
top_500_data <- left_join(top_500, temp_2018, by=c("Symbol"="X"))
# create an ID column for sector column
top_500_data$Sector_id<-as.numeric(as.factor(top_500_data$Sector))
#visulization# #1# bar chart for sectors
ggplot(top_500_data, aes(x = fct_rev(fct_infreq(Sector))), y = value)+
  geom_bar( position = "dodge", fill="lightblue", stat = "count", width = 0.8)+ 
  labs(x="Sectors", y="Number of Sectors",
       title="Number of Sector in S&P 500 Stocks")+
  theme_minimal()+
  coord_flip()

###2### pre-processing
# remove NA
top_500_data <- drop_na(top_500_data)
# #test# 
# temp_top_500_data <- top_500_data#
# top_500_data <- temp_top_500_data#
# complement <- setdiff(temp_top_500_data, top_500_data)#
# write.csv(complement, "complement.csv", row.names=FALSE)#
# visualization # #2# outliers
ggplot(top_500_data) +
  aes(x = "", y = assetTurnover) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal()
#remove outliers (rows with values more than 3 standard deviations from the mean)
#1-assetTurnover 
mean <- mean(top_500_data$assetTurnover)
sd <- sd(top_500_data$assetTurnover)
top_500_data <- top_500_data[abs(top_500_data$assetTurnover - mean) <=3 * sd, ]
#2-cashRatio
mean <- mean(top_500_data$cashRatio)
sd <- sd(top_500_data$cashRatio)
top_500_data <- top_500_data[abs(top_500_data$cashRatio - mean) <= 3 * sd, ]
#3-debtRatio
mean <- mean(top_500_data$debtRatio)
sd <- sd(top_500_data$debtRatio)
top_500_data <- top_500_data[abs(top_500_data$debtRatio - mean) <= 3 * sd, ]
#4-returnOnEquity
mean <- mean(top_500_data$returnOnEquity)
sd <- sd(top_500_data$returnOnEquity)
top_500_data <- top_500_data[abs(top_500_data$returnOnEquity - mean) <= 3 * sd, ]
#5-dividendYield
mean <- mean(top_500_data$dividendYield)
sd <- sd(top_500_data$dividendYield)
top_500_data <- top_500_data[abs(top_500_data$dividendYield - mean) <= 3 * sd, ]
#6-priceEarningsToGrowthRatio
mean <- mean(top_500_data$priceEarningsToGrowthRatio)
sd <- sd(top_500_data$priceEarningsToGrowthRatio)
top_500_data <- top_500_data[abs(top_500_data$priceEarningsToGrowthRatio - mean) <= 3 * sd, ]

###3### clustering
# remove non-number 
top_500_data1 <- subset(top_500_data, select = -c(Symbol,Name,Sector))
# remove column that no need in cluster
top_500_data1 <- subset(top_500_data1, select = -c(Class,Sector_id))
# scale the data
top_500_data1 <- scale(top_500_data1)
# calculate th number of clusters
fviz_nbclust(top_500_data1, kmeans, method = "wss", k.max = 20, nstart=1) +
  labs(subtitle = "Elbow Method")
# kmeans
km.out <- kmeans(top_500_data1, centers = 12, nstart = 25)
km.out$size #
# visualization # #3# the clustering result PCA
km.clusters <- km.out$cluster
rownames(top_500_data1) <- top_500_data$Symbol
fviz_cluster(labelsize = 8,
             ggtheme = theme_minimal(),
             list(data = top_500_data1, clusters = km.clusters))
# #test# # build a dataframe that contain the result
# top_500_cluster <- subset(top_500_data, select = c("Symbol",
#                                                    "Name",
#                                                    "Class",
#                                                    "Sector"))
# km.out_cluster <- data.frame(km.out$cluster) #stock name and cluster group
# top_500_cluster <- cbind(top_500_cluster, km.out_cluster)
# write.csv(top_500_cluster, "top_500_cluster.csv", row.names=FALSE)#
#test# # save result into dataframe
km.out_centers <- data.frame(km.out$centers)
serial_number <- seq_len(nrow(km.out_centers))
km.out_centers <- cbind(serial_number, km.out_centers)
km.out_centers <- km.out_centers %>%
  mutate(serial_number = as.character(serial_number)) %>%
  mutate(serial_number = paste0("Group_", serial_number))
write.csv(km.out_centers, "km.out_centers.csv", row.names=FALSE)#
# visualization # #4# cluster result using radar plot
# install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar",
                         dependencies = TRUE, force=TRUE)
library(ggradar)
library(ggplot2)
library(scales)
library(tidyverse)
km.out_centers %>% 
  mutate_if(is.numeric, rescale)%>%
  mutate(new_serial_number=str_replace_all(serial_number, " ", "_")) %>%
  group_by(new_serial_number) %>%
  summarise_if(is.numeric, mean) %>%
  ggradar(axis.label.size = 3,
          axis.label.offset = 1.1,
          legend.text.size = 8,
          group.line.width = 1,
          grid.label.size = 4,
          group.point.size = 2,
          gridline.min.colour = "grey",
          gridline.mid.colour = "grey",
          gridline.max.colour = "grey",
          background.circle.colour = "#faf9f5",
          axis.line.colour = "white")

# visualizaztion # #5# sector & cluster using Sankey diagrams
library(ggalluvial)
library(tidyverse)
ggplot(data = top_500_cluster,
       aes(axis1 = fct_infreq(Sector), axis2 = km.out.cluster)) +
  geom_alluvium(aes(fill = Sector)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Sector", "Clustering group"),
                   expand = c(0.01, 0.05)) +
  theme_void()+
  theme(panel.background = element_blank(), 
        axis.line.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 10, face = "bold") ,
        axis.ticks = element_blank())

# visualization # #6# polt price_var by clustering
temp_var <- subset(Data_2018, select = c("X", "X2019.PRICE.VAR...."))
# join var X2019.PRICE.VAR
top_500_cluster2 <- left_join(top_500_cluster, temp_var, by=c("Symbol"="X"))
top_500_cluster2 <- top_500_cluster2 %>%
  mutate(cluster_group = paste0("Group_", km.out.cluster))
# plot it
ggplot(top_500_cluster2, aes(cluster_group, X2019.PRICE.VAR....)) +
  geom_boxplot(varwidth=TRUE, fill="lightblue") +
  theme_minimal()+
  labs(title="Price Variation of Each Clustering Group",
       x="Clustering Group", y="Price Variation")

#test# ### regression of 6 indicator clustering
## pre-processing
# install.packages('caTools')
top_500_cluster1 <- top_500_cluster
# remove non numeric
top_500_cluster1 <- subset(top_500_cluster1, select = -c(Symbol, Name, Sector))
library(caTools)
set.seed(123)
top_500_cluster1 <- data.frame(top_500_cluster1)
split = sample.split(top_500_cluster1$Class, SplitRatio = 0.75)
training_set = subset(top_500_cluster1, split == TRUE)
test_set = subset(top_500_cluster1, split == FALSE)
# Feature Scaling ("[]" is independent variables)
training_set[, 2:3] = scale(training_set[, 2:3])
test_set[, 2:3] = scale(test_set[, 2:3])
# Fitting Logistic Regression to the Training set
classifier = glm(formula = Class ~ .,
                 family = binomial,
                 data = training_set)
# Predicting the Test set results ("[]" is to remove dependent variable)
prob_pred = predict(classifier, type = 'response', newdata = test_set[-1])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
# Making the Confusion Matrix ("[]" is dependent variable)
cm = table(test_set[, 1], y_pred)

#test# ###chi-square (cluster & class)
# Create a contingency table
library(stats)
my_table <- table(top_500_cluster$Class, top_500_cluster$km.out.cluster)
# Perform a chi-square test on the contingency table
chisq.test(my_table)


