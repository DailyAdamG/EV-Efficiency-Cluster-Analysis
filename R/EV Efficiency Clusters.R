#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/EV-Efficiency-Cluster-Analysis/R")

#Loading libraries

library(tidyverse)
library(factoextra)
library(scales)
library(writexl)
library(clue)
library(grid)
library(gridExtra)

#Loading data

data_2019 <- read.csv("C:/Users/daily/Desktop/Repositories/EV-Efficiency-Cluster-Analysis/Csvs/2019 Hitter Data.csv")

data_2020 <- read.csv("C:/Users/daily/Desktop/Repositories/EV-Efficiency-Cluster-Analysis/Csvs/2020 Hitter Data.csv")

fg_data <- read.csv("C:/Users/daily/Desktop/Repositories/EV-Efficiency-Cluster-Analysis/Csvs/FG 2019-2020.csv")

milb_test <- read.csv("C:/Users/daily/Desktop/Repositories/EV-Efficiency-Cluster-Analysis/Csvs/MiLB Hitters.csv")

#Combine data

source_data <- rbind(data_2019, data_2020)

#Find each player's max EV

Max_EV <- source_data %>%
  filter(type == "X") %>%
  group_by(batter) %>%
  summarise(MaxEV = max(as.numeric(launch_speed), na.rm = TRUE)) %>%
  ungroup()

#Limit data set to EV

EV <- source_data %>%
  filter(type == "X" & launch_speed != "null") %>%
  select(batter, player_name, launch_speed, launch_angle, woba_value, estimated_woba_using_speedangle)

#Combine EV with maxEV

Combined_Data <- merge(EV, Max_EV, by = "batter")

#Create a column to see if a player reached 90% of his max EV

Combined_Data <- Combined_Data %>%
  mutate(well_hit = ifelse(as.numeric(launch_speed) >= MaxEV * 0.9, 1, 0))

#Create leaderboard for bat control

Leaderboard <- Combined_Data %>%
  group_by(batter, player_name) %>%
  summarise(well_hit_rate = mean(well_hit), wOBACON = mean(as.numeric(woba_value)), 
            xwOBACON = mean(as.numeric(estimated_woba_using_speedangle)),
            LA = mean(as.numeric(launch_angle)), EV = mean(as.numeric(launch_speed)), 
            MaxEV = max(MaxEV), n = n()) %>%
  ungroup()

Leaderboard <- Leaderboard[order(-Leaderboard$well_hit_rate),] %>%
  filter(n >= 100)

#Combine Leaderboard with FanGraphs hitter data

Leaderboard <- merge(Leaderboard, fg_data, by.x = "batter", by.y = "batter")

#Create scatter plots to compare MaxEV and well_hit_rate to wOBACON

scatter_one <- Leaderboard %>%
  ggplot(aes(x = well_hit_rate, y = wOBACON)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(labels = percent, breaks = seq(.1, .5, .05)) +
  scale_y_continuous(labels = number_format(accuracy = .001), breaks = seq(.25, .6, .05)) +
  labs(title = "Exit Velocity Efficiency vs. wOBACON", x = "Exit Velocity Efficiency") +
  theme(plot.title = element_text(hjust = 0.5))
  

scatter_two <- Leaderboard %>%
  ggplot(aes(x = MaxEV, y = wOBACON)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(breaks = seq(100,120,3)) +
  scale_y_continuous(labels = number_format(accuracy = .001), breaks = seq(.25, .6, .05)) +
  labs(title = "Maximum Exit Velocity vs. wOBACON", x = "Maximum Exit Velocity") +
  theme(plot.title = element_text(hjust = 0.5))

#Combine both visuals into one graphic

grid.arrange(scatter_one, scatter_two, nrow = 1, top = textGrob("Scatter Plot Comparison",
                                                                gp = gpar(fontsize = 28)))

#Compare correlations

cor(Leaderboard$well_hit_rate, Leaderboard$wOBACON, method = "pearson")

cor(Leaderboard$MaxEV, Leaderboard$wOBACON, method = "pearson")

#Limit variables and normalize the data

scaled_data <- Leaderboard %>%
  select(well_hit_rate, MaxEV) %>%
  scale()

#Determine the optimal number of clusters using the elbow method

fviz_nbclust(scaled_data, kmeans, method = "wss")

#Determine the optimal number of clusters using the average silhouette method

fviz_nbclust(scaled_data, kmeans, method = "silhouette")

#Determine the optimal number of clusters using the average silhouette method

fviz_nbclust(scaled_data, kmeans, method = "gap_stat")

#Clustering the data using kmeans function

k <- kmeans(scaled_data, centers = 5, nstart = 25)

#View the number of observations in each cluster

k$size

#View the centers of the clusters

k$centers

#Visualize the clusters

fviz_cluster(k, data = scaled_data, geom = "point", alpha = .4,
             main = "Cluster Plot Based on Exit Velocity Efficiency and Maximum Exit Velocity",
             xlab = "Scaled Exit Velocity Efficiency", ylab = " Scaled Maximum Exit Velocity") +
  labs(caption = "* k-means clustering needs to be scaled to account for variable magnitude*") +
  theme(plot.title = element_text(hjust = 0.5))


#See how other attributes vary across clusters

Leaderboard <- Leaderboard %>%
  mutate(cluster = k$cluster)

excel_table <- Leaderboard %>%
  select(cluster, MaxEV, well_hit_rate, wRCplus, wOBA, wOBACON, xwOBACON, BB_rate, K_rate, LA, EV) %>%
  group_by(cluster) %>%
  summarise_at(vars(MaxEV:EV), mean) %>%
  ungroup()

#Export table to Excel

write_xlsx(excel_table, "C:/Users/daily/Desktop/Repositories/EV-Efficiency-Cluster-Analysis/Cluster Results Table.csv")

#Filter to only group 5 players

Group_5 <- Leaderboard %>%
  filter(cluster == 5)

#Create a density plot for Group 5 players

Group_5 %>%
  ggplot(aes(x = wRCplus)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", bins = 15) +
  geom_density(fill = "#E76BF3", alpha = .4) +
  scale_x_continuous(breaks = seq(40, 200, 10)) +
  geom_vline(aes(xintercept = mean(wRCplus), color = "Mean"), linetype = "dashed", size = 1.25) +
  scale_color_manual(name = "wRC+", values = "black") +
  labs(title = "Density Plot of wRC+ for Group 5 Players", x = "wRC+", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5))

#Filter to only group 2 players

Group_2 <- Leaderboard %>%
  filter(cluster == 2)

#Create a density plot for Group 2 players

Group_2 %>%
  ggplot(aes(x = wRCplus)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", bins = 15) +
  geom_density(fill = "#A3A500", alpha = .4) +
  scale_x_continuous(breaks = seq(40, 200, 10)) +
  geom_vline(aes(xintercept = mean(wRCplus), color = "Mean"), linetype = "dashed", size = 1.25) +
  scale_color_manual(name = "wRC+", values = "black") +
  labs(title = "Density Plot of wRC+ for Group 2 Players", x = "wRC+", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5))

#Filter to only group 4 players

Group_4 <- Leaderboard %>%
  filter(cluster == 4)

#Create a density plot for Group 4 players

Group_4 %>%
  ggplot(aes(x = wRCplus)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", bins = 15) +
  geom_density(fill = "#00B0F6", alpha = .4) +
  scale_x_continuous(breaks = seq(40, 200, 10)) +
  geom_vline(aes(xintercept = mean(wRCplus), color = "Mean"), linetype = "dashed", size = 1.25) +
  scale_color_manual(name = "wRC+", values = "black") +
  labs(title = "Density Plot of wRC+ for Group 4 Players", x = "wRC+", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5))

#Filter to only group 1 players

Group_1 <- Leaderboard %>%
  filter(cluster == 1)

#Create a density plot for Group 1 players

Group_1 %>%
  ggplot(aes(x = wRCplus)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", bins = 15) +
  geom_density(fill = "#F8766D", alpha = .4) +
  scale_x_continuous(breaks = seq(40, 200, 10)) +
  geom_vline(aes(xintercept = mean(wRCplus), color = "Mean"), linetype = "dashed", size = 1.25) +
  scale_color_manual(name = "wRC+", values = "black") +
  labs(title = "Density Plot of wRC+ for Group 1 Players", x = "wRC+", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5))

#Filter to only group 3 players

Group_3 <- Leaderboard %>%
  filter(cluster == 3)

#Create a density plot for Group 3 players

Group_3 %>%
  ggplot(aes(x = wRCplus)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white", bins = 15) +
  geom_density(fill = "#00BF7D", alpha = .4) +
  scale_x_continuous(breaks = seq(40, 200, 10)) +
  geom_vline(aes(xintercept = mean(wRCplus), color = "Mean"), linetype = "dashed", size = 1.25) +
  scale_color_manual(name = "wRC+", values = "black") +
  labs(title = "Density Plot of wRC+ for Group 3 Players", x = "wRC+", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5))

#Create vectors for MiLB players and combine them into a data frame

Scaled_well_hit_rate <- (milb_test$Hard_Hit - mean(Leaderboard$well_hit_rate)) / sd(Leaderboard$well_hit_rate)

Scaled_MaxEV<- (milb_test$Max_EV - mean(Leaderboard$MaxEV)) / sd(Leaderboard$MaxEV)

prediction_df <- cbind(Scaled_well_hit_rate, Scaled_MaxEV)

#Predict which cluster the prospects belong to

milb_test <- milb_test %>%
  mutate(cluster = cl_predict(k, prediction_df))
