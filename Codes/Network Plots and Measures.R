# ==============================================================================
# Analyzing the robustness of Bus Rapid Transit Networks: The case of Bogotá
#
# Term paper for the course Networks I: Concepts and Algorithms
# DSDM - Program. Barcelona School of Economics
#
# Students: - Simón Caicedo
#           - Diego Eslava
#           - Philine Meyjohann
#           - Eszter Pazmandi
#
# Date: March 26, 2022
# ==============================================================================

#-----------------Set directory and load relevant libraries--------------------#
rm(list=ls())
setwd("~/GitHub/TermPaperNetworks")

library(igraph)
library(readxl)
library(tidyverse)
library(ggmap)
library(purrr)
library(ggplot2)
library(sp)
library(scales)
library(ggpubr)
library(grid)
library(ggsn)
library(stargazer)

#----------------------1. Read data and create graph---------------------------#
# Read data 
df<-read_xlsx("Data/Data Created/Data Final Transmilenio.xlsx")

df_network<-df[,c(4,5, 10)] %>% 
  rename(weight = number_routes)

# Define dataframe with nodes ID and longitude/latitude 
node_coords<-df[,c("stop_id", "stop_lat", "stop_lon")] %>% 
  rename(long = stop_lon, lat = stop_lat) %>% 
  distinct(stop_id, .keep_all = T)

# Create a Bounding box of Bogotá for plotting the network
node_bounds<-node_coords %>% 
  sp::SpatialPointsDataFrame(coords = .[,2:3],
                             data = .) %>%
  sp::bbox() * 1

# Create the network
network <- graph_from_data_frame(d=df_network, directed=T) 

# Assign coordinates to each node
el_coords <-
  as_edgelist(network) %>%
  as_tibble() %>%
  full_join(node_coords, by = c("V1" = "stop_id")) %>%
  full_join(node_coords, by = c("V2" = "stop_id")) %>%
  drop_na(V1, V2)

bog_bb <- c(left = node_bounds[2,1]-0.005,
            bottom = node_bounds[1,1]-0.005,
            right = node_bounds[2,2]+0.02,
            top = node_bounds[1,2]+0.02)

# Get map of Bogotá from Google Maps (needs API key)
bog_st <- get_map(location= bog_bb, maptype = "terrain", zoom=13)

# Plot map of Bogotá with the BRT network
base_gg <- ggmap(bog_st) + 
  geom_point(data=node_coords,
             aes(x=long, y=lat), 
             color="red3",
             size=2) +
  geom_segment(data = el_coords, 
               aes(x = long.x, xend = long.y,
                   y = lat.x, yend = lat.y), 
               size=0.25, 
               alpha=0.5) +
  scalebar(location="bottomleft", x.min = node_bounds[2,1], 
           x.max = node_bounds[2,2], y.min = node_bounds[1,1], 
           y.max = node_bounds[1,2],
           dist = 2, dist_unit = "km", st.size=3,
           st.bottom = FALSE, st.color = "black",
           transform = TRUE, model = "WGS84") +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(axis.text = element_blank())

north2(base_gg, x=0.3, y=0.9, scale=0.08, symbol = 3)

#---------------------2. Assign attributes to network--------------------------#
# Computing Manhattan distance (in kilometers) between each station (node)
manhattan_distances<-as.matrix(dist(node_coords[,c(2,3)], method="manhattan"))*111
colnames(manhattan_distances)<-node_coords$stop_id
rownames(manhattan_distances)<-node_coords$stop_id

# Adding calculated distances to each edge
df_network$distance<-NA
for(i in 1:nrow(df_network)){
  x<-as.character(df_network[i,1])
  y<-as.character(df_network[i,2])
  df_network$distance[i]<-manhattan_distances[x,y]
}

# Computing trip kilometers for robustness analysis
df_network$trip_km<-df_network$weight*df_network$distance

# Create the network again to take into account the distance attribute
network <- graph_from_data_frame(d=df_network, directed=T) 


#----------------3. Calculate topological profile measures---------------------#
# Average Degree
degree_nodes <- degree(network, mode="all", loops = F, normalized = F)

# Average wighted degree
wdegree_nodes <- strength(network, mode="all", loops = F, weights=E(network)$distance)

# Betweenness centrality - node
btw <- betweenness(network, directed=T, weights=E(network)$distance)

# Clustering
cluster <- transitivity(network, type="local")

# Average Path Length
path_length <- distances(network, mode = "all", weights = E(network)$distance)
avg_pl <- mean(path_length)


# Latex table of topological measures results
top_measures <- data.frame("degree" = degree_nodes, 
                           "weighted_degree" = wdegree_nodes,
                           "betweenness" = btw,
                           "clustering_coeff" = cluster)

colnames(top_measures)<-c("Degree", 
                          "Weighted Degree",
                          "Betweenness Centrality",
                          "Clustering Coefficient")

stargazer(top_measures, digits=2, nobs = F, title = "Topological measures")
# the average path length was added manually to the final table

#------------------4. Calculate robustness-static measures---------------------#

# Define types of edges for calculating measures (based on Abdelaty et al. (2020)
df_network$lm <- ifelse(df_network$weight>1, 1, 0)
Lt <- sum(table(df_network$lm))
Lm <- as.numeric(table(df_network$lm)[2])
Nt <- vcount(network)

# Robustness metrics
robustness_indicator<-(Lt-Nt-Lm+1)/Nt
robustness_metric<-log(Lt-Nt+2)/Nt

# Critical threshold
avg_degree<-mean(degree(network, mode="all", loops = F, normalized = F))
critical_threshold<-1-(1/((avg_degree^2/avg_degree)-1))

list(robustness_indicator, robustness_metric, critical_threshold)

# Latex table of robustness-static measures results
static_m <- t(matrix(c(robustness_indicator, robustness_metric, critical_threshold)))
colnames(static_m)<-c("Robustness Indicator", 
                      "Robustness Metric",
                      "Critical Threshold")
stargazer(static_m, title = "Static Robustness measures")

#------------------5. Calculate robustness-dynamic measures--------------------#
# Robustness Indicator
`%ni%` <- Negate(`%in%`)

# Function to compute the robustness metrics
robustness<-function(network, df_net, node_to_remove) {
  
  tot_weight<-sum(df_net$weight)
  tot_trip_km<-sum(df_net$trip_km)
  tot_dist<-sum(df_net$distance)
  
  df_net_filt<-df_net[df_net$previous_station %ni% node_to_remove & 
                        df_net$previous_station %ni% node_to_remove,]
  
  weight<-sum(df_net_filt$weight)/tot_weight
  trip_km<-sum(df_net_filt$trip_km)/tot_trip_km
  dist<-sum(df_net_filt$distance)/tot_dist
  
  return(list(weight, dist, trip_km))
}

# Vector with nodes to be removed iteratively (order by degree centrality)
nodes_degree<-names(sort(degree(network, mode="all", loops = F, 
                                normalized = F), decreasing=T))

# Vector with nodes to be removed iteratively (order by betweeness)
nodes_betweness<-names(sort(betweenness(network, directed=T, 
                                        weights=E(network)$distance), decreasing=T))


# Empty dataframe to store results
results_robustness<-as.data.frame(matrix(NA, nrow=10, ncol=7))
colnames(results_robustness)<-c("prop_removed_nodes", "robustness_index_w_deg", 
                                "robustness_index_d_deg", "robustness_index_t_km_deg",
                                "robustness_index_w_bet", "robustness_index_d_bet",
                                "robustness_index_t_km_bet")


# Remove iteratively 10% of nodes and compute metrics
for(i in 1:10){
  
  # Results degree
  results_degree<-robustness(network, df_network, nodes_degree[1:(round(length(nodes_degree)/(10)*i))])
  # Results betweeness
  results_betweeness<-robustness(network, df_network, nodes_betweness[1:(round(length(nodes_betweness)/(10)*i))])
  
  # Filling data frame with results
  results_robustness[i, 1]<-length(nodes_degree[1:(round(length(nodes_degree)/(10)*i))])/length(nodes_degree)
  results_robustness[i, 2]<-results_degree[[1]]
  results_robustness[i, 3]<-results_degree[[2]]
  results_robustness[i, 4]<-results_degree[[3]]
  results_robustness[i, 5]<-results_betweeness[[1]]
  results_robustness[i, 6]<-results_betweeness[[2]]
  results_robustness[i, 7]<-results_betweeness[[3]]
}


# Add Starting points
start_points<-matrix(c(0, rep(1,6)), nrow=1, ncol=7)
colnames(start_points)<-c("prop_removed_nodes", "robustness_index_w_deg", 
                          "robustness_index_d_deg", "robustness_index_t_km_deg",
                          "robustness_index_w_bet", "robustness_index_d_bet",
                          "robustness_index_t_km_bet")

results_robustness<-rbind(start_points, results_robustness)

# Plot for number of routes
p1<-ggplot(results_robustness)+
  geom_line(aes(x=prop_removed_nodes, y=robustness_index_w_deg, colour="Degree"), size=1.2)+
  geom_line(aes(x=prop_removed_nodes, y=robustness_index_w_bet, colour="Betweenness"), size=1.2)+
  geom_point(aes(x=prop_removed_nodes, y=robustness_index_w_deg),color="coral2", size=2)+
  geom_point(aes(x=prop_removed_nodes, y=robustness_index_w_bet), color="cornflowerblue", size=2)+
  xlab("Proportion of removed nodes")+
  ylab("Proportion of removed routes")+
  labs(colour="")+
  #ggtitle("Robustness indicators due to the removal of nodes",
  #        subtitle = "Removed in order by degree and betweenness importance")+
  theme_minimal()+
  theme(plot.title = element_text(face="bold", hjust = 0.5),
        plot.subtitle = element_text(face="italic", hjust = 0.5),
        axis.text = element_text(size=7),
        legend.position = c(0.85, 0.85))+
  scale_x_continuous(limits=c(0,1), n.breaks = 10, labels =  scales::percent_format(accuracy = 1))+
  scale_y_continuous(limits=c(0,1), n.breaks = 10, labels = scales::percent_format(accuracy = 1))+
  scale_colour_manual(values=c("cornflowerblue", "coral2"))


# Plot for distance
p2<-ggplot(results_robustness)+
  geom_line(aes(x=prop_removed_nodes, y=robustness_index_d_deg, colour="Degree"), size=1.2)+
  geom_line(aes(x=prop_removed_nodes, y=robustness_index_d_bet, colour="Betweenness"), size=1.2)+
  geom_point(aes(x=prop_removed_nodes, y=robustness_index_d_deg),color="coral2", size=2)+
  geom_point(aes(x=prop_removed_nodes, y=robustness_index_d_bet), color="cornflowerblue", size=2)+
  xlab("Proportion of removed nodes")+
  ylab("Proportion of removed travel distance")+
  labs(colour="")+
  #ggtitle("Robustness indicators due to the removal of nodes",
  #        subtitle = "Removed in order by degree and betweenness importance")+
  theme_minimal()+
  theme(plot.title = element_text(face="bold", hjust = 0.5),
        plot.subtitle = element_text(face="italic", hjust = 0.5),
        axis.text = element_text(size=7),
        legend.position = c(0.85, 0.85))+
  scale_x_continuous(limits=c(0,1), n.breaks = 10, labels =  scales::percent_format(accuracy = 1))+
  scale_y_continuous(limits=c(0,1), n.breaks = 10, labels = scales::percent_format(accuracy = 1))+
  scale_colour_manual(values=c("cornflowerblue", "coral2"))


p3<-ggplot(results_robustness)+
  geom_line(aes(x=prop_removed_nodes, y=robustness_index_t_km_deg, colour="Degree"), size=1.2)+
  geom_line(aes(x=prop_removed_nodes, y=robustness_index_t_km_bet, colour="Betweenness"), size=1.2)+
  geom_point(aes(x=prop_removed_nodes, y=robustness_index_t_km_deg),color="coral2", size=2)+
  geom_point(aes(x=prop_removed_nodes, y=robustness_index_t_km_bet), color="cornflowerblue", size=2)+
  xlab("Proportion of removed nodes")+
  ylab("Proportion of removed total trip km")+
  labs(colour="")+
  #ggtitle("Robustness indicator due to the removal of nodes",
  #        subtitle = "Removed in order by degree and betweenness importance")+
  theme_minimal()+
  theme(plot.title = element_text(face="bold", hjust = 0.5),
        plot.subtitle = element_text(face="italic", hjust = 0.5), 
        axis.text = element_text(size=7),
        legend.position = c(0.85, 0.85))+
  scale_x_continuous(limits=c(0,1), n.breaks = 10, labels =  scales::percent_format(accuracy = 1))+
  scale_y_continuous(limits=c(0,1), n.breaks = 10, labels = scales::percent_format(accuracy = 1))+
  scale_colour_manual(values=c("cornflowerblue", "coral2"))

all_plots<-ggarrange(p1, p2, p3, hjust = -0.5,
          labels = c("Number of trips", "Travel Distance (km)", "Total Trip Distance"),
          ncol = 3, nrow = 1, common.legend = T, legend = "bottom", 
          font.label = list(size = 12, face = "italic"))

all_plots



# annotate_figure(all_plots, 
#                 top = text_grob("Robustness after removal of nodes by degree/betweeness importance", 
#                                       color = "black", face = "bold", size = 14))






















# robustness<-function(network, df_net, node_to_remove){
#   
#   df_net_filt<-df_net[df_net$previous_station %ni% node_to_remove & 
#                    df_net$previous_station %ni% node_to_remove,]
#   
#   network_filtered <- graph_from_data_frame(d=df_net_filt, directed=T) 
#   
#   
#   df_net_filt$lm<-ifelse(df_net_filt$weight>1, 1, 0)
#   Lt<-sum(table(df_net_filt$lm))
#   Lm<-as.numeric(table(df_net_filt$lm)[2])
#   Nt<-vcount(network_filtered)
#   
#   # Robustness metrics
#   robustness_indicator<-(Lt-Nt-Lm+1)/Nt
#   robustness_metric<-log(Lt-Nt+2)/Nt
#   
#   # Critical threshold
#   avg_degree<-mean(degree(network_filtered, mode="all", loops = F, normalized = F))
#   
#   critical_threshold<-1-(1/((avg_degree^2/avg_degree)-1))
#   
#   return(list(robustness_indicator, robustness_metric, critical_threshold))
# }


# # Vector with nodes to be removed iteratively (order by degree centrality)
# nodes_degree<-names(sort(degree(network, mode="all", loops = F, normalized = F), decreasing=T))
# 
# # Vector with nodes to be removed iteratively (order by betweeness)
# nodes_to_removed<-names(sort(betweenness(network, directed=T, weights=E(network)$distance), decreasing=T))
# 
# 
# 
# 
# results_robustness<-as.data.frame(matrix(NA, nrow=length(nodes_to_removed), ncol=4))
# colnames(results_robustness)<-c("prop_removed_nodes", "robustness_indicator", "robustness_metric", "critical_threshold")
# 
# for(i in 1:length(nodes_to_removed)){
#   results<-robustness(network, df_network, nodes_to_removed[1:i])
#   results_robustness[i, 1]<-length(nodes_to_removed[1:i])/length(nodes_to_removed)
#   results_robustness[i, 2]<-results[[1]]
#   results_robustness[i, 3]<-results[[2]]
#   results_robustness[i, 4]<-results[[3]]
# }
# 
# 
# results_robustness$robustness_indicator[results_robustness$robustness_indicator<0]<-0
# results_robustness$robustness_indicator[is.na(results_robustness$robustness_indicator)]<-0
# results_robustness$robustness_metric[is.nan(results_robustness$robustness_metric)]<-0
# results_robustness$robustness_metric[is.infinite(results_robustness$robustness_metric)]<-0
# 
# results_robustness$critical_threshold[is.nan(results_robustness$critical_threshold)]<-0
# results_robustness$critical_threshold[is.infinite(results_robustness$critical_threshold)]<-0
# results_robustness$critical_threshold[results_robustness$critical_threshold<0]<-0
# 
# # Removing nodes by Betweeness ------
# 
# 
# # Vector with nodes to be removed iteratively (order by betweenesss)
# nodes_to_removed<-names(sort(betweenness(network, directed=T, weights=E(network)$distance), decreasing=T))
# results_robustness<-as.data.frame(matrix(NA, nrow=length(nodes_to_removed), ncol=4))
# colnames(results_robustness)<-c("prop_removed_nodes", "robustness_indicator", "robustness_metric", "critical_threshold")
# 
# for(i in 1:length(nodes_to_removed)){
#   results<-robustness(network, df_network, nodes_to_removed[1:i])
#   results_robustness[i, 1]<-length(nodes_to_removed[1:i])/length(nodes_to_removed)
#   results_robustness[i, 2]<-results[[1]]
#   results_robustness[i, 3]<-results[[2]]
#   results_robustness[i, 4]<-results[[3]]
# }
# 
# 
# results_robustness$robustness_indicator[results_robustness$robustness_indicator<0]<-0
# results_robustness$robustness_indicator[is.na(results_robustness$robustness_indicator)]<-0
# results_robustness$robustness_metric[is.nan(results_robustness$robustness_metric)]<-0
# results_robustness$robustness_metric[is.infinite(results_robustness$robustness_metric)]<-0
# 
# results_robustness$critical_threshold[is.nan(results_robustness$critical_threshold)]<-0
# results_robustness$critical_threshold[is.infinite(results_robustness$critical_threshold)]<-0
# results_robustness$critical_threshold[results_robustness$critical_threshold<0]<-0
# 
# 
# 
# ggplot(results_robustness)+
#   geom_line(aes(x=prop_removed_nodes, y=robustness_indicator), color="cornflowerblue", size=1.2)+
#   xlab("Proportion of Removed Nodes")+
#   ylab("Robustness Indicator")+
#   ggtitle("Robustness indicator due to the removal of nodes by betweeness order",
#           subtitle = "Transmilenio 2022")+
#   theme_minimal()+
#   theme(plot.title = element_text(face="bold", hjust = 0.5),
#         plot.subtitle = element_text(face="italic", hjust = 0.5))+
#   scale_x_continuous(n.breaks = 10)
# 
# 
# # Transform to zero when negative or not defined 
# 
# 
# 
# 
# robustness<-function(network, df_net, node_to_remove){
#   
#   tot_weight<-sum(df_net$weight)
#   tot_trip_km<-sum(df_net$trip_km)
#   tot_dist<-sum(df_net$distance)
#   
#   df_net_filt<-df_net[df_net$previous_station %ni% node_to_remove & 
#                         df_net$previous_station %ni% node_to_remove,]
#   
#   
#   weight<-sum(df_net_filt$weight)/tot_weight
#   trip_km<-sum(df_net_filt$trip_km)/tot_trip_km
#   dist<-sum(df_net_filt$distance)/tot_dist
#   
#   return(list(weight, dist, trip_km))
# }
# 
