rm(list=ls())

setwd("/Users/deslava/Documents/GitHub/TermPaperNetworks")

library(igraph)
library(readxl)
library(tidyverse)
library(ggmap)
library(purrr)
library(ggplot2)
library(sp)

# Read data 
df<-read_xlsx("Data/Data Created/Data Final Transmilenio.xlsx")

df_network<-df[,c(4,5, 10)] %>% 
  rename(weight = number_routes)

# Data with nodes and longitude/latitude 
node_coords<-df[,c("stop_id", "stop_lat", "stop_lon")] %>% 
  rename(long = stop_lon, lat = stop_lat) %>% 
  distinct(stop_id, .keep_all = T)

# Bounding box of Bogotá
node_bounds<-node_coords %>% 
  sp::SpatialPointsDataFrame(coords = .[,2:3],
                             data = .) %>%
  sp::bbox() * 1

# Create network
network <- graph_from_data_frame(d=df_network, directed=T) 

# Coordinates of each point
el_coords <-
  as_edgelist(network) %>%
  as_tibble() %>%
  full_join(node_coords, by = c("V1" = "stop_id")) %>%
  full_join(node_coords, by = c("V2" = "stop_id")) %>%
  drop_na(V1, V2)

bog_bb <- c(left = node_bounds[2,1]-0.01,
            bottom = node_bounds[1,1]-0.01,
            right = node_bounds[2,2]+0.01,
            top = node_bounds[1,2]+0.01)

bog_st <- get_map(location= bog_bb, maptype = "terrain", zoom=13)

# Map with transportation network
base_gg <- ggmap(bog_st) + 
  geom_point(data=node_coords,
             aes(x=long, y=lat), color="red")+
  geom_segment(data = el_coords, 
               aes(x = long.x, xend = long.y,
                   y = lat.x, yend = lat.y), 
               size=0.25, 
               alpha=0.5)+
  theme_minimal()

base_gg


#----------Computing Manhattan distance between each station (node)------------#
# Creating manhattan distance matrix 
manhattan_distances<-as.matrix(dist(node_coords[,c(2,3)], method="manhattan"))*111
colnames(manhattan_distances)<-node_coords$stop_id
rownames(manhattan_distances)<-node_coords$stop_id

# Adding distances between any adge
df_network$distance<-NA
for(i in 1:nrow(df_network)){

  x<-as.character(df_network[i,1])
    
  y<-as.character(df_network[i,2])
 
  df_network$distance[i]<-manhattan_distances[x,y]
}

#--------------------------Topological Profile---------------------------------#
# Average Degree
degree_nodes<-degree(network, mode="all", loops = F, normalized = F)
mean(degree_nodes)

# Betweenness centrality - node
btw <- betweenness(network, directed=T, weights=E(network)$distance)
mean(btw)

# Clustering
transitivity(network, type="global")

#Average Path Length
path_length <- distances(network, mode = "all", weights = E(network)$distance)
mean(path_length)


#--------------------------Robustness measures---------------------------------#
# Robustness Indicator

# Robustness Metric

# Critical Threshold

# # Now that we have the distance, let's define the network again
# network <- graph_from_data_frame(d=df_network, directed=T) 
# 
# # Average Degree
# degree_nodes<-degree(network, mode="all", loops = F, normalized = F)
# mean(degree_nodes)
# 
# # Edge Betweenness
# edge_betweenness(network, weights= E(network)$distance)
# 
# # Node Betweenness
# betw<-betweenness(network, weights= E(network)$distance)
# 
# # Mean betweeness
# mean(betw)


