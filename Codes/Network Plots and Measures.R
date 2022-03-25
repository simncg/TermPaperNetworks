
setwd("G:/Mi unidad/Classroom/21D009 Networks  Concepts and Algorithms 21-22 DS T2/Paper")

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
  sp::bbox() * 1.1



# Create network
network <- graph_from_data_frame(d=df_network, directed=T) 


# Coordinates of each point
el_coords <-
  as_edgelist(network) %>%
  as_tibble() %>%
  full_join(node_coords, by = c("V1" = "stop_id")) %>%
  full_join(node_coords, by = c("V2" = "stop_id")) %>%
  drop_na(V1, V2)


# Same map as https://datosabiertos-transmilenio.hub.arcgis.com/datasets/5365d814bbdd4062a59234eea7d70db7/explore?location=4.661887%2C-74.108975%2C11.86

# Map with transportation network
base_gg<-ggplot()+
  geom_point(data=node_coords,
             aes(x=long, y=lat), color="red")+
  geom_segment(data = el_coords, 
               aes(x = long.x, xend = long.y,
                   y = lat.x, yend = lat.y), 
               size=0.25, 
               alpha=0.5)+
  theme_minimal()


base_gg


# Computing Manhattan distance between each station (node) 

# Creating manhattan distance matrix 
manhattan_distances<-as.matrix(dist(node_coords[,c(2,3)], method="manhattan"))
colnames(manhattan_distances)<-node_coords$stop_id
rownames(manhattan_distances)<-node_coords$stop_id

# Adding distances between any adge
df_network$distance<-NA
for(i in 1:nrow(df_network)){

  # if(str_starts(as.character(df_network[i,1]), "0")){
  #   x<-sub('.', '', df_network[i,1])
  #   
  # } else{
    x<-as.character(df_network[i,1])
    
  # }
  # if(str_starts(as.character(df_network[i,2]), "0")){
  #  y<-sub('.', '', df_network[i,2])
    
  #} else{
    y<-as.character(df_network[i,2])
  #}
  
  # print("New pair")
  # print(x)
  # print(y)
  # print(paste("Row", i))
  # 
  
  df_network$distance[i]<-manhattan_distances[x,y]
}


# Now that we have the distance, let's define the network again
network <- graph_from_data_frame(d=df_network, directed=T) 

# Average Degree
degree_nodes<-degree(network, mode="all", loops = F, normalized = F)
mean(degree_nodes)

# Edge Betweeness
edge_betweenness(network, weights= E(network)$distance)

# Node Betweeness
betw<-betweenness(network, weights= E(network)$distance)

# Mean betweeness
mean(betw)

