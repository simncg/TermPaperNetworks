# Term Paper Networks

**Group:**

- Eszter Pazmandi
- Diego Eslava
- Philine Meyjohann
- Simón Caicedo

### Structure repository

- In the folder `Codes` you can find all the codes necessary for preprocessing the data
- In the folder `Data` there are two folders: 
   1. `Raw Data`: Data non-cleaned and exactly equal as the ones retrieved from https://datosabiertos-transmilenio.hub.arcgis.com/.
   2. `Created Data`: Processed data.


## Data 

The final data (*Data Final Transmilenio.xlsx*) contains 15 variables: 

- `trip_id`: Route ID
- `stop_id`: Station ID
- `stop_sequence`: Stop Sequence for each route
- `current_station`:  Current station of the route
- `previous_station`: Previous station with respect to `current_station`
- `station`: (code station) name station
- `mean_passangers_enter`: Daily Average number of passangers entering the station
- `mean_passangers_leave`: Daily Average number of passangers leaving the station
- `edge`: Edge constructed using Previous Station with Current Station
- `number_routes`: Number of routes (`trip_id`) doing that specific edge.
- `number_buses_trip`: Number of daily buses doing the route (`trip_id`)
- `number_buses_route`: Number of daily buses doing that specific edge 
- `stop_name`: Station Name
- `stop_lat`: Station Latitude
- `stop_lon`: Station Longitude
