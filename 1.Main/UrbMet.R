####################################################################################################################################################################
################################################################# Urban metrics ####################################################################################
####################################################################################################################################################################

ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
  
}
# Load libraries
ipak(c("sp","sf","raster","tidyverse","rgdal", "ggplot2","viridis",
       "ggdark","magrittr", "leaflet","RColorBrewer","osmdata","r5r","geobr","here","osmextract",
       "classInt","LandCoverEntropy","ggplotgui","ggpubr","ggthemes","ggridges", "ggpomological", "data.table", "dplyr", "tmap"))

###################################################### building density metric ########################################################################################


#retrieve bounding box for region of interest
iz_bbox <- getbb("Würzburg", format_out = "polygon")

#retrieve level 8 administrative boundaries 
iz_boundary <- opq(iz_bbox) %>%
  add_osm_feature(key = "admin_level", value = "8") %>%
  osmdata_sf()

iz_boundary

#select only df multipolygons
iz_polys <- iz_boundary$osm_multipolygons

#remove digits from any distirct name 
iz_polys$name <- gsub('[[:digit:]]+', '', iz_polys$name)

#remove . from any district name
iz_polys$name <- gsub("[.]", '', iz_polys$name)

#trim whitespace
iz_polys$name  <- trimws(iz_polys$name, "both")

#factorize 
iz_polys$name <- as.factor(iz_polys$name)

#calculate polygon areas for later analysis and append to new column
iz_polys$poly_area <- st_area(iz_polys)

#remove original osmdata object
rm(iz_boundary)

#visulally check boundarys
ggplot(iz_polys) +
  geom_sf()

#retrieve buildings
iz_buildings <- opq(iz_bbox) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

build_polys <- iz_buildings$osm_polygons
rm(iz_buildings)

#drop unecessary columns
build_polys <- build_polys %>%
  select(osm_id, geometry)

#check buildings
ggplot(iz_polys) +
  geom_sf() +
  geom_sf(data = build_polys)

#calculate surface area of buildings
build_polys$area <- sf::st_area(build_polys)

#calculate centroids
build_cents <- sf::st_centroid(build_polys)

#create a shape object out of original bounding polygon
iz_bbox_geom <- 
  sfheaders::sf_polygon(as.data.frame(iz_bbox),
                        x="V1",
                        y="V2"
  )
#make sure that the projection matches the other data
st_crs(iz_bbox_geom) <- 4326

#plot
ggplot(iz_polys) +
  geom_sf() +
  geom_sf(data=iz_bbox_geom, col="red")

#filtering join with a points df, polygon df
clipped <- st_join(build_cents, iz_bbox_geom, join = st_within)

clipped %>%
  filter(id == 1) %>%
  ggplot() +
  geom_sf() +
  geom_sf(data=iz_bbox_geom, color = 'red', fill = NA)


#filtering the right points and then create a new dataframe from the iz_polys dataframe but with the filtered points.

clipped <- clipped %>%
clipped <- clipped %>%
  filter(id == 1)
joined <- st_join(clipped, iz_polys)


#aggregating and summing total building area
density_calc <- aggregate(joined$area, list(joined$osm_id.y),
                          FUN = sum)
#rename columns
colnames(density_calc) <- c("osm_id", "area")

#create final df that contains district polygons and building area
bounds_blds_sf <- merge(iz_polys, density_calc) 

#calculate building density
bounds_blds_sf <- bounds_blds_sf %>%
  mutate(bounds_blds_sf, b_dens = area/poly_area * 100)

tmap_mode('view')

tm_basemap("Stamen.TonerLite") +
  tm_shape(bounds_blds_sf) +
  tm_polygons(col="b_dens",
              id="name",
              title= "Building Density as % of Land Area",
              alpha=.8) 




