#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Initial AGU Analysis
#Coder: Lexie Thornton (lvthornton@crimson.ua.edu)
#Purpose: AGU, First Chapter, and BSC Final graphics
#Date: November 17th, 2023
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup workspace ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear memory
remove(list = ls())

#load packages
library(tidyverse)
library(raster)
library(sf)
library(elevatr)
library(mapview)
library(dygraphs)
library(xts)

#defining watershed outline for Site 62
pnts <- read_csv("data/data.csv") %>% 
  filter(Article!=79) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  mutate(
    Longitude = st_coordinates(.)[,1],
    Latitude = st_coordinates(.)[,2])

#download coastline
beach <- st_read("data/Gulf_Of_Mexico.shp") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Create function to pull elevation profile --------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fun <- function(x,y,beach, dem=NA){
  
  #Create point
  pnt <- data.frame(x = x, y = y)
  
  if(dem=NA){
    #download dem
    dem <- get_elev_raster(locations = pnt, prj = st_crs('+proj=longlat +datum=WGS84 +no_defs') , z=14)
  }  
  
  #Convert point to spatial point
  pnt <- sf::st_as_sf(pnt, coords = c("x", "y"), crs = '+proj=longlat +datum=WGS84 +no_defs')
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 3: Create transect ------------------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3.1 Reproject into planar coordinates ----------------------------------------
  #Define utm zone
  utm_zone <- floor((st_coordinates(pnt)[1] + 180)/6)+1
  
  #Define CRS
  crs <- paste0("+proj=utm +zone=",utm_zone," +datum=WGS84 +units=m +no_defs")
  
  #reproject raster datasets of interest
  dem <- projectRaster(dem,crs = crs)
  beach <- st_transform(beach, crs = st_crs(crs))
  pnt <- st_transform(pnt, crs = st_crs(crs))
  
  #3.2 Create Transect Line ------------------------------------------------------
  #Find point along beach ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Clip beach to dem extent
  beach_clip <- st_crop(beach, st_bbox(dem))
  
  #Convert beach to points
  beach_pnts <- st_cast(beach_clip, "POINT")
  
  #Find distances to points
  beach_pnts$dist <- st_distance(beach_pnts, pnt)
  
  #Find point along beach closest to point
  beach_pnt <- beach_pnts %>% dplyr::filter(dist == min(dist, na.rm=T)) %>% st_geometry()
  
  #Find point 1000 m distance from point ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Create points around sampling point buffer
  buffer_pnts <- st_buffer(pnt, 1000) %>% st_cast("POINT")
  
  #identify point furthest from beach_point
  buffer_pnts$dist <- st_distance(buffer_pnts, beach_pnt)
  
  #Find point furthest from beach point
  inland_pnt <- buffer_pnts %>% dplyr::filter(dist == max(dist, na.rm=T)) %>% st_geometry()
  
  #Create transect with beach and inland point ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Create a line using beach_pnt and pnt
  transect <- rbind(st_coordinates(beach_pnt), st_coordinates(inland_pnt)) %>% 
    as_tibble() %>% 
    st_as_sf(coords = c("X","Y")) %>% 
    st_coordinates() %>% 
    st_linestring() %>% 
    st_sfc(.) %>% 
    st_set_crs(st_crs(crs)) %>% 
    st_as_sf() 
  
  #3.3 Create dataframe of elevations and distance along transect ----------------
  #Define distance interval between points
  interval <- st_length(transect) %>% paste %>% as.numeric() %>% floor()
  
  #convert transect to points
  transect_pnts <- st_sample(transect, interval, type="regular") %>% st_cast("POINT") %>% st_as_sf()
  
  #add add x,y, and z values to line
  transect_pnts <- transect_pnts %>% 
    rename(geometry = x) %>% 
    mutate(
      x = st_coordinates(.)[,1],
      y = st_coordinates(.)[,2], 
      ele = raster::extract(dem, .))
  
  #Estimate distance along line
  transect_pnts <- transect_pnts %>% 
    #Estimate distance along transect using distance formula
    mutate(dist = ((x[1]-x)^2 + (y[1]-y)^2)^0.5) 
  
  transect_pnts
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Apply function to points -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.6 Row 6! --------------------------------------------------------------------
#Step 1: Define points
n<-6
x = pnts$Longitude[n]
y = pnts$Latitude[n]

#Step 2: Define beach
mapview(pnts[n,]) + mapview(beach)
beach_subset <- beach %>% filter(ATTRIBUTE=="Natural.Mean High Water")

#Step 3: Run function
output <- fun(x,y,beach_subset)

