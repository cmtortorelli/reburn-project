#### Description ####

# Adapted from Alison Paulson: https://github.com/akpaulson/2020_Fires_GEB/blob/main/Code/03_extract_gridMET.R

# This code will extract gridMET weather data for each point/pixel within a fire based on the day it burned
# gridMET data were downloaded from http://www.climatologylab.org/gridmet.html


#### Packages  & set up  ####

library(ncdf4)
library(terra)
#library(chron) #to deal with dates in netCDF
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(furrr)

#### Set Geospatial Data Directory (wherever you saved raw data downloaded from Box): ####
# data_dir = readLines(here("data_dir.txt"), n=1)
# source(here("scripts/convenience_functions.R"))

setwd("D:\\UCDavis\\Reburn-project\\reburn-project\\scripts\\prep_gridmet_weather_data_for_percentiles")

#### Make a grid of the focal area to extract to ####

#read in sample points
points = st_read("D:\\UCDavis\\Reburn-project\\reburn-project\\data\\prelim-outputs\\points\\data_sub_1-12-23.gpkg") 


#bring in .nc weather files as raster stack
erc_list <- list.files(path = "gridmet/erc", pattern='.nc$', all.files=TRUE, full.names=TRUE)
erc_all = terra::rast(erc_list)

vpd_list <- list.files(path = "gridmet/vpd", pattern='.nc$', all.files=TRUE, full.names=TRUE)
vpd_all = terra::rast(vpd_list)

vs_list <- list.files(path = "gridmet/vs", pattern='.nc$', all.files=TRUE, full.names=TRUE)
vs_all = terra::rast(vs_list)

#### extract weather variables to points

#erc
erc_extract <- terra::extract(erc_all, points |> st_transform(st_crs(erc_all[[1]])), method = "bilinear") #"bilinear" the returned values are interpolated from the values of the four nearest raster cells
erc_extract <- erc_extract[,-1] #remove row ids column
erc_extract_t <- t(erc_extract) |> data.frame()

#vpd
vpd_extract <- terra::extract(vpd_all, points |> st_transform(st_crs(vpd_all[[1]])), method = "bilinear") #"bilinear" the returned values are interpolated from the values of the four nearest raster cells
vpd_extract <- vpd_extract[,-1] #remove row ids column
vpd_extract_t <- t(vpd_extract) |> data.frame()

#vs
vs_extract <- terra::extract(vs_all, points |> st_transform(st_crs(vs_all[[1]])), method = "bilinear") #"bilinear" the returned values are interpolated from the values of the four nearest raster cells
vs_extract <- vs_extract[,-1] #remove row ids column
vs_extract_t <- t(vs_extract) |> data.frame()

#### extract day burning weather data to new df
dob <- points[,c("pointid", "erc_02_21", "vpd_02_21", "vs_02_21")] |> data.frame()

### calcualte percentile of day of burning weather data
#erc
dob$erc_perc <- NA 

for (i in 1:nrow(dob)) {
  percentile <- ecdf(erc_extract_t[,i])
  dob$erc_perc[i] <- percentile(dob$erc_02_21[i])
}

#vpd
dob$vpd_perc <- NA 

for (i in 1:nrow(dob)) {
  percentile <- ecdf(vpd_extract_t[,i])
  dob$vpd_perc[i] <- percentile(dob$vpd_02_21[i])
}

#vs
dob$vs_perc <- NA 

for (i in 1:nrow(dob)) {
  percentile <- ecdf(vs_extract_t[,i])
  dob$vs_perc[i] <- percentile(dob$vs_02_21[i])
}

#write points for analysis
st_write(dob, "D:\\UCDavis\\Reburn-project\\reburn-project\\data\\prelim-outputs\\points\\dob_percentiles_1-12-23.gpkg") 
