rm(list = ls())

library(lubridate)
library(raster)
library(M3)
library(readxl)
library(rgdal)
library(RCurl)
library(lattice)
library(rasterVis)
library(ggplot2)
library(jsonlite)

latlong    <- "+init=epsg:4326"
ukgrid     <- "+init=epsg:27700"
google     <- "+init=epsg:3857"
cmaqurban  <- "+proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +a=6370000 +b=6370000"
cmaq       <- "+proj=lcc +lat_1=46 +lat_2=46 +lat_0=46 +lon_0=17 +a=6370000 +b=6370000"

setwd("/home/james/Desktop")

## want to make annual average weekday, 9am to 10am for August and September

## Get list of the data
days_needed         <- data.frame(days = seq.POSIXt(as.POSIXct('2016-08-01'), as.POSIXct('2016-09-30'), 'days'))

days_needed$weekday <- weekdays(days_needed$days)

days_needed         <- days_needed[!days_needed$weekday %in% c('Saturday', 'Sunday'),]

days_needed$file    <-  paste0("001.cmaqurban.",
                              year(days_needed$days), format(days_needed$days, '%m'), format(days_needed$days, '%d'),
                              ".1.ERG20m.NO2.CONC.ncf")

for (i in 1:nrow(days_needed)) {

  print(paste0("Getting day ",  days_needed$days[i]))
  
  system(paste0("sshpass -p '***' scp james@10.0.4.226:/mnt/C3filestore/cope/COPE_VBS_2016_scem/cmaqurban/*/",
               days_needed$file[i],
               " ."))
  
  print("Got it")
  
  ncf_no2_file <-  days_needed$file[i]
  
  cmaq_no2_data <- brick(ncf_no2_file, var = "NO2", values=T)
  
  print('Sorting out the parameters')
  
  xmin(cmaq_no2_data)          <- get.grid.info.M3(ncf_no2_file)$x.orig
  ymin(cmaq_no2_data)          <- get.grid.info.M3(ncf_no2_file)$y.orig
  ymax(cmaq_no2_data)          <- get.grid.info.M3(ncf_no2_file)$y.orig + 
    get.grid.info.M3(ncf_no2_file)$y.cell.width * 
    get.grid.info.M3(ncf_no2_file)$nrows
  xmax(cmaq_no2_data)          <- get.grid.info.M3(ncf_no2_file)$x.orig + 
    get.grid.info.M3(ncf_no2_file)$x.cell.width * 
    get.grid.info.M3(ncf_no2_file)$ncols
  
  proj4string(cmaq_no2_data)  <- CRS(get.proj.info.M3(ncf_no2_file))
  
  res(cmaq_no2_data)          <- 20 # (It's a 20m by 20m grid file)
  
  print('Getting the 9am to 10am data')
  
  cmaq_no2_data               <- cmaq_no2_data$X10
  
  print('Adding it to the previous one')
  
  if (i == 1) {
    aug_sept_layer            <- cmaq_no2_data
  } else {
    aug_sept_layer            <- aug_sept_layer + cmaq_no2_data
    
    print('Thats this one done. Going to delete it now.')
    
    system(paste0("rm ", days_needed$file[i]))
  }
  
  rm(cmaq_no2_data)
  
}

system(paste0("rm ", days_needed$file[1]))

aug_sept_layer       <- aug_sept_layer / nrow(days_needed)

rm(ncf_no2_file, i)

writeRaster(aug_sept_layer, overwrite=T, '/home/james/mounts/James/PhD/11 - Evaluation Chapter/pollutant_files/no2_london_aug_sep.tif', format = 'GTiff')
writeRaster(my_area_no2, '/home/james/mounts/James/PhD/11 - Evaluation Chapter/pollutant_files/no2_cycling_area_aug_sep.tif', format = 'GTiff')