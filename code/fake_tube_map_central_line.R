rm(list=ls())

## Load libraries
library("RPostgreSQL")
library("ggplot2")
library("scales")
library("reshape")
library("grid")
library("devtools")
library("openair")
library("gdata")

setwd("/home/james/mounts/James/PhD/10 - Tube Monitoring Chapter/Results")

## Connect to PostgreSQL database where data is stored
drv = dbDriver("PostgreSQL")
con = dbConnect(drv, dbname="tube_monitoring", user="james", password="", host="10.0.4.240")

## Get daily background averages for London from OpenAir. for the days we did monitoring on.
background_pm25 <- importKCL(site = "kc1", year = c(2014,2015,2016), pollutant = "pm25", met = FALSE,
                             units = "mass", extra = FALSE)
background_pm25 <- data.frame(background_pm25, day = as.Date(format(background_pm25$date)))
background_pm25 <- aggregate(pm25 ~ day, background_pm25, mean)

## Extract the data from database. Note the CASE statement below. it's because want the raw PM25 data to do our own scaling, but the scaled version of other pollutants.
tube_data <- dbGetQuery(con, "
                        WITH  station_depths AS (
                        SELECT  	station_depths_import.station_name,
                        station_depths_import.line,
                        station_depths_import.platform_depth,
                        station_info.shortname,
                        station_info.the_geom
                        FROM		station_depths_import
                        LEFT JOIN	(	SELECT		shortname,
                        line,
                        the_geom
                        FROM		station_geom_depth
                        GROUP BY 	shortname,
                        line,
                        the_geom) AS station_info
                        ON		station_depths_import.station_name 	= station_info.shortname
                        AND		station_depths_import.line		= station_info.line
                        )
                        SELECT		tube_pollution_mapping.species,
                        tube_pollution_mapping.environment,
                        tube_pollution_mapping.date_time,
                        CASE WHEN tube_pollution_mapping.species = 'PM25' THEN tube_pollution_mapping.concentration
                        ELSE tube_pollution_mapping.scaled_concentration
                        END AS concentration,
                        tube_pollution_mapping.tube_diary_stop,
                        tube_pollution_mapping.line,
                        station_depths.platform_depth
                        FROM		tube_pollution_mapping
                        LEFT JOIN	station_depths
                        ON		tube_pollution_mapping.tube_diary_stop 	= 	station_depths.station_name
                        AND		tube_pollution_mapping.line		=	station_depths.line
                        ORDER BY	tube_pollution_mapping.date_time,
                        tube_pollution_mapping.environment,
                        tube_pollution_mapping.species
                        ")

## Link the tube data to the background air quality data
tube_data <- data.frame(tube_data, day = as.Date(format(tube_data$date_time)))
tube_data <- merge(tube_data, background_pm25, by="day", all.x=TRUE)

# Find that concentration for 1 Feb 2016 is missing from the London Air background PM2.5 data. Need some numbers for that. So going to use the data from 8 Feb 2016 instead. It's the same day of the week, the week after.
tube_data[is.na(tube_data$pm25),]$pm25 <- background_pm25[background_pm25$day == '2016-02-08',]$pm25

# Now do the correction process. Create new field to put the data in.
tube_data$corrected_concentration <- as.numeric('')

## Now adjust the data. Using the background concentration data, adjust the tube concentration data using the regression formula determined by Dave Green and Ben Barratt
for (i in 1:nrow(tube_data)) {
  if (tube_data[i,]$concentration > tube_data[i,]$pm25/0.44 & tube_data[i,]$species == 'PM25') {
    tube_data[i,]$corrected_concentration <- (tube_data[i,]$pm25 + (tube_data[i,]$concentration - tube_data[i,]$pm25/0.44) * 1.82)
  } else
  {
    if (tube_data[i,]$concentration <= tube_data[i,]$pm25/0.44 & tube_data[i,]$species == 'PM25') {
      tube_data[i,]$corrected_concentration <-tube_data[i,]$concentration * 0.44
    } else
    {}
  }
}


tube_data$corrected_concentration[is.na(tube_data$corrected_concentration)] <- tube_data$concentration[is.na(tube_data$corrected_concentration)]

## Make a data frame of tube lines and colours for plotting
colours_lines <- data.frame(line = c("Victoria","Piccadilly","Northern","Circle","Jubilee","District","Bakerloo","Metropolitan","Docklands Light Railway","Central","Hammersmith & City"),
                            colour = c("#0099CC","#000099","#000000","#FFCC00","#868F98","#006633","#996633","#660066","#009999","#CC3333", "#CC9999"),
                            stringsAsFactors = FALSE)

## Clean up some data don't need anymore
rm(background_pm25, con, drv, i)

## Aggregate the data that we're going to need so it's suitable for plotting
map_plot <- aggregate(corrected_concentration ~ tube_diary_stop + line + platform_depth, tube_data[tube_data$species == 'PM25' & tube_data$line == 'Central' & tube_data$environment == 'CAR',], mean)

## Rename the variables
names(map_plot)[names(map_plot) == 'tube_diary_stop'] <- 'station'
names(map_plot)[names(map_plot) == 'platform_depth'] <- 'depth'
names(map_plot)[names(map_plot) == 'corrected_concentration'] <- 'pm25'

## We don't want to use the lat and long to plot, as we want to create a Beck style map of the tube. So we are going to give the stations pseudo coordinates to plot it how we like.
map_plot$fake_x <- NA
map_plot$fake_y <- NA

## Having planned this out on graph paper, the coordinates for the stations are now entered
map_plot[map_plot$line == 'Central' & map_plot$station == 'West Ruislip',c("fake_x", "fake_y")]            <- c(1,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Ruislip Gardens',c("fake_x", "fake_y")]         <- c(2,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'South Ruislip',c("fake_x", "fake_y")]           <- c(3,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Northolt',c("fake_x", "fake_y")]                <- c(4,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Greenford',c("fake_x", "fake_y")]               <- c(5,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Perivale',c("fake_x", "fake_y")]                <- c(6,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Hanger Lane',c("fake_x", "fake_y")]             <- c(7,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'North Acton',"fake_x"]                          <- 8
map_plot[map_plot$line == 'Central' & map_plot$station == 'North Acton',"fake_y"]                          <- 2
map_plot[map_plot$line == 'Central' & map_plot$station == 'East Acton',c("fake_x", "fake_y")]              <- c(9,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'White City',c("fake_x", "fake_y")]              <- c(10,2)
map_plot[map_plot$line == 'Central' & map_plot$station == "Shepherd's Bush",c("fake_x", "fake_y")]         <- c(11,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Holland Park',c("fake_x", "fake_y")]            <- c(12,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Notting Hill Gate',c("fake_x", "fake_y")]       <- c(13,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Queensway',c("fake_x", "fake_y")]               <- c(14,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Lancaster Gate',c("fake_x", "fake_y")]          <- c(15,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Marble Arch',c("fake_x", "fake_y")]             <- c(16,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Bond Street',c("fake_x", "fake_y")]             <- c(17,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Oxford Circus',c("fake_x", "fake_y")]           <- c(18,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Tottenham Court Road',c("fake_x", "fake_y")]    <- c(19,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Holborn',c("fake_x", "fake_y")]                 <- c(20,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Chancery Lane',c("fake_x", "fake_y")]           <- c(21,2)
map_plot[map_plot$line == 'Central' & map_plot$station == "St. Paul's", c("fake_x", "fake_y")]             <- c(22,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Bank',c("fake_x", "fake_y")]                    <- c(23,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Liverpool Street',c("fake_x", "fake_y")]        <- c(24,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Bethnal Green',c("fake_x", "fake_y")]           <- c(25,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Mile End',c("fake_x", "fake_y")]                <- c(26,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Stratford',c("fake_x", "fake_y")]               <- c(27,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Leyton',c("fake_x", "fake_y")]                  <- c(28,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Leytonstone',"fake_x"]                          <- 29
map_plot[map_plot$line == 'Central' & map_plot$station == 'Leytonstone',"fake_y"]                          <- 2
map_plot[map_plot$line == 'Central' & map_plot$station == 'Wanstead',c("fake_x", "fake_y")]                <- c(30,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Redbridge',c("fake_x", "fake_y")]               <- c(31,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Gants Hill',c("fake_x", "fake_y")]              <- c(32,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Newbury Park',c("fake_x", "fake_y")]            <- c(33,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Barkingside',c("fake_x", "fake_y")]             <- c(34,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Fairlop',c("fake_x", "fake_y")]                 <- c(35,2)
map_plot[map_plot$line == 'Central' & map_plot$station == 'Hainault',c("fake_x", "fake_y")]                <- c(36,2)
#map_plot[map_plot$line == 'Central' & map_plot$station == 'West Acton',c("fake_x", "fake_y")]              <- c(6,1)
#map_plot[map_plot$line == 'Central' & map_plot$station == 'Ealing Broadway',c("fake_x", "fake_y")]         <- c(5,1)

## Now categorise the continuous depth data for plotting as different colours.

map_plot$depth_categorised <- NA
map_plot[map_plot$depth < 0,]$depth_categorised  <- 'above ground (>0m)' 
map_plot[map_plot$depth < 10 & map_plot$depth > 0,]$depth_categorised  <- 'shallow (0-10m)' 
map_plot[map_plot$depth < 20 & map_plot$depth > 10,]$depth_categorised  <- 'medium (10-20m)' 
map_plot[map_plot$depth > 20,]$depth_categorised  <- 'deep (>20m)' 

## Make into factors for plotting
map_plot$depth_categorised <- as.factor(map_plot$depth_categorised)
map_plot$depth_categorised <- factor(map_plot$depth_categorised, levels = c("above ground (>0m)", "shallow (0-10m)",
                                                                            "medium (10-20m)", "deep (>20m)"))

## Rescale the PM2.5 to be between 0 and 1 so that we can visualise it better
map_plot[map_plot$line == 'Central',"scaled_pm25"] <- rescale(map_plot[map_plot$line == 'Central',]$pm25, to=c(0,1))

## Make the plot and save as a PNG
png("fake_central_line_pm25_no_spur.png", width =1000, height = 400, units="px")
ggplot(map_plot[map_plot$line == 'Central',], aes(fake_x, fake_y, label = station, colour = depth_categorised)) +
      geom_segment(data = map_plot[map_plot$line == 'Central',], aes(x = fake_x, y = fake_y,
                                                                     xend = fake_x, yend = fake_y +scaled_pm25),
                   colour = "black", size = 3, lineend="round", alpha=0.4) +
      geom_line(data = data.frame(fake_x = c(1,36), fake_y = 2, station = NA, depth_categorised = NA),
                aes(x = fake_x1=,enorsuv, y = fake_y), size = 2, colour = "red") +
     # geom_line(data = data.frame(fake_x = c(5,8), fake_y = 1, station = NA, depth_categorised = NA),
     #          aes(x = fake_x, y = fake_y), size = 2, colour = "red") +
     # geom_line(data = data.frame(fake_x = 8, fake_y = c(1,2), station = NA, depth_categorised = NA),
     #           aes(x = fake_x, y = fake_y), size = 2, colour = "red") +
            geom_point(size = 5.5, colour = "black") +
      geom_point(size = 5) +
           scale_colour_manual(values = c("white", "pink", "red", "black"), guide = guide_legend(title = "Station depth")) +
      geom_text(angle = 55, hjust = 1, nudge_y = -0.05, size = 5, colour = "black") +
       annotate(geom = "text", x = 0, y = 2.5, label = "PM[2.5] ~mu~g/m^3", color = "black", vjust = 0.1,
               angle = 90, parse = TRUE, size = 6) +
      annotate("segment", x = 0.5, xend = 0.5, y = 2, yend = 3,
               colour = "black") +
      annotate(geom = "text", x = 0.25, y = 2, label = "0", color = "black", hjust = 0.8,
               angle = 0, parse = TRUE, size = 6) +
      annotate(geom = "text", x = 0, y = 3, label = "500", hjust = 0.8, color = "black",
               angle = 0, parse = TRUE, size = 6) +
      ylim(1,3) +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks=element_blank(),
            legend.position = c(0.86,0.86),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            legend.key.size = unit(1, "cm"),
            legend.background = element_rect(color = "black"),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.margin = unit(c(1,1,1,1), "cm"))
dev.off()
