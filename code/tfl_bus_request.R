



url <- paste("https://api.tfl.gov.uk/Journey/JourneyResults/", start_lat, ",", start_lon, "/to/", end_lat, ",", end_lon,
             "?journeyPreference=LeastTime&mode=bus&app_id=", tfl_app_id, "&app_key=", tfl_app_key, 
             "&date=", format(start_time, '%Y%m%d'),
             "&time=", format(start_time, '%H%M'),
             sep="")

received <- RCurl::getURL(url)

temp <- try(fromJSON(received, simplify = FALSE))

bus_journey_leg_times <- data.frame(id = '',
                                duration = '',
                                stringsAsFactors = FALSE)

if (class(temp) != 'try-error') {

json_data <- fromJSON(received, simplify = FALSE)

} else { print('routing failed')}

if (json_data$`$type` == 'Tfl.Api.Presentation.Entities.JourneyPlanner.ItineraryResult, Tfl.Api.Presentation.Entities') {
  
  temp_results_frame <- data.frame(id = numeric(),
                                   lat = numeric(),
                                   lon = numeric(),
                                   mode = character(),
                                   line = character(),
                                   stringsAsFactors = FALSE)
  
  for (r in 1:length(json_data$journeys[[2]]$legs)) {
      
    if ('lineString' %in% names(json_data$journeys[[2]]$legs[[r]]$path)) {
      
      ## Need the leg durations
      bus_journey_leg_times[r,]     <- c(paste(3,r,sep=""), as.numeric(json_data$journeys[[2]]$legs[[r]]$duration))
      line                          <-  json_data$journeys[[2]]$legs[[r]]$routeOptions[[1]]$name
      linestring                    <-  json_data$journeys[[2]]$legs[[r]]$path$lineString
      linestring                    <- gsub(" ", "", linestring, fixed=TRUE)
      linestring                    <- gsub("[", "", linestring, fixed = TRUE)
      linestring                    <- gsub("]", "", linestring, fixed = TRUE)
      linestring                    <- unlist(strsplit(linestring, split = ","))
      
      per_leg_results <- data.frame(id = numeric(),
                                    lat = numeric(),
                                    lon = numeric(),
                                    mode = character(),
                                    line = character(),
                                    stringsAsFactors = FALSE)
      
      l <- 2
      m <- 1
      for (k in 1:(length(linestring)/2)){
        per_leg_results[k,] <- c(paste(3,r,sep=""), as.numeric(linestring[m]), as.numeric(linestring[l]), 'bus', line)
        l <- l+2
        m <- m+2
      }
      
      temp_results_frame <- rbindlist(list(temp_results_frame, per_leg_results), use.names=TRUE)
      rm(per_leg_results)
      temp_results_frame$id <-    as.numeric(temp_results_frame$id)
      temp_results_frame$lat       <-    as.numeric(temp_results_frame$lat)
      temp_results_frame$lon       <-    as.numeric(temp_results_frame$lon)
      temp_results_frame           <-    data.frame(temp_results_frame)
    } else {} 
    
  }
  temp_results_frame[temp_results_frame$line == "","line"] <- NA
  temp_results_frame[is.na(temp_results_frame$line),"mode"] <- 'walk'
  
  bus_result <- temp_results_frame
  rm(temp_results_frame, k, l, m, r, line, linestring, url, json_data,tfl_app_id, tfl_app_key,received,temp)
  print('Bus routing was completed using TFL Journey Planned API')
} else {
  print('routing failed')
}
