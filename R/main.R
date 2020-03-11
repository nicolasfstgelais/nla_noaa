
options(noaakey = "QWlWJuQsHjCHRPUfYuOybgyeADcltdVJ")
library('rnoaa')
library("lubridate")


#station_data <- ghcnd_stations()

stations=read.csv("data/raw/transform_station_output.csv")
temporal=read.csv("data/raw/transform_temporal_output.csv")

sampling_date=data.frame(NLA_ID=stringr::str_sub(temporal$X, 1, -12),
                         date=stringr::str_sub(temporal$X, -10, -1))

location=data.frame(NLA_ID=stations$station,latitude=stations$latitude,longitude=stations$longitude)

sampling_date_location=merge(sampling_date,location)


output <- data.frame()
for(i in 1:nrow(sampling_date_location))
{

  
  lat_lon_df <- data.frame(id =as.character(sampling_date_location$NLA_ID[i]), 
                           latitude = sampling_date_location$latitude[i], 
                           longitude = sampling_date_location$longitude[i])
  
  met_station=meteo_nearby_stations( lat_lon_df,station_data = station_data,radius = 30,var = c("PRCP","TMAX","TMIN","TOBS"),year_min=year(d_sampling),year_max=year(d_sampling),limit=1)
  
 
  
  d_sampling=as.Date(sampling_date_location$date[i])
  d_before=d_sampling
  day(d_before) <- day(d_before)-1
  month(d_before) <- lubridate::month(d_before) - 1
  
  
  t=meteo_pull_monitors(met_station[[1]]$id,date_min=d_before,date_max=d_sampling)
  

  
  
  output <- rbind(output,                  
                  data.frame(NLA_ID = sampling_date_location$NLA_ID[i],  
                             prcp_max_1m=max(t$prcp,na.rm = T),
                             prcp_mean_1m=mean(t$prcp,na.rm = T),
                             temp_max_1m=max(t$tmax,na.rm = T),
                             temp_min_1m=min(t$tmin,na.rm = T),
                             temp_mean_1m=mean(t$tmax,na.rm = T)
                  ))
  
}



write.csv(output,"data/output.csv")
