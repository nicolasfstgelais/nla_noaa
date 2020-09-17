rm(list = ls()) 

options(noaakey = "QWlWJuQsHjCHRPUfYuOybgyeADcltdVJ")
library('rnoaa')
library("lubridate")


#station_data <- ghcnd_stations()

stations=read.csv("data/raw/transform_station_output.csv")
temporal=read.csv("data/raw/transform_temporal_output.csv")

sampling_date=data.frame(NLA_ID=temporal$X,station=stringr::str_sub(temporal$X, 1, -12),
                         date=stringr::str_sub(temporal$X, -10, -1))

location=data.frame(station=stations$station,latitude=stations$latitude,longitude=stations$longitude)

sampling_date_location=merge(sampling_date,location)

#to init


output_prcp <- data.frame(NLA_ID=sampling_date_location$NLA_ID,
                         date=sampling_date$date,
                         station=sampling_date$station,
                         prcp_max_1m=-99,
                          prcp_mean_1m=-99,
                          prcp_max_5m=-99,
                          prcp_mean_5m=-99,
                          prcp_max_7d=-99,
                          prcp_mean_7d=-99)
#save(output_prcp,file="data/interim/output_prcp")

i=which(output_prcp$NLA_ID=="NLA06608-0001-2007-07-31")

#for(i in 1:nrow(sampling_date_location))
for(i in 1:2482)
{
  limit=1
  load("data/interim/output_prcp")
  print(i)
  # if already in the data frame skip
  if(output_prcp$prcp_max_1m[i]!=-99){
    # if a line have any missing information run but with more stations
    if(any(is.na(output_prcp[i,]))){limit=20}else{next}
    }
  
  lat_lon_df <- data.frame(id =as.character(sampling_date_location$NLA_ID[i]), 
                           latitude = sampling_date_location$latitude[i], 
                           longitude = sampling_date_location$longitude[i])
  
 
  d_sampling=as.Date(sampling_date_location$date[i])
  
  met_station=meteo_nearby_stations( lat_lon_df,station_data = station_data,radius = 100
                                     ,var = c("PRCP"),year_min=year(d_sampling),
                                     year_max=year(d_sampling),limit=limit)
  
  d_min=d_sampling
  # when 31th, get an error when month-1 so first remove 1 day
  day(d_min) <- day(d_min)-4
  
  d_min_1m=d_min;month( d_min_1m) <- month(d_min) - 1
  d_min_5m=d_min;month(d_min_5m) <- month(d_min) - 5
  d_min_7d=d_min;day(d_min_7d) <- day(d_min) - 3
  
  

  t_1m=meteo_pull_monitors(met_station[[1]]$id,date_min=d_min_1m,date_max=d_sampling,var="PRCP",)
  
  t_5m=meteo_pull_monitors(met_station[[1]]$id,date_min=d_min_5m,date_max=d_sampling,var="PRCP")

  t_7d=meteo_pull_monitors(met_station[[1]]$id,date_min=d_min_7d,date_max=d_sampling,var="PRCP")
  
  
  if(ncol(output_prcp)==0){
    
  }
    
    
  output_prcp[output_prcp$NLA_ID==as.character(sampling_date_location$NLA_ID[i]),]=
            data.frame(NLA_ID = sampling_date_location$NLA_ID[i],  
                             prcp_max_1m=max(t_1m$prcp,na.rm = T),
                             prcp_mean_1m=mean(t_1m$prcp,na.rm = T),
                             prcp_max_5m=max(t_5m$prcp,na.rm = T),
                             prcp_mean_5m=mean(t_5m$prcp,na.rm = T),
                             prcp_max_7d=max(t_7d$prcp,na.rm = T),
                             prcp_mean_7d=mean(t_7d$prcp,na.rm = T)
                             )
  
  
  save(output_prcp,file="data/interim/output_prcp")
  
  output_prcp[any(is.na(output_prcp))]
  
  output_prcp=cbind(output_prcp,sampling_date)

write.csv(output_prcp,"data//processed/precip_noaa_output.csv")
