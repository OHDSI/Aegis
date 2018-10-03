GIS.background<-function(bbox, country){
  country <- country
  switch(country,
         "South Korea"={
           map <- ggmap::ggmap(ggmap::get_map(location = bbox, maptype='roadmap') )
         },
         "United States"={
           map <-  ggmap::ggmap(ggmap::get_map(location = "Massachusetts", zoom=7, maptype='roadmap') )
         }
        )
return(map)
}