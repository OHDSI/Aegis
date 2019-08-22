GIS.background<-function(bbox, country){

  country <- country
  MAP.path <- paste0(paste0(.libPaths()[1],"/AEGIS/map"))

  if(!file.exists(MAP.path))
    dir.create(MAP.path)

  switch(country,
         "South Korea"={
           if (file.exists(paste0(MAP.path,"/",country,"_map.RDS"))){
            map <- readRDS(paste0(MAP.path,"/",country,"_map.RDS"))
           } else {
            map <- ggmap::ggmap(ggmap::get_map(location = "South Korea", maptype='roadmap', zoom=7))
            saveRDS(map, paste0(MAP.path,"/",country,"_map.RDS"))
           }
         },
         "United States"={
           if (file.exists(paste0(MAP.path,"/",country,"_map.RDS"))){
             map <- readRDS(paste0(MAP.path,"/",country,"_map.RDS"))
           } else {
             #map <-  ggmap::ggmap(ggmap::get_map(location = "Massachusetts", zoom=7, maptype='roadmap') )
               map <- ggmap::ggmap(ggmap::get_map(location = "United States", zoom=3, maptype='roadmap') )
             saveRDS(map, paste0(MAP.path,"/",country,"_map.RDS"))
           }
         }

        )
  return(map)
}