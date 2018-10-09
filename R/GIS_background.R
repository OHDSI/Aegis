GIS.background<-function(bbox, country){

  country <- country
  MAP.file <- paste0(paste0(.libPaths()[1],"/AEGIS/map"))

  if(!file.exists(MAP.file))
    dir.create(MAP.file)

  switch(country,
         "South Korea"={
           if (file.exists(paste0(MAP.file,"/",country,"_map.RDS"))){
            map <- readRDS(paste0(MAP.file,"/",country,"_map.RDS"))
           } else {
            map <- ggmap::ggmap(ggmap::get_map(location = bbox, maptype='roadmap') )
            saveRDS(map, paste0(MAP.file,"/",country,"_map.RDS"))
           }
         },
         "United States"={
           if (file.exists(paste0(MAP.file,"/",country,"_map.RDS"))){
             map <- readRDS(paste0(MAP.file,"/",country,"_map.RDS"))
           } else {
             map <-  ggmap::ggmap(ggmap::get_map(location = "Massachusetts", zoom=7, maptype='roadmap') )
             saveRDS(map, paste0(MAP.file,"/",country,"_map.RDS"))
           }
         }

        )
  return(map)
}