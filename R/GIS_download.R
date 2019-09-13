GIS.download <- function(country, maxLevel){

  GADM.path <- paste0(.libPaths()[1], "/AEGIS/map/", country)
  GADM.file <- paste0("GADM_2.8_",country,"_adm_total.rda")

  if (!file.exists(paste0(.libPaths()[1], "/AEGIS/map")))
    dir.create(paste0(.libPaths()[1], "/AEGIS/map"))
  if (!file.exists(GADM.path))
    dir.create(file.path(GADM.path))

  setwd(GADM.path)

  if(!file.exists(file.path(GADM.path, GADM.file))){
    GADM_list <- list()
    for(i in 0:maxLevel){
      j <- i+1
      x <- raster::getData("GADM", country=country, level=i)
      GADM_list[[j]] <- x
    }
    save(GADM_list, file=file.path(GADM.path, GADM.file))
    load(file.path(GADM.path, GADM.file))
    return(GADM_list)
  } else {
    load(file.path(GADM.path, GADM.file))
  }
  return(GADM_list)
}
