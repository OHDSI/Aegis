GIS.download <- function(country, MAX.level){
  GADM.file <- paste0(.libPaths()[1], "/AEGIS/", "GADM_2.8_",country,"_adm_total.rda")
  GADM_list <- list()
  if(!file.exists(GADM.file)){
    for(i in 0:MAX.level){
      j <- i+1
      x <- raster::getData("GADM", country=country, level=i)
      GADM_list[[j]] <- x
    }
    save(GADM_list, file=paste0("GADM_2.8_",country,"_adm_total.rda"))
    GADM_list <- load(file.path(GADM.file))
    return(GADM_list)
  }
  else
  {
    GADM_list <- load(file.path(GADM.file))
    return(GADM_list)
  }
}
