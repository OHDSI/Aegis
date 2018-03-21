GIS.download <- function(country, MAX.level){
  GADM.file <- paste0("GADM_2.8_",country,"_adm_total.rda")
  GADM_list <- list()
  if(!file.exists(GADM.file)){
    for(i in 0:MAX.level){
      j <- i+1
      x <- raster::getData("GADM", country=country, level=i)
      GADM_list[[j]] <- x
    }
    save(GADM_list, file=paste0("GADM_2.8_",country,"_adm_total.rda"))
    load(paste0("GADM_2.8_",country,"_adm_total.rda"))
    return(GADM_list)
  }
  else
  {
    load(paste0("GADM_2.8_",country,"_adm_total.rda"))
    return(GADM_list)
  }
}
