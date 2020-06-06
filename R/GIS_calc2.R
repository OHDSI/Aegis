GIS.calc2 <- function(countdf_level, GADM, GIS.level, fraction){
  GIS.level <- as.numeric(GIS.level)
  GIS.level <- GIS.level + 1
  fraction <- as.numeric(fraction)
  id <- paste0("ID_", GIS.level-1)

  #Function simplifies the given geometry using the Douglas-Peuker algorithm
  #This output should be writing on libpath
  polydf <- rgeos::gSimplify(GADM[[GIS.level]], tol=0.01, topologyPreserve=TRUE)

  #Extract polygons
  polygons <- fortify(polydf, region=id)
  polygons$id <- as.numeric(polygons$id)

  polygons <- polygons %>%
    left_join(select(GADM[[GIS.level]]@data, OBJECTID, id), by=c('id'='OBJECTID'))

  polygons$id2 <- polygons[,8]

  colnames(polygons) <- tolower(colnames(polygons))

  mapdf <- dplyr::left_join(polygons, countdf_level, by=c("id2"="id"))

  return(mapdf)

}
