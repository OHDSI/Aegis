GIS.calc2 <- function(GIS.level, fraction){
  # GIS.level <- as.numeric(GIS.level)
  # GIS.level <- GIS.level + 1
  # fraction <- as.numeric(fraction)
  # mapdf <- data.frame()
  # countdf_level$duplicated <- 0
  # id <- paste0("ID_", GIS.level-1)
  #
  # for(i in 1:nrow(countdf_level)){
  #   idx<-as.numeric(which(GADM[[GIS.level]]@data[,id] %in% countdf_level$id[i]))
  #   #Duplicated area check
  #   {
  #     for (j in which(GADM[[GIS.level]]@data[,id] %in% i)[1]:max(which(GADM[[GIS.level]]@data[,id] %in% i)))
  #     {
  #       if (idx > max(GADM[[GIS.level]]@data[,id])){
  #         next
  #       } else
  #       {
  #         countdf_level$duplicated[j] <- max(idx) #which %in% need here
  #       }
  #     }
  #   }
  #   #Drawing the polygons
  #   polygon <- GADM[[GIS.level]]@polygons[[max(idx)]]
  #   for(k in 1:length(polygon@Polygons)){
  #     if(polygon@Polygons[[k]]@area<0.001)
  #       next
  #     tempdf <- ggplot2::fortify(polygon@Polygons[[k]])
  #     tempdf$id <- max(idx)
  #     tempdf$group <- as.numeric(paste0(max(idx),".",k))
  #     mapdf <- rbind(mapdf, tempdf)
  #   }
  # }
  #
  #
  # if (isTRUE(GADM[[GIS.level]]@data[nrow(GADM[[GIS.level]]), "OBJECTID"] == GADM[[GIS.level]]@data[nrow(GADM[[GIS.level]]), id]) == TRUE) {
  #   mapdf$id2 <- mapdf$id
  # } else {
  #   mapdf$id2[i] <- GADM[[GIS.level]]@data[which(GADM[[GIS.level]]@data[,"OBJECTID"] %in% mapdf$id[i]), id]
  # }
  #
  # mapdf <- dplyr::left_join(mapdf, countdf_level, by=c("id2"="id"))
  # return(mapdf)

    #New calc2
    #PARAMS
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
