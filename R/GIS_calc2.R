GIS.calc2 <- function(GIS.level, fraction){
  GIS.level <- as.numeric(GIS.level)
  GIS.level <- GIS.level + 1
  fraction <- as.numeric(fraction)
  mapdf <- data.frame()
        for(i in 1:nrow(countdf_level)){
          #countdf_level$prop_count[i] <- (countdf_level$outcome_count[i] / countdf_level$target_count[i])*fraction
          #countdf_level$sir[i] <-(countdf_level$outcome_count[i] / countdf_level$target_count[i]) /
          #  (sum(countdf_level$outcome_count) / sum(countdf_level$target_count))
          idx<-as.numeric(as.character(countdf_level$id[i]))
          polygon <- GADM[[GIS.level]]@polygons[[idx]]
          for(j in 1:length(polygon@Polygons)){
            if(polygon@Polygons[[j]]@area<0.001)
              next
            tempdf <- ggplot2::fortify(polygon@Polygons[[j]])
            tempdf$id <- idx
            tempdf$group <- as.numeric(paste0(idx,".",j))
            mapdf <- rbind(mapdf, tempdf)
          }
        }
  mapdf <- dplyr::left_join(mapdf, countdf_level, by=c("id"="id"))
  return(mapdf)
}
