leafletClustering <- function(GIS.level,GADM,colorsClustering){

  idxNum <- paste0("ID_", GIS.level)
  idxName <- paste0("NAME_", GIS.level)
  GIS.leafletEstimate <- "std_sir"

  tempGADM <- GADM[[GIS.level+1]]@data
  tempGADM <- dplyr::left_join(GADM[[GIS.level+1]]@data, CDM.table, by = structure(names = idxNum ,"gadm_id"))

  m <- leaflet(tempGADM) %>%
    addTiles %>%
    fitBounds (
      lng1=GADM[[1]]@bbox[1,1], lng2=GADM[[1]]@bbox[1,2],
      lat1=GADM[[1]]@bbox[2,1], lat2=GADM[[1]]@bbox[2,2])

  tempGADM$mappingEstimate <- tempGADM[,GIS.leafletEstimate]
  #tempGADM@data[is.na(tempGADM@data[, "mappingEstimate"]), "mappingEstimate"] <- 0

  #Color to fill the polygons
  pal <- colorQuantile(colorsClustering, domain=tempGADM$mappingEstimate,
                       n=3, probs = seq(0, 1, length.out = 4), na.color = "#FFFFFF",
                       alpha = FALSE, reverse = FALSE)

  #pal <- colorBin("YlOrRd", domain = tempGADM@data$mappingEstimate, quantile(tempGADM@data$mappingEstimate, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)))

  #Estimates into pop up objects
  polygon_popup <- paste0("<strong>Name: </strong>", tempGADM[, idxName], "<br>",
                          "<strong>Target: </strong>", tempGADM$target_count, "<br>",
                          "<strong>Outcome: </strong>", tempGADM$outcome_count, "<br>",
                          "<strong>SIR: </strong>", round(tempGADM$std_sir, 2), " (", round(tempGADM$std_sirlower, 2), "-", round(tempGADM$std_sirupper, 2), ")", "<br>",
                          "<strong>Proportion: </strong>", round(tempGADM$std_prop, 2), " (", round(tempGADM$std_proplower, 2), "-", round(tempGADM$std_propupper, 2), ")")

  #create leaflet map



  polydf <- rgeos::gSimplify(GADM[[GIS.level+1]], tol=0.01, topologyPreserve=TRUE)
  tempGADM <- SpatialPolygonsDataFrame(polydf, data=tempGADM)



  m <<- m %>% addPolygons(data = tempGADM,
                          fillColor= ~pal(tempGADM@data$mappingEstimate),
                          fillOpacity = 0,
                          weight = 1,
                          color = "black",
                          dashArray = "3",
                          popup = polygon_popup,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE)) %>%
    addLegend(pal = pal, values = ~tempGADM@data$mappingEstimate, opacity = 0.7, title = NULL,
              position = "bottomright")

  return(m)

}
