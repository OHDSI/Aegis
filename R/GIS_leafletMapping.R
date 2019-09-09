leafletMapping <- function(GIS.level, GIS.age, GIS.distribution, country){

  idxNum <- paste0("ID_", GIS.level)
  idxName <- paste0("NAME_", GIS.level)

  tempGADM <- GADM[[GIS.level+1]]@data
  tempGADM <- dplyr::left_join(GADM[[GIS.level+1]]@data, CDM.table, by = structure(names = idxNum ,"gadm_id"))

  # m <- leaflet(tempGADM) %>%
  #   addTiles %>%
  #   fitBounds (
  #     lng1=GADM[[1]]@bbox[1,1], lng2=GADM[[1]]@bbox[1,2],
  #     lat1=GADM[[1]]@bbox[2,1], lat2=GADM[[1]]@bbox[2,2])


  ##### select the estimator
  switch(GIS.distribution,
         "count"={
                    tempGADM$mappingEstimate <- tempGADM[,"target_count"]
         },
         "proportion"={
           switch(GIS.age,
                  "no"={
                    tempGADM$mappingEstimate <- tempGADM[,"crd_prop"]
                  },
                  "yes"={
                    tempGADM$mappingEstimate <- tempGADM[,"std_prop"]
                  }
           )
         },
         "SIR"={
           switch(GIS.age,
                  "no"={
                    tempGADM$mappingEstimate <- tempGADM[,"crd_sir"]
                  },
                  "yes"={
                    tempGADM$mappingEstimate <- tempGADM[,"std_sir"]
                  }
           )
          }#,
         # "BYM"={
         #   country <- country
         #   MAP.path <- paste0(paste0(.libPaths()[1],"/AEGIS/map/", input$country))
         #   MAP.file <- paste0(paste0(country, "_", GIS.level,".graph"))
         #   setwd(MAP.path)
         #
         #   if(!file.exists(MAP.path))
         #     dir.create(MAP.path)
         #
         #   if(!file.exists(file.path(MAP.path, MAP.file))){
         #     a <- poly2nb(GADM[GIS.level][[1]])
         #     nb2INLA(file.path(MAP.path, MAP.file), a)
         #   }
         #
         #   kr <- GADM[[GIS.level]]
         #   CDM.table$id2 <- CDM.table$gadm_id
         #   kr@data <- dplyr::left_join(kr@data, CDM.table, by=c("ID_2" = "gadm_id"))
         #
         #
         #   switch(GIS.Age,
         #          "no"={
         #            m1 <- inla(outcome_count ~ 1 + f(ID_2, model = "iid") +
         #                         f(id2, model = "bym2", graph = file.path(MAP.path, MAP.file), adjust.for.con.comp=TRUE), family = "poisson",
         #                       data = as.data.frame(kr), E=crd_expected,
         #                       control.predictor = list(compute = TRUE))
         #          },
         #          "yes"={
         #            m1 <- inla(outcome_count ~ 1 + f(ID_2, model = "iid") +
         #                         f(id2, model = "bym2", graph = file.path(MAP.path, MAP.file), adjust.for.con.comp=TRUE), family = "poisson",
         #                       data = as.data.frame(kr), E=std_expected,
         #                       control.predictor = list(compute = TRUE))
         #          }
         #
         #   )
         # }
  )

           #kr$RRmean <- m1$summary.fitted.values[, 1]

  #create leaflet map
  polydf <- rgeos::gSimplify(GADM[[GIS.level+1]], tol=0.01, topologyPreserve=TRUE)
  tempGADM <- SpatialPolygonsDataFrame(polydf, data=tempGADM)
  tableProxy <- tempGADM
  return(tableProxy)
}
