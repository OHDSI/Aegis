leafletMapping <- function(GIS.level, GIS.age, GIS.distribution, country){

  idxNum <- paste0("ID_", GIS.level)
  idxName <- paste0("NAME_", GIS.level)
  country <- country
  GIS.level <- GIS.level


  tempGADM <- dplyr::left_join(GADM[[GIS.level+1]]@data, CDM.table, by = structure(names = "OBJECTID","gadm_id"))

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
          },
          "BYM"={
         GADM.path <- paste0(.libPaths()[1], "/AEGIS/map/", country)
          if (!file.exists(paste0(.libPaths()[1], "/AEGIS/map")))
            dir.create(paste0(.libPaths()[1], "/AEGIS/map"))
          if (!file.exists(GADM.path))
            dir.create(file.path(GADM.path))

            setwd(GADM.path)

            MAP.path <- paste0(paste0(.libPaths()[1],"/AEGIS/map/", country))
            MAP.file <- paste0(paste0(country, "_", GIS.level,".graph"))
            setwd(MAP.path)

            if(!file.exists(MAP.path))
              dir.create(MAP.path)

            if(!file.exists(file.path(MAP.path, MAP.file))){
              a <- poly2nb(GADM[GIS.level+1][[1]])
              nb2INLA(file.path(MAP.path, MAP.file), a)
            }

            tempGADM$id2 <- tempGADM$OBJECTID

            switch(GIS.age,
                   "no"={
                     m1 <- inla(outcome_count ~ 1 + f(OBJECTID, model = "iid") +
                                  f(id2, model = "bym2", graph = file.path(MAP.path, MAP.file), adjust.for.con.comp=TRUE), family = "poisson",
                                data = as.data.frame(tempGADM), E=crd_expected,
                                control.predictor = list(compute = TRUE))
                   },
                   "yes"={
                     m1 <- inla(outcome_count ~ 1 + f(OBJECTID, model = "iid") +
                                  f(id2, model = "bym2", graph = file.path(MAP.path, MAP.file), adjust.for.con.comp=TRUE), family = "poisson",
                                data = as.data.frame(tempGADM), E=std_expected,
                                control.predictor = list(compute = TRUE))
                   }

            )

            tempGADM$RRmean <- m1$summary.fitted.values[, 1]
            tempGADM$mappingEstimate <- tempGADM[,"RRmean"]
          }
  )


  #create leaflet map
  polydf <- rgeos::gSimplify(GADM[[GIS.level+1]], tol=0.01, topologyPreserve=TRUE)
  tempGADM <- SpatialPolygonsDataFrame(polydf, data=tempGADM)
  tableProxy <- tempGADM
  return(tableProxy)
}
