leafletClustering <- function(parameter, GIS.age, GIS.level, colorsClustering){

  parameter <- as.numeric(parameter)
  GIS.age <- GIS.age
  GIS.level <- as.numeric(GIS.level)

  idxNum <- paste0("ID_", GIS.level)
  idxName <- paste0("NAME_", GIS.level)
  country <- country

  tempGADM <- GADM[[GIS.level+1]]@data
  tempGADM <- dplyr::left_join(GADM[[GIS.level+1]]@data, CDM.table, by = structure(names = idxNum ,"gadm_id"))

  gadm_id <- tempGADM[,"OBJECTID"]

  ls <- list()
  for(i in 1:length(gadm_id)) {
    idx <- gadm_id[i]
    geo<-GADM[[GIS.level+1]]@polygons[[idx]]
    a<-idx
    y<-geo@labpt[1]
    x<-geo@labpt[2]
    theta <- cbind(a,x,y)
    theta <- data.frame(theta)
    ls[[i]] <- theta
  }

  df <- do.call(rbind, ls)

  geo <- SpatialEpi::latlong2grid(df[, c(2,3)])

  pop.upper.bound <- parameter
  n.simulations <- 999
  alpha.level <- 0.05
  n.strata <- 16
  plot <- TRUE

  df <- dplyr::left_join(CDM.table, df, by=c("gadm_id" = "a"))

  population <- tapply(df$target_count,df$gadm_id,sum)
  cases <- tapply(df$outcome_count,df$gadm_id,sum)

  switch(GIS.age,
         "no"={
           df <- df[, c("gadm_id", "target_count", "outcome_count", "crd_expected", "x", "y")]
           colnames(df) <- c("gadm_id", "target_count", "Observed", "Expected", "x", "y")
           expected.cases <- df$Expected
         },
         "yes"={
           df <- df[, c("gadm_id", "target_count", "outcome_count", "std_expected", "x", "y")]
           colnames(df) <- c("gadm_id", "target_count", "Observed", "Expected", "x", "y")
           expected.cases <- df$Expected
         }
  )

  result <- SpatialEpi::kulldorff(geo, cases, population, expected.cases, pop.upper.bound,
                                  n.simulations, alpha.level, plot=FALSE)
  n.cluster <- 1 + length(result$secondary.clusters)

  cluster.indexs <- list(result$most.likely.cluster)
  if(!is.null(result$secondary.clusters)){
    for (i in 1:length(result$secondary.clusters)){
      cluster.indexs <- c(cluster.indexs,list(result$secondary.clusters[[i]]))
    }
  }


  tempGADM$k.cluster <- NA
  tempGADM$population <- NA
  tempGADM$number.of.cases <- NA
  tempGADM$expected.cases <- NA
  tempGADM$SMR <- NA
  tempGADM$log.likelihood.ratio <- NA
  tempGADM$monte.carlo.rank <- NA
  tempGADM$p.value <- NA
  for (i in 1:length(cluster.indexs)){
    temp <- cluster.indexs[[i]][[1]]
    tempGADM$k.cluster[temp] <- i
    tempGADM$population[temp] <- cluster.indexs[[i]]$population
    tempGADM$number.of.cases[temp] <- cluster.indexs[[i]]$number.of.cases
    tempGADM$expected.cases[temp] <- cluster.indexs[[i]]$expected.cases
    tempGADM$SMR[temp] <- cluster.indexs[[i]]$SMR
    tempGADM$log.likelihood.ratio[temp] <- cluster.indexs[[i]]$log.likelihood.ratio
    tempGADM$monte.carlo.rank[temp] <- cluster.indexs[[i]]$monte.carlo.rank
    tempGADM$p.value[temp] <- cluster.indexs[[i]]$p.value
  }

  max_value <- max(na.omit(unique(tempGADM$k.cluster)))
  # pal <- colorNumeric(topo.colors(max_value),domain =  NULL, na.color = "#FFFFFF", alpha = FALSE,
  #                     reverse = FALSE)
  #

  #create leaflet map
  polydf <- rgeos::gSimplify(GADM[[GIS.level+1]], tol=0.01, topologyPreserve=TRUE)
  tempGADM <- SpatialPolygonsDataFrame(polydf, data=tempGADM)
  tableProxy <- tempGADM
  return(tableProxy)
}
