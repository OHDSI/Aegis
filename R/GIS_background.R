Cluster.plot <- function(method, parameter, GIS.Age, country,GADM,GIS.level){

           parameter <- as.numeric(parameter)

           idxNum <- paste0("ID_", GIS.level)
           idxName <- paste0("NAME_", GIS.level)
           tempGADM <- GADM[[GIS.level+1]]
           tempGADM@data <- dplyr::left_join(GADM[[GIS.level+1]]@data, CDM.table, by = structure(names = idxNum ,"gadm_id"))

           gadm_id <- tempGADM@data[,idxNum]

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


           clusterDf <- do.call(rbind, ls)

           geo <- SpatialEpi::latlong2grid(clusterDf[, c(2,3)])

           pop.upper.bound <- parameter
           n.simulations <- 999
           alpha.level <- 0.05
           n.strata <- 16
           plotValue <- F

           clusterDf <- dplyr::left_join(tempGADM@data, clusterDf, by = structure(names = idxNum ,"a"))

           clusterDf$gadm_id = clusterDf[[idxNum]]

           population <- tapply(clusterDf$target_count,clusterDf$gadm_id,sum)
           cases <- tapply(clusterDf$outcome_count,clusterDf$gadm_id,sum)

           switch(GIS.Age,
                  "no"={
                    clusterDf <- clusterDf[, c("gadm_id", "target_count", "outcome_count", "crd_expected", "x", "y")]
                    colnames(clusterDf) <- c("gadm_id", "target_count", "Observed", "Expected", "x", "y")
                    expected.cases <- clusterDf$Expected
                  },
                  "yes"={
                    clusterDf <- clusterDf[, c("gadm_id", "target_count", "outcome_count", "std_expected", "x", "y")]
                    colnames(clusterDf) <- c("gadm_id", "target_count", "Observed", "Expected", "x", "y")
                    expected.cases <- clusterDf$Expected
                  }
           )
           result <- SpatialEpi::kulldorff(geo, cases, population, expected.cases, pop.upper.bound,
                                           n.simulations, alpha.level, plotValue)
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
           for (i in 1:2){ #length(cluster.indexs)
             temp <- cluster.indexs[[i]][[1]]
             tempGADM@data$k.cluster[temp] <- i
             tempGADM@data$population[temp] <- cluster.indexs[[i]]$population
             tempGADM@data$number.of.cases[temp] <- cluster.indexs[[i]]$number.of.cases
             tempGADM@data$expected.cases[temp] <- cluster.indexs[[i]]$expected.cases
             tempGADM@data$SMR[temp] <- cluster.indexs[[i]]$SMR
             tempGADM@data$log.likelihood.ratio[temp] <- cluster.indexs[[i]]$log.likelihood.ratio
             tempGADM@data$monte.carlo.rank[temp] <- cluster.indexs[[i]]$monte.carlo.rank
             tempGADM@data$p.value[temp] <- cluster.indexs[[i]]$p.value
           }


           polygon_popup <- paste0("<strong>Name: </strong>", tempGADM@data[, idxName], "<br>",
                                   "<strong>Target: </strong>", tempGADM@data$target_count, "<br>",
                                   "<strong>Outcome: </strong>", tempGADM@data$outcome_count, "<br>",
                                   "<strong>SIR: </strong>", round(tempGADM@data$std_sir, 2), " (", round(tempGADM@data$std_sirlower, 2), "-", round(tempGADM@data$std_sirupper, 2), ")", "<br>",
                                   "<strong>Proportion: </strong>", round(tempGADM@data$std_prop, 2), " (", round(tempGADM@data$std_proplower, 2), "-", round(tempGADM@data$std_propupper, 2), ")",
                                   "<br>------------------Clustering Result>------------------<br>",
                                   "<strong>population: </strong>", tempGADM@data$population,"<br>",
                                   "<strong>number.of.cases: </strong>", tempGADM@data$number.of.cases,"<br>",
                                   "<strong>expected.cases: </strong>", round(tempGADM@data$expected.cases,4),"<br>",
                                   "<strong>SMR: </strong>", round(tempGADM@data$SMR,4),"<br>",
                                   "<strong>log.likelihood.ratio: </strong>", round(tempGADM@data$log.likelihood.ratio,4),"<br>",
                                   "<strong>p.value: </strong>", round(tempGADM@data$p.value,3),"<br>"
           )

           max_value <- max(na.omit(unique(tempGADM@data$k.cluster)))
           pal <- colorNumeric(c('Red','Green'),domain =  NULL, na.color = "#FFFFFF", alpha = FALSE,
                               reverse = FALSE)

           m <- leaflet(tempGADM) %>%
             addTiles %>%
             fitBounds (
               lng1=GADM[[1]]@bbox[1,1], lng2=GADM[[1]]@bbox[1,2],
               lat1=GADM[[1]]@bbox[2,1], lat2=GADM[[1]]@bbox[2,2])

           #create leaflet map
           clusterPlot <- m %>% addPolygons(data = tempGADM,
                                     fillColor= pal(tempGADM@data$k.cluster),
                                     fillOpacity = 0.5,
                                     weight = 1,
                                     color = "black",
                                     dashArray = "3",
                                     popup = polygon_popup,
                                     highlight = highlightOptions(
                                       weight = 5,
                                       color = "#666",
                                       dashArray = "",
                                       fillOpacity = 0.7,
                                       bringToFront = TRUE)) # %>%
             addLegend(pal = pal, values = ~tempGADM@data$k.cluster, opacity = 0.7, title = NULL,
                       position = "bottomright")


  return(clusterPlot)
}
