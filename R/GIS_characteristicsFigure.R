GIS.characteristicsFigure <- function(extraCSV, lon, lat, value, cha_BM){

  df <- extraCSV

  longitude <- lon
  latitude <- lat
  value <- value

  df$longitude <- df[,longitude]
  df$latitude <- df[,latitude]
  df$value <- df[,value]

  #exclude no lat/lon rows
  df <- data.frame(df %>% filter(!is.na(longitude)))
  df <- data.frame(df %>% filter(longitude!="NA"))

  df <- data.frame(df %>% filter(!is.na(latitude)))
  df <- data.frame(df %>% filter(latitude!="NA"))

  df$longitude <- as.double(df$longitude)
  df$latitude <- as.double(df$latitude)

  coordinates(df) <- ~ longitude + latitude
  proj4string(df) <- proj4string(GADM[[maxLevel+1]])

  output <- sp::over(df, GADM[[maxLevel+1]])
  output <- cbind(df@data, output)

  output <- data.frame(
    output %>%
      group_by(ID_2) %>%
      summarise(value = mean(value))
  )


  output <- data.frame(
        left_join(output, CDM.table, by = c("ID_2"="gadm_id"))
  )


   if(cha_BM=="yes"){
    MAP.path <- paste0(paste0(.libPaths()[1],"/AEGIS/map/", country))
    MAP.file <- paste0(paste0(country, "_", GIS.level,".graph"))

    output$id2 <- output$ID_2

    m1 <- inla(outcome_count ~ 1 + f(ID_2, model = "iid") + value +
                f(id2, model = "bym2", graph = file.path(MAP.path, MAP.file), adjust.for.con.comp=TRUE), family = "poisson",
              data = as.data.frame(output), E=std_expected,
              control.predictor = list(compute = TRUE))

    output$RRmean <- m1$summary.fitted.values[, 1]

    output <- GADM[[3]]@data %>%
      left_join(select(output, ID_2, value, RRmean), by=c('ID_2'='ID_2'))
   } else {
     output <- GADM[[3]]@data %>%
       left_join(select(output, ID_2, value), by=c('ID_2'='ID_2'))
   }



  tempGADM <- dplyr::left_join(GADM[[GIS.level+1]]@data, CDM.table, by = structure(names = "OBJECTID","gadm_id"))


  polydf <- rgeos::gSimplify(GADM[[GIS.level+1]], tol=0.01, topologyPreserve=TRUE)
  tempGADM <- SpatialPolygonsDataFrame(polydf, data=output)

  return(tempGADM)
}
