Cluster.plot <- function(method, parameter){
  switch(method,
  "moran" = {
    #get out island district
    moran_poly <- GADM[[3]][-c(125, 134, 145, 153, 158, 161, 162, 189, 190, 193, 195),]

    moran_poly@data <- moran_poly@data %>%
    left_join(select(CDM.table, SIR, gadm_id), by=c("ID_2"="gadm_id"))


    spatmatrix <- poly2nb(moran_poly)

    # create a neighbours list with spatial weights
    listw <- nb2listw(spatmatrix)

    # calculate the local moran of the distribution of target population
    lmoran <- localmoran(moran_poly$SIR, listw)
    summary(lmoran)

    # padronize the variable and save it to a new column
    moran_poly$SIR_s <- scale(moran_poly$SIR)  %>% as.vector()

    # create a spatially lagged variable and save it to a new column
    moran_poly$lag_s_SIR <- lag.listw(listw, moran_poly$SIR_s)

    # summary of variables, to inform the analysis
    summary(moran_poly$SIR_s)
    summary(moran_poly$lag_s_SIR)

    # moran sccaterplot, in basic graphics (with identification of influential observations)
    x <- moran_poly$SIR_s
    y <- moran_poly$lag_s_SIR %>% as.vector()
    xx <- data_frame(x, y)

    moran_poly$quad_sig <- NA

    # high-high quadrant
    moran_poly[(moran_poly$SIR_s >= 0 &
                  moran_poly$lag_s_SIR >= 0) &
                 (lmoran[, 5] <= 0.05), "quad_sig"] <- "high-high"
    # low-low quadrant
    moran_poly[(moran_poly$SIR_s <= 0 &
                  moran_poly$lag_s_SIR <= 0) &
                 (lmoran[, 5] <= 0.05), "quad_sig"] <- "low-low"
    # high-low quadrant
    moran_poly[(moran_poly$SIR_s >= 0 &
                  moran_poly$lag_s_SIR <= 0) &
                 (lmoran[, 5] <= 0.05), "quad_sig"] <- "high-low"
    # low-high quadrant
    moran_poly@data[(moran_poly$SIR_s <= 0
                     & moran_poly$lag_s_SIR >= 0) &
                      (lmoran[, 5] <= 0.05), "quad_sig"] <- "low-high"
    # non-significant observations
    moran_poly@data[(lmoran[, 5] > 0.05), "quad_sig"] <- "not signif."

    moran_poly$quad_sig <- as.factor(moran_poly$quad_sig)
    moran_poly@data$id <- rownames(moran_poly@data)

    df <- fortify(moran_poly, region="id")
    df <- left_join(df, moran_poly@data)

    map <- GIS.background(GADM[[3]]@bbox)
    plot <- map +
      geom_polygon(data=df,aes(x=long,y=lat,group=group,fill=quad_sig),colour="black", size=.05)+
      coord_equal() +
      theme_void() + scale_fill_brewer( palette = "Set1")

  },
  "kulldorff" = {
    parameter <- as.numeric(parameter)
    ls <- list()
    for(i in 1:length(GADM[[3]])){
      geo<-GADM[[3]]@polygons[[i]]
      a<-i
      y<-geo@labpt[1]
      x<-geo@labpt[2]
      theta <- cbind(a,x,y)
      theta <- data.frame(theta)
      ls[[i]] <- theta
    }

    df <- do.call(rbind, ls)

    df <- dplyr::left_join(CDM.table, df, by=c("gadm_id" = "a"))
    df <- na.omit(df)
    df$Observed <- df$outcome_count
    df <- df[, c("gadm_id", "target_count", "Observed", "Expected", "x", "y")]

    mle<-calculate.mle(df, model="poisson")
    results<-opgam(data=df, thegrid=df[,c("x","y")], alpha=.05,
                   iscluster=kn.iscluster, fractpop=parameter, R=999, model="poisson", mle=mle)

    results$gadm_id <- as.numeric(row.names(results))
    results <- dplyr::left_join(results, GADM.table, by=c("gadm_id" = "ID_2"))
    results <- results[, c("x", "y", "statistic", "cluster", "pvalue", "size", "gadm_id", "NAME_0", "NAME_1", "NAME_2")]

    gadm <- GADM[[3]]

    mapdf<-data.frame()
    for(i in 1:length(gadm))
    {
      idx<-as.numeric(as.character(gadm$ID_2[i]))
      polygon <- gadm@polygons[[idx]]
      for(j in 1:length(polygon@Polygons))
      {
        if(polygon@Polygons[[j]]@area<0.001)
          next
        tempdf <- ggplot2::fortify(polygon@Polygons[[j]])
        tempdf$id <- idx
        tempdf$group <- as.numeric(paste0(idx,".",j))
        mapdf <- rbind(mapdf, tempdf)

      }
    }

    df_2 <- dplyr::left_join(mapdf, results, by=c("id"="gadm_id"))
    df_2[is.na(df_2)] <- 0
    df_2$cluster <- as.character(df_2$cluster)

    map <- GIS.background(GADM[[3]]@bbox)
    plot <- map +
      geom_polygon(data=df_2,aes(x=long,y=lat,group=group,fill=cluster),alpha=0.3,colour="black",lwd=0.2)+
      scale_fill_manual(values = c("white", "red", "green"))
  }
  )
  plot <- plot
  return(plot)
}
