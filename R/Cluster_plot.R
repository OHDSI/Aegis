Cluster.plot <- function(method, parameter, GIS.Age, country){
  switch(method,
  "moran" = {
    #get out island district
    moran_poly <- GADM[[3]][-c(125, 134, 145, 153, 158, 161, 162, 189, 190, 193, 195),]


    switch(GIS.Age,
           "no"={
             df_moran <- CDM.table[, c("SIR", "gadm_id", "outcome_count", "expected")]
             colnames(df_moran) <- c("SIR", "gadm_id", "outcome_count", "expected")

           },
           "indrect"={
             df_moran <- CDM.table[, c("indirect_SIR", "gadm_id", "outcome_count", "indirect_expected")]
             colnames(df_moran) <- c("SIR", "gadm_id", "outcome_count", "expected")

           },
           "direct"={
             df_moran <- CDM.table[, c("direct_SIR", "gadm_id", "outcome_count", "direct_expected")]
             colnames(df_moran) <- c("SIR", "gadm_id", "outcome_count", "expected")
           }

    )

    moran_poly@data <- moran_poly@data %>%
    left_join(dplyr::select(df_moran, SIR, gadm_id, outcome_count, expected), by=c("ID_2"="gadm_id"))

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

    ## ###############################
    #moran <- moranI.test(outcome_count~offset(expected), moran_poly@data, model="poisson", R=99,
    #            listw=listw, n=length(spatmatrix), S0=Szero(listw))

    #str(moran)






    #global_moran <- paste0("Global Moran'I Statistic and p-value :", round(result$most.likely.cluster$p.value,digits = 4))


    map <- GIS.background(GADM[[3]]@bbox, country)
    plot <- map +
      geom_polygon(data=df,aes(x=long,y=lat,group=group,fill=quad_sig),colour="black", size=.05)+
      coord_equal() +
      theme_void() + scale_fill_brewer( palette = "Set1") +
      coord_fixed(ratio=1.1)

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

    geo<- latlong2grid(df)

    pop.upper.bound <- parameter
    n.simulations <- 999
    alpha.level <- 0.05
    n.strata <- 16
    plot <- TRUE

    df <- dplyr::left_join(CDM.table, df, by=c("gadm_id" = "a"))
    df <- na.omit(df)


    population <- tapply(df$target_count,df$gadm_id,sum)
    cases <- tapply(df$outcome_count,df$gadm_id,sum)



    switch(GIS.Age,
           "no"={
             df <- df[, c("gadm_id", "target_count", "outcome_count", "expected", "x", "y")]
             colnames(df) <- c("gadm_id", "target_count", "Observed", "Expected", "x", "y")
             expected.cases <- df$Expected
           },
           "indrect"={
             df <- df[, c("gadm_id", "target_count", "outcome_count", "indirect_expected", "x", "y")]
             colnames(df) <- c("gadm_id", "target_count", "Observed", "Expected", "x", "y")
             expected.cases <- df$Expected
           },
           "direct"={
             df <- df[, c("gadm_id", "target_count", "outcome_count", "direct_expected", "x", "y")]
             colnames(df) <- c("gadm_id", "target_count", "Observed", "Expected", "x", "y")
             expected.cases <- df$Expected
           }
    )

    result <<- kulldorff(geo, cases, population, expected.cases, pop.upper.bound,
                         n.simulations, alpha.level, plot)

    df_2 <- GADM.table
    df_2$cluster <- ifelse(
                    GADM.table$ID_2 %in% result$most.likely.cluster$location.IDs.included, "Primary cluster",
                    ifelse(GADM.table$ID_2 %in% result$secondary.clusters[[1]]$location.IDs.included, "Secondary cluster", "Non-clustered"))

    df_2$cluster <- factor(df_2$cluster, levels = c("Non-clustered", "Secondary cluster", "Primary cluster"))

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

    df_2 <- dplyr::left_join(mapdf, df_2, by=c("id"="ID_2"))

    map <- AEGIS::GIS.background(GADM[[3]]@bbox, country)

    primary_labs <- paste0("Primary cluster p-value :", round(result$most.likely.cluster$p.value,digits = 4))

    ifelse(is.null(result$secondary.clusters[[1]]$p.value),
           secondary_labs <- paste0("Secondary cluster p-value : None"),
           secondary_labs <- paste0("Secondary cluster p-value :", round(result$secondary.clusters[[1]]$p.value, digits = 4))
           )

    plot <- map +
      geom_polygon(data=df_2,aes(x=long,y=lat,group=group,fill=cluster),alpha=0.3,colour="black",lwd=0.2)+
      scale_fill_manual(values = c("white", "green", "red")) +
      labs(title = primary_labs, subtitle =  secondary_labs)
  }
  )
  plot <- plot
  return(plot)
}
