Cluster.find<-function(method, parameter){
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

  parameter <- as.numeric(parameter)

  switch(method,
         "besag"={
           results<-opgam(data=df, thegrid=df[,c("x","y")], alpha=.05,
           iscluster=bn.iscluster, k=parameter, R=999, model="poisson",
           mle=calculate.mle(df))
         },
         "kulldorff"={
           mle<-calculate.mle(df, model="poisson")
           results<-opgam(data=df, thegrid=df[,c("x","y")], alpha=.05,
           iscluster=kn.iscluster, fractpop=parameter, R=999, model="poisson", mle=mle)
         }
  )
  results$gadm_id <- as.numeric(row.names(results))
  results <- dplyr::left_join(results, GADM.table, by=c("gadm_id" = "ID_2"))
  results <- results[, c("x", "y", "statistic", "cluster", "pvalue", "size", "gadm_id", "NAME_0", "NAME_1", "NAME_2")]
  return(results)
}
