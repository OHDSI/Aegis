GIS.plot <- function(GIS.distribution, input.legend, input.title, GIS.Age){

  map <- GIS.background(GADM[[3]]@bbox, country)
  switch(GIS.distribution,
         "count"={
           min_value <- min(mapdf$target_count)
           max_value <- max(mapdf$target_count)

           plot <- map+
             geom_polygon(data=mapdf,aes(x=long,y=lat,group=group,fill=target_count),alpha=0.8,colour="black",lwd=0.2)+
             scale_fill_gradientn(colours = rev(heat.colors(5)), limit = c(min_value,max_value)) + labs(fill=input.legend) +
             ggtitle(input.title) + theme(plot.title=element_text(face="bold", size=30, vjust=2, color="black")) +
             theme(legend.title=element_text(size=20, face="bold")) + theme(legend.text = element_text(size=15)) + theme(legend.key.width=unit(2, "cm"), legend.key.height = unit(2,"cm"))

           plot
         },
         "proportion"={
           min_value <- min(mapdf$proportion)
           max_value <- max(mapdf$proportion)

           plot <- map+
             geom_polygon(data=mapdf,aes(x=long,y=lat,group=group,fill=proportion),alpha=0.8,colour="black",lwd=0.2)+
             scale_fill_gradientn(colours = rev(heat.colors(5)), limit = c(min_value,max_value)) + labs(fill=input.legend) +
             ggtitle(input.title) + theme(plot.title=element_text(face="bold", size=30, vjust=2, color="black")) +
             theme(legend.title=element_text(size=20, face="bold")) + theme(legend.text = element_text(size=15)) + theme(legend.key.width=unit(2, "cm"), legend.key.height = unit(2,"cm"))

           plot
         },
         "SIR"={
           min_value <- min(mapdf$SIR)
           max_value <- max(mapdf$SIR)

           plot <- map+
             geom_polygon(data=mapdf,aes(x=long,y=lat,group=group,fill=SIR),alpha=0.8,colour="black",lwd=0.2)+
             scale_fill_gradientn(colours = rev(heat.colors(5)), limit = c(min_value, max_value)) + labs(fill=input.legend) +
             ggtitle(input.title) + theme(plot.title=element_text(face="bold", size=30, vjust=2, color="black")) +
             theme(legend.title=element_text(size=20, face="bold")) + theme(legend.text = element_text(size=15)) + theme(legend.key.width=unit(2, "cm"), legend.key.height = unit(2,"cm"))

           plot
         },
         "BYM"={

           a <- poly2nb(GADM[3][[1]])
           nb2INLA("Korea.graph", a)
           a <- GADM
           kr <- a[[3]]
           nrow(CDM.table)
           CDM.table$id2 <- CDM.table$gadm_id
           kr@data <- dplyr::left_join(kr@data, CDM.table, by=c("ID_2" = "gadm_id"))


           switch(GIS.Age,
                  "no"={
                    m1 <- inla(outcome_count ~ 1 + f(ID_2, model = "iid") +
                                 f(id2, model = "bym2", graph = "Korea.graph", adjust.for.con.comp=TRUE), family = "poisson",
                               data = as.data.frame(kr), E=crd_expected,
                               control.predictor = list(compute = TRUE))
                  },
                  "yes"={
                    m1 <- inla(outcome_count ~ 1 + f(ID_2, model = "iid") +
                                 f(id2, model = "bym2", graph = "Korea.graph", adjust.for.con.comp=TRUE), family = "poisson",
                               data = as.data.frame(kr), E=std_expected,
                               control.predictor = list(compute = TRUE))
                  }

           )

           kr$RRmean <- m1$summary.fitted.values[, 1]
           kr <- kr@data

           min_value <- min(kr$RRmean)
           max_value <- max(kr$RRmean)

           mapdf <- mapdf %>%
             left_join(dplyr::select(kr, RRmean, id2), by=c("id"="id2"))

           plot <- map+
             geom_polygon(data=mapdf,aes(x=long,y=lat,group=group,fill=RRmean),alpha=0.8,colour="black",lwd=0.2)+
             scale_fill_gradientn(colours = rev(heat.colors(5)), limit = c(min_value,max_value)) + labs(fill=input.legend) +
             ggtitle(input.title) + theme(plot.title=element_text(face="bold", size=30, vjust=2, color="black")) +
             theme(legend.title=element_text(size=20, face="bold")) + theme(legend.text = element_text(size=15)) + theme(legend.key.width=unit(2, "cm"), legend.key.height = unit(2,"cm"))

           plot

         }
  )
  return(plot)
}
