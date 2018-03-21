GIS.plot <- function(GIS.distribution, input.legend, input.title){

  map <- GIS.background(GADM[[3]]@bbox)
  switch(GIS.distribution,
         "count"={
           t <- max(mapdf$outcome_count)

           plot <- map+
             geom_polygon(data=mapdf,aes(x=long,y=lat,group=group,fill=outcome_count),alpha=0.8,colour="black",lwd=0.2)+
             scale_fill_gradientn(colours = rev(heat.colors(3)), limit = c(0,t)) + labs(fill=input.legend) +
             ggtitle(input.title) + theme(plot.title=element_text(face="bold", size=30, vjust=2, color="black")) +
             theme(legend.title=element_text(size=20, face="bold")) + theme(legend.text = element_text(size=15)) + theme(legend.key.width=unit(2, "cm"), legend.key.height = unit(2,"cm"))

           plot
         },
         "proportion"={
           t <- max(mapdf$prop_count)
           plot <- map+
             geom_polygon(data=mapdf,aes(x=long,y=lat,group=group,fill=prop_count),alpha=0.8,colour="black",lwd=0.2)+
             scale_fill_gradientn(colours = rev(heat.colors(3)), limit = c(0,t)) + labs(fill=input.legend) +
             ggtitle(input.title) + theme(plot.title=element_text(face="bold", size=30, vjust=2, color="black")) +
             theme(legend.title=element_text(size=20, face="bold")) + theme(legend.text = element_text(size=15)) + theme(legend.key.width=unit(2, "cm"), legend.key.height = unit(2,"cm"))

           plot
         },
         "SIR"={
           t <- max(mapdf$sir)
           plot <- map+
             geom_polygon(data=mapdf,aes(x=long,y=lat,group=group,fill=sir),alpha=0.8,colour="black",lwd=0.2)+
             scale_fill_gradientn(colours = rev(heat.colors(3)), limit = c(0,t)) + labs(fill=input.legend) +
             ggtitle(input.title) + theme(plot.title=element_text(face="bold", size=30, vjust=2, color="black")) +
             theme(legend.title=element_text(size=20, face="bold")) + theme(legend.text = element_text(size=15)) + theme(legend.key.width=unit(2, "cm"), legend.key.height = unit(2,"cm"))

           plot
         }
  )
  return(plot)
}
