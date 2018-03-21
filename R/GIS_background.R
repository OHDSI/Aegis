GIS.background<-function(bbox){
map <- ggmap::ggmap(ggmap::get_map(location = bbox, maptype='roadmap') )
return(map)
}
