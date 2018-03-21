Set.wd<-function(DB.name){
  wd<-paste0(.libPaths()[1],"/AEGIS/", DB.name)
  if(exists(wd)==FALSE) {
    dir.create(wd, showWarnings = FALSE)
    setwd(wd)
  } else
  {
    setwd(wd)
  }
}
