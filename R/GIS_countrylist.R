GIS.countrylist <- function(){

    temp <- paste0(.libPaths()[1],"/AEGIS/data/Country_list.Rdata")
  load(temp)
  country_list <- Country_list[order(Country_list$USABLE),]

}
