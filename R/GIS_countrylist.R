GIS.countrylist <- function(){

    temp <- paste0(.libPaths()[1],"/AEGIS/data/Country_list.Rdata")
  load(temp)
  country_list <- Country_list[order(Country_list$USABLE),]
    # 지원, 미지원 구분 필
return(country_list)
}
