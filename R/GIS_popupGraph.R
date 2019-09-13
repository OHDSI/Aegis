GIS.popupGraph <- function(idxList){

  sql <- "select cast(year_of_birth as int) as year_of_birth, cast(gender_concept_id as int) as gender_concept_id,
            cast(location_id as int) as location_id from @cdmDatabaseSchema.person
            where location_id in (@idxList)"
  sql <- SqlRender::renderSql(sql,
                              cdmDatabaseSchema=CDMschema,
                              idxList=idxList)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect=connectionDetails$dbms)$sql
  person <- DatabaseConnector::querySql(connection, sql)
  colnames(person) <- tolower(colnames(person))
  person$gender_concept_id <- factor(person$gender_concept_id)

    mu <- data.frame(person %>%
                     group_by(gender_concept_id) %>%
                     summarise(mean = mean(year_of_birth)))

  p<-ggplot(person, aes(x=year_of_birth, color=gender_concept_id)) +
    geom_density(alpha=0.3, aes(fill=gender_concept_id))
  p.data <- ggplot_build(p)$data[[1]]
  p.text <- lapply(split(p.data, f = p.data$group), function(df){
    df[which.max(df$scaled), ]
  })
  p.text <- do.call(rbind, p.text)
  p.text$rate <- p.text$n/sum(p.text$n)
  p <- p + annotate('text', x = p.text$x, y = p.text$y,
                    label = sprintf('n = %d, (%.2f)', p.text$n, p.text$rate), vjust = 0, col = c("red", "blue"))

  return(p)
}


# p2 <- p + annotate(geom = "text", label = expression(atop("Location id:"~bar(x), "under"~H[0])),
#                    x = -Inf, y = Inf, hjust = 0, vjust = 1,
#                    color = "red")
#
#
# prePopup = paste("<strong>Location id: </strong>", preTable[idxNum,"location_id"], "<br>",
#                  "<strong>GADM id: </strong>", preTable[idxNum,"objectid"], "<br>",
#                  "<strong>Coordinates: </strong>", preTable[idxNum,"longitude"], ", ", preTable[idxNum,"latitude"], "<br>",
#                  "<strong>Administrative name: </strong>", preGADM@data[,idxName])
