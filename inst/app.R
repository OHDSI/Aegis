##install&require packages
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
  else
  {
    require(x,character.only=TRUE)
  }
}

options(expressions=500000)

packages(DCluster)
packages(AEGIS)
packages(raster)
packages(maps)
packages(mapdata)
packages(mapproj)
packages(leaflet)
packages(ggmap)
packages(dplyr)
packages(sqldf)
packages(shiny)
packages(bindrcpp)
packages(pkgconfig)
packages(shinyjs)
packages(SqlRender)
packages(DatabaseConnector)
packages(shinydashboard)
packages(maptools)
packages(SpatialEpi)
packages(lubridate)
packages(rgdal)
packages(gpclib)
packages(rgeos)
packages(RColorBrewer)
packages(DT)
packages(leafpop)

#INLA package is very heavy
#So, INLA packages must be downloaded separately
if(isTRUE(which(installed.packages()[,1] %in% "INLA")>1)){
  library(INLA)
}

gpclibPermit()
Sys.setlocale(category = "LC_ALL", locale = "us")

shinyApp(
  # Define UI for dataset viewer application
  ui <- dashboardPage(
    dashboardHeader(title = "AEGIS"),
    dashboardSidebar(sidebarMenu(menuItem("DB connection",tabName= "db" ),
                                 menuItem("Cohorts", tabName = "Cohorts" ),
                                 menuItem("Disease mapping", tabName = "Disease_mapping" ),
                                 menuItem("Clustering",tabName = "Clustering" ),
                                 menuItem("Factors",tabName = "Factors")
    )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "db",
                fluidRow(
                  titlePanel("Database Connection"),
                  sidebarPanel(
                    textInput("ip","IP","",placeholder = 'xxx.xxx.xxx.xxx')
                    ,uiOutput("sqltype")
                    ,textInput("CDMschema","CDM Database schema","",placeholder = 'yourCdmDb.schema')
                    ,textInput("Resultschema","CDM Result schema","",placeholder = 'yourCdmResultDb.schema')
                    ,textInput("usr","USER","")
                    ,passwordInput("pw","PASSWORD","")
                    ,textInput('WebapiDBserver','WebAPI DB Server IP','',placeholder = 'xxx.xxx.xxx.xxx')
                    ,textInput('WebapiDBschema','WebAPI DB Schema','',placeholder = 'yourWebAPIDb.schema')
                    ,uiOutput("country_list")
                    #input text to db information
                    ,actionButton("db_load","Load DB")

                    ,width=2
                  ),
                  mainPanel
                  (
                    div(dataTableOutput('GIS.preTable'),style='font-size: 70%;'),
                    leafletOutput(outputId = "GIS.leafletPre", height = 400),
                    plotOutput(outputId = "GIS.demographics")
                  )
                )
        ),
        tabItem(tabName = "Cohorts",
                fluidRow(
                  titlePanel("Cohort selection"),
                  sidebarPanel(
                    uiOutput("cohort_tcdi")
                    ,uiOutput("cohort_ocdi")
                    ,hr()
                    ,dateRangeInput(inputId = "dateRange", label = "Select Windows",  start = "2002-01-01", end = "2013-12-31")
                    ,hr()
                    ,radioButtons("GIS.age","Age and gender adjust",choices = c("No" = "no", "Yes"="yes"))
                    ,numericInput("GIS.timeatrisk_startdt","Define the time-at-risk window start, relative to target cohort entry:", 0, min=0)
                    ,numericInput("GIS.timeatrisk_enddt","Define the time-at-risk window end:", 99999, min=0)
                    ,selectInput("GIS.timeatrisk_enddt_panel","GIS.timeatrisk_enddt_panel", choices =
                                   c("from cohort start date" = "cohort_start_date","from cohort end date" = "cohort_end_date"),selected = "cohort_end_date")
                    ,textInput("fraction","fraction",100000)
                    ,actionButton("submit_table","submit")
                    ,width=2
                  ),
                  mainPanel
                  (
                    div(dataTableOutput('GIS.table'),style='font-size: 90%;')
                  )
                )
        ),
        tabItem(tabName = "Disease_mapping",
                fluidRow(
                  titlePanel("Disease mapping setting"),
                  sidebarPanel(
                    uiOutput("GIS.level")
                    ,radioButtons("GIS.distribution","Select distribution options", choices = c("Count of the target cohort (n)" = "count","Propotion" = "proportion", "Standardized Incidence Ratio"="SIR", "Bayesian mapping"="BYM"),selected = "count")
                    ,selectInput("colorsMapping", "Color Scheme",
                                 rownames(subset(brewer.pal.info, category %in% c("seq", "div"))))
                    ,actionButton("submit_plot","submit") #Draw plot button
                    ,width=2
                  ),
                  mainPanel(
                    leafletOutput(outputId = "GIS.leafletMapping", height = 800)
                  )
                )
        ),
        tabItem(tabName ="Clustering",
                fluidRow(
                  titlePanel("Disease clustering"),
                  sidebarPanel(
                    radioButtons("Cluster.method","Cluster Method",choices = c("Kulldorff's method" = "kulldorff"))
                    ,textInput("Cluster.parameter","Kulldorff's method parameter", "0.1")
                    ,selectInput("colorsClustering", "Color Scheme",
                                 rownames(subset(brewer.pal.info, category %in% c("seq", "div"))))
                    ,actionButton("submit_cluster","submit") #Draw plot button
                    ,width=2
                  ),
                  mainPanel(
                    leafletOutput(outputId = "GIS.leafletClustering", height = 800)
                  )
                )
        ),
        tabItem(tabName = "Factors",
                fluidRow(
                  titlePanel("Other factors"),
                  sidebarPanel(
                    # Input: Select a file ----
                    fileInput("file1", "Choose CSV File",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    # Horizontal line ----
                    tags$hr(),
                    uiOutput("cha_lon"),
                    uiOutput("cha_lat"),
                    uiOutput("cha_value"),
                    selectInput("colorsMappingCha", "Color Scheme",
                                rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
                    radioButtons("cha_BM","Bayesian mapping",choices = c("No" = "no", "Yes"="yes")),
                    actionButton("submit_cha_plot", "submit"),
                    width=2
                  ),
                  mainPanel(
                    leafletOutput(outputId = "GIS.leafletCHA", height = 800)
                  )
                )
        )
      )
    )
  ),

  #define server for dataset handling and spatial statistical calculation
  server <- function(input, output,session)
  {
    #country list
    output$country_list <- renderUI({
      output$country_list <- renderUI({
        country_list <- GIS.countrylist()
        selectInput("country", "Select country", choices = country_list[,1])
      })
    })

    ################
    render.preTable <- eventReactive(input$db_load,{
      connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms=input$sqltype,
                                                                       server=input$ip,
                                                                       schema=input$Resultschema,
                                                                       user=input$usr,
                                                                       password=input$pw)
      connection <<- DatabaseConnector::connect(connectionDetails)

      GADM <<- GIS.download(input$country, maxLevel)
      preTable <<- PIP(input$CDMschema, maxLevel, input$country)
      country <<- input$country
      preTable
    })

    output$GIS.preTable <- renderDataTable(
      DT::datatable(render.preTable(), options = list(pageLength = 7,
                                                      autoWidth = TRUE,
                                                      columnDefs = list(list(width='200px',targets='_all'))
      ),
      selection = "single")
    )

    output$GIS.leafletPre <- renderLeaflet({
      if(!exists("GADM")){
        leaflet() %>%
          addTiles %>%
          fitBounds (
            lng1="-179.1506", lng2="179.7734",
            lat1="18.90986", lat2="72.6875")
      } else {
        leaflet() %>%
          addTiles %>%
          fitBounds (
            lng1=GADM[[1]]@bbox[1,1], lng2=GADM[[1]]@bbox[1,2],
            lat1=GADM[[1]]@bbox[2,1], lat2=GADM[[1]]@bbox[2,2])
      }
    })


    observeEvent(input$GIS.preTable_rows_selected, {

      row_selected <- render.preTable()[input$GIS.preTable_rows_selected,]

      idxNum <- row_selected[,"objectid"]
      idxName <- paste0("NAME_", maxLevel)
      idxList <- preTable[which(preTable[,"objectid"]==idxNum),1]
      lng <- row_selected[,"longitude"]
      lat <- row_selected[,"latitude"]

      preGADM <- GADM[[3]][idxNum,]

      sql <- "select cast(year_of_birth as int) as year_of_birth, cast(gender_concept_id as int) as gender_concept_id,
      cast(location_id as int) as location_id from @cdmDatabaseSchema.person
      where location_id in (@idxList)"
      sql <- SqlRender::renderSql(sql,
                                  cdmDatabaseSchema=input$CDMschema,
                                  idxList=idxList)$sql
      sql <- SqlRender::translateSql(sql,
                                     targetDialect=connectionDetails$dbms)$sql
      person <- DatabaseConnector::querySql(connection, sql)
      colnames(person) <- tolower(colnames(person))
      person$gender_concept_id <- factor(person$gender_concept_id)

      mu <- data.frame(person %>%
                         group_by(gender_concept_id) %>%
                         summarise(mean = mean(year_of_birth)))

      p <- ggplot(person, aes(x=year_of_birth, color=gender_concept_id)) +
        geom_density(alpha=0.3, aes(fill=gender_concept_id))
      p.data <- ggplot_build(p)$data[[1]]
      p.text <- lapply(split(p.data, f = p.data$group), function(df){
        df[which.max(df$scaled), ]
      })
      p.text <- do.call(rbind, p.text)
      p.text$rate <- p.text$n/sum(p.text$n)
      p <- p + annotate('text', x = p.text$x, y = p.text$y,
                        label = sprintf('n = %d, (%.2f)', p.text$n, p.text$rate), vjust = 0, col = c("red", "blue"))

      proxy <- leafletProxy('GIS.leafletPre')
      # prePopup = paste("<strong>Location id: </strong>", preTable[idxNum,"location_id"], "<br>",
      #                "<strong>GADM id: </strong>", preTable[idxNum,"objectid"], "<br>",
      #                "<strong>Coordinates: </strong>", preTable[idxNum,"longitude"], ", ", preTable[idxNum,"latitude"], "<br>",
      #                "<strong>Administrative name: </strong>", preGADM@data[,idxName])

      #   proxy %>%
      #     clearShapes() %>%
      #     clearControls() %>%
      #       addPolygons(data = preGADM,
      #         fillColor= "yellow",
      #         fillOpacity = 0.5,
      #         weight = 1,
      #         color = "black",
      #         dashArray = "3",
      #         popup = popupGraph(p),
      #         highlight = highlightOptions(
      #           weight = 5,
      #           color = "#666",
      #           dashArray = "",
      #           fillOpacity = 0.7,
      #           bringToFront = TRUE)) %>%
      #             setView(lng = lng, lat = lat, zoom = 11)
      #
      # })

      proxy %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = preGADM,
                    fillColor= "yellow",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    dashArray = "3",
                    popup = leafpop::popupGraph(p),
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE)) %>%
        setView(lng = lng, lat = lat, zoom = 11)

    })
    ####################

    cohort_listup <- eventReactive(input$db_load, {
      cohort_list <<- Call.Cohortlist(input$WebapiDBserver,input$WebapiDBschema,input$Resultschema)
    })

    output$sqltype <- renderUI({
      selectInput("sqltype", "Select DBMS",
                  choices = c(
                    "sql server" = "sql server",
                    "PostgreSQL" = "postresql",
                    "Amazon Redshift" = "redshift",
                    "Microsoft Parallel Data Warehouse" = "pdw",
                    "IBM Netezza" = "netezza",
                    "Google BigQuery" = "bigquery"
                  )
      )
    })

    output$cohort_tcdi <- renderUI({
      cohort_list <- cohort_listup()
      selectInput("tcdi", "Select target cohort", choices = cohort_list)
    })

    output$cohort_ocdi <- renderUI({
      cohort_list <- cohort_listup()
      selectInput("ocdi", "Select target cohort", choices = cohort_list)
    })


    ##define Administrative level##############################
    output$GIS.level<-renderUI({
      maxLevel <<- country_list[country_list$NAME == input$country,]$MAX_LEVEL
      radioButtons("GIS.level", "Administrative level",
                   choices = c(rep(1:maxLevel)),
                   selected = maxLevel
      )
    })

    ###########################################################


    ##cohort###################################################
    render.table <- eventReactive(input$submit_table,{

      country_list <<- GIS.countrylist()
      GADM.table <<- GADM[[maxLevel+1]]@data

      tcdi <<- substr(input$tcdi,1,gregexpr(' ',input$tcdi)[[1]][1]-1)
      ocdi <<- substr(input$ocdi,1,gregexpr(' ',input$ocdi)[[1]][1]-1)


      cdmDatabaseSchema <<- input$CDMschema
      resultDatabaseSchema <<- input$Resultschema
      startdt <<- input$dateRange[1]
      enddt <<- input$dateRange[2]
      timeatrisk_startdt <<- input$GIS.timeatrisk_startdt
      timeatrisk_enddt <<- input$GIS.timeatrisk_enddt
      timeatrisk_enddt_panel <<- input$GIS.timeatrisk_enddt_panel

      #Conditional input cohort number
      CDM.table <<- GIS.extraction(connectionDetails,input$CDMschema, input$Resultschema, targettab="cohort", input$dateRange[1], input$dateRange[2],
                                   tcdi, ocdi, input$fraction, input$GIS.timeatrisk_startdt, input$GIS.timeatrisk_enddt, input$GIS.timeatrisk_enddt_panel, maxLevel)

      table <- dplyr::left_join(CDM.table, GADM.table, by=c("gadm_id" = "ID_2"))
      switch(input$GIS.age,
             "no"={
               table <- table[, c("ID_1", "NAME_1", "NAME_2",
                                  "target_count", "outcome_count", "crd_expected", "crd_prop", "crd_sir"
               )]#"ID_2"

             },
             "yes"={
               table <- table[, c("ID_1", "NAME_1",  "NAME_2",
                                  "target_count", "outcome_count", "std_expected", "std_prop", "std_sir"
               )]#"ID_2",

             }
      )
      table
    })

    output$GIS.table <- renderDataTable(
      render.table()
    )
    ##end of cohort###################################################

    ##disease##################################################
    output$GIS.leafletMapping <- renderLeaflet({
      if(!exists("GADM")){
        leaflet() %>%
          addTiles %>%
          fitBounds (
            lng1="-179.1506", lng2="179.7734",
            lat1="18.90986", lat2="72.6875")
      } else {
        leaflet() %>%
          addTiles %>%
          fitBounds (
            lng1=GADM[[1]]@bbox[1,1], lng2=GADM[[1]]@bbox[1,2],
            lat1=GADM[[1]]@bbox[2,1], lat2=GADM[[1]]@bbox[2,2])
      }
    })

    observeEvent(input$submit_plot,{
      #countdf_level <<- GIS.calc1(GADM.table, CDM.table, input$GIS.level, input$GIS.distribution, input$GIS.age)
      GIS.level <<- as.numeric(input$GIS.level)
      GIS.age <<- input$GIS.age
      tableProxy <<- AEGIS::leafletMapping(as.numeric(input$GIS.level), input$GIS.age, input$GIS.distribution, input$country)

      idxNum <- paste0("ID_", input$GIS.level)
      idxName <- paste0("NAME_", input$GIS.level)

      #Color to fill the polygons
      if (length(tableProxy)==1) {
        pal <- colorQuantile(input$colorsMapping, domain=tableProxy@data$mappingEstimate,
                             n=1, probs = seq(0, 1, length.out = 2), na.color = "#FFFFFF",
                             alpha = FALSE, reverse = FALSE)
      } else {
        # pal <- colorQuantile(input$colorsMapping, domain=tableProxy@data$mappingEstimate,
        #                      n=10, probs = seq(0, 1, length.out = 11), na.color = "#FFFFFF",
        #                      alpha = FALSE, reverse = FALSE)

        quantileNum <- 10

        probs <- seq(0, 1, length.out = quantileNum + 1)
        bins <- quantile(tableProxy@data$mappingEstimate, probs, na.rm = TRUE, names = FALSE)

        while (length(unique(bins)) != length(bins)) {
          quantileNum <- quantileNum - 1
          probs <- seq(0, 1, length.out = quantileNum + 1)
          bins <- quantile(tableProxy@data$mappingEstimate, probs, na.rm = TRUE, names = FALSE)
        }

        pal <- colorBin(input$colorsMapping, bins = bins)

      }

      #Estimates into pop up objects
      # if (input$GIS.age == "yes"){
      #   polygon_popup <- paste0("<strong>Name: </strong>", tableProxy@data[,idxName], "<br>",
      #                           "<strong>Target: </strong>", tableProxy@data$target_count, "<br>",
      #                           "<strong>Outcome: </strong>", tableProxy@data$outcome_count, "<br>",
      #                           "<strong>SIR: </strong>", round(tableProxy@data$std_sir, 2), " (", round(tableProxy@data$std_sirlower, 2), "-", round(tableProxy@data$std_sirupper, 2), ")", "<br>",
      #                           "<strong>Proportion: </strong>", round(tableProxy@data$std_prop, 2), " (", round(tableProxy@data$std_proplower, 2), "-", round(tableProxy@data$std_propupper, 2), ")")
      # } else {
      #   polygon_popup <- paste0("<strong>Name: </strong>", tableProxy@data[,idxName], "<br>",
      #                           "<strong>Target: </strong>", tableProxy@data$target_count, "<br>",
      #                           "<strong>Outcome: </strong>", tableProxy@data$outcome_count, "<br>",
      #                           "<strong>SIR: </strong>", round(tableProxy@data$crd_sir, 2), " (", round(tableProxy@data$crd_sirlower, 2), "-", round(tableProxy@data$crd_sirupper, 2), ")", "<br>",
      #                           "<strong>Proportion: </strong>", round(tableProxy@data$crd_prop, 2), " (", round(tableProxy@data$crd_proplower, 2), "-", round(tableProxy@data$crd_propupper, 2), ")")
      # }
      if (input$GIS.age == "yes"){
        polygon_popup <- paste0("<html> <head>
                                <style>
                                .leaflet-popup {
                                position: absolute;
                                text-align: center;
                                margin-bottom: 20px;
                                }
                                .leaflet-popup-content-wrapper {
                                padding: 1px;
                                text-align: left;
                                border-radius: 12px;
                                }
                                .leaflet-popup-content {
                                margin: 13px 19px;
                                line-height: 1.4;
                                }
                                .leaflet-popup-content p {
                                margin: 18px 0;
                                }
                                .leaflet-popup-tip-container {
                                width: 40px;
                                height: 20px;
                                position: absolute;
                                left: 50%;
                                margin-left: -20px;
                                overflow: hidden;
                                pointer-events: none;
                                }
                                .leaflet-popup-tip {
                                width: 17px;
                                height: 17px;
                                padding: 1px;

                                margin: -10px auto 0;

                                -webkit-transform: rotate(45deg);
                                -moz-transform: rotate(45deg);
                                -ms-transform: rotate(45deg);
                                -o-transform: rotate(45deg);
                                transform: rotate(45deg);
                                }
                                .leaflet-popup-content-wrapper {
                                background: white;
                                color: #333;
                                box-shadow: 0 3px 14px rgba(0,0,0,0.4);
                                }
                                .leaflet-popup-tip {
                                background: white;
                                color: #333;
                                box-shadow: 0 3px 14px rgba(0,0,0,0.4);
                                }
                                .leaflet-container a.leaflet-popup-close-button {
                                position: absolute;
                                top: 0;
                                right: 0;
                                padding: 4px 4px 0 0;
                                border: none;
                                text-align: center;
                                width: 18px;
                                height: 14px;
                                font: 16px/14px Tahoma, Verdana, sans-serif;
                                color: #c3c3c3;
                                text-decoration: none;
                                font-weight: bold;
                                background: transparent;
                                }
                                .leaflet-container a.leaflet-popup-close-button:hover {
                                color: #999;
                                }
                                .leaflet-popup-scrolled {
                                overflow: auto;
                                border-bottom: 1px solid #ddd;
                                border-top: 1px solid #ddd;
                                }
                                #mapping{
                                font-size: 18px;
      }
    }
                                </style>
                                </head>
                                <body>",
                                "<div id='mapping'>",
                                "<strong>Name: </strong>", tableProxy@data[,idxName], "<br>",
                                "<strong>Target: </strong>", tableProxy@data$target_count, "<br>",
                                "<strong>Outcome: </strong>", tableProxy@data$outcome_count, "<br>",
                                "<strong>SIR: </strong>", round(tableProxy@data$std_sir, 2), " (", round(tableProxy@data$std_sirlower, 2), "-", round(tableProxy@data$std_sirupper, 2), ")", "<br>",
                                "<strong>Proportion: </strong>", round(tableProxy@data$std_prop, 2), " (", round(tableProxy@data$std_proplower, 2), "-", round(tableProxy@data$std_propupper, 2), ")",
                                "</div>",
                                "</body>
                                </html> "
        )
        } else {
          polygon_popup <- paste0("<html> <head>
                                  <style>
                                  .leaflet-popup {
                                  position: absolute;
                                  text-align: center;
                                  margin-bottom: 20px;
                                  }
                                  .leaflet-popup-content-wrapper {
                                  padding: 1px;
                                  text-align: left;
                                  border-radius: 12px;
                                  }
                                  .leaflet-popup-content {
                                  margin: 13px 19px;
                                  line-height: 1.4;
                                  }
                                  .leaflet-popup-content p {
                                  margin: 18px 0;
                                  }
                                  .leaflet-popup-tip-container {
                                  width: 40px;
                                  height: 20px;
                                  position: absolute;
                                  left: 50%;
                                  margin-left: -20px;
                                  overflow: hidden;
                                  pointer-events: none;
                                  }
                                  .leaflet-popup-tip {
                                  width: 17px;
                                  height: 17px;
                                  padding: 1px;

                                  margin: -10px auto 0;

                                  -webkit-transform: rotate(45deg);
                                  -moz-transform: rotate(45deg);
                                  -ms-transform: rotate(45deg);
                                  -o-transform: rotate(45deg);
                                  transform: rotate(45deg);
                                  }
                                  .leaflet-popup-content-wrapper {
                                  background: white;
                                  color: #333;
                                  box-shadow: 0 3px 14px rgba(0,0,0,0.4);
                                  }
                                  .leaflet-popup-tip {
                                  background: white;
                                  color: #333;
                                  box-shadow: 0 3px 14px rgba(0,0,0,0.4);
                                  }
                                  .leaflet-container a.leaflet-popup-close-button {
                                  position: absolute;
                                  top: 0;
                                  right: 0;
                                  padding: 4px 4px 0 0;
                                  border: none;
                                  text-align: center;
                                  width: 18px;
                                  height: 14px;
                                  font: 16px/14px Tahoma, Verdana, sans-serif;
                                  color: #c3c3c3;
                                  text-decoration: none;
                                  font-weight: bold;
                                  background: transparent;
                                  }
                                  .leaflet-container a.leaflet-popup-close-button:hover {
                                  color: #999;
                                  }
                                  .leaflet-popup-scrolled {
                                  overflow: auto;
                                  border-bottom: 1px solid #ddd;
                                  border-top: 1px solid #ddd;
                                  }
                                  #mapping{
                                  font-size: 18px;
                                  }
                                  </style>
                                  </head>
                                  <body>",
                                  "<div id='mapping'>",
                                  "<strong>Name: </strong>", tableProxy@data[,idxName], "<br>",
                                  "<strong>Target: </strong>", tableProxy@data$target_count, "<br>",
                                  "<strong>Outcome: </strong>", tableProxy@data$outcome_count, "<br>",
                                  "<strong>SIR: </strong>", round(tableProxy@data$crd_sir, 2), " (", round(tableProxy@data$crd_sirlower, 2), "-", round(tableProxy@data$crd_sirupper, 2), ")", "<br>",
                                  "<strong>Proportion: </strong>", round(tableProxy@data$crd_prop, 2), " (", round(tableProxy@data$crd_proplower, 2), "-", round(tableProxy@data$crd_propupper, 2), ")",
                                  "</div>",
                                  "</body>
                                  </html> ")
          }




      #Incremental changes to the map
      leafletProxy("GIS.leafletMapping", data = tableProxy) %>%
        clearShapes() %>%
        clearControls() %>%
        leaflet::addPolygons(#data = tableProxy,
          fillColor= ~pal(tableProxy@data$mappingEstimate),
          fillOpacity = 0.5,
          weight = 1,
          color = "black",
          dashArray = "3",
          popup = polygon_popup,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE)) %>%
        addLegend(pal = pal, values = tableProxy@data$mappingEstimate, opacity = 0.7, title = NULL,
                  position = "bottomright")
      })

    ##Clustering###############################################


    observeEvent(input$submit_cluster,{


      #Estimates into pop up objects
      clustertableProxy <- leafletClustering(input$Cluster.parameter, input$GIS.age, as.numeric(input$GIS.level), input$colorsClustering)

      if (max(na.omit(clustertableProxy@data$k.cluster))==1) {
        pal <- colorQuantile(input$colorsClustering, domain=tableProxy@data$mappingEstimate,
                             n=1, probs = seq(0, 1, length.out = 2), na.color = "#FFFFFF",
                             alpha = FALSE, reverse = FALSE)
      } else {
        quantileNum <- 10

        probs <- seq(0, 1, length.out = quantileNum + 1)
        bins <- quantile(tableProxy@data$mappingEstimate, probs, na.rm = TRUE, names = FALSE)

        while (length(unique(bins)) != length(bins)) {
          quantileNum <- quantileNum - 1
          probs <- seq(0, 1, length.out = quantileNum + 1)
          bins <- quantile(tableProxy@data$mappingEstimate, probs, na.rm = TRUE, names = FALSE)
        }

        pal <- colorBin(input$colorsClustering, bins = bins)
      }

      clusterpolygon_popup <- paste0("<html> <head>
                                     <style>
                                     .leaflet-popup {
                                     position: absolute;
                                     text-align: center;
                                     margin-bottom: 20px;
                                     }
                                     .leaflet-popup-content-wrapper {
                                     padding: 1px;
                                     text-align: left;
                                     border-radius: 12px;
                                     }
                                     .leaflet-popup-content {
                                     margin: 13px 19px;
                                     line-height: 1.4;
                                     }
                                     .leaflet-popup-content p {
                                     margin: 18px 0;
                                     }
                                     .leaflet-popup-tip-container {
                                     width: 40px;
                                     height: 20px;
                                     position: absolute;
                                     left: 50%;
                                     margin-left: -20px;
                                     overflow: hidden;
                                     pointer-events: none;
                                     }
                                     .leaflet-popup-tip {
                                     width: 17px;
                                     height: 17px;
                                     padding: 1px;

                                     margin: -10px auto 0;

                                     -webkit-transform: rotate(45deg);
                                     -moz-transform: rotate(45deg);
                                     -ms-transform: rotate(45deg);
                                     -o-transform: rotate(45deg);
                                     transform: rotate(45deg);
                                     }
                                     .leaflet-popup-content-wrapper {
                                     background: white;
                                     color: #333;
                                     box-shadow: 0 3px 14px rgba(0,0,0,0.4);
                                     }
                                     .leaflet-popup-tip {
                                     background: white;
                                     color: #333;
                                     box-shadow: 0 3px 14px rgba(0,0,0,0.4);
                                     }
                                     .leaflet-container a.leaflet-popup-close-button {
                                     position: absolute;
                                     top: 0;
                                     right: 0;
                                     padding: 4px 4px 0 0;
                                     border: none;
                                     text-align: center;
                                     width: 18px;
                                     height: 14px;
                                     font: 16px/14px Tahoma, Verdana, sans-serif;
                                     color: #c3c3c3;
                                     text-decoration: none;
                                     font-weight: bold;
                                     background: transparent;
                                     }
                                     .leaflet-container a.leaflet-popup-close-button:hover {
                                     color: #999;
                                     }
                                     .leaflet-popup-scrolled {
                                     overflow: auto;
                                     border-bottom: 1px solid #ddd;
                                     border-top: 1px solid #ddd;
                                     }
                                     </style>
                                     </head>
                                     <body>",
                                     "<strong>Cluster: </strong>", clustertableProxy@data$k.cluster,"<br>",
                                     "<strong>population: </strong>", clustertableProxy@data$population,"<br>",
                                     "<strong>number.of.cases: </strong>", clustertableProxy@data$number.of.cases,"<br>",
                                     "<strong>expected.cases: </strong>", round(clustertableProxy@data$expected.cases,4),"<br>",
                                     "<strong>SMR: </strong>", round(clustertableProxy@data$SMR,4),"<br>",
                                     "<strong>log.likelihood.ratio: </strong>", round(clustertableProxy@data$log.likelihood.ratio,4),"<br>",
                                     "<strong>p.value: </strong>", round(clustertableProxy@data$p.value,3),"<br>"
                                     ,"</body>
                                     </html> ")


      paste0("<strong>Cluster: </strong>", clustertableProxy@data$k.cluster,"<br>",
             "<strong>population: </strong>", clustertableProxy@data$population,"<br>",
             "<strong>number.of.cases: </strong>", clustertableProxy@data$number.of.cases,"<br>",
             "<strong>expected.cases: </strong>", round(clustertableProxy@data$expected.cases,4),"<br>",
             "<strong>SMR: </strong>", round(clustertableProxy@data$SMR,4),"<br>",
             "<strong>log.likelihood.ratio: </strong>", round(clustertableProxy@data$log.likelihood.ratio,4),"<br>",
             "<strong>p.value: </strong>", round(clustertableProxy@data$p.value,3),"<br>"
      )
      #Incremental changes to the map
      leafletProxy("GIS.leafletClustering", data = clustertableProxy) %>%
        clearShapes() %>%
        clearControls() %>%
        leaflet::addPolygons(#data = tableProxy,
          fillColor= ~pal(clustertableProxy@data$k.cluster),
          fillOpacity = 0.5,
          weight = 1,
          color = "black",
          dashArray = "3",
          popup = clusterpolygon_popup,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE)) %>%
        addLegend(pal = pal, values = clustertableProxy@data$k.cluster, opacity = 0.7, title = NULL,
                  position = "bottomright")
    })

    output$GIS.leafletClustering <- renderLeaflet({
      if(!exists("GADM")){
        leaflet() %>%
          addTiles %>%
          fitBounds (
            lng1="-179.1506", lng2="179.7734",
            lat1="18.90986", lat2="72.6875")
      } else {
        leaflet() %>%
          addTiles %>%
          fitBounds (
            lng1=GADM[[1]]@bbox[1,1], lng2=GADM[[1]]@bbox[1,2],
            lat1=GADM[[1]]@bbox[2,1], lat2=GADM[[1]]@bbox[2,2])
      }
    })
    ##end of clustering###############################################


    output$cha_lon <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("cha_lon", "Select longitude column", choices = items)
    })

    output$cha_lat <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("cha_lat", "Select latitude column", choices = items)
    })

    output$cha_value <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("cha_value", "Select value column", choices = items)
    })

    ################ characteristics

    observeEvent(input$submit_cha_plot,{

      #Estimates into pop up objects
      cha_df <- filedata()
      if (is.null(df)) return(NULL)

      chatableProxy <<- GIS.characteristicsFigure(extraCSV=cha_df, input$cha_lon, input$cha_lat, input$cha_value, input$cha_BM)

      if(input$cha_BM=="yes"){
        chatableProxy$value <- chatableProxy$RRmean
      }

      if (length(chatableProxy)==1) {
        pal <- colorQuantile(input$colorsMappingCha, domain=chatableProxy@data$value,
                             n=1, probs = seq(0, 1, length.out = 2), na.color = "#FFFFFF",
                             alpha = FALSE, reverse = FALSE)
      } else {
        # pal <- colorQuantile(input$colorsMapping, domain=tableProxy@data$mappingEstimate,
        #                      n=10, probs = seq(0, 1, length.out = 11), na.color = "#FFFFFF",
        #                      alpha = FALSE, reverse = FALSE)

        quantileNum <- 10

        probs <- seq(0, 1, length.out = quantileNum + 1)
        bins <- quantile(chatableProxy@data$value, probs, na.rm = TRUE, names = FALSE)

        while (length(unique(bins)) != length(bins)) {
          quantileNum <- quantileNum - 1
          probs <- seq(0, 1, length.out = quantileNum + 1)
          bins <- quantile(chatableProxy@data$value, probs, na.rm = TRUE, names = FALSE)
        }

        pal <- colorBin(input$colorsMappingCha, bins = bins)

      }

      idxNum <- paste0("ID_", maxLevel)
      idxName <- paste0("NAME_", maxLevel)

      cha_popup <- paste0("<strong>Name: </strong>", chatableProxy@data[,idxName], "<br>",
                          "<strong>Target: </strong>", chatableProxy@data$target_count, "<br>",
                          "<strong>Outcome: </strong>", chatableProxy@data$outcome_count, "<br>",
                          "<strong>value: </strong>", round(chatableProxy@data$value, 2), " (", round(chatableProxy@data$value, 2), "-", round(chatableProxy@data$value, 2), ")")

      #Incremental changes to the map
      leafletProxy("GIS.leafletCHA", data = chatableProxy) %>%
        clearShapes() %>%
        clearControls() %>%
        leaflet::addPolygons(#data = tableProxy,
          fillColor= ~pal(chatableProxy@data$value),
          fillOpacity = 0.5,
          weight = 1,
          color = "black",
          dashArray = "3",
          popup = cha_popup,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE)) %>%
        addLegend(pal = pal, values = chatableProxy@data$value, opacity = 0.7, title = NULL,
                  position = "bottomright")
    })


    output$GIS.leafletCHA <- renderLeaflet({
      if(!exists("GADM")){
        leaflet() %>%
          addTiles %>%
          fitBounds (
            lng1="-179.1506", lng2="179.7734",
            lat1="18.90986", lat2="72.6875")
      } else {
        leaflet() %>%
          addTiles %>%
          fitBounds (
            lng1=GADM[[1]]@bbox[1,1], lng2=GADM[[1]]@bbox[1,2],
            lat1=GADM[[1]]@bbox[2,1], lat2=GADM[[1]]@bbox[2,2])
      }
    })


    ################

    output$CHA.plot <- renderPlot ({
      draw.chaPlot()
    }, width = 1280, height = 1024, res = 100)

    filedata <- reactive({
      infile <- input$file1
      if (is.null(infile)){
        return(NULL)
      }
      read.csv(infile$datapath)
    })

    ## End of server
    }, options = list(height = 1000)
    )
