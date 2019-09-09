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
                                 menuItem("Clustering",tabName = "Clustering" )
    )
    ),
    dashboardBody(tabItems(
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
                  #input text to db information
                  ,actionButton("db_load","Load DB")

                  ,width=2
                ),
                mainPanel
                (
                  tableOutput("view")
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
                  ,uiOutput("country_list")
                  ,actionButton("submit_table","submit")
                  ,width=2
                ),
                mainPanel
                (
                  dataTableOutput('GIS.table')
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
              )

      )
  )
  ),

  #define server for dataset handling and spatial statistical calculation
  server <- function(input, output,session)
  {

    #DataBase Connect
    cohort_listup <- eventReactive(input$db_load, {
      connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms=input$sqltype,
                                                                       server=input$ip,
                                                                       schema=input$Resultschema,
                                                                       user=input$usr,
                                                                       password=input$pw)
      connection <<- DatabaseConnector::connect(connectionDetails)
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

    output$country_list <- renderUI({
      country_list <<- GIS.countrylist()
      selectInput("country", "Select country", choices = country_list[,1])
    })


    ##define Administrative level##############################
    output$GIS.level<-renderUI({

      maxLevel <- country_list[country_list$NAME == input$country,]$MAX_LEVEL
      radioButtons("GIS.level", "Administrative level",
                   choices = c(rep(0:maxLevel)),
                   selected = 0
      )

      })


    ###########################################################


    ##cohort###################################################
    render.table <- eventReactive(input$submit_table,{

        country_list <- GIS.countrylist()
        MAX.level <- country_list[country_list$NAME==input$country,3]
        GADM <<- GIS.download(input$country, MAX.level)
        GADM.table <<- GADM[[MAX.level+1]]@data

        tcdi <<- substr(input$tcdi,1,gregexpr(' ',input$tcdi)[[1]][1]-1)
        ocdi <<- substr(input$ocdi,1,gregexpr(' ',input$ocdi)[[1]][1]-1)

        #Conditional input cohort number
        CDM.table <<- GIS.extraction(connectionDetails,input$CDMschema, input$Resultschema, targettab="cohort", input$dateRange[1], input$dateRange[2],
                                     tcdi, ocdi, input$fraction, input$GIS.timeatrisk_startdt, input$GIS.timeatrisk_enddt, input$GIS.timeatrisk_enddt_panel)

        table <- dplyr::left_join(CDM.table, GADM.table, by=c("gadm_id" = "ID_2"))
        switch(input$GIS.age,
               "no"={
                 table <- table[, c("OBJECTID","ID_0", "ISO", "NAME_0", "ID_1", "NAME_1", "NAME_2",
                                    "target_count", "outcome_count", "crd_expected", "crd_prop", "crd_sir"
                 )]#"ID_2"

               },
               "yes"={
                 table <- table[, c("OBJECTID","ID_0", "ISO", "NAME_0", "ID_1", "NAME_1",  "NAME_2",
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
        countdf_level <<- GIS.calc1(GADM.table, CDM.table, input$GIS.level, input$GIS.distribution, input$GIS.age)
        mapdf <<- GIS.calc2(countdf_level,GADM,input$GIS.level, input$fraction)
        tableProxy <<- AEGIS::leafletMapping(as.numeric(input$GIS.level), input$GIS.age, input$GIS.distribution, input$country)

          #Color to fill the polygons
          pal <- colorQuantile(input$colorsMapping, domain=tableProxy@data$mappingEstimate,
                               n=7, probs = seq(0, 1, length.out = 8), na.color = "#FFFFFF",
                               alpha = FALSE, reverse = FALSE)
          #Estimates into pop up objects
          if (input$GIS.age == "yes"){
            polygon_popup <- paste0("<strong>Name: </strong>", tableProxy@data$idxName, "<br>",
                                    "<strong>Target: </strong>", tableProxy@data$target_count, "<br>",
                                    "<strong>Outcome: </strong>", tableProxy@data$outcome_count, "<br>",
                                    "<strong>SIR: </strong>", round(tableProxy@data$std_sir, 2), " (", round(tableProxy@data$std_sirlower, 2), "-", round(tableProxy@data$std_sirupper, 2), ")", "<br>",
                                    "<strong>Proportion: </strong>", round(tableProxy@data$std_prop, 2), " (", round(tableProxy@data$std_proplower, 2), "-", round(tableProxy@data$std_propupper, 2), ")")
          } else {
            polygon_popup <- paste0("<strong>Name: </strong>", tableProxy@data$idxName, "<br>",
                                    "<strong>Target: </strong>", tableProxy@data$target_count, "<br>",
                                    "<strong>Outcome: </strong>", tableProxy@data$outcome_count, "<br>",
                                    "<strong>SIR: </strong>", round(tableProxy@data$crd_sir, 2), " (", round(tableProxy@data$crd_sirlower, 2), "-", round(tableProxy@data$crd_sirupper, 2), ")", "<br>",
                                    "<strong>Proportion: </strong>", round(tableProxy@data$crd_prop, 2), " (", round(tableProxy@data$crd_proplower, 2), "-", round(tableProxy@data$crd_propupper, 2), ")")
          }
          #Incremental changes to the map
          leafletProxy("GIS.leafletMapping", data = tableProxy) %>%
            clearShapes() %>%
            clearControls() %>%
            addPolygons(#data = tableProxy,
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
    draw.leafletClustering <- eventReactive(input$submit_cluster,{
      plot <- Cluster.plot(input$Cluster.method, input$Cluster.parameter,input$GIS.age, input$country,GADM,as.numeric(input$GIS.level))
    })

    output$GIS.leafletClusering <- renderLeaflet({
      if(!exists("GADM")){
        m <- leaflet() %>%
          addTiles %>%
          fitBounds (
            lng1="-179.1506", lng2="179.7734",
            lat1="18.90986", lat2="72.6875")
      } else {
        m <- leaflet() %>%
          addTiles %>%
          fitBounds (
            lng1=GADM[[1]]@bbox[1,1], lng2=GADM[[1]]@bbox[1,2],
            lat1=GADM[[1]]@bbox[2,1], lat2=GADM[[1]]@bbox[2,2])
      }
    })
    ##end of clustering###############################################

    ## End of server
  }, options = list(height = 1000)
 )
