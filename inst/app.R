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
                                 menuItem("Clustering",tabName = "Clustering" ),
                                 menuItem("Interactive disease map(beta)", tabName = "Leaflet(beta)" )
    )
    ),
    dashboardBody(tabItems(
      tabItem(tabName = "db",
              fluidRow(
                titlePanel("Database Connection"),
                sidebarPanel(
                  textInput("ip","IP","")
                  ,uiOutput("sqltype")
                  ,textInput("CDMschema","CDM Database schema","")
                  ,textInput("Resultschema","CDM Result schema","")
                  ,textInput("usr","USER","")
                  ,passwordInput("pw","PASSWORD","")
                  ,textInput('WebapiDBserver','WebAPI DB Server IP','')
                  ,textInput('WebapiDBname','WebAPI DB Name','')
                  ,textInput('WebapiDBschema','WebAPI DB Schema','')
                  #input text to db information
                  ,actionButton("db_load","Load DB")

                  ,width=2
                ),
                mainPanel(
                  verbatimTextOutput("txt"),
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
                  ,radioButtons("GIS.Age","Age and gender adjust",choices = c("No" = "no", "Yes"="yes"))
                  ,numericInput("GIS.timeatrisk_startdt","Define the time-at-risk window start, relative to target cohort entry:", 0, min=0)
                  ,numericInput("GIS.timeatrisk_enddt","Define the time-at-risk window end:", 0, min=0)
                  ,selectInput("GIS.timeatrisk_enddt_panel","GIS.timeatrisk_enddt_panel", choices =
                                 c("from cohort start date" = "cohort_start_date","from cohort end date" = "cohort_end_date"),selected = "cohort_end_date")
                  ,textInput("fraction","fraction",100000)
                  ,uiOutput("country_list")
                  ,actionButton("submit_table","submit")
                  ,width=2
                ),
                mainPanel
                (dataTableOutput('GIS.table'))
              )
      ),
      tabItem(tabName = "Disease_mapping",
              fluidRow(
                titlePanel("Disease mapping setting"),
                sidebarPanel(
                  radioButtons("GIS.level","Administrative level",choices = c("Level 2" = 1, "Level 3" = 2),selected = 2)
                  #radioButtons("GIS.level","Administrative level",choices = c("Level 1" = 0, "Level 2" = 1, "Level 3" = 2),selected = 1)
                  ,radioButtons("GIS.distribution","Select distribution options", choices = c("Count of the target cohort (n)" = "count","Propotion" = "proportion", "Standardized Incidence Ratio"="SIR", "Bayesian mapping"="BYM"),selected = "count")
                  #,radioButtons("distinct","Select distinct options", c("Yes" = "distinct","No" = "" ),inline= TRUE)
                  ,textInput("plot.title","title"," ")
                  ,textInput("plot.legend","legend"," ")
                  ,actionButton("submit_plot","submit") #Draw plot button
                  ,width=2
                ),
                mainPanel(
                  #verbatimTextOutput("test")
                  #,
                  leafletOutput("GIS.plot")
                  #,textOutput("text")
                )
              )
      ),

      tabItem(tabName = "Leaflet(beta)",
              fluidRow(
                titlePanel("Interactive disease map(beta)"),
                mainPanel(
                  leafletOutput("mappingLeaflet")
                )
              )
      ),


      tabItem(tabName ="Clustering",
              fluidRow(
                titlePanel("Disease clustering"),
                sidebarPanel(
                  radioButtons("Cluster.method","Cluster Method",choices = c("Local Moran's I" = "moran", "Kulldorff's method" = "kulldorff"))
                  ,textInput("Cluster.parameter","Kulldorff's method parameter", "0.1")
                  ,actionButton("submit_cluster","submit") #Draw plot button
                  ,width=2
                ),
                mainPanel(
                  dataTableOutput('Cluster.table')
                  ,plotOutput("Cluster.plot")
                  ,textOutput("Cluster.test")
                )
              )
      )
    )
    )
  ),


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
      cohort_list <<- Call.Cohortlist(input$WebapiDBserver,input$WebapiDBname,input$WebapiDBschema,input$Resultschema)
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
      country_list <- GIS.countrylist()
      selectInput("country", "Select country", choices = country_list[,1])
    })

    ##cohort###################################################
    render.table <- eventReactive(input$submit_table,{

        country_list <- GIS.countrylist()
        MAX.level <- country_list[country_list$NAME==input$country,3]
        GADM <<- GIS.download(input$country, MAX.level)
        GADM.table <<- GADM[[3]]@data

        tcdi <- substr(input$tcdi,1,gregexpr(' ',input$tcdi)[[1]][1]-1)
        ocdi <- substr(input$ocdi,1,gregexpr(' ',input$ocdi)[[1]][1]-1)

        #Conditional input cohort number
        CDM.table <<- GIS.extraction(input$CDMschema, input$Resultschema, targettab="cohort", input$dateRange[1], input$dateRange[2],
                                     tcdi, ocdi, input$fraction, input$GIS.timeatrisk_startdt, input$GIS.timeatrisk_enddt, input$GIS.timeatrisk_enddt_panel)
        table <- dplyr::left_join(CDM.table, GADM.table, by=c("gadm_id" = "ID_2"))
        switch(input$GIS.Age,
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
    ##cohort###################################################

    ##disease##################################################

    draw.plot <- eventReactive(input$submit_plot,{
        countdf_level <<- GIS.calc1(GADM.table,CDM.table,input$GIS.level, input$GIS.distribution, input$GIS.Age)
        mapdf <<- GIS.calc2(countdf_level,GADM,input$GIS.level, input$fraction)
        plot <- leafletMapping(as.numeric(input$GIS.level))

    })


    output$GIS.plot <- renderLeaflet ({
      draw.plot()
    })


    ##disease##################################################

    ## End of server
  }, options = list(height = 1000)
)

