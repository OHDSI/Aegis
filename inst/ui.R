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
#Sys.setlocale(category = "LC_ALL", locale = "us")

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
                  textInput("ip","IP","")
                  ,uiOutput("sqltype")
                  ,textInput("CDMschema","CDM Database schema","")
                  ,textInput("Resultschema","CDM Result schema","")
                  ,textInput("usr","USER","")
                  ,passwordInput("pw","PASSWORD","")
                  #input text to db information
                  ,actionButton("db_load","Load DB")
                  #,hr()
                  #,uiOutput("db_conn")
                  #,actionButton("cohort_load","Load cohort")
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
                  ,radioButtons("GIS.Age","Age-adjustment",choices = c("No" = "no", "Indirect"="indrect", "Direct" = "direct"))
                  ,numericInput("GIS.timeatrisk_startdt","Define the time-at-risk window start, relative to target cohort entry:", 0, min=0)
                  ,numericInput("GIS.timeatrisk_enddt","Define the time-at-risk window end:", 0, min=0)
                  ,selectInput("GIS.timeatrisk_enddt_panel","", choices =
                                c("from cohort start date" = "cohort_start_date","from cohort end date" = "cohort_start_date"),selected = "cohort_end_date")
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
                  plotOutput("GIS.plot")
                  #,textOutput("text")
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
      if (length(cohort_list)>1) {
        selectInput("tcdi", "Select target cohort", choices = cohort_list[,3])
      } else {
        selectInput("tcdi", "Select target cohort", choices = cohort_list[,1])
      }
    })


    output$cohort_ocdi <- renderUI({
      cohort_list <- cohort_listup()
      #selectInput("ocdi", "Select outcome cohort", choices = cohort_list[,3])
      if (length(cohort_list)>1) {
          selectInput("ocdi", "Select target cohort", choices = cohort_list[,3])
        } else {
          selectInput("ocdi", "Select target cohort", choices = cohort_list[,1])
        }
    })


    output$country_list <- renderUI({
      country_list <- GIS.countrylist()
      selectInput("country", "Select country", choices = country_list[,1])
    })

    cohort_listup <- eventReactive(input$db_load, {
      connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms=input$sqltype,
                                                                       server=input$ip,
                                                                       schema=input$Resultschema,
                                                                       user=input$usr,
                                                                       password=input$pw)
      connection <<- DatabaseConnector::connect(connectionDetails)
      cohort_list <<- Call.Cohortlist(connectionDetails, connection, input$Resultschema)
    })


    render.table <- eventReactive(input$submit_table,{
      isolate({
        country_list <<- GIS.countrylist()
        country <<- input$country
        MAX.level <<- country_list[country_list$NAME==country,3]
        GADM <<- GIS.download(country, MAX.level)
        GADM.table <<- GADM[[3]]@data

        #Conditional input cohort number
        if (length(cohort_list)>1) {
          tcdi <- cohort_list[which(cohort_list[,3] %in% input$tcdi == TRUE),1]
          ocdi <- cohort_list[which(cohort_list[,3] %in% input$ocdi == TRUE),1]
        } else {
          tcdi <- input$tcdi
          ocdi <- input$ocdi
        }

        CDM.table <<- AEGIS::GIS.extraction(connectionDetails, input$CDMschema, input$Resultschema, targettab="cohort", input$dateRange[1], input$dateRange[2], input$distinct,
                                            tcdi, ocdi, input$fraction, input$GIS.timeatrisk_startdt, input$GIS.timeatrisk_enddt, input$GIS.timeatrisk_enddt_panel)
        table <- dplyr::left_join(CDM.table, GADM.table, by=c("gadm_id" = "ID_2"))
        switch(input$GIS.Age,
               "no"={
                 table <- table[, c("OBJECTID","ID_0", "ISO", "NAME_0", "ID_1", "NAME_1",
                                    "NAME_2",
                                    "target_count", "outcome_count", "proportion", "SIR", "expected"
                 )]#"ID_2"

               },
               "indrect"={
                 table <- table[, c("OBJECTID","ID_0", "ISO", "NAME_0", "ID_1", "NAME_1",  "NAME_2",
                                    "target_count", "outcome_count",
                                    "indirect_expected", "indirect_incidence", "indirect_SIR"
                 )]#"ID_2",

               },
               "direct"={
                 table <- table[, c("OBJECTID","ID_0", "ISO", "NAME_0", "ID_1", "NAME_1",  "NAME_2",
                                    "target_count", "outcome_count",
                                    "direct_expected", "direct_incidence", "direct_SIR"
                 )]#"ID_2",

               }

        )
      })
      table
    })

    output$GIS.table <- renderDataTable(
      render.table()
    )


    draw.plot <- eventReactive(input$submit_plot,{
      isolate({
        GADM.table <<- GADM[[3]]@data
        countdf_level <<- GIS.calc1(input$GIS.level, input$GIS.distribution, input$GIS.Age)
        mapdf <<- GIS.calc2(input$GIS.level, input$fraction)
        plot <- GIS.plot(input$GIS.distribution, input$plot.legend, input$plot.title, input$GIS.Age)
      })
      plot
    })

    output$GIS.plot <- renderPlot ({
      draw.plot()
    }, width = 1280, height = 1024, res = 100)

    #testing.cluster <- eventReactive(input$submit_cluster,{
    #  isolate({
    #    CDM.table$Observed <- CDM.table$outcome_count
    #    test.summ <- DCluster::achisq.stat(CDM.table, lambda=1)
    #  })
    #  test.summ[[1]]
    #})

    #output$Cluster.test <- renderText({
    #  testing.cluster()
    #})

    #finding.cluster <- eventReactive(input$submit_cluster,{
    #  isolate({
    #      table <- Cluster.find(input$Cluster.method, input$Cluster.parameter)
    #  })
    #  table
    #})

    #output$Cluster.table <- renderDataTable(
    #  finding.cluster()
    #)

    plotting.cluster <- eventReactive(input$submit_cluster,{
      isolate({
        plot <- Cluster.plot(input$Cluster.method, input$Cluster.parameter, input$GIS.Age, input$country)
      })
      plot
    })



    output$Cluster.plot <- renderPlot ({
      plotting.cluster()
    }, width = 1024, height = 800, res = 100)


    ## End of server
  }, options = list(height = 1000)
)