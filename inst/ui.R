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
setTimeLimit()

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
gpclibPermit()
#Sys.setlocale(category = "LC_ALL", locale = "us")

shinyApp(
  # Define UI for dataset viewer application
  ui <- dashboardPage(
    dashboardHeader(title = "AEGIS"),
    dashboardSidebar(sidebarMenu(menuItem("DB connection",tabName= "db" ),
                                 menuItem("Cohorts", tabName = "Cohorts" ),
                                 menuItem("Visualization", tabName = "Visualization" ),
                                 menuItem("Clustering",tabName = "Clustering" )
    )
    ),
    dashboardBody(tabItems(
      tabItem(tabName = "db",
              fluidRow(
                titlePanel("Database connection"),
                sidebarPanel(
                  textInput("ip","IP","")
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
                  ,textInput("description_1","","")
                  ,uiOutput("cohort_ocdi")
                  ,textInput("description_1","","")
                  ,hr()
                  ,dateRangeInput(inputId = "dateRange", label = "Select Windows",  start = "2002-01-01", end = "2013-12-31")
                  ,hr()
                  ,uiOutput("country_list")
                  ,actionButton("submit_table","submit")
                  ,width=2
                ),
                mainPanel
                (dataTableOutput('GIS.table'))
              )
      ),
      tabItem(tabName = "Visualization",
              fluidRow(
                titlePanel("Visualization setting"),
                sidebarPanel(
                  radioButtons("GIS.level","Administrative level",choices = c("Level 2" = 1, "Level 3" = 2),selected = 1)
                  #radioButtons("GIS.level","Administrative level",choices = c("Level 1" = 0, "Level 2" = 1, "Level 3" = 2),selected = 1)
                  ,radioButtons("GIS.distribution","Select distribution options", choices = c("Count of the target cohort (n)" = "count","Propotion" = "proportion", "Standardized Incidence Ratio"="SIR", "Age-adjusted incidence"="age_mortality"),selected = "count")
                  #,radioButtons("distinct","Select distinct options", c("Yes" = "distinct","No" = "" ),inline= TRUE)
                  ,textInput("fraction","fraction (only for proportion)",100)
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
                  radioButtons("Cluster.method","Cluster Method",choices = c("Local Moran's I" = "moran", "Kulldorff and Nagarwalla's method" = "kulldorff"))
                  ,textInput("Cluster.parameter","Kulldorff's method parameter", ".15")
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

    cohort_listup <- eventReactive(input$db_load, {
      connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms="sql server",
                                                                       server=input$ip,
                                                                       schema=input$Resultschema,
                                                                       user=input$usr,
                                                                       password=input$pw)
      connection <<- DatabaseConnector::connect(connectionDetails)
      cohort_list <- Call.Cohortlist(connectionDetails, connection, input$Resultschema)
    })


    output$cohort_tcdi <- renderUI({
      cohort_list <- cohort_listup()
      selectInput("tcdi", "Select target cohort", choices = cohort_list[,1])
    })


    output$cohort_ocdi <- renderUI({
      cohort_list <- cohort_listup()
      selectInput("ocdi", "Select outcome cohort", choices = cohort_list[,1])
    })


    output$country_list <- renderUI({
      country_list <- GIS.countrylist()
      selectInput("country", "Select country", choices = country_list[,1])
    })


    render.table <- eventReactive(input$submit_table,{
      isolate({
        country_list <<- GIS.countrylist()
        country <- input$country
        MAX.level <<- country_list[country_list$NAME==country,3]
        GADM <<- GIS.download(country, MAX.level)
        GADM.table <<- GADM[[3]]@data
        CDM.table <<- AEGIS::GIS.extraction(connectionDetails, input$CDMschema, input$Resultschema, targettab="cohort", input$dateRange[1], input$dateRange[2], input$distinct,
                                            input$tcdi, input$ocdi, fraction=1)
        table <- dplyr::left_join(GADM.table, CDM.table, by=c("ID_2" = "gadm_id"))
        table <- table[, c("OBJECTID","ID_0", "ISO", "NAME_0", "ID_1", "NAME_1", "ID_2", "NAME_2",
                           "target_count", "outcome_count", "proportion", "SIR", "Expected", "age_mortality")]
      })
      table
    })

    output$GIS.table <- renderDataTable(
      render.table()
    )


    draw.plot <- eventReactive(input$submit_plot,{
      isolate({
        GADM.table <<- GADM[[3]]@data
        countdf_level <<- GIS.calc1(input$GIS.level, input$GIS.distribution, input$fraction)
        mapdf <<- GIS.calc2(input$GIS.level, input$fraction)
        plot <- GIS.plot(input$GIS.distribution, input$plot.legend, input$plot.title)
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
        plot <- Cluster.plot(input$Cluster.method, input$Cluster.parameter)
      })
      plot
    })



    output$Cluster.plot <- renderPlot ({
      plotting.cluster()
    }, width = 1024, height = 800, res = 100)


    ## End of server
  }, options = list(height = 1000)
)
