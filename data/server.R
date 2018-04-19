server <- function(input, output,session)
{

  cohort_listup <- eventReactive(input$db_load, {
    connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms="sql server",
                                                                     server=input$ip,
                                                                     schema=input$schema,
                                                                     user=input$usr,
                                                                     password=input$pw)
    connection <<- DatabaseConnector::connect(connectionDetails)
    cohort_list <- Call.Cohortlist(connectionDetails, connection, input$schema)
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
      GADM.table <- GADM[[3]]@data
      CDM.table <<- AEGIS::GIS.extraction(connectionDetails, input$schema, targettab="cohort", input$dateRange[1], input$dateRange[2], input$distinct,
                                          input$tcdi, input$ocdi, fraction=1)
      table <- dplyr::left_join(GADM.table, CDM.table, by=c("ID_2" = "gadm_id"))
      table <- table[, c("OBJECTID","ID_0", "ISO", "NAME_0", "ID_1", "NAME_1", "ID_2", "NAME_2",
                         "target_count", "outcome_count", "proportion", "SIR", "Expected")]
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
  }, width = 1024, height = 800, res = 100)

  testing.cluster <- eventReactive(input$submit_cluster,{
    isolate({
      CDM.table$Observed <- CDM.table$outcome_count
      test.summ <- DCluster::achisq.stat(CDM.table, lambda=1)
    })
    test.summ[[1]]
  })

  output$Cluster.test <- renderText({
    testing.cluster()
  })

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
}


