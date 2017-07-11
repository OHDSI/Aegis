##install packages
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

options(expressions=500000)
setTimeLimit()
packages(sp) 
packages(maps)
packages(mapdata)
packages(mapproj)
packages(ggmap)
packages(RODBC)
packages(dplyr)
packages(plyr)
packages(sqldf)
packages(shiny)
packages(bindrcpp) 
packages(pkgconfig)
packages(shinyjs)
packages(shinythemes)
packages(SqlRender)

## select database
#connectionDetails<-createConnectionDetails(dbms="sql server",
#                                           server="server",
#                                           user="user",
#                                           password="pw")

#connection<-connect(connectionDetails)

#sql <- "SELECT name FROM sys.databases"
#sql <- renderSql(sql)$sql
#sql <- translateSql(sql,
#                    targetDialect=connectionDetails$dbms)$sql
#database_list<- querySql(connection, sql)


## select cohort
connectionDetails<-createConnectionDetails(dbms="sql server",
                                           server="server",
                                           schema="schema",
                                           user="user",
                                           password="pw")
cdmDatabaseSchema <- "schema"
targettab <- "table"
cdmVersion <- "5" 
connection<-connect(connectionDetails)

sql <- "SELECT distinct cohort_definition_id FROM @cdmDatabaseSchema.@targettab"
sql <- renderSql(sql,
                 cdmDatabaseSchema=cdmDatabaseSchema,
                 targettab=targettab)$sql
sql <- translateSql(sql,
                    targetDialect=connectionDetails$dbms)$sql
cohort_list<- querySql(connection, sql)

###########################


##shiny UI
shinyApp(
  
  ui<-fluidPage(
    theme= shinytheme("superhero"),
    tags$head(tags$style((".shiny-output-error{color: #4E5D6C}"))),
    shinyjs::useShinyjs(),
    titlePanel("AEGIS"),
    fluidRow(
      column(3,
             wellPanel(
               selectInput("ocdi","Outcome Cohort",choices = c('',sort(cohort_list$COHORT_DEFINITION_ID)),selected = NULL)
               ,hr(),
               selectInput("tcdi","Target cohort",choices = c('',sort(cohort_list$COHORT_DEFINITION_ID)),selected = NULL)
               ,hr()
               ,dateRangeInput(inputId = "dateRange", label = "Select Windows",  start = "2002-01-01", end = "2013-12-31")
               ,hr()
               ,radioButtons("level","Administrative level",choices = c("Level 1" = 0, "Level 2" = 1, "Level 3" = 2),selected = 1)
               ,radioButtons("abs","Select distribution options", c("Absolute" = "disabled","Propotion" = "enabled"),inline= TRUE)
               ,radioButtons("distinct","Select distinct options", c("Yes" = "distinct","No" = "" ),inline= TRUE)
               ,textInput("fraction","fraction",10000)
               
             ),
             wellPanel(
               textInput("title","title","")
               ,textInput("legend","legend","")
               ,submitButton("submit")
             )
      ),
      column(9,
             wellPanel(
               plotOutput("plot")
               
             )
      )
    ) 
    
  ),
  
  
  server <- function(input, output,session) {
    
    observe({
      
      shinyjs::toggleState("fraction",input$abs == "enabled")
      shinyjs::toggleState("tcdi",input$abs == "enabled")
      
    })
    
    output$plot <- renderPlot({
      
      level <- input$level
      tcdi <- input$tcdi
      ocdi <- input$ocdi
      fraction<-as.numeric(input$fraction)
      startdt <- input$dateRange[1]
      enddt <- input$dateRange[2]
      distinct <- input$distinct
      
      
      ##Load cohort
      
      
      if( input$abs == "disabled"){
        sql <-"
        SELECT o.ID_0, o.ID_1, o.ID_2, count(o.subject_id) as outcome_count
        FROM 
        (
        SELECT @distinct a.subject_id, a.cohort_definition_id, a.cohort_start_date, a.cohort_end_date, b.location_id, c.ID_0, c.ID_1, c.ID_2  
        FROM @cdmDatabaseSchema.@targettab a 
        LEFT JOIN @cdmDatabaseSchema.person b 
        ON a.subject_id = b.person_id 
        LEFT JOIN JHCho_EMR.dbo.gadm c 
        ON b.location_id = c.location_id
        WHERE a.cohort_definition_id = @ocdi
        )
        o
        where '@startdt' <= o.cohort_start_date
        AND '@enddt' >= o.cohort_end_date
        GROUP BY o.ID_2, o.ID_1, o.ID_0
        "
        sql <- renderSql(sql,
                         cdmDatabaseSchema=cdmDatabaseSchema,
                         targettab=targettab,
                         startdt=startdt,
                         enddt=enddt,
                         distinct=distinct,
                         ocdi=ocdi)$sql
        sql <- translateSql(sql,
                            targetDialect=connectionDetails$dbms)$sql
        cohort<- querySql(connection, sql)
      }
      else
      {
        sql <-"
        SELECT t.cohort_definition_id, t.subject_id, t.cohort_start_date, t.cohort_end_date
        INTO #target_cohort
        FROM 
        (
        SELECT 
        @distinct subject_id,
        cohort_definition_id,
        cohort_start_date,
        cohort_end_date
        FROM
        @cdmDatabaseSchema.@targettab
        ) t
        WHERE cohort_definition_id = @tcdi
        AND '@startdt' <= t.cohort_start_date
        AND '@enddt' >= t.cohort_end_date
        
        --outcome cohort
        SELECT o.cohort_definition_id, o.subject_id, o.cohort_start_date, o.cohort_end_date
        INTO #outcome_cohort
        FROM 
        (
        SELECT 
        @distinct subject_id,
        cohort_definition_id,
        cohort_start_date,
        cohort_end_date
        FROM
        @cdmDatabaseSchema.@targettab
        ) o
        WHERE cohort_definition_id = @ocdi
        AND '@startdt' <= o.cohort_start_date
        AND '@enddt' >= o.cohort_end_date
        
        SELECT o.subject_id, o.cohort_definition_id, o.cohort_start_date, o.cohort_end_date 
        INTO #including_cohort
        FROM #outcome_cohort o
        LEFT JOIN #target_cohort t
        ON t.subject_id = o.subject_id
        WHERE t.cohort_start_date <= o.cohort_start_date
        AND t.cohort_end_date >= o.cohort_end_date
        
        SELECT a.ID_0, a.ID_1, a.ID_2, a.target_count, b.outcome_count
        FROM
        (
        SELECT c.ID_0, c.ID_1, c.ID_2, count(a.subject_id) AS target_count
        FROM @cdmDatabaseSchema.@targettab a
        LEFT JOIN
        @cdmDatabaseSchema.person b ON a.subject_id = b.person_id LEFT JOIN JHCho_EMR.dbo.gadm c ON b.location_id = c.location_id
        WHERE cohort_definition_id = @tcdi
        GROUP BY c.ID_2, c.ID_1, c.ID_0
        )
        A LEFT JOIN
        (
        SELECT c.ID_0, c.ID_1, c.ID_2, count(a.subject_id) AS outcome_count
        FROM #including_cohort a
        LEFT JOIN
        @cdmDatabaseSchema.person b ON a.subject_id = b.person_id LEFT JOIN JHCho_EMR.dbo.gadm c ON b.location_id = c.location_id
        GROUP BY c.ID_2, c.ID_1, c.ID_0
        )
        B
        ON a.ID_2 = b.ID_2
        GROUP BY a.ID_2, a.ID_1, a.ID_0, a.target_count, b.outcome_count
        ORDER BY id_2
        
        DROP TABLE #including_cohort
        DROP TABLE #target_cohort
        DROP TABLE #outcome_cohort
        "
        sql <- renderSql(sql,
                         cdmDatabaseSchema=cdmDatabaseSchema,
                         targettab=targettab,
                         startdt=startdt,
                         enddt=enddt,
                         distinct=distinct,
                         tcdi=tcdi,
                         ocdi=ocdi)$sql
        sql <- translateSql(sql,
                            targetDialect=connectionDetails$dbms)$sql
        cohort<- querySql(connection, sql)
      }
      
      ##load GADM & getting map
      gadm <- readRDS(paste0("[file_path]", level,".rds")) # local change
      map <-ggmap(get_map(location = gadm@bbox, maptype='roadmap') )
      
      ##tolower column names
      colnames(cohort) <- tolower(colnames(cohort))
      
      ##remove NA
      countdf <- na.omit(cohort)
      
      ##cohort extraction by level
      if( input$abs == "disabled"){
        countdf_level <- sqldf(paste0("select id_",level, " as id, sum(outcome_count) as count from countdf group by id_", level, " order by id_", level))
      }
      else
      {
        countdf_level <- sqldf(paste0("select id_",level, " as id, sum(outcome_count) as count, sum(target_count) as target_count from countdf group by id_", level, " order by id_", level))
      }
      
      ##polygon data & proportion calc 
      mapdf <- data.frame()
      for(i in 1:length(countdf_level$id))
      {
        countdf_level$target_count[i] <- (countdf_level$count[i] / countdf_level$target_count[i])*fraction
        idx<-as.numeric(as.character(countdf_level$id[i]))
        polygon <- gadm@polygons[[idx]]
        for(j in 1:length(polygon@Polygons))
        {
          if(polygon@Polygons[[j]]@area<0.001)
            next
          tempdf <- fortify(polygon@Polygons[[j]])
          tempdf$id <- idx
          tempdf$group <- as.numeric(paste0(idx,".",j))
          mapdf <- rbind(mapdf, tempdf)
        }
      }
      
      mapdf <- join(mapdf,countdf_level,by = "id", type="inner")
      
      
      
      #plotting on kormap
      
      if( input$abs == "disabled"){
        t <- max(countdf_level$count)
        
        plot <- map+
          geom_polygon(data=mapdf,aes(x=long,y=lat,group=group,fill=count),alpha=0.8,colour="black",lwd=0.2)+
          scale_fill_gradientn(colours = rev(heat.colors(3)), limit = c(0,t)) + labs(fill=input$legend) +
          ggtitle(input$title) + theme(plot.title=element_text(face="bold", size=30, vjust=2, color="black")) +
          theme(legend.title=element_text(size=20, face="bold")) + theme(legend.text = element_text(size=15)) + theme(legend.key.width=unit(2, "cm"), legend.key.height = unit(2,"cm"))
        
        plot
        
      }
      else
      {
        t <- max(countdf_level$target_count)
        plot <- map+
          geom_polygon(data=mapdf,aes(x=long,y=lat,group=group,fill=target_count),alpha=0.8,colour="black",lwd=0.2)+
          scale_fill_gradientn(colours = rev(heat.colors(3)), limit = c(0,t)) + labs(fill=input$legend) +
          ggtitle(input$title) + theme(plot.title=element_text(face="bold", size=30, vjust=2, color="black")) +
          theme(legend.title=element_text(size=20, face="bold")) + theme(legend.text = element_text(size=15)) + theme(legend.key.width=unit(2, "cm"), legend.key.height = unit(2,"cm"))
        
        plot
      }
      
      
    })
  }
  )