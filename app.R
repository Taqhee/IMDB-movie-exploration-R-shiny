#app.R

library(shiny)
library(shinydashboard)
library(dcr)
library(dplyr)
library(plotly)

data1 <- read.csv("dataexport.csv")
data1$year <- as.character(data1$year)
data1$year <- as.Date(data1$year,"%Y")
data1$year1 <- as.character(data1$year1)
data1$year1 <- as.Date(data1$year1,"%Y")

#-----------------------------------------------------
# Network plot global.R code
library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(magrittr)
library(knitr)
library(visNetwork)
library(igraph)


data_path <- ''


cleanData <- function(data){
  data$movie_title <- gsub('[','_',data$movie_title, fixed = T)
  data %<>%
    mutate(id = (1:nrow(data)))
  return(data)
}

getIMDBGraph<-function(data,
                       firstyear =1,
                       lastyear = 3000,
                       genre = FALSE,
                       minscore = 0,
                       maxscore = 10){
  if(is.character(genre)){
    data<-data[grep(genre, data$genres),]}

  data<-subset(data,
               data$title_year >= firstyear &
                 data$title_year <= lastyear &
                 data$imdb_score <= maxscore &
                 data$imdb_score >= minscore)
  edges <- data.frame(data$id,
                      data$actor_1_name,
                      data$actor_2_name,
                      data$actor_3_name) %>%
    gather(actornum, to, -data.id) %>%
    rename(from = data.id) %>%
    select(-actornum) %>%
    mutate(from = as.character(from))
  movie <- select(data,
                  -actor_1_name,
                  -actor_2_name,
                  -actor_3_name)
  actor <- data.frame(actor = unique(edges$to))

  nodes <- rbind(
    data.frame(id = movie$id,
               label = movie$movie_title,
               shape = 'square',
               title = paste0('Line1','<br>',
                              'Line2'),
               size = 10,
               color = 'orange',
               imdb_score = movie$imdb_score,
               revenues =movie$gross,
               budget = movie$budget
    ),
    data.frame(id = actor$actor,
               label = actor$actor,
               shape = 'none',
               title = paste0('Line1','<br>',
                              'Line2'),
               size = 10,
               color = 'lightblue',
               imdb_score = NA,
               revenues = NA,
               budget = NA
    )
  )

  return(graph_from_data_frame(edges, directed=F, vertices=nodes))}

selectBestBetweenness <- function(full_graph, topN){
  V(full_graph)$closeness_norm <- closeness(full_graph, normalized = T)
  V(full_graph)$betweenness_norm<- betweenness(full_graph)

  full_graph_nodes <- get.data.frame(full_graph, what="vertices") %>%
    top_n(topN, betweenness_norm)

  full_graph_edges <- get.data.frame(full_graph, what="edges") %>%
    filter(from %in% full_graph_nodes$name | to %in% full_graph_nodes$name)
  full_graph_nodes <- get.data.frame(full_graph, what="vertices") %>%
    filter(name %in% full_graph_edges$from | name %in% full_graph_edges$to)
  full_graph_nodes <- full_graph_nodes%>%
    mutate(title = ifelse(full_graph_nodes$color == 'lightblue', "",
                          paste0('IMDB score: ',imdb_score,'<br>',
                                 'Revenues (M$): ', round(revenues/1000000, digits = 1),'<br>',
                                 'Budget (M$): ', round(budget/1000000,digits = 1)))
    ) %>%
    mutate(id = name)


  return(list(full_graph_nodes, full_graph_edges))
}

cleaned_data <- read.csv(paste0(data_path, 'movie_metadata.csv')) %>%
  cleanData()
# #---------------------------------------------------------------------------------
genredata <- read.csv("Genre.csv")
Alldata <- read.csv("Alldata.csv")
Alldata$size <- sqrt(Alldata$imdb_score*50 )
genredata$size<-sqrt(genredata$imdb_score*50)

colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')
# 
# #---------------------------------------------------------------------------------


ui <- dashboardPage( 
  #Need to come up with good name for the visual system
  dashboardHeader(title = "IMDB Movies"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Network Viz", tabName = "network", icon=icon("th")),
      menuItem("ROI", tabName = "Scatterplot", icon=icon("th"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard", 
              shinyUI(fluidPage(
                fluidRow(
                  box(title = "Country" , width = 3, status = "primary" , solidHeader = TRUE, chartOutput("example"),
                      fluidRow(column(8, dc_ui(id = "chart1",  show_filter = TRUE))),height = 300),
                  (box(title = "Movie Duration" ,width = 3,status = "primary" , solidHeader = TRUE,chartOutput("plot1"),
                       fluidRow(column(4, dc_ui(id = "chart2",  show_filter = TRUE))),height = 300)),
                  (box(title = "IMDB Rating" ,width = 6,status = "primary" , solidHeader = TRUE,chartOutput("plot2"),
                       fluidRow(column(4, dc_ui(id = "chart3",  show_filter = TRUE))),height = 300))),
                
                fluidRow(
                  box(title = "Genre",chartOutput("plot3"),status = "primary" , solidHeader = TRUE,
                      fluidRow(column(4, dc_ui(id = "chart4",  show_filter = TRUE))),height = 490),
                  
                  box(title = "Avg. Rating",  chartOutput("plot4"),status = "primary" , solidHeader = TRUE,
                      fluidRow(column(4, dc_ui(id = "chart5", show_filter = TRUE))),height = 280),
                  box(chartOutput("plot7"),title = "No. of Movies" ,status = "primary", solidHeader = TRUE,
                      fluidRow(column(4, dc_ui(id = "chart6", show_filter = TRUE))),height = 187)),
                
                
                
                
                #Second tab content
                ###),
                #Second tab content
                # tabItem(tabName = "Insight2",
                #  h2("Insight2")
                fluidRow(
                  box( status = "primary", title = "Movie List",solidHeader = TRUE,
                       chartOutput("table"), width = 12, height = 2500, 
                       hr(),
                       dataTableOutput("filtered_data")
                  )))
              )
      ),
      tabItem(tabName = "network",
              h2(""),
              shinyUI( fluidPage(
                fluidRow(box(
                  selectInput("variable", "IMDB Rating",
                              c(">8" = ">8",
                                "5-8" = "5-8",
                                "<5" = "<5")), width = 12, height = 600 ,
                  #actionButton("Update", "Update" ,icon("refresh")),
                  helpText("It takes 10 -15 seconds for processing the Network Graph"),
                  visNetworkOutput("value")))
              ))),
      
      tabItem(tabName = "Scatterplot",
              h2(""),
              shinyUI( fluidPage(
                fluidRow(box(
                  selectInput("Select", "Select",
                              c("By Genre" = "By Genre",
                                "All movies" = "All movies"
                                
                              )),width = 12, height = 600,plotlyOutput("plot")))
                #actionButton("Update", "Update" ,icon("refresh")),
                
              )))
    )))

server <- shinyServer(function(input, output) {
  
  
  
  output$example <- renderChart( {
    mydcr <- dcr(data1)
    chart_country <- dcrchart(type = "pieChart", id = "chart1", dimension = "country",
                              reduce = reduceCount(), width = 250, height = 200)
    
    chart_duration <- dcrchart(type = "pieChart", id = "chart2", dimension = "duration",
                               reduce = reduceCount(), width = 250, height = 200,
                               innerRadius = 40)
    
    chart_rating <- dcrchart(type = "rowChart", id = "chart3", dimension = "imdb_rating",
                             reduce = reduceCount(), width = 500, height = 200,
                             margins = list(top = 0, right = 50, bottom = 20, left = 40))
    
    
    chart_genre <- dcrchart(type = "rowChart",id = "chart4", dimension = "genre",
                            reduce = reduceCount(),width = 600, height = 430,
                            margins = list(top = 0, right = 50, bottom = 20, left = 40))
    
    
    chart_year <- dcrchart(type = "lineChart", id = "chart5", dimension = "year",
                           reduce = reduceMean("imdb_score"), width = 600, height = 230,
                           renderArea = TRUE, mouseZoomable = FALSE, elasticY = FALSE,
                           brushOn = TRUE,
                           rangeChart = chartname("chart6"),
                           yAxisLabel = "Average Rating")
    #round = dc_code("d3.time.year.round"))
    #xUnits = dc_code("d3.time.months"))
    
    
    chart_year1 <- dcrchart(type = "barChart", id = "chart6", dimension = "year1", 
                            reduce = reduceCount(), 
                            width = 600, height = 130,
                            #margins = list(top = 0, right = 50, bottom = 20, left = 40),
                            centerBar = FALSE, gap = 0.1, #round = dc_code("d3.time.year.round"),
                            alwaysUseRounding = TRUE, xUnits = dc_code("d3.time.years"),
                            
                            #xAxis = x_axis(label_keyvalue(keymap = NULL)),
                            yAxisLabel = "No.of Movies",
                            yAxis = y_axis(ticks = 3, tickSize = 0.4))
    
    
    mydcr + chart_country + chart_duration + chart_rating + chart_genre + chart_year + chart_year1 + 
      chart_year
  },input_binding = TRUE)
  
  output$filtered_data <- renderDataTable(options = list(
    pageLength = 25, width = 2500),{
      data1 <- data1
      f_country <- input$chart1
      f_duration <- input$chart2
      f_imdb_rating <- input$chart3
      f_genre <- input$chart4
      f_year <- input$chart5
      f_year1 <- input$chart6
      if(!is.null(f_country)) data1 <- filter(data1,country %in% f_country)
      if(!is.null(f_duration)) data1 <- filter(data1,duration %in% f_duration)
      if(!is.null(f_imdb_rating)) data1 <- filter(data1,imdb_rating %in% f_imdb_rating)
      if(!is.null(f_genre)) data1 <- filter(data1,genre %in% f_genre)
      if(!is.null(f_year)) data1 <- filter(data1,between(year, f_year[1], f_year[2]))
      if(!is.null(f_year1)) data1 <- filter(data1,between(year1, f_year1[1], f_year1[2]))
      
      data2 <- subset(data1, select = c(1,12,7,16,2,3,8,9,5,6,14,15))
      data2$Year <- as.numeric(format(data1$year1, "%Y"))
      data2
    })
  
  minscore1 <-  reactive(
    if (input$variable == ">8"){
      minscore=8
      #maxscore=10
    }
    else if (input$variable == "5-8"){
      minscore=5
      #maxscore=8
    }
    else if (input$variable == "<5"){
      minscore = 1
      #maxscore = 5
    }
  )
  maxscore1 <- reactive(
    if (input$variable == ">8"){
      #minscore=9
      maxscore=10
    }
    else if (input$variable == "5-8"){
      #minscore=7
      maxscore=8
    }
    else if (input$variable == "<5"){
      #minscore = 4
      maxscore = 5
    }
  )



  output$value <- renderVisNetwork({
    min1 = minscore1()
    max1 = maxscore1()
    best_graph <- getIMDBGraph(cleaned_data, minscore=min1, maxscore =max1 )
    best_output <- selectBestBetweenness(best_graph, 50)
    nodes<- best_output[[1]]
    edges<-best_output[[2]]
    visNetwork(nodes, edges)
  })
  # 
  # #data3<- Alldata
  inputdata <-  reactive(
    if (input$Select == "By Genre")
    {
      data3<-genredata

    }
    else if(input$Select == "All movies"){
      data3<-Alldata

    }

  )
  # 
  # 
  # #data3 <- inputdata()
  # #data3$size <- sqrt(data3$imdb_score*50 )
  # 
  # 
  # 
  # 
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    data3 <- inputdata()
    scatter <-  plot_ly(data3, id="chart1",  x = ~budget, width = 930, height = 400, y = ~gross, color = ~genres, size = ~size, colors = colors,
                        type = 'scatter', mode = 'markers', sizes = c(min(data3$size), max(data3$size)),
                        marker = list(symbol = 'circle', sizemode = 'diameter',
                                      line = list(width = 2, color = '#FFFFFF')),
                        text = ~paste('Movie:', movie_title, '<br>Genre:', genres, '<br>IMDB Rating:', imdb_score,
                                      '<br>Director:',director_name)) %>%
      layout(title = 'Gross vs Budget',
             xaxis = list(title = 'Budget(USD)',
                          gridcolor = 'rgb(255, 255, 255)',
                          range = c(-1000000, max(data3$budget)+1000000),
                          #type = 'log',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 0),
             yaxis = list(title = 'Gross(USD)',
                          gridcolor = 'rgb(255, 255, 255)',
                          range = c(-10000000, max(data3$gross)+1000000),
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 0,
                          paper_bgcolor = 'rgb(243, 243, 243)',
                          plot_bgcolor = 'rgb(243, 243, 243)'
             ))
    scatter
  })
  
})
shinyApp(ui, server)