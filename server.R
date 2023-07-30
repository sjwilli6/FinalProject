#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(randomForest)
library(tree)
library(caret)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # MLB Data
  mlb_data <- read.csv("/Users/monicabeingolea/Documents/ST558/mlb2019.csv")
  mlb_data <- mlb_data[ , c(2:3,9,11,13:31,36)]
  
### EDA Section ##################################################
  map_var_name = function(){
    return(switch(input$expl_variable, "Homeruns"="HR",
                  "RBI's"="RBI",
                  "Runs"="R",
                  "Batting Avg"="avg",
                  "Stolen Bases"="SB"))
  }
  
  output$expl_plot_title = renderUI(paste("Plot of", input$expl_variable))

  exploratory_plot = function(){
    
    g <- ggplot(mlb_data, aes(x = AB))
    
    if(input$expl_variable == "Homeruns"){
      g + geom_point(aes(y = HR)) +
        labs(x ="At Bats", y = "Homeruns", title = "Homeruns per AB")
    } else if(input$expl_variable == "RBI's"){ 
      g + geom_point(aes(y = RBI)) +
        labs(x ="At Bats", y = "RBI's", title = "RBI's per AB")
    } else if(input$expl_variable == "Runs"){ 
      g + geom_point(aes(y = R)) +
        labs(x ="At Bats", y = "Runs", title = "Runs per AB")
    } else if(input$expl_variable == "Batting Avg"){ 
      g + geom_point(aes(y = avg)) +
        labs(x ="At Bats", y = "Avg", title = "Batting Avergae")
    } else if(input$expl_variable == "Stolen Bases"){ 
      g + geom_point(aes(y = SB)) +
        labs(x ="At Bats", y = "Stolen Bases", title = "Stolen Bases per AB")
    }
  }
  
  output$expl_plot = renderPlot({exploratory_plot()})
  
  output$downloadPlot = downloadHandler(
    filename = function(){paste0(input$expl_variable, ".png")},
    content = function(file){ggsave(file, exploratory_plot(), device = "png")}
  )
  
  output$downloadData = downloadHandler(
    filename = function(){paste(map_var_name(),"data.csv")},
    content = function(file){write.csv(mlb_data1[,c("AB", "HR", map_var_name())],
                                       file, row.names = FALSE)}
  )
  
  output$downloadAllData = downloadHandler(
    filename = function(){"all_data.csv"},
    content = function(file){write.csv(mlb_data1, file)}
  )
  
  #create text info
  output$info <- renderText({
    
    if(input$expl_variable == "Homeruns"){
      paste("The average MLB player in the 2019 season had", round(mean(mlb_data$HR, na.rm = TRUE), 2), "Homeruns.", sep = " ")
    } else if(input$expl_variable == "RBI's"){ 
      paste("The average MLB player in the 2019 season had", round(mean(mlb_data$RBI, na.rm = TRUE), 2), "RBI's.", sep = " ")
    } else if(input$expl_variable == "Runs"){ 
      paste("The average MLB player in the 2019 season had", round(mean(mlb_data$R, na.rm = TRUE), 2), "Runs.", sep = " ")
    } else if(input$expl_variable == "Stolen Bases"){ 
      paste("The average MLB player in the 2019 season had", round(mean(mlb_data$SB, na.rm = TRUE), 2), "Stolen Bases.", sep = " ")
    } else if(input$expl_variable == "Batting Avg"){ 
      paste("The average MLB player in the 2019 season had a ", round(mean(mlb_data$avg, na.rm = TRUE), 2), "Batting Average.", sep = " ")
    }
    
  })
  
  stat_df = function(){
    if(input$expl_variable == "Homeruns"){
      mlb_data[ , c(1:5,10)]
    } else if(input$expl_variable == "RBI's"){
      mlb_data[ , c(1:5,11)]
    } else if(input$expl_variable == "Runs"){
      mlb_data[ , c(1:5,6)]
    } else if(input$expl_variable == "Stolen Bases"){
      mlb_data[ , c(1:5,12)]
    } else if(input$expl_variable == "Batting Avg"){
      mlb_data[ , c(1:5,21)]
    }
  }
  
  output$statdf = renderDataTable({stat_df()})
  
### MathJAX ################################################### 
  
  output$mod1 <- renderUI({
    withMathJax(
      helpText('Y',tags$sub("i"), '=b',tags$sub("0"),'+b',tags$sub("1"),'x',tags$sub("1"),'+b',tags$sub("2"),'x',tags$sub("2"),'+...+b',tags$sub("p"),'x',tags$sub("p"))
    )
  })
  
######################################################
  
## Model Fitting ####################################################

  set.seed(5432)
  # split data into test and training sets
  cut <- reactive({sample(1:nrow(mlb_data), input$proportion * nrow(mlb_data))})

  mlb_train <- reactive({mlb_data[cut,]})
  mlb_test <- reactive({mlb_data[-cut,]})
  
  fit_mod1 <- eventReactive(input$ready, {
    if(length(input$mod1_var) > 0) {
      df = mlb_train[ ,c(input$mod1_var)]
    return(lm(pos1 ~ ., data = df, preProcess = c("center", "scale"), trControl = trainControl(method = "cv", number = 5)))}
    else if (length(input$mod1_var) < 0) {
      (print("No Results"))}})

  fit_mod2 <- eventReactive(input$ready, {
    if(length(input$mod2_var) > 0) {
      d = mlb_train[ ,c(input$mod2_var)]
      return(tree(pos1 ~ ., data = d))}
  else if (length(input$mod2_var) < 0) {
    (print("No Results"))}})
  
  fit_mod3 <- eventReactive(input$ready, {
    if(length(input$mod3_var) > 0) {
      x = mlb_train[ ,c(input$mod3_var)]
      x = x[complete.cases(x),]
      y = mlb_train$pos1
      return(randomForest(x = x, y = as.factor(y), mtry = ncol(mlb_train) - 1,
                           ntree = 200, importance = TRUE))}
    else if (length(input$mod3_var) < 0) {
      (print("No Results"))}})
  
  output$fitted_mod1 = renderUI({fit_mod1()})
  output$fitted_mod2 = renderUI({fit_mod2()})
  output$fitted_mod3 = renderUI({fit_mod3()})
  
######################################################
  
## Data ####################################################

  output$all_data = renderDataTable({mlb_data})
  
###################################################### 
  
}
