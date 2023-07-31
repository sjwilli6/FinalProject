#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Libraries Used
library(shiny)
library(randomForest)
library(tree)
library(caret)
library(prediction)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Loading in MLB Data and columns I want to use
  mlb_data <- read.csv("/Users/monicabeingolea/Documents/ST558/mlb2019.csv")
  mlb_data <- mlb_data[ , c(2:3,9,11,13:31,36)]
  mlb_data$pos1 <- as.factor(mlb_data$pos1)
  
### EDA Section ##################################################
  map_var_name = function(){
    return(switch(input$expl_variable, "Homeruns"="HR",
                  "RBI's"="RBI",
                  "Runs"="R",
                  "Batting Avg"="avg",
                  "Stolen Bases"="SB"))
  }
  # Plot title
  output$expl_plot_title = renderUI(paste("Plot of", input$expl_variable))
  # Creating scatter plots depending on variable selected
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
  # Output plot
  output$expl_plot = renderPlot({exploratory_plot()})
  # Be able to download plot
  output$downloadPlot = downloadHandler(
    filename = function(){paste0(input$expl_variable, ".png")},
    content = function(file){ggsave(file, exploratory_plot(), device = "png")}
  )
  # Download data for specific variable name
  output$downloadData = downloadHandler(
    filename = function(){paste(map_var_name(),"data.csv")},
    content = function(file){write.csv(mlb_data1[,c("AB", "HR", map_var_name())],
                                       file, row.names = FALSE)}
  )
  # Output all of data
  output$downloadAllData = downloadHandler(
    filename = function(){"all_data.csv"},
    content = function(file){write.csv(mlb_data1, file)}
  )
  
  #create text info
  output$info <- renderText({
    # Find the means for each variable selected 
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
  # Change the data table depending on what variable was selected
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
  # Output table
  output$statdf = renderDataTable({stat_df()})
  
### MathJAX ################################################### 
  
  output$mod1 <- renderUI({
    # Using mathJAX to output multiple linear regression equation
    withMathJax(
      helpText('Y',tags$sub("i"), '=b',tags$sub("0"),'+b',tags$sub("1"),'x',tags$sub("1"),'+b',tags$sub("2"),'x',tags$sub("2"),'+...+b',tags$sub("p"),'x',tags$sub("p"))
    )
  })
  
######################################################
  
## Model Fitting ####################################################

  # split data into test and training sets
  cut <- reactive({sample(1:nrow(mlb_data), input$proportion * nrow(mlb_data))})
  # Create training and testing data sets
  mlb_train <- reactive({mlb_data[cut,]})
  mlb_test <- reactive({mlb_data[-cut,]})
  
  # Creating data set based on the variables selected
  df = function(){
    mlb_train[cut,c(input$mod1_var, 11)]
  }
  df <- data.frame(df)
  d = function(){
    mlb_train[cut,c(input$mod2_var, 11)]
  }
  d <- data.frame(d)
  y = function(){
    mlb_train[cut,c(input$mod3_var, 11)]
  }
  y <- data.frame(y)
  
  # Fitting the multiple linear regression model
  fit_mod1 <- eventReactive(input$ready, {
    if(length(input$mod1_var) > 0) {
      #df <- mlb_train[ ,c(input$mod1_var)]
      #df <- df[complete.cases(df)]
      lin_model <- lm(RBI ~ ., data = df, preProcess = c("center", "scale"), trControl = trainControl(method = "cv", number = 5))
      return(summary(lin_model))}
    else if (length(input$mod1_var) < 0) {
      (print("No Results"))}})

    # Fitting the classification tree
  fit_mod2 <- eventReactive(input$ready, {
    if(length(input$mod2_var) > 0) {
      #d <- mlb_train[ ,c(input$mod2_var)]
      #d <- d[complete.cases(d),]
      tree_model <- tree(RBI ~ ., data = d)
      return(plot(tree_model))}
  else if (length(input$mod2_var) < 0) {
    (print("No Results"))}})
  
  # Fitting the random forest model
  fit_mod3 <- eventReactive(input$ready, {
    if(length(input$mod3_var) > 0) {
      #x <- mlb_train[ ,c(input$mod3_var)]
      #x <- x[complete.cases(x),]
      y <- mlb_train$RBI
      rf <- randomForest(x = x, y = y, mtry = ncol(mlb_train) - 1,
                           ntree = 200, importance = TRUE)
      return(rf$results, rf$bestTune)}
    else if (length(input$mod3_var) < 0) {
      (print("No Results"))}})
  
  # Output the results
  output$fitted_mod1 = renderUI({fit_mod1()})
  output$fitted_mod2 = renderPlot({fit_mod2()})
  output$fitted_mod3 = renderUI({fit_mod3()})
  
  # statistics
  #pred_1 <- predict(fit_mod1, newdata = mlb_test)
  #stat_1 <- postResample(pred_1, obs = mlb_test$RBI)
  #pred_2 <- predict(fit_mod2, newdata = mlb_test)
  #stat_2 <- postResample(pred_2, obs = mlb_test$RBI)
  #pred_3 <- predict(fit_mod3, newdata = mlb_test)
  #stat_3 <- postResample(pred_3, obs = mlb_test$RBI)
  
  # Output the statistics
  #output$compare_1 = renderUI({stat_1})
  #output$compare_2 = renderUI({stat_2})
  #output$compare_3 = renderUI({stat_3})
  
######################################################
  
## Prediction ####################################################
  
  
  
######################################################
  
## Data ####################################################

    # Data table
  output$all_data = renderDataTable({mlb_data})
  
###################################################### 
  
}
