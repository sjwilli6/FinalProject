#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load Libraries
library(shiny)
library(randomForest)
library(tree)
library(rpart)
library(caret)
library(MuMIn)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # MLB Data
  mlb_data <- read.csv("mlb2019.csv")
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
  #Output Title
  output$expl_plot_title = renderUI(paste("Plot of", input$expl_variable))
  # create graphs for each variable selected by user
  exploratory_plot = function(){
    # base of plot
    g <- ggplot(mlb_data, aes(x = AB))
    # create variable graphs
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
  # output plots
  output$expl_plot = renderPlot({exploratory_plot()})
  # download plots
  output$downloadPlot = downloadHandler(
    filename = function(){paste0(input$expl_variable, ".png")},
    content = function(file){ggsave(file, exploratory_plot(), device = "png")}
  )
  # download data for variable
  output$downloadData = downloadHandler(
    filename = function(){paste(map_var_name(),"data.csv")},
    content = function(file){write.csv(mlb_data[,c("AB", "HR", map_var_name())],
                                       file, row.names = FALSE)}
  )
  # download all of data
  output$downloadAllData = downloadHandler(
    filename = function(){"all_data.csv"},
    content = function(file){write.csv(mlb_data, file)}
  )
  
  #create text info
  output$info <- renderText({
    # send stats for variable to page
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
  # get the correct columns for the data table depending on the variable
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
  # output to tab
  output$statdf = renderDataTable({stat_df()})
  
  ### MathJAX ################################################### 
  # create MLR formula
  output$mod1 <- renderUI({
    withMathJax(
      helpText('Y',tags$sub("i"), '=b',tags$sub("0"),'+b',tags$sub("1"),'x',tags$sub("1"),'+b',tags$sub("2"),'x',tags$sub("2"),'+...+b',tags$sub("p"),'x',tags$sub("p"))
    )
  })
  
  ######################################################
  
  ## Model Fitting ####################################################
  # create model 1
  fit_mod1 <- eventReactive(input$ready, {
    set.seed(5432)
    # split data into test and training sets
    cut <- sample(1:nrow(mlb_data), input$proportion/50 * nrow(mlb_data))
    mlb_train <- mlb_data[cut,]
    mlb_test <- mlb_data[-cut,]
    # if statement depending on variables selected
    if(length(input$mod1_var) > 0) {
      df = mlb_train[ ,c(input$mod1_var,"RBI")]
      df = df[complete.cases(df),] #added comma
      # linear model
      lin_model <- lm(RBI ~ . - RBI, data = df)
      return(summary(lin_model))}
    else if (length(input$mod1_var) < 0) {
      (return("No Results"))}
  })
  # return results
  output$fitted_mod1 = renderPrint({fit_mod1()})
  # fit model 2
  fit_mod2 <- eventReactive(input$ready, {
    set.seed(5432)
    # split data into test and training sets
    cut <- sample(1:nrow(mlb_data), input$proportion/50 * nrow(mlb_data))
    mlb_train <- mlb_data[cut,]
    mlb_test <- mlb_data[-cut,]
    # create tree model for variables selected
    if(length(input$mod2_var) > 0) {
      d = mlb_train[ ,c(input$mod2_var,"RBI")]
      d = d[complete.cases(d),]
      tree_model <- tree(RBI ~ . - RBI, data = d)
      # plot tree
      return(plot(tree_model))}
    else if (length(input$mod2_var) < 0) {
      (return("No Results"))}
  })
  # output to tabs
  output$fitted_mod2 = renderPlot({fit_mod2()})
  # try to get text or summary for classification tree
  fit_mod21 <- eventReactive(input$ready, {
    set.seed(5432)
    # split data into test and training sets
    cut <- sample(1:nrow(mlb_data), input$proportion/50 * nrow(mlb_data))
    mlb_train <- mlb_data[cut,]
    mlb_test <- mlb_data[-cut,]
    # get the selected variables
    if(length(input$mod2_var) > 0) {
      d = mlb_train[ ,c(input$mod2_var,"RBI")]
      d = d[complete.cases(d),]
      tree_model <- tree(RBI ~ . - RBI, data = d)
      # get statistics
      return(summary(tree_model))}
    else if (length(input$mod2_var) < 0) {
      (return("No Results"))}
  })
  # output to tabs
  output$fitted_mod21 = renderPrint({fit_mod21()})
  
  # create the random forest model
  fit_mod3 <- eventReactive(input$ready, {
    set.seed(5432)
    # split data into test and training sets
    cut <- sample(1:nrow(mlb_data), input$proportion/50 * nrow(mlb_data))
    mlb_train <- mlb_data[cut,]
    mlb_test <- mlb_data[-cut,]
    # get the correct variables
    if(length(input$mod3_var) > 0) {
      x = mlb_train[ ,c(input$mod3_var,"RBI")]
      x = x[complete.cases(x),]
      rf <- randomForest(RBI ~ . - RBI, data = x)
      # plot the rf model
      return(plot(rf))}
    else if (length(input$mod3_var) < 0) {
      (return("No Results"))}
  })
  # output to tabs
  output$fitted_mod3 = renderPlot({fit_mod3()})
  # compare the models
  fit_compare <- eventReactive(input$ready, {
      set.seed(5432)
      cut <- sample(1:nrow(mlb_data), input$proportion/50 * nrow(mlb_data))
      mlb_train <- mlb_data[cut,]
      mlb_test <- mlb_data[-cut,]
      # compare for model 1 depending on variables selected
      if(length(input$mod1_var) > 0) {
        df = mlb_train[ ,c(input$mod1_var,"RBI")]
        df1 = mlb_train[ ,c(input$mod1_var,"RBI")]
        df = df[complete.cases(df),]
        df1 = df1[complete.cases(df1),]
        lin_model <- lm(RBI ~ . - RBI, data = df)
        lin_model1 <- lm(RBI ~ . - RBI, data = df1)
        return(list(lin_model, lin_model1))}
      else if (length(input$mod1_var) < 0) {
        (return("No Results"))}
    })
  # output to tabs
  output$fitted_compare = renderPrint({fit_compare()})
  # compare the classification trees
  fit_compare2 <- eventReactive(input$ready, {
    set.seed(5432)
    # split data into test and training sets
    cut <- sample(1:nrow(mlb_data), input$proportion/50 * nrow(mlb_data))
    mlb_train <- mlb_data[cut,]
    mlb_test <- mlb_data[-cut,]
    # get selected variables
    if(length(input$mod2_var) > 0) {
      d = mlb_train[ ,c(input$mod2_var,"RBI")]
      d1 = mlb_train[ ,c(input$mod2_var,"RBI")]
      d = d[complete.cases(d),]
      d1 = d1[complete.cases(d1),]
      tree_model <- tree(RBI ~ . - RBI, data = d)
      tree_model2 <- tree(RBI ~ . - RBI, data = d1)
      # return statistics
      return(list(tree_model, tree_model2))}
    else if (length(input$mod2_var) < 0) {
      (return("No Results"))}
  })
  # output to tabs
  output$fitted_compare2 = renderPrint({fit_compare2()})
  # compare the rf models
  fit_compare3 <- eventReactive(input$ready, {
    set.seed(5432)
    # split data into test and training sets
    cut <- sample(1:nrow(mlb_data), input$proportion/50 * nrow(mlb_data))
    mlb_train <- mlb_data[cut,]
    mlb_test <- mlb_data[-cut,]
    # get selected variables
    if(length(input$mod3_var) > 0) {
      x = mlb_train[ ,c(input$mod3_var,"RBI")]
      x1 = mlb_train[ ,c(input$mod3_var,"RBI")]
      x = x[complete.cases(x),]
      x1 = x1[complete.cases(x1),]
      rf <- randomForest(RBI ~ . - RBI, data = x)
      rf1 <- randomForest(RBI ~ . - RBI, data = x1)
      # return statistics
      return(list(rf, rf1))}
    else if (length(input$mod3_var) < 0) {
      (return("No Results"))}
  })
  # output to tabs
  output$fitted_compare3 = renderPrint({fit_compare3()})
  
  ######################################################
  
  ## Prediction ####################################################
  
  pred <- eventReactive(input$ready2, {
    set.seed(5432)
    # split data into test and training sets
    cut <- sample(1:nrow(mlb_data), input$proportion2/50 * nrow(mlb_data))
    mlb_train1 <- mlb_data[cut,c("G", "AB", "R", "H", "db", "tr", "HR", "SB", "BB", "SO", "avg", "RBI")]
    mlb_test1 <- mlb_data[-cut,c("G", "AB", "R", "H", "db", "tr", "HR", "SB", "BB", "SO", "avg", "RBI")]
    df = mlb_train1[complete.cases(mlb_train1),]
    # depending on model selected, create predictions for each variable
    if(input$mod_pred == "MLR") {
      lin_model <- lm(RBI ~ . - RBI, data = df)
      pred_lin <- lin_model$coefficients[1] + lin_model$coefficients[2]*input$pred_G + lin_model$coefficients[3]*input$pred_AB +
        lin_model$coefficients[4]*input$pred_R + lin_model$coefficients[5]*input$pred_H + lin_model$coefficients[6]*input$pred_db +
        lin_model$coefficients[7]*input$pred_tr + lin_model$coefficients[8]*input$pred_HR + lin_model$coefficients[9]*input$pred_SB +
        lin_model$coefficients[10]*input$pred_BB + lin_model$coefficients[11]*input$pred_SO + lin_model$coefficients[12]*input$pred_avg
      return(pred_lin)}
    # classification model
    else if (input$mod_pred == "Classification"){
        tree_model <- tree(RBI ~ . - RBI, data = df)
        pred_lin <- tree_model$coefficients[1] + tree_model$coefficients[2]*input$pred_G + tree_model$coefficients[3]*input$pred_AB +
          tree_model$coefficients[4]*input$pred_R + tree_model$coefficients[5]*input$pred_H + tree_model$coefficients[6]*input$pred_db +
          tree_model$coefficients[7]*input$pred_tr + tree_model$coefficients[8]*input$pred_HR + tree_model$coefficients[9]*input$pred_SB +
          tree_model$coefficients[10]*input$pred_BB + tree_model$coefficients[11]*input$pred_SO + tree_model$coefficients[12]*input$pred_avg
        return(pred_lin)}
    # rf model
    else if (input$mod_pred == "Random_Forest"){
      rf <- randomForest(RBI ~ . - RBI, data = df)
      pred_lin <- rf$coefficients[1] + rf$coefficients[2]*input$pred_G + rf$coefficients[3]*input$pred_AB +
        rf$coefficients[4]*input$pred_R + rf$coefficients[5]*input$pred_H + rf$coefficients[6]*input$pred_db +
        rf$coefficients[7]*input$pred_tr + rf$coefficients[8]*input$pred_HR + rf$coefficients[9]*input$pred_SB +
        rf$coefficients[10]*input$pred_BB + rf$coefficients[11]*input$pred_SO + rf$coefficients[12]*input$pred_avg
      return(pred_lin)}
      else {
        (return("No Results"))}
  })
  # return to tabs
    output$p_value = renderPrint({pred()})
  
  ###################################################### 
  
  
  ## Data ####################################################
  
  output$all_data = renderDataTable({mlb_data})
  
  ###################################################### 
  
}
