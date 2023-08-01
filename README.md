Final Project
================
Spencer Williams
2023-07-31

# Description and Purpose

This app uses hitting statistics from the 2019 MLB season. Users can select different variables for visualizations and model selections. Our ulitmate goal of this app is to determine which model will predict the number of RBI's by a player using the 2019 mlb hitting statistics. We will use the multiple linear regression, classification tree, and random forest models on our test data to compare which model is better for prediction. 

# Packages

library(shinydashboard)

library(shiny)

library(randomForest)

library(tree)

library(rpart)

library(caret)

library(MuMIn)

``` r
# Install Packages
install.packages(shinydashboard)
install.packages(shiny)
install.packages(randomForest)
install.packages(tree)
install.packages(caret)
install.packages(rpart)
install.packages(MuMIn)

library(shinydashboard)
library(shiny)
library(randomForest)
library(tree)
library(rpart)
library(caret)
```

# Code to run app
shiny::runGitHub('sjwilli6/FinalProject')
