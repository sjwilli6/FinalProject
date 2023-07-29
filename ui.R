## Spencer Williams
## Final Project ST558
## July 31, 2023

library(shinydashboard)
library(plotly)

### Info for the about section ###############################################################
about_string = HTML("<h2>2019 MLB Season</h2>
            <p>The Washington Nationals won the 2019 MLB World Series behind their star pitcher,
            Stephen Strasburg. Data was gathered throughout the regular 2019 season and can be
            found at <a href='https://baseballguru.com/bbdata1.html'>here</a>, along with past years.
            Only the batting statistics will be used for analysis and visualizations.
            <br>
            The purpose of this app is to take a deeper look into the 2019 MLB Season. A couple of questions
            that come to mind are if certain positions have better hitting statistics or which teams performed
            better in specific categories. The app is also going to look at different models that will help
            predict certain qualities of a batter.
            </p>")
about_tab = tabItem("about",fluidRow(box(about_string, width = 8)))
####################################################################################################


### UI layout for the exploration tab ##############################################################
input_options = c("Homeruns", "RBI's", "Runs", "Batting Avg", "Stolen Bases")

expl_controls = box(width = 4,
                    selectizeInput("expl_variable", "Select variable to examine: ", choices = input_options),
                    HTML("<br>"),
                    downloadButton("downloadPlot", label = "Download Current Plot"),
                    HTML("<br>"),
                    downloadButton("downloadData", label = "Download Current Data")
)

expl_plot = box(title = uiOutput("expl_plot_title"), width = 8,
                plotOutput("expl_plot"),
                HTML("<br>"),
                textOutput("info"),
                dataTableOutput("statdf"))

exploration_tab = tabItem("exploration", fluidRow(expl_controls, expl_plot))
###########################################################

### UI layout for clustering tab ###################################################################

# Note: checkboxGroupInput outputs a character vector of the selected choices.
modInfo_string1 = HTML("<h2>Model Information</h2>
  <br>
  <h3>Multiple Linear Regression</h3>
  <p>A multiple linear regression model is used to determine a mathematical relationship among several random variables. It is fit by minimizing the sum of squared errors. Multiple linear regression models can include explanatory variables and higher order terms for predicting the dependent variable. Numerical and categorical data can also be included in these prediction models. Also, a big advantage of using these models is that it does a good job of predicting a response for a linear relationship.</p>
  <p>Many assumptions are made to fit a multiple linear regression model on data. If one of these assumptions are not met, the linear regression model will not be accurate. Some of the assumptions are that the variance of the residuals are constant, observations are independent, and multivariate normality. Multiple linear regression models can also be hard to interpret if there are complex variables and interaction terms.</p>
  ")
  uiOutput('mod1')
  
  modInfo_string2 = HTML("<h3>Classification Tree Analysis</h3>
  <p>A classification tree analysis is a structural mapping of binary decisions that lead to a prediction of a response variable. It is used for predicting a categorical response. Predictor variables are split up to create different prediction models for each model. Some of the advantages of using a classification tree analysis are that it’s easy to understand, predictors don’t need to be scaled, no statistical analysis assumptions necessary, built in variable selection, and it can handle numerical and categorical data. A disadvantage of using this type of analysis is that a small change in the data can change the outcome significantly. The calculations behind the tree are very complex and can take more time to run. Pruning will normally need to take place to run the classification tree correctly. It is also not adequate for applying regression an predicting continuous values.</p>
  ")
  
  modInfo_string3 = HTML("<h3>Random Forest Model</h3>
  <p>A random forest model combines multiple decision trees to reach a single model for prediction. These models average across different trees to come to the final prediction model, often using bootstrapping. This type of model will less variance than a single tree. We can still look at the variable importance measured, but lose interpretability. Other advantages of using a random forest model is that we do not have to use all of the predictors. This model is preferred over bagged trees because it usually has less correlation due to strong predictors. One of the big limitations to a random forest model is that a large number of trees can make the algorithm too slow and ineffective in real time predictions. Random forest models are also more complicated to interpret than running a linear regression on data.</p>
  ")
  
modInfo_tab = tabItem("model_info",fluidRow(box(modInfo_string1, uiOutput('mod1'), modInfo_string2, modInfo_string3, width = 8)))
####################################################################################################

### Model Fitting #################################################################
modelFit_string = HTML("<h2>Model Information</h2>")

modFit_tab = tabItem("model_fit", fluidRow(box(modelFit_string, width = 8)))
####################################################################################################

### Data for Last Page ##############################################################
all_data = HTML("<h2>2019 MLB Data</h2>",)

data_table <- dataTableOutput("all_data")

data_tab = tabItem("data", fluidRow(all_data, data_table))
####################################################################################################

dashboardPage(
  title="MLB Dashboard",
  skin="red",
  dashboardHeader(title = "2019 MLB Season"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About This Data", tabName = "about", icon=icon("info")),
      menuItem("Data Exploration", tabName = "exploration", icon=icon("book-open")),
      menuItem("Modeling Info", tabName = "model_info", icon=icon("infinity")),
      menuItem("Model Fitting", tabName = "model_fit", icon=icon("plus")),
      menuItem("Prediction", tabName = "prediction", icon=icon("magnifying-glass")),
      menuItem("Data", tabName = "data", icon=icon("baseball"))
    ),
    HTML("<br>"),
    downloadButton("downloadAllData", "Download Data Set")
  ),
  dashboardBody(
    tabItems(
      about_tab, exploration_tab, modInfo_tab, modFit_tab, data_tab
    )
  )
)
