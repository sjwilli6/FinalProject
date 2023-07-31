## Spencer Williams
## Final Project ST558
## July 31, 2023

library(shinydashboard)
library(plotly)

### Info for the about section ###############################################################
about_string = HTML("<h2>2019 MLB Season</h2>
            <p>The Washington Nationals won the 2019 MLB World Series behind their star pitcher, Stephen Strasburg. Data was gathered throughout the regular 2019 season and can be found <a href='https://baseballguru.com/bbdata1.html'>here</a>, along with past years. Only the batting statistics will be used for analysis and visualizations.
            <br>
            The purpose of this app is to take a deeper look into the 2019 MLB Season. Ultimately, we want to be able to predict the amount of RBI's a player is going to hit in a future season. This will all depend on the type of model and variables chosen by the user.
            <br>
            The app will have a total of six tabs. The EDA tab will contain different plots using at-bats (AB) as a variable. The user can determine the other variable and output will be created. The average during the 2019 season will be produced along with the correct variables in the data table. There will also be a modeling information page that will have details about the three different models I have chosen. Details include an overall definition, advantages, and disadvantages. The model fitting tab will allow the user to determine the variables used in the models. Statistics and plots will be outputted to help choose which model is best based on the testing set. The prediction tab will allow the user to select different values for the predictors to obtain a prediction for the response. The last tab will be the full data set that the user can scroll through and filter as needed.
            <br>
            </p>")
mlb_image <- img(src = 'mlbpicture.png', align = 'center', height = '300px', width = '400px')

about_tab = tabItem("about",fluidRow(box(about_string, mlb_image, width = 8)))
####################################################################################################


### UI layout for the exploration tab ##############################################################
# Choices for figure/data
input_options = c("Homeruns", "RBI's", "Runs", "Batting Avg", "Stolen Bases")

# Create an input for the user
expl_controls = box(width = 4,
                    selectizeInput("expl_variable", "Select variable to examine: ", choices = input_options),
                    HTML("<br>"),
                    downloadButton("downloadPlot", label = "Download Current Plot"),
                    HTML("<br>"),
                    downloadButton("downloadData", label = "Download Current Data")
)

# Create the plot and data table based on the variable selected
expl_plot = box(title = uiOutput("expl_plot_title"), width = 8,
                plotOutput("expl_plot"),
                HTML("<br>"),
                textOutput("info"),
                dataTableOutput("statdf"))

# Output to tab
exploration_tab = tabItem("exploration", fluidRow(expl_controls, expl_plot))
###########################################################

# Model Paragraphs
modInfo_string1 = HTML("<h2>Model Information</h2>
  <br>
  <h3>Multiple Linear Regression</h3>
  <p>A multiple linear regression model is used to determine a mathematical relationship among several random variables. It is fit by minimizing the sum of squared errors. Multiple linear regression models can include explanatory variables and higher order terms for predicting the dependent variable. Numerical and categorical data can also be included in these prediction models. Also, a big advantage of using these models is that it does a good job of predicting a response for a linear relationship.</p>
  <p>Many assumptions are made to fit a multiple linear regression model on data. If one of these assumptions are not met, the linear regression model will not be accurate. Some of the assumptions are that the variance of the residuals are constant, observations are independent, and multivariate normality. Multiple linear regression models can also be hard to interpret if there are complex variables and interaction terms.</p>
  ")
# MathJAX
  uiOutput('mod1')
  
  modInfo_string2 = HTML("<h3>Classification Tree Analysis</h3>
  <p>A classification tree analysis is a structural mapping of binary decisions that lead to a prediction of a response variable. It is used for predicting a categorical response. Predictor variables are split up to create different prediction models for each model. Some of the advantages of using a classification tree analysis are that it’s easy to understand, predictors don’t need to be scaled, no statistical analysis assumptions necessary, built in variable selection, and it can handle numerical and categorical data. A disadvantage of using this type of analysis is that a small change in the data can change the outcome significantly. The calculations behind the tree are very complex and can take more time to run. Pruning will normally need to take place to run the classification tree correctly. It is also not adequate for applying regression an predicting continuous values.</p>
  ")
  
  modInfo_string3 = HTML("<h3>Random Forest Model</h3>
  <p>A random forest model combines multiple decision trees to reach a single model for prediction. These models average across different trees to come to the final prediction model, often using bootstrapping. This type of model will less variance than a single tree. We can still look at the variable importance measured, but lose interpretability. Other advantages of using a random forest model is that we do not have to use all of the predictors. This model is preferred over bagged trees because it usually has less correlation due to strong predictors. One of the big limitations to a random forest model is that a large number of trees can make the algorithm too slow and ineffective in real time predictions. Random forest models are also more complicated to interpret than running a linear regression on data.</p>
  ")
  
  # Output to tabs
modInfo_tab = tabItem("model_info",fluidRow(box(modInfo_string1, uiOutput('mod1'), modInfo_string2, modInfo_string3, width = 8)))
####################################################################################################

### Model Fitting #################################################################
# Breif explanation
modelFit_string = HTML("<h2>Model Information</h2>
                       <p>We will be fitting the three models to our data to predict the RBI variable. The models will be able to predict how many RBI's a player will have according to some of the hitting statistics recorded in the data set.</p>")


# Select size for training/testing sets
train_prop = box(width = 3, sliderInput("proportion", "Select Proportion for Training Data Set: ", min = 1, max = 100, value = 50, step = 1),)

# Select the variables
mod1_selection = box(width = 3,
                     checkboxGroupInput("mod1_var", "Variables to include in the Multiple Linear Regression Model: ",
                                        names(mlb_data[c(4:10, 12, 14:15, 21)])))
# Select the variables
mod2_selection = box(width = 3,
                     checkboxGroupInput("mod2_var", "Variables to include in the Classification Tree Model: ",
                                        names(mlb_data[c(4:10, 12, 14:15, 21)])))
# Select the variables
mod3_selection = box(width = 3,
                     checkboxGroupInput("mod3_var", "Variables to include in the Random Forest Model: ",
                                        names(mlb_data[c(4:10, 12, 14:15, 21)])))
# Action Button
run_selection = actionButton("ready", "Run the Models")

# Output the results
model_output = box(uiOutput("fitted_mod1"),
                   plotOutput("fitted_mod2"),
                   uiOutput("fitted_mod3"))

# Compare the models to the testing set
#compare_output = box(uiOutput("stat_1"),
 #                  uiOutput("stat_2"),
  #                 uiOutput("stat_3"))

# Output to tabs
modFit_tab = tabItem("model_fit", fluidRow(modelFit_string, train_prop, mod1_selection, mod3_selection, mod3_selection, run_selection, model_output))
####################################################################################################

### Prediction ##############################################################
pred_string = HTML("<h2>Prediction</h2>")

# Output to tabs
pred_tab = tabItem("prediction", fluidRow(pred_string))
####################################################################################################

### Data for Last Page ##############################################################
all_data = HTML("<h2>2019 MLB Data</h2>",)

# Create data table
data_table <- dataTableOutput("all_data")

# Output to tabs
data_tab = tabItem("data", fluidRow(all_data, data_table))
####################################################################################################

# Create dashboard with output tabs from above
dashboardPage(
  # Design pages
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
      about_tab, exploration_tab, modInfo_tab, modFit_tab, pred_tab, data_tab
    )
  )
)
