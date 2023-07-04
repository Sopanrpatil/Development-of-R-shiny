# load the required packages
library(tools)#to check file extension
library(readxl)
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(datasets)
library(caTools)
library(party)
library(magrittr)
library(class)#KNN
library(MASS)#Discriminant Analysis
library(e1071)#Naive Bayes
#library(shinyWidgets) 
library(rsample)  #data splitting  #detach("package:rsample", unload=TRUE)
library(lattice) # to visualize multipanel plot
library(caret)   # to train model by providing method



header <- dashboardHeader(
  title = "Model Monitoring Tool",
  #dropdownMenuOutput("messageMenu")
  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/sopan-patil-24a995210",icon("linkedin"),"Sopan Patil",target="_blank")),
  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/samiya-pathan-3a784b240",icon("linkedin"),"Samiya Pathan",target="_blank")),
  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/prachi-gore-4772a11a5",icon("linkedin"),"Prachi Gore",target="_blank"))
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("View",tabName = "view", icon = icon("eye")),
    menuItem("EDA", tabName = "DS", icon = icon("chart-column"),
             menuItem("Graphs", tabName = "graph", 
                      menuSubItem("Bar Chart", tabName = "Bar-Chart"),
                      menuSubItem("Box Plot", tabName = "Box-Plot"),
                      menuSubItem("Histogram", tabName = "Histogram"),
                      menuSubItem("Scatter Plot", tabName = "Scatter-Plot"),
                      menuSubItem("Line Chart", tabName = "Line-Chart")
             ),
             menuItem("Summary", tabName = "Summary")),
    menuItem("Model", tabName="model",icon = icon("list-alt"),
             menuItem("Regression", tabName = "regression",icon = icon("chart-line"),
                      menuSubItem("Simple Linear", tabName = "Simple"),
                      menuSubItem("Multiple Linear", tabName = "Multiple"),
                      menuSubItem("Polynomial", tabName = "Polynomial"),
                      menuSubItem("Logistic",tabName = "LogisticRegression"),
                      menuSubItem("Principle Component",tabName = "PCR")
             ),
             menuItem("Classification", tabName = "classification",icon=icon("sitemap"),
                      menuSubItem("Logistic", tabName = "LC"),
                      menuSubItem("KNN", tabName = "KNN"),
                      menuSubItem("LDA", tabName = "LDA"),
                      menuSubItem("QDA", tabName = "QDA"),
                      menuSubItem("Naive Bayes", tabName = "NB")))))

file_input=function(file_id){
  fileInput(inputId = file_id, label = "Select Dataset",
            accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".xlsx"),
            buttonLabel = "Browse...",
            placeholder = "No file selected")
  
}
file_input_model=function(file_id,label){
  fileInput(inputId = file_id, label = label,
            accept = c("text/csv","text/comma-separated-values,text/plain",".csv",".xlsx"),
            buttonLabel = "Browse...",
            placeholder = "No file selected")
  
}
select_input=function(input_id,label){selectInput(inputId = input_id,
                                                  label = label,
                                                  choices=NULL)
  
}

view_layout=sidebarPanel(file_input("file_view"),
                         # Input: Checkbox if file has header ----
                         checkboxInput(inputId = "header",label =  "Header", value=TRUE),
                         hr(),
                         # Input: Select number of rows to display ----
                         radioButtons(inputId = "disp",label =  "Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head"))

hist_layout=sidebarPanel(file_input("file_hist"),
                         select_input("hist_var_id","Select variable"),
                         sliderInput(inputId = "bins",label = "Number of bins:", min = 1, max = 50,value = 30)
                         
)
barchart_layout=sidebarPanel(file_input("file_bar"),select_input("bar_var1_id","Select Numerical variable"),select_input("bar_var2_id","Select Categorical variable"))
scatter_layout=sidebarPanel(file_input("file_scatter"),select_input("scatter_var1_id","Select x variable"),select_input("scatter_var2_id","Select Y variable"))
boxplot_layout=sidebarPanel(file_input("file_boxplot"),select_input("boxplot_var1_id","Select variable"),select_input("boxplot_var2_id","Select variable"))
line_layout=sidebarPanel(file_input("file_line"),select_input("line_var1_id","Select x variable"),select_input("line_var2_id","Select Y variable"))

summary_layout=sidebarPanel(file_input("file_summary"),
                            actionButton("calculate_summary", "Calculate Summary"))

model_layout=function(fileId,depVarId,indVarId,sizeId){
  return(sidebarPanel(file_input(fileId),
                      select_input(depVarId, "Select dependent variable"),
                      selectInput(indVarId, "Select independent variables", choices = NULL,multiple = TRUE),
                      numericInput(sizeId, label = "Enter a size of train dataset (eg., 80%)", value = 80,step=10,min=10,max=100))
  )}
model_layout_=function(fileId1,fileId2,depVarId,indVarId){
  return(sidebarPanel(file_input_model(fileId1,"Upload Train Dataset"),file_input_model(fileId2,"Upload Test Dataset"),
                      select_input(depVarId, "Select Response variable"),
                      selectInput(indVarId, "Select Predictor variables", choices = NULL,multiple = TRUE)
  )
  )}
logistic_layout=model_layout(fileId = "file_logistic",depVarId = "logd_var_id",indVarId = "logi_var_id",sizeId = "logistic_size")
logistic_layout_=model_layout_(fileId1 = "file_logistic_train",fileId2 = "file_logistic_test",depVarId = "logistic_response_id",indVarId = "logistic_pred_id")


knn_layout=sidebarPanel(file_input_model("file_knn_train","Upload Train Dataset"),file_input_model("file_knn_test","Upload Test Dataset"),select_input("knn_response_id","Select Response Variable"),
                        selectInput("knn_pred_id", "Select Predictors", choices = NULL,multiple = TRUE),numericInput(inputId = "k", label = "Enter a K:", value = "",min=1))
lda_layout=sidebarPanel(file_input_model("file_lda_train","Upload Train Dataset"),file_input_model("file_lda_test","Upload Test Dataset"),select_input("lda_response_id","Select Response Variable"),selectInput("lda_pred_id", "Select Predictors", choices = NULL,multiple = TRUE))
qda_layout=sidebarPanel(file_input_model("file_qda_train","Upload Train Dataset"),file_input_model("file_qda_test","Upload Test Dataset"),select_input("qda_response_id","Select Response Variable"),selectInput("qda_pred_id", "Select Predictors", choices = NULL,multiple = TRUE))
nb_layout=sidebarPanel(file_input_model("file_nb_train","Upload Train Dataset"),file_input_model("file_nb_test","Upload Test Dataset"),select_input("nb_response_id","Select Response Variable"),selectInput("nb_pred_id", "Select Predictors", choices = NULL,multiple = TRUE))


view_ui=fluidPage(
  title="Preview Dataset",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    view_layout,# sidebarpanel
    mainPanel (
      dataTableOutput("contents")
    )
  )
)
hist_ui=fluidPage(title="histogram",sidebarLayout(hist_layout,mainPanel (plotOutput("histogram"))))
scatter_ui=fluidPage(title="scatter plot",sidebarLayout(scatter_layout,mainPanel (plotOutput("scatter")) ))       
barchart_ui=fluidPage(title="barchart",sidebarLayout(barchart_layout,mainPanel (plotOutput("barchart"))))
boxplot_ui=fluidPage(title="boxplot",sidebarLayout(boxplot_layout,mainPanel (plotOutput("boxplot"))))
line_ui=fluidPage(title="linechart",sidebarLayout(line_layout,mainPanel (plotOutput("lineplot"))))
summary_ui=fluidPage(title="",sidebarLayout(summary_layout,mainPanel(verbatimTextOutput("result"))))
simple_ui=fluidPage(titlePanel("Simple Linear Regression"),
                    sidebarLayout(sidebarPanel(
                      fileInput("file_simple", "Choose Dataset",accept = c(".csv", ".xlsx")),
                      selectInput("response", "Select Response Variable", choices = NULL),
                      selectInput("predictor", "Select Predictor Variable", choices = NULL),
                      numericInput("train_prop", "Training data proportion", value = 0.7, min = 0, max = 1, step = 0.05),
                      actionButton("calculate_simple", "Calculate_simple")
                    ),mainPanel(plotOutput("plot_simple"),verbatimTextOutput("summary_simple"),plotOutput("residual_plot"))))
multi_ui=fluidPage(
  sidebarPanel(
    fileInput("file_multi", "Choose data file"),
    selectInput("dependent_var", "Select dependent variable", choices = NULL),
    selectInput("independent_var", "Select independent variables", choices = NULL, multiple = TRUE),
    numericInput("prop_train", "Proportion of data to use for training:", value = 0.7, min = 0, max = 1, step = 0.05),
    actionButton("train_test", "Fit Model")
  ),
  mainPanel(
    plotOutput("matrix_plot"),
    verbatimTextOutput("summary_multi")
  )
)
pcr_ui=fluidPage(titlePanel("Principle Component Regression"),
                 sidebarLayout(sidebarPanel(
                   fileInput("file_pcr", "Choose Dataset",accept = c(".csv", ".xlsx", ".txt")),
                   selectInput("x_var", "Select Independent Variables", choices = NULL, multiple = TRUE),
                   selectInput("y_var", "Select Dependent Variable", choices = NULL),
                   numericInput("n_components", "Number of Principal Components to Use", value = 3, min = 1),
                   numericInput("train_prop", "Training Data Proportion", value = 0.8, min = 0, max = 1, step = 0.05),
                   actionButton("runpcrRegression", "Run PCR Regression")),
                   mainPanel(
                     verbatimTextOutput("summary_pcr"))))
log_ui=fluidPage(
  titlePanel(" Logistic Regression "),
  sidebarLayout(sidebarPanel(
    fileInput("file_logistic_var", "Choose Dataset",accept = c(".csv", ".xlsx")),
    selectInput("Response_log", "Select Binary Response_log Variable", choices = NULL),
    selectInput("Predictors_log", "Select Independent Variables", choices = NULL, multiple = TRUE),
    numericInput("train_prop", "Training data proportion", value = 0.7, min = 0, max = 1, step = 0.05),
    actionButton("calculate", "Calculate")),
    mainPanel(verbatimTextOutput("summary_log"))))


poly_ui=fluidPage(titlePanel("Polynomial Regression"),
                  sidebarLayout(
                    sidebarPanel(fileInput("file", "Choose Dataset",accept = c(".csv", ".xlsx", ".txt")),
                                 selectInput("indep_var", "Select Independent Variables", choices = NULL, multiple = TRUE),
                                 selectInput("dep_var", "Select Dependent Variable", choices = NULL),
                                 numericInput("degree", "Degree of Polynomial", value = 1, min = 1, max = 10),
                                 numericInput("train_prop", "Training Data Proportion", value = 0.7, min = 0, max = 1, step = 0.05),
                                 actionButton("runRegression", "Run Regression")),
                    mainPanel(plotOutput("scatterplot"),verbatimTextOutput("summary"),plotOutput("residualPlot"))))

logistic_ui=fluidPage(
  tags$head(
    tags$style(HTML("
      .horizontal-radio .form-group div.radio {
        display: inline-block;
        margin-right: 10px;
      }
    "))
  ),
  div(class = "horizontal-radio",
      radioButtons("option_logistic", label = "",
                   choices = c("Specific splitting","Random splitting"))),
  conditionalPanel(
    condition = "input.option_logistic == 'Specific splitting'",
    title="Logistic Classification",
    sidebarLayout(logistic_layout_,mainPanel(h3(textOutput("textLogistic")),h4(textOutput("logisticAccuracy")),verbatimTextOutput("logisticCM")))
    # ,verbatimTextOutput("summaryLogistic")
  ),
  conditionalPanel(
    condition = "input.option_logistic == 'Random splitting'",
    title="Logistic Classification",
    sidebarLayout(logistic_layout,mainPanel(h3(textOutput("text_logistic")),h4(textOutput("logistic_accuracy")),verbatimTextOutput("logistic_cm")))
    # ,verbatimTextOutput("summary_logistic")
  ))

knn_ui = fluidPage(
  tags$head(
    tags$style(HTML("
      .horizontal-radio .form-group div.radio {
        display: inline-block;
        margin-right: 10px;
      }
    "))
  ),
  div(class = "horizontal-radio",
      radioButtons("option_knn", label = "",
                   choices = c("Specific splitting", "Random splitting"))),
  conditionalPanel(
    condition = "input.option_knn == 'Specific splitting'",
    title="K Nearest Neighbours",
    sidebarLayout(
      knn_layout,
      mainPanel (h3(textOutput("knnAccuracy")),verbatimTextOutput("knnCM"))
    )
  ),
  conditionalPanel(
    condition = "input.option_knn == 'Random splitting'",
    title="K Nearest Neighbours",
    sidebarLayout(
      # model_layout("file_knn","knnd_var_id","knni_var_id","knn_size"),
      sidebarPanel(file_input_model("file_knn","Upload Dataset"),select_input("knnd_var_id","Select Response Variable"),
                   selectInput("knni_var_id", "Select Predictors", choices = NULL,multiple = TRUE),numericInput(inputId = "k_", label = "Enter a K:", value = "",min=1),
                   numericInput("knn_size", label = "Enter a size of train dataset (eg., 80%)", value = 80,min=10,max=99.99,step=10)),
      mainPanel(h3(textOutput("knn_accuracy")),verbatimTextOutput("knn_cm"))
      
      
    )
  )
)
lda_ui = fluidPage(
  tags$head(
    tags$style(HTML("
      .horizontal-radio .form-group div.radio {
        display: inline-block;
        margin-right: 10px;
      }
    "))
  ),
  div(class = "horizontal-radio",
      radioButtons("option_lda", label = "",
                   choices = c("Specific splitting", "Random splitting"))),
  conditionalPanel(
    condition = "input.option_lda == 'Specific splitting'",
    title="Linear Discriminant Analysis",
    sidebarLayout(
      lda_layout,
      mainPanel (h3(textOutput("ldaAccuracy")),verbatimTextOutput("ldaCM"))
    )
  ),
  conditionalPanel(
    condition = "input.option_lda == 'Random splitting'",
    title="Linear Discriminant Analysis",
    sidebarLayout(
      model_layout("file_lda","ldad_var_id","ldai_var_id","lda_size"),
      mainPanel(h3(textOutput("lda_accuracy")),verbatimTextOutput("lda_cm"))
    )
  )
)

qda_ui=fluidPage(
  tags$head(
    tags$style(HTML("
      .horizontal-radio .form-group div.radio {
        display: inline-block;
        margin-right: 10px;
      }
    "))
  ),
  div(class = "horizontal-radio",
      radioButtons("option_qda", label = "",
                   choices = c("Specific splitting", "Random splitting"))),
  conditionalPanel(
    condition = "input.option_qda == 'Specific splitting'",
    title="Quadratic Discriminant Analysis",
    
    sidebarLayout(
      qda_layout,
      mainPanel (h3(textOutput("qdaAccuracy")),verbatimTextOutput("qdaCM"))
    )
  ),
  conditionalPanel(
    condition = "input.option_qda == 'Random splitting'",
    title="Quadratic Discriminant Analysis",
    sidebarLayout(
      model_layout("file_qda","qdad_var_id","qdai_var_id","qda_size"),
      mainPanel(h3(textOutput("qda_accuracy")),verbatimTextOutput("qda_cm"))
    )
  )
)

nb_ui=fluidPage(
  tags$head(
    tags$style(HTML("
      .horizontal-radio .form-group div.radio {
        display: inline-block;
        margin-right: 10px;
      }
    "))
  ),
  div(class = "horizontal-radio",
      radioButtons("option_nb", label = "",
                   choices = c("Specific splitting", "Random splitting"))),
  conditionalPanel(
    condition = "input.option_nb == 'Specific splitting'",
    title="Naive Bayes",
    sidebarLayout(
      nb_layout,
      mainPanel (h3(textOutput("nbAccuracy")),verbatimTextOutput("nbCM"))
    )
  ),
  conditionalPanel(
    condition = "input.option_nb == 'Random splitting'",
    title="Naive Bayes",
    sidebarLayout(
      model_layout("file_nb","nbd_var_id","nbi_var_id","nb_size"),
      mainPanel(h3(textOutput("nb_accuracy")),verbatimTextOutput("nb_cm"))
    )
  )
)


body <- dashboardBody(
  tabItems(
    tabItem("view",view_ui),
    tabItem("Bar-Chart",barchart_ui),
    tabItem("Box-Plot",boxplot_ui),
    tabItem("Histogram",hist_ui),
    tabItem("Scatter-Plot",scatter_ui),
    
    tabItem("Line-Chart",line_ui),
    tabItem("Simple",simple_ui),
    tabItem("Multiple",multi_ui),
    tabItem("Polynomial",poly_ui),
    tabItem("LogisticRegression",log_ui),
    tabItem("PCR",pcr_ui),
    tabItem("Summary",summary_ui),
    
    tabItem("LC",logistic_ui),
    tabItem("KNN",knn_ui),
    tabItem("LDA",lda_ui),
    tabItem("QDA",qda_ui),
    tabItem("NB",nb_ui)
    
    
  )
)
ui = dashboardPage(
  header,
  sidebar,
  body,
  title = "Model Monitoring Tool"
)

server <- function(input, output,session) {
  
  data_view= reactive({
    
    req(input$file_view)
    file_ext= file_ext(input$file_view$datapath)
    
    
    if(file_ext=="xlsx"|file_ext=="xls"){
      if(!input$header){
        df=read_excel(input$file_view$datapath)
        names(df)=NULL
        
      }else{
        df=read_excel(input$file_view$datapath)
      }
    }
    else{
      df <- read.csv(input$file_view$datapath,header = input$header )
    }
    if(input$disp == "head") {
      return(head(df))
    }else{
      return(df)
    }
  })
  #histogram
  
  data_hist= reactive({
    
    req(input$file_hist)
    file_ext= file_ext(input$file_hist$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_hist$datapath)
      return(select_if(df, is.numeric))
    }
    else{
      df <- read.csv(input$file_hist$datapath )
      return(select_if(df, is.numeric))
    }
    
  })
  #scatter
  data_scatter=reactive({
    req(input$file_scatter)
    file_ext= file_ext(input$file_scatter$datapath)
    
    if(file_ext=="xlsx"|file_ext=="xls"){
      df=read_excel(input$file_scatter$datapath)
      
    }
    else{
      df <- read.csv(input$file_scatter$datapath )
      
    }
    return(select_if(df, is.numeric))
  })
  # bar chart
  data_barchart= reactive({
    
    req(input$file_bar)
    file_ext= file_ext(input$file_bar$datapath)
    
    
    if(file_ext=="xlsx"|file_ext=="xls"){
      df=as.data.frame(read_excel(input$file_bar$datapath))
    }
    else{
      df <- read.csv(input$file_bar$datapath )
    }
    return(df)
    
  })
  data_bar_numeric=reactive(select_if(data_barchart(), is.numeric))
  data_bar_categorical=reactive(select_if(data_barchart(), is.character))
  
  # boxplot
  data_boxplot= reactive({
    
    req(input$file_boxplot)
    file_ext= file_ext(input$file_boxplot$datapath)
    
    
    if(file_ext=="xlsx"|file_ext=="xls"){
      df=as.data.frame(read_excel(input$file_boxplot$datapath))
    }
    else{
      df <- read.csv(input$file_boxplot$datapath )
    }
    return(df)
    
  })
  data_boxplot_numeric=reactive(select_if(data_boxplot(), is.numeric))
  data_boxplot_categorical=reactive(select_if(data_boxplot(), is.character))
  
  #lineplot
  data_line=reactive({
    req(input$file_line)
    file_ext= file_ext(input$file_line$datapath)
    
    if(file_ext=="xlsx"|file_ext=="xls"){
      df=read_excel(input$file_line$datapath)
      
    }
    else{
      df <- read.csv(input$file_line$datapath )
      
    }
    return(as.data.frame(select_if(df, is.numeric)))
  })
  
  
  
  #summary
  data_summary= reactive({
    
    req(input$file_summary)
    file_ext= file_ext(input$file_summary$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_summary$datapath)
      
    }
    else{
      df <- read.csv(input$file_summary$datapath )
      
    }
    return(df)
    
  })
  

  ## principle component regression
  data_pcr <- reactive({
    req(input$file_pcr)
    if (endsWith(input$file_pcr$name, ".csv")) {
      read.csv(input$file_pcr$datapath, header = TRUE)
    } else if (endsWith(input$file_pcr$name, ".xlsx")) {
      read_excel(input$file_pcr$datapath, sheet = 1)
    } else if (endsWith(input$file_pcr$name, ".txt")) {
      read.table(input$file_pcr$datapath, header = TRUE)
    }
  })
  observe({
    req(data_pcr())
    updateSelectInput(session, "x_var", choices = names(data_pcr()), selected = names(data_pcr())[1:2])
    updateSelectInput(session, "y_var", choices = names(data_pcr()), selected = names(data_pcr())[3])
  })
  pcrFit <- eventReactive(input$runpcrRegression, {
    y_var <- input$y_var
    x_vars <- input$x_var
    formula <- as.formula(paste(y_var, "~ ."))
    train_size <- input$train_prop
    n <- nrow(data_pcr())
    test_size <- 1 - train_size
    test_index <- sample(1:n, round(n*test_size))
    train_data <- data_pcr()[-test_index,]
    test_data <- data_pcr()[test_index,]
    pcr_model <- preProcess(train_data[, x_vars], method = "pca", pcaComp = input$n_components)
    pcr_fit <- train(x = predict(pcr_model, train_data[, x_vars]), 
                     y = train_data[, y_var], 
                     method = "lm")
    list(pcr_fit = pcr_fit, 
         summary = summary(pcr_fit), 
         pcr_model = pcr_model, 
         test_data = test_data, 
         train_data = train_data)
  })
  output$summary_pcr <- renderPrint({
    req(input$runpcrRegression)
    cat("Training data proportion: ", input$train_prop, "\n\n")
    cat("Test data proportion: ", round(nrow(pcrFit()$test_data) / nrow(data_pcr()), 2), "\n\n")
    pcrFit()$summary
  })
  # logistic data
  data_logistic_train= reactive({
    
    req(input$file_logistic_train)
    file_ext= file_ext(input$file_logistic_train$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_logistic_train$datapath)
      
    }
    else{
      df = read.csv(input$file_logistic_train$datapath )
      
    }
    return(df)
  })
  data_logistic_test= reactive({
    
    req(input$file_logistic_test)
    file_ext= file_ext(input$file_logistic_test$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_logistic_test$datapath)
      
    }
    else{
      df = read.csv(input$file_logistic_test$datapath )
      
    }
    return(df)
  })
  data_logistic = reactive({
    req(input$file_logistic)
    file_ext= file_ext(input$file_logistic$datapath)
    
    if(file_ext=="xlsx"|file_ext=="xls"){
      df=read_excel(input$file_logistic$datapath)
      
    }
    else{
      df = read.csv(input$file_logistic$datapath )
      
    }
    return(df)
  })
  #KNN Data
  data_knn_train= reactive({
    
    req(input$file_knn_train)
    file_ext= file_ext(input$file_knn_train$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_knn_train$datapath)
      
    }
    else{
      df = read.csv(input$file_knn_train$datapath )
      
    }
    return(df)
  })
  data_knn_test= reactive({
    
    req(input$file_knn_test)
    file_ext= file_ext(input$file_knn_test$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_knn_test$datapath)
      
    }
    else{
      df = read.csv(input$file_knn_test$datapath )
      
    }
    return(as.data.frame(df))
  })
  data_knn= reactive({
    
    req(input$file_knn)
    file_ext= file_ext(input$file_knn$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_knn$datapath)
      
    }
    else{
      df = read.csv(input$file_knn$datapath )
      
    }
    #View(df)
    return(as.data.frame(df))
  })
  
  #lda Data
  data_lda_train= reactive({
    
    req(input$file_lda_train)
    file_ext= file_ext(input$file_lda_train$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_lda_train$datapath)
      
    }
    else{
      df = read.csv(input$file_lda_train$datapath )
      
    }
    return(as.data.frame(df))
  })
  data_lda_test= reactive({
    
    req(input$file_lda_test)
    file_ext= file_ext(input$file_lda_test$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_lda_test$datapath)
      
    }
    else{
      df = read.csv(input$file_lda_test$datapath )
      
    }
    return(as.data.frame(df))
  })
  data_lda= reactive({
    
    req(input$file_lda)
    file_ext= file_ext(input$file_lda$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_lda$datapath)
      
    }
    else{
      df = read.csv(input$file_lda$datapath )
      
    }
    #View(df)
    return(as.data.frame(df))
  })
  #qda Data
  data_qda_train= reactive({
    
    req(input$file_qda_train)
    file_ext= file_ext(input$file_qda_train$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_qda_train$datapath)
      
    }
    else{
      df = read.csv(input$file_qda_train$datapath )
      
    }
    return(as.data.frame(df))
  })
  data_qda_test= reactive({
    
    req(input$file_qda_test)
    file_ext= file_ext(input$file_qda_test$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_qda_test$datapath)
      
    }
    else{
      df = read.csv(input$file_qda_test$datapath )
      
    }
    return(as.data.frame(df))
  })
  data_qda= reactive({
    
    req(input$file_qda)
    file_ext= file_ext(input$file_qda$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_qda$datapath)
      
    }
    else{
      df = read.csv(input$file_qda$datapath )
      
    }
    #View(df)
    return(as.data.frame(df))
  })
  #nb Data
  data_nb_train= reactive({
    
    req(input$file_nb_train)
    file_ext= file_ext(input$file_nb_train$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_nb_train$datapath)
      
    }
    else{
      df = read.csv(input$file_nb_train$datapath )
      
    }
    return(as.data.frame(df))
  })
  data_nb_test= reactive({
    
    req(input$file_nb_test)
    file_ext= file_ext(input$file_nb_test$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_nb_test$datapath)
      
    }
    else{
      df = read.csv(input$file_nb_test$datapath )
      
    }
    return(as.data.frame(df))
  })
  data_nb= reactive({
    
    req(input$file_nb)
    file_ext= file_ext(input$file_nb$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_nb$datapath)
      
    }
    else{
      df = read.csv(input$file_nb$datapath )
      
    }
    
    return(as.data.frame(df))
  })
  
  
  
  update_input= function(input_id,label,data){
    return(
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = input_id,
        label = label,
        choices = names(data()),
        selected = NULL
      ) )
  }
  update_input_categorical= function(input_id,label,data){
    return(
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = input_id,
        label = label,
        choices = names(select_if(data(), is.character)),
        selected = NULL
      ) )
  }
  update_input_numerical= function(input_id,label,data){
    return(
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = input_id,
        label = label,
        choices = names(select_if(data(), is.numeric)),
        selected = names(select_if(data(), is.numeric))[1:2]
      ) )
  }
  
  observe(update_input("hist_var_id","choose Variable",data_hist))
  observe(update_input("scatter_var1_id",label="select X variable",data=data_scatter))
  observe(update_input("scatter_var2_id",label="select Y variable",data=data_scatter))
  observe(update_input("bar_var1_id","choose Numerical Variable",data_bar_numeric))
  observe(update_input("bar_var2_id","choose Categorical Variable",data_bar_categorical))
  observe(update_input("boxplot_var1_id",label="select x variable",data_boxplot_categorical))
  observe(update_input("boxplot_var2_id",label="select Y variable",data_boxplot_numeric))
  
  observe(update_input("line_var1_id",label="select X variable",data_line))
  observe(update_input("line_var2_id",label="select Y variable",data_line))
  observe({update_input("summary_var","Select Variable",data_summary)})
  
  #logistic random splitting
  observe({updateSelectInput(
    session = getDefaultReactiveDomain(),
    inputId = "logd_var_id",
    label = "Select Dependent Variable",
    
    choices =names(data_logistic())[sapply(data_logistic(), function(x) (setequal(c(0,1),unique(x))))],
    selected = NULL
  )
    update_input("logi_var_id","Select Independent Variable",data_logistic)})
  
  #logistic specific splitting
  observe({updateSelectInput(
    session = getDefaultReactiveDomain(),
    inputId = "logistic_response_id",
    label = "Select Dependent Variable",
    
    choices =names(data_logistic_test())[sapply(data_logistic_test(), function(x) (setequal(c(0,1),unique(x))))],
    selected = NULL
  )
    
    update_input("logistic_pred_id","Select Independent Variable",data_logistic_test)})
  
  
  
  observe({update_input_categorical("knn_response_id","Select Response Variable",data_knn_test)
    update_input_numerical("knn_pred_id","Select Predictors",data_knn_test)})
  
  observe({
    update_input_categorical("knnd_var_id","Select Response Variable",data_knn)
    update_input_numerical("knni_var_id","Select Predictors",data_knn)
  })
  
  observe({update_input_categorical("lda_response_id","Select Response Variable",data_lda_test)})
  observe({update_input_numerical("lda_pred_id","Select Predictors",data_lda_test)})
  
  observe({
    update_input_categorical("ldad_var_id","Select Response Variable",data_lda)
    update_input_numerical("ldai_var_id","Select Predictors",data_lda)
  })
  
  observe({update_input_categorical("qda_response_id","Select Response Variable",data_qda_test)})
  observe({update_input_numerical("qda_pred_id","Select Predictors",data_qda_test)})
  
  observe({
    update_input_categorical("qdad_var_id","Select Response Variable",data_qda)
    update_input_numerical("qdai_var_id","Select Predictors",data_qda)
  })
  
  observe({update_input_categorical("nb_response_id","Select Response Variable",data_nb_test)})
  observe({update_input_numerical("nb_pred_id","Select Predictors",data_nb_test)})
  
  observe({
    update_input_categorical("nbd_var_id","Select Response Variable",data_nb)
    update_input_numerical("nbi_var_id","Select Predictors",data_nb)
  })
  output$contents <-renderDataTable(data_view())
  
  theme=theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    panel.background = element_rect(fill = "lightgray"),
    plot.title = element_text(size = 16, color = "darkblue", face = "bold",hjust = 0.5,margin = margin(b = 20)),
    axis.title = element_text(size = 14, color = "black", face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(size = 12, color = "black")
    
  )
  output$histogram <- renderPlot({
    req(input$hist_var_id)
    x = as.numeric(unlist(data_hist()[,input$hist_var_id]))

    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    ggplot(data.frame(x), aes(x)) +
      geom_histogram(breaks = bins, color = "black", fill = "blue") +
      labs(x = input$hist_var_id, y = "Frequency") +
      ggtitle(paste("Histogram of ",input$hist_var_id))+
      
      
      theme
  }
  )
  output$scatter <- renderPlot({
    req(input$scatter_var1_id,input$scatter_var2_id)
    
    x = as.numeric(unlist(data_scatter()[,input$scatter_var1_id]))
    y=as.numeric(unlist(data_scatter()[,input$scatter_var2_id]))
    ggplot(data.frame(x,y), aes(x = x, y = y)) +
      geom_point(color = "blue")+
      labs(x = input$scatter_var1_id, y =input$scatter_var2_id ) +
      ggtitle(paste("Scatter Plot of ",input$scatter_var1_id," VS ",input$scatter_var2_id))+
      
      
      theme
  }
  )
  output$barchart <- renderPlot({
    req(input$bar_var1_id,input$bar_var2_id)
    y=data_barchart()[,input$bar_var1_id]
    x=data_barchart()[,input$bar_var2_id]
    ggplot(data=data_barchart(), aes(x=x, y=y)) +
      
      geom_bar(stat="identity",fill="blue",color="black" )+
      xlab(input$bar_var2_id) +
      ylab(input$bar_var1_id) +
      ggtitle(paste("Barchart of" ,input$bar_var2_id,"vs",input$bar_var1_id))+
      
      theme
    
  }
  )
  output$boxplot <- renderPlot({
    req(input$boxplot_var1_id,input$boxplot_var2_id)
    
    x = data_boxplot()[,input$boxplot_var1_id]
    y=data_boxplot()[,input$boxplot_var2_id]
    # boxplot(y~x,xlab=input$boxplot_var1_id,ylab=input$boxplot_var2_id,main = paste("Boxplot of" ,input$boxplot_var1_id," vs ",input$boxplot_var2_id))
    ggplot(data_boxplot(), aes(x = x, y =y )) +
      geom_boxplot(width = 0.5, outlier.shape = 1, fill = "blue", color = "black")+
      labs(x = input$boxplot_var1_id, y =input$boxplot_var2_id ) +
      ggtitle(paste("Boxplot Plot of ",input$boxplot_var1_id," VS ",input$boxplot_var2_id))+
      
      
      theme
    
  }
  )
  output$lineplot <- renderPlot({
    req(input$line_var1_id,input$line_var2_id)
    x = data_line()[,input$line_var1_id]
    y=data_line()[,input$line_var2_id]
    # plot(x,y,type = "l",xlab=input$line_var1_id,ylab=input$line_var2_id,main = paste("Line plot of" ,input$line_var1_id,"vs",input$line_var2_id))
    ggplot(data.frame(x,y), aes(x = x, y = y)) +
      geom_line(color = "blue", size = 1.5) +
      labs(x = input$line_var1_id, y =input$line_var2_id ) +
      ggtitle(paste("line Plot of ",input$line_var1_id," VS ",input$line_var2_id))+
      theme
  }
  )
  # Calculate summary statistics when button is pressed
  observeEvent(input$calculate_summary, {
    output$result <- renderPrint({
      summary(data_summary()) # to get summary of all columns of dataset
    }) })
  
  # Simple linear regression
  data_simple <- reactive({
    if (is.null(input$file_simple)) {return(NULL)}
    ext <- tools::file_ext(input$file_simple$datapath)
    switch(ext, 
           "csv" = read.csv(input$file_simple$datapath),
           "xlsx" = read_xlsx(input$file_simple$datapath))
  })
  observe({
    if (!is.null(data_simple())) {
      updateSelectInput(session, "response", choices = names(data_simple()))
      updateSelectInput(session, "predictor", choices = names(data_simple()), selected = names(data_simple())[1])
    }
  })
  # Generate and display plot
  output$plot_simple <- renderPlot({
    req(input$response)
    req(input$predictor)
    req(input$calculate_simple)
    # Split data into training and testing sets
    split <- initial_split(data_simple(), prop = input$train_prop)
    train_data <- training(split)
    # Fit linear regression model
    formula <- as.formula(paste(input$response, "~", input$predictor))
    model <- lm(formula = formula, data = train_data)
    # Create scatter plot with regression line
    plot1 <- ggplot(data_simple(), aes_string(x = input$predictor, y = input$response)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE)
    # Create residual plot
    plot2 <- ggplot(model, aes_string(x = ".fitted", y = ".resid")) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed")
    # Arrange plots
    gridExtra::grid.arrange(plot1, plot2, ncol = 2)
  })
  # Generate and display summary output
  output$summary_simple <- renderPrint({
    req(input$response)
    req(input$predictor)
    req(input$calculate_simple)
    # Split data into training and testing sets
    split <- initial_split(data_simple(), prop = input$train_prop)
    train_data <- training(split)
    # Fit linear regression model
    formula <- as.formula(paste(input$response, "~", input$predictor))
    model <- lm(formula = formula, data = train_data)
    # Display summary output
    cat("Training set size:", nrow(train_data), "\n")
    summary(model)
  })
  
  data_multiple <- reactive({
    req(input$file_multi)
    file_ext <- tools::file_ext(input$file_multi$datapath)
    if (file_ext %in% c("xlsx", "xls")) {
      df <- readxl::read_excel(input$file_multi$datapath)
    } else {
      df <- readr::read_csv(input$file_multi$datapath)
    }
    return(dplyr::select_if(df, is.numeric))
  })
  
  # Multiple linear regression
  observe({
    req(data_multiple())
    updateSelectInput(session, "dependent_var", choices = names(data_multiple()))
    updateSelectInput(session, "independent_var", choices = names(data_multiple()))
  })
  
  data_train_test <- eventReactive(input$train_test, {
    set.seed(123)
    prop_train <- input$prop_train
    train_index <- sample(1:nrow(data_multiple()), round(prop_train*nrow(data_multiple())))
    train_data <- data_multiple()[train_index, ]
    test_data <- data_multiple()[-train_index, ]
    list(train_data = train_data, test_data = test_data)
  })
  
  output$matrix_plot <- renderPlot({
    req(input$independent_var, input$dependent_var, data_train_test())
    pairs(data_train_test()[["train_data"]][c(input$dependent_var, input$independent_var)])
  })
  
  output$summary_multi <- renderPrint({
    req(input$independent_var, input$dependent_var, input$train_test, data_train_test())
    if (input$train_test > 0) {
      model <- lm(formula = as.formula(paste(input$dependent_var, paste(input$independent_var, collapse = " + "), sep = " ~ ")), data = data_train_test()[["train_data"]])
      summary(model)
    }
  })
  # polynomial regression
  data <- reactive({
    req(input$file)
    if (endsWith(input$file$name, ".csv")) {
      read.csv(input$file$datapath, header = TRUE)
    } else if (endsWith(input$file$name, ".xlsx")) {
      read_excel(input$file$datapath, sheet = 1)
    } else if (endsWith(input$file$name, ".txt")) {
      read.table(input$file$datapath, header = TRUE)
    }
  })
  # Update variable choices based on uploaded data
  observe({
    req(data())
    numeric_vars <- sapply(data(), is.numeric)
    updateSelectInput(session, "indep_var", choices = names(data()[numeric_vars]), selected = names(data())[numeric_vars][1:2])
    updateSelectInput(session, "dep_var", choices = names(data()[numeric_vars]), selected = names(data())[numeric_vars][3])
  })
  # Define polynomial fit
  polyFit <- eventReactive(input$runRegression, {
    dep_var <- input$dep_var
    indep_vars <- paste(input$indep_var, collapse = " + ")
    formula <- as.formula(paste(dep_var, "~ poly(", indep_vars, 
                                ", degree = ", input$degree, ")"))
    fit <- lm(formula, data = data())
    train_size <- input$train_prop
    n <- nrow(data())
    test_size <- 1 - train_size
    test_index <- sample(1:n, round(n*test_size))
    test_data <- data()[test_index,]
    train_data <- data()[-test_index,]
    list(fit = fit, summary = summary(fit), test_data = test_data, train_data = train_data)
  })
  # Display summary
  output$summary <- renderPrint({
    req(input$runRegression)
    polyFit()$summary
  })
  # Matrix scatterplot
  output$scatterplot <- renderPlot({
    req(data())
    vars <- c(input$dep_var, input$indep_var) # include the dependent variable
    pairs(data()[, vars])
  })
  # Fitted values versus residuals plot
  output$residualPlot <- renderPlot({
    req(polyFit())
    plot(polyFit()$fit$fitted.values, polyFit()$fit$residuals, 
         xlab = "Fitted Values", ylab = "Residuals", 
         main = "Fitted Values vs Residuals")
  })
  # Action button
  observeEvent(input$runRegression, {output$plot <- renderPlot({})})
  data_log <- reactive({
    if (is.null(input$file_logistic)) {return(NULL)}
    ext <- tools::file_ext(input$file_logistic$datapath)
    switch(ext, "csv" = read.csv(input$file_logistic$datapath),
           "xlsx" = read_xlsx(input$file_logistic$datapath))
  })
  observe({
    if (!is.null(data_log())) {
      updateSelectInput(session, "Response_log", choices = names(data_log()))
      updateSelectInput(session, "Predictors_log", choices = names(data_log()), selected = names(data_log())[1])}
  })
  # Generate and display curve plot
  output$curve <- renderPlot({
    req(input$Response_log)
    req(input$Predictors_log)
    req(input$calculate)
    # Split data into training and testing sets
    split <- initial_split(data_log(), prop = input$train_prop)
    train_data <- training(split)
    # Create sigmoid plot
    ggplot(data_log(), aes_string(x = input$Predictors_log[1], y = input$Response_log)) +
      geom_point() +
      stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)
  })
  # Generate and display summary output
  output$summary_log<- renderPrint({
    req(input$Response_log)
    req(input$Predictors_log)
    req(input$calculate)
    # Split data into training and testing sets
    split <- initial_split(data_log(), prop = input$train_prop)
    train_data <- training(split)
    # Fit logistic regression model_log
    formula <- as.formula(paste(input$Response_log, "~", paste(input$Predictors_log, collapse="+")))
    model_log <- glm(formula = formula, data = train_data, family = binomial)
    # Display summary output
    cat("Training set size:", nrow(train_data), "\n")
    summary(model_log)
  })
  
  # logistic regression fitting
  # Read in the file and update choices for Response_log and predictor variables
  data_log <- reactive({
    if (is.null(input$file_logistic_var)) {
      return(NULL)
    }
    ext <- tools::file_ext(input$file_logistic_var$datapath)
    switch(ext, 
           "csv" = read.csv(input$file_logistic_var$datapath),
           "xlsx" = read_xlsx(input$file_logistic_var$datapath))
  })
  
  observe({
    if (!is.null(data_log())) {
      updateSelectInput(session, "Response_log", choices = names(data_log()))
      updateSelectInput(session, "Predictors_log", choices = names(data_log()), selected = names(data_log())[1])
    }
  })
  # Generate and display summary output
  output$summary_log<- renderPrint({
    req(input$Response_log)
    req(input$Predictors_log)
    req(input$calculate)
    
    # Split data into training and testing sets
    split <- initial_split(data_log(), prop = input$train_prop)
    train_data <- training(split)
    
    # Fit logistic regression model_log
    formula <- as.formula(paste(input$Response_log, "~", paste(input$Predictors_log, collapse="+")))
    model_log <- glm(formula = formula, data = train_data, family = binomial)
    
    # Display summary output
    cat("Training set size:", nrow(train_data), "\n")
    summary(model_log)
  })
  
  # logistic classification
  # logistic random splitting
  train_indices_logistic = reactive({
    req(input$logistic_size, data_logistic())
    return(sample(nrow(data_logistic()), round(0.01*(as.numeric(input$logistic_size)) * nrow(data_logistic()))))
  })
  train_data_logistic = reactive({
    req(data_logistic(),train_indices_logistic(),input$logistic_size)
    return(data_logistic()[train_indices_logistic(), ])
    
  })
  test_data_logistic =  reactive({
    req(data_logistic(),train_indices_logistic())
    return(data_logistic()[-(train_indices_logistic()), ])
  })
  logistic_model=reactive({
    req(input$logi_var_id, input$logd_var_id,input$logistic_size)
    glm(formula = as.formula(paste(input$logd_var_id, paste(input$logi_var_id, collapse = " + "), sep = " ~ ")), data = train_data_logistic(),family = "binomial") 
    
  })
  output$summary_logistic = renderPrint({
    req(logistic_model())
    summary(logistic_model())
  })
  #predict 
  predict_logistic_=reactive({
    req(logistic_model())
    predicted_prob=predict(logistic_model(), newdata = test_data_logistic(),type="response")
    predicted_classes=ifelse(predicted_prob > 0.5, 1, 0)
    predicted_classes
  })
  #Accuracy
  output$logistic_accuracy=renderText({
    req(test_data_logistic())
    # Get the accuracy of model
    accuracy = paste("Accuracy of logistic model is",round(mean(predict_logistic_() == test_data_logistic()[,input$logd_var_id])*100,2),"%")
    accuracy
  })
  output$logistic_cm=renderPrint({
    req(test_data_logistic(),input$logd_var_id)
    # Get the confusion matrix
    confusion = table( actual=as.numeric(unlist(test_data_logistic()[,input$logd_var_id])),predicted=predict_logistic_())
    confusion
  })
  output$text_logistic=renderText({
    req(logistic_model())
    return("model's performance on the test data")
  }
  )
  #logistic specific splitting
  logistic.model=reactive({
    req(input$logistic_pred_id, input$logistic_response_id,input$logistic_size)
    glm(formula = as.formula(paste(input$logistic_response_id, paste(input$logistic_pred_id, collapse = " + "), sep = " ~ ")), data = data_logistic_train(),family = "binomial") 
    
  })
  output$summaryLogistic = renderPrint({
    req(logistic.model())
    summary(logistic.model())
  })
  #predict 
  predict_logistic=reactive({
    req(logistic.model())
    predicted_prob=predict(logistic.model(), newdata = data_logistic_test(),type="response")
    predicted_classes=ifelse(predicted_prob > 0.5, 1, 0)
    predicted_classes
  })
  #Accuracy
  output$logisticAccuracy=renderText({
    req(data_logistic_test())
    # Get the accuracy of model
    accuracy = paste("Accuracy of logistic model is",round(mean(predict_logistic() == data_logistic_test()[,input$logistic_response_id])*100,2),"%")
    accuracy
  })
  output$logisticCM=renderPrint({
    req(data_logistic_test(),input$logistic_response_id)
    # Get the confusion matrix
    confusion = table( actual=as.numeric(unlist(data_logistic_test()[,input$logistic_response_id])),predicted=predict_logistic())
    confusion
  })
  output$textLogistic=renderText({
    req(logistic.model())
    return("model's performance on the test data")
  }
  )

  train_indices_knn = reactive({
    req(input$knn_size,data_knn())
    z=sample(nrow(data_knn()), round(0.01*(as.numeric(input$knn_size)) * nrow(data_knn())))
    return(z)
  })
  train_data_knn = reactive({
    req(data_knn(),train_indices_knn(),input$knn_size)
    return(data_knn()[train_indices_knn(), ])
    
  })
  test_data_knn =  reactive({
    req(data_knn(),train_indices_knn())
    return(data_knn()[-(train_indices_knn()), ])
  })
  knn_model=reactive({
    req(input$file_knn,input$k_,input$knnd_var_id,input$knni_var_id)
    knn_model = knn(train =as.data.frame(scale(train_data_knn()[,input$knni_var_id])), test = as.data.frame(scale(test_data_knn()[,input$knni_var_id])), cl = train_data_knn()[,input$knnd_var_id], k = input$k_)
    
    return(knn_model)
    
  })
  output$knn_accuracy=renderText({
    req(test_data_knn(),input$knnd_var_id)
    # Get the accuracy of model
    accuracy = paste("Accuracy of KNN model with k=",input$k_," is",round(mean(knn_model() == test_data_knn()[,input$knnd_var_id])*100,2),"%")
    #print(accuracy)
    return(accuracy)
  })
  output$knn_cm=renderPrint({
    req(test_data_knn(),input$knnd_var_id,knn_model())
    # Get the confusion matrix
    confusion = table( actual=test_data_knn()[,input$knnd_var_id],predicted=knn_model())
    return(confusion)
  })
  
  #KNN specific splitting
  knn.model=reactive({
    req(input$file_knn_train,input$file_knn_test,input$k,input$knn_response_id,input$knn_pred_id)
    train.data=(data_knn_train())
    test.data=(data_knn_test())
    # print(data_knn_train())
    # View(data_knn_train())
    
    # Fit a KNN model with k
    knn.model = knn(train =as.data.frame(scale(train.data[,input$knn_pred_id])), test = as.data.frame(scale(test.data[,input$knn_pred_id])), cl = train.data[,input$knn_response_id], k = input$k)
    return(knn.model)
  })
  output$knnAccuracy=renderText({
    req(data_knn_test(),input$knn_response_id)
    # Get the accuracy of model
    accuracy = paste("Accuracy of KNN model with k=",input$k," is",round(mean(knn.model() == data_knn_test()[,input$knn_response_id])*100,2),"%")
    accuracy
  })
  output$knnCM=renderPrint({
    req(data_knn_test(),input$knn_response_id,knn.model())
    # Get the confusion matrix
    confusion = table( actual=data_knn_test()[,input$knn_response_id],predicted=knn.model())
    confusion
  })
  #LDA random splitting
  train_indices_lda = reactive({
    req(input$lda_size,data_lda())
    z=sample(nrow(data_lda()), round(0.01*(as.numeric(input$lda_size)) * nrow(data_lda())))
    return(z)
  })
  train_data_lda = reactive({
    req(data_lda(),train_indices_lda(),input$lda_size)
    return(data_lda()[train_indices_lda(), ])
    
  })
  test_data_lda =  reactive({
    req(data_lda(),train_indices_lda())
    return(data_lda()[-(train_indices_lda()), ])
  })
  lda_predict= reactive({
    req(input$file_lda,input$ldad_var_id,length(input$ldai_var_id)>=2)
    # Fit a LDA model
    lda_model = lda(formula = as.formula(paste(input$ldad_var_id, paste(input$ldai_var_id, collapse = " + "), sep = " ~ ")), data =train_data_lda() )
    # predict the class labels for the test data
    lda_predict = predict(lda_model, newdata = as.data.frame(test_data_lda()[,input$ldai_var_id]))
    return(lda_predict)
  })
  output$lda_accuracy=renderText({
    req(test_data_lda(),input$ldad_var_id)
    # Get the accuracy of model
    accuracy = paste("Accuracy of LDA model is ",round(mean(lda_predict()$class == test_data_lda()[,input$ldad_var_id])*100,2),"%")
    accuracy
    
  })
  # Get the confusion matrix
  output$lda_cm=renderPrint({
    req(test_data_lda(),input$ldad_var_id)
    confusion = table( actual=test_data_lda()[,input$ldad_var_id],predicted=lda_predict()$class)
    confusion
  })
  #LDA specific splitting
  lda.predict= reactive({
    req(input$file_lda_train,input$file_lda_test,input$lda_response_id,length(input$lda_pred_id)>=2)
    train.data=data_lda_train()
    test.data=data_lda_test()
    # Fit a LDA model
    lda.model = lda(formula = as.formula(paste(input$lda_response_id, paste(input$lda_pred_id, collapse = " + "), sep = " ~ ")), data =train.data )
    # predict the class labels for the test data
    lda.predict = predict(lda.model, newdata = as.data.frame(test.data[,input$lda_pred_id]))
    return(lda.predict)
  })
  output$ldaAccuracy=renderText({
    req(data_lda_test(),input$lda_response_id)
    # Get the accuracy of model
    accuracy = paste("Accuracy of LDA model is ",round(mean(lda.predict()$class == data_lda_test()[,input$lda_response_id])*100,2),"%")
    accuracy
    
  })
  # Get the confusion matrix
  output$ldaCM=renderPrint({
    req(data_lda_test(),input$lda_response_id)
    confusion = table( actual=data_lda_test()[,input$lda_response_id],predicted=lda.predict()$class)
    confusion
  })
  #QDA
  #QDA random splitting
  train_indices_qda = reactive({
    req(input$qda_size,data_qda())
    z=sample(nrow(data_qda()), round(0.01*(as.numeric(input$qda_size)) * nrow(data_qda())))
    return(z)
  })
  train_data_qda = reactive({
    req(data_qda(),train_indices_qda(),input$qda_size)
    return(data_qda()[train_indices_qda(), ])
    
  })
  test_data_qda =  reactive({
    req(data_qda(),train_indices_qda())
    return(data_qda()[-(train_indices_qda()), ])
  })
  qda_predict= reactive({
    req(input$file_qda,input$qdad_var_id,length(input$qdai_var_id)>=2)
    # Fit a QDA model
    qda_model = qda(formula = as.formula(paste(input$qdad_var_id, paste(input$qdai_var_id, collapse = " + "), sep = " ~ ")), data =train_data_qda() )
    # predict the class labels for the test data
    qda_predict = predict(qda_model, newdata = as.data.frame(test_data_qda()[,input$qdai_var_id]))
    return(qda_predict)
  })
  output$qda_accuracy=renderText({
    req(data_qda(),test_data_qda(),qda_predict(),input$qdad_var_id,input$qdai_var_id)
    # Get the accuracy of model
    accuracy = paste("Accuracy of QDA model is ",round(mean(qda_predict()$class == test_data_qda()[,input$qdad_var_id])*100,2),"%")
    accuracy
    
  })
  # Get the confusion matrix
  output$qda_cm=renderPrint({
    req(test_data_qda(),input$qdad_var_id,qda_predict())
    confusion = table( actual=test_data_qda()[,input$qdad_var_id],predicted=qda_predict()$class)
    confusion
  })
  # QDA specific splitting
  qda.predict= reactive({
    req(input$file_qda_train,input$file_qda_test,input$qda_response_id,length(input$qda_pred_id)>=2)
    train.data=data_qda_train()
    test.data=data_qda_test()
    # Fit a QDA model
    qda.model = qda(formula = as.formula(paste(input$qda_response_id, paste(input$qda_pred_id, collapse = " + "), sep = " ~ ")), data =train.data )
    # predict the class labels for the test data
    qda.predict = predict(qda.model, newdata = as.data.frame(test.data[,input$qda_pred_id]))
    return(qda.predict)
  })
  output$qdaAccuracy=renderText({
    req(data_qda_test(),input$qda_response_id)
    # Get the accuracy of model
    accuracy = paste("Accuracy of QDA model is ",round(mean(qda.predict()$class == data_qda_test()[,input$qda_response_id])*100,2),"%")
    accuracy
    
  })
  # Get the confusion matrix
  output$qdaCM=renderPrint({
    req(data_qda_test(),input$qda_response_id)
    confusion = table( actual=data_qda_test()[,input$qda_response_id],predicted=qda.predict()$class)
    confusion
  })
  
  #NB
  #NB random splitting
  train_indices_nb = reactive({
    req(input$nb_size,data_nb())
    z=sample(nrow(data_nb()), round(0.01*(as.numeric(input$nb_size)) * nrow(data_nb())))
    return(z)
  })
  train_data_nb = reactive({
    req(data_nb(),train_indices_nb(),input$nb_size)
    return(data_nb()[train_indices_nb(), ])
    
  })
  test_data_nb =  reactive({
    req(data_nb(),train_indices_nb())
    return(data_nb()[-(train_indices_nb()), ])
  })
  nb_predict= reactive({
    req(input$file_nb,input$nbd_var_id,length(input$nbi_var_id)>=2)
    # Fit a NB model
    nb_model = naiveBayes(formula = as.formula(paste(input$nbd_var_id, paste(input$nbi_var_id, collapse = " + "), sep = " ~ ")), data =train_data_nb() )
    # predict the class labels for the test data
    nb_predict = predict(nb_model, newdata = as.data.frame(test_data_nb()[,input$nbi_var_id]))
    return(nb_predict)
  })
  output$nb_accuracy=renderText({
    req(test_data_nb(),input$nbd_var_id)
    # Get the accuracy of model
    accuracy = paste("Accuracy of Naive Bayes model is ",round(mean(nb_predict() == test_data_nb()[,input$nbd_var_id])*100,2),"%")
    accuracy
    
  })
  # Get the confusion matrix
  output$nb_cm=renderPrint({
    req(test_data_nb(),input$nbd_var_id)
    confusion = table( actual=test_data_nb()[,input$nbd_var_id],predicted=nb_predict())
    confusion
  })
  
  
  
  #NB specific splitting
  nb.predict= reactive({
    req(input$file_nb_train,input$file_nb_test,input$nb_response_id,length(input$nb_pred_id)>=2)
    train.data=data_nb_train()
    test.data=data_nb_test()
    # Fit a NB model
    nb.model = naiveBayes(formula = as.formula(paste(input$nb_response_id, paste(input$nb_pred_id, collapse = " + "), sep = " ~ ")), data =train.data )
    # predict the class labels for the test data
    nb.predict = predict(nb.model, newdata = as.data.frame(test.data[,input$nb_pred_id]))
    return(nb.predict)
  })
  output$nbAccuracy=renderText({
    req(data_nb_test(),input$nb_response_id)
    # Get the accuracy of model
    accuracy = paste("Accuracy of Naive Bayes model is ",round(mean(nb.predict() == data_nb_test()[,input$nb_response_id])*100,2),"%")
    accuracy
    
  })
  # Get the confusion matrix
  output$nbCM=renderPrint({
    req(data_nb_test(),input$nb_response_id)
    confusion = table( actual=data_nb_test()[,input$nb_response_id],predicted=nb.predict())
    confusion
  })
}

shinyApp(ui,server)
