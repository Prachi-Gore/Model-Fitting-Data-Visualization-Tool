library(tools)#to check file extension
library(readxl)
library(shiny)
library(DT)
library("dplyr")
library(ggplot2)
library(shinydashboard)
library(datasets)
library(caTools)
library(party)
library(magrittr)
library(class)#KNN
library(MASS)#Discriminant Analysis
library(e1071)#Naive Bayes
library(GGally)# ggpairs
library(caret)#confusionMatrix

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
    menuItem("Graphs", tabName = "graph", icon = icon("chart-column"),
             menuSubItem("Bar Chart", tabName = "Bar-Chart"),
             menuSubItem("Box Plot", tabName = "Box-Plot"),
             menuSubItem("Histogram", tabName = "Histogram"),
             menuSubItem("Scatter Plot", tabName = "Scatter-Plot"),
             menuSubItem("Line Chart", tabName = "Line-Chart")
    ),
    menuItem("Summary Statistics",tabName = "SS",icon = icon("bar-chart")),
    menuItem("Model", tabName="model",icon = icon("list-alt"),
             menuItem("Regression", tabName = "regression",icon = icon("chart-line"),
                      menuSubItem("Simple", tabName = "Simple"),
                      menuSubItem("Multiple", tabName = "Multiple")
                     ),
             menuItem("Classification", tabName = "classification",icon=icon("sitemap"),
                      menuSubItem("Logistic", tabName = "Logistic"),
                      menuSubItem("KNN", tabName = "KNN"),
                      menuSubItem("LDA", tabName = "LDA"),
                      menuSubItem("QDA", tabName = "QDA"),
                      menuSubItem("Naive Bayes", tabName = "NB")
                      
             )
    )
  )
  
)

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

summary_layout=sidebarPanel(file_input("file_summary"),select_input("summary_var", "Select a variable"),actionButton("calculate", "Calculate"))

simple_layout=sidebarPanel(file_input("file_simple"),select_input("sd_var_id", "Select Response Variable"),
                           select_input("si_var_id", "Select Predictors"),numericInput("simple_size_s", label = "Enter a size of train dataset in %:", value = 80,min=10,max=100))

multi_layout=sidebarPanel(file_input("file_multi"),
select_input("md_var_id", "Select dependent variable"),
selectInput("mi_var_id", "Select independent variables", choices = NULL,multiple = TRUE),
numericInput("multiple_size", label = "Enter a size of train dataset in %:", value = 80,min=10,max=100))


log_layout=sidebarPanel(file_input("file_log"),select_input("ld_var_id", "Select Response variable"),
                                      selectInput("li_var_id", "Select Predictors", choices = NULL,multiple = TRUE),
                        numericInput("logistic_size", label = "Enter a size of train dataset in %:", value = 80,min=10,max=100))

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
scatter_ui=fluidPage(title="scatter",sidebarLayout(scatter_layout,mainPanel (plotOutput("scatter")) ))       
barchart_ui=fluidPage(title="bar graph",sidebarLayout(barchart_layout,mainPanel (plotOutput("barchart"))))
boxplot_ui=fluidPage(title="bar line plot",sidebarLayout(boxplot_layout,mainPanel (plotOutput("boxplot"))))
line_ui=fluidPage(title="line-chart",sidebarLayout(line_layout,mainPanel (plotOutput("lineplot"))))
summary_ui=fluidPage(title="",sidebarLayout(summary_layout,mainPanel(verbatimTextOutput("result"))))
simple_ui=fluidPage(title="Simple Regression",sidebarLayout(simple_layout,mainPanel(h3(textOutput("text_simple")),h4(textOutput("mse_simple")),h5(textOutput("adjr2_simple")),plotOutput("plot_simple"),verbatimTextOutput("summary_simple"))))
multi_ui=fluidPage(title="Multiple Regression",sidebarLayout(multi_layout,mainPanel(h3(textOutput("text_multiple")),h4(textOutput("mse_multiple")),h5(textOutput("adjr2_multiple")),plotOutput("matrix_plot"),
                                                                                    verbatimTextOutput("summary_multi"))))

log_ui=fluidPage(title="Logistic Classification",sidebarLayout(log_layout,mainPanel(h3(textOutput("text_logistic")),h4(textOutput("logisticAccuracy")),
                                                                                    verbatimTextOutput("logisticCM"),verbatimTextOutput("summary_logistic"))))

knn_ui=fluidPage(
  title="K Nearest Neighbours",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    knn_layout,
    mainPanel (h3(textOutput("knnAccuracy")),verbatimTextOutput("knnCM"))
  )
)
lda_ui=fluidPage(
  title="Linear Discriminant Analysis",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    lda_layout,
    mainPanel (h3(textOutput("ldaAccuracy")),verbatimTextOutput("ldaCM"))
  )
)
qda_ui=fluidPage(
  title="Quadratic Discriminant Analysis",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    qda_layout,
    mainPanel (h3(textOutput("qdaAccuracy")),verbatimTextOutput("qdaCM"))
  )
)
nb_ui=fluidPage(
  title="Naive Bayes",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    nb_layout,
    mainPanel (h3(textOutput("nbAccuracy")),verbatimTextOutput("nbCM"))
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
    tabItem("SS",summary_ui),
    tabItem("Simple",simple_ui),
    tabItem("Multiple",multi_ui),
    tabItem("Logistic",log_ui),
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
  
  #simple
  data_simple <- reactive({
    req(input$file_simple)
    file_ext= file_ext(input$file_simple$datapath)
    
    if(file_ext=="xlsx"|file_ext=="xls"){
      # write.csv(read_excel(input$file_simple$datapath),"converted_data.csv",row.names = FALSE)
      # df=read.csv("converted_data.csv")
      df=read_excel(input$file_simple$datapath)
      
    }
    else{
      df <- read.csv(input$file_simple$datapath )
      
    }
    return(select_if(df, is.numeric))
  })
  # Data_multiple
  data_multiple <- reactive({
    req(input$file_multi)
    file_ext= file_ext(input$file_multi$datapath)
    
    if(file_ext=="xlsx"|file_ext=="xls"){
      df=read_excel(input$file_multi$datapath)
      
    }
    else{
      df <- read.csv(input$file_multi$datapath )
      
    }
    return(select_if(df, is.numeric))
  })
  # logistic data
  data_log <- reactive({
    req(input$file_log)
    file_ext= file_ext(input$file_log$datapath)
    
    if(file_ext=="xlsx"|file_ext=="xls"){
      df=read_excel(input$file_log$datapath)
      
    }
    else{
      df <- read.csv(input$file_log$datapath )
      
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
      df <- read.csv(input$file_knn_train$datapath )
      
    }
    return(as.data.frame(df))
  })
  data_knn_test= reactive({
    
    req(input$file_knn_test)
    file_ext= file_ext(input$file_knn_test$datapath)
    
    
    if(file_ext=="xlsx"||file_ext=="xls"){
      df=read_excel(input$file_knn_test$datapath)
      
    }
    else{
      df <- read.csv(input$file_knn_test$datapath )
      
    }
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
      df <- read.csv(input$file_lda_train$datapath )
      
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
      df <- read.csv(input$file_lda_test$datapath )
      
    }
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
      df <- read.csv(input$file_qda_train$datapath )
      
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
      df <- read.csv(input$file_qda_test$datapath )
      
    }
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
      df <- read.csv(input$file_nb_train$datapath )
      
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
      df <- read.csv(input$file_nb_test$datapath )
      
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
  
  observe({update_input("sd_var_id","Select Dependent Variable",data_simple)})
  observe({update_input("si_var_id","Select Independent Variable",data_simple)})
  
  observe({
    update_input( "md_var_id","Select Dependent Variable",data_multiple )
    update_input("mi_var_id","Select Independent Variable",data_multiple )
  })
  
  observe({updateSelectInput(
    session = getDefaultReactiveDomain(),
    inputId = "ld_var_id",
    label = "Select Dependent Variable",
    
    choices =names(data_log())[sapply(data_log(), function(x) (setequal(c(0,1),unique(x))))],
    selected = NULL
  )
  # observe({update_input_categorical("ld_var_id","Select Dependent Variable",data_log)
  update_input("li_var_id","Select Independent Variable",data_log)})
  
 
  
  observe({update_input_categorical("knn_response_id","Select Response Variable",data_knn_test)})
  observe({update_input_numerical("knn_pred_id","Select Predictors",data_knn_test)})
  
  observe({update_input_categorical("lda_response_id","Select Response Variable",data_lda_test)})
  observe({update_input_numerical("lda_pred_id","Select Predictors",data_lda_test)})
  
  observe({update_input_categorical("qda_response_id","Select Response Variable",data_qda_test)})
  observe({update_input_numerical("qda_pred_id","Select Predictors",data_qda_test)})
  
  observe({update_input_categorical("nb_response_id","Select Response Variable",data_nb_test)})
  observe({update_input_numerical("nb_pred_id","Select Predictors",data_nb_test)})
  
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
    # print(is.numeric(x))
    # print(x)
    
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    ggplot(data.frame(x), aes(x)) +
      geom_histogram(breaks = bins, color = "black", fill = "blue") +
      labs(x = input$hist_var_id, y = "Frequency") +
      ggtitle(paste("Histogram of ",input$hist_var_id))+
      
      
      theme
    # hist(x,xlab=input$hist_var_id, breaks=bins,col="#75AADB",border = "white",main = paste("Histogram of" ,input$hist_var_id))
  }
  )
  output$scatter <- renderPlot({
    req(input$scatter_var1_id,input$scatter_var2_id)
    
    x = as.numeric(unlist(data_scatter()[,input$scatter_var1_id]))
    y=as.numeric(unlist(data_scatter()[,input$scatter_var2_id]))
    # plot(x,y,xlab=input$scatter_var1_id,ylab=input$scatter_var2_id,main = paste("Scatter plot of" ,input$scatter_var1_id,"vs",input$scatter_var2_id))
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
  observeEvent(input$calculate, {
    output$result <- renderPrint({
      x <- data_summary()[, input$summary_var]
      summary(x)
      # summary(data_summary()) # to get summary of all columns of dataset
      
    }) })
  
  # Set up the plot output
  output$plot_simple <- renderPlot({
    # this plot is for whole dataset
    req(input$sd_var_id, input$si_var_id, data_simple())
    
    # x = as.numeric(unlist(data_simple()[,input$si_var_id]))
    # y = as.numeric(unlist(data_simple()[,input$sd_var_id]))
    x=input$si_var_id
    y=input$sd_var_id
    # Create a scatter plot of the data
    # plot(x, y,xlab = input$independent_var_id, ylab = input$dependent_var_id)
      ggplot(data_simple(), aes(x = as.numeric(unlist(data_simple()[,x])), y = as.numeric(unlist(data_simple()[,y]))) )+
        geom_point(color = "blue")+
        # Add the regression line to the plot
        geom_smooth(method = "lm") +
        labs(x = x, y =y ) +
        ggtitle(paste("Scatter Plot of ",x," VS ",y))+
        theme
    }
    )
  
  
  
  # Set up the output for simple
  train_indices_simple <- reactive({
  req(input$simple_size_s, data_simple())
  return(sample(nrow(data_simple()), round(0.01*(as.numeric(input$simple_size_s)) * nrow(data_simple()))))
  })
  train_data_simple <- reactive({
    req(data_simple(),train_indices_simple(),input$simple_size_s)
   return(data_simple()[train_indices_simple(), ])
  
  })
  test_data_simple <-  reactive({
    req(data_simple(),train_indices_simple())
    return(data_simple()[-(train_indices_simple()), ])
  })
  
  simple_model <-reactive({
    req(input$sd_var_id, input$si_var_id, data_simple(),train_data_simple())
   # x = as.numeric(unlist(data_simple()[,input$si_var_id]))
   # y = as.numeric(unlist(data_simple()[,input$sd_var_id]))
   x= input$si_var_id
   y=input$sd_var_id
  # Fit a linear regression model
   return(lm(formula = as.formula(paste(y,x, sep = " ~ ")),data=train_data_simple()))
  })
  
  # Print the summary of the model and the regression equation
  output$summary_simple <- renderPrint({
    req(simple_model())
    summary(simple_model())
  })
  #predict 
  y_pred_simple=reactive({
    req(simple_model())
     predict(simple_model(), newdata = test_data_simple())
    
  })
   
  
  #performance of test data
  #mse
  mse_simple <- reactive({
    req(simple_model(),test_data_simple(),input$sd_var_id)
    y=input$sd_var_id
  mean(((as.numeric(unlist(test_data_simple()[,y]))) - y_pred_simple())^2)
  
  })
  output$mse_simple=renderText({
    return(paste("mean squared error  ",round(mse_simple(),4)))
    
  })
  #adjusted r2
  output$adjr2_simple=renderText({
    req(simple_model(),test_data_simple(),input$sd_var_id)
    y=input$sd_var_id
    tss <- sum((as.numeric(unlist(test_data_simple()[,y])) - mean(as.numeric(unlist(test_data_simple()[,y]))))^2)
    r_squared <- 1 - (mse_simple() / tss)
    n <- nrow(test_data_simple())
    p <- length(simple_model()$coefficients) - 1
    adj_r_squared <- 1 - ((mse_simple() / (n - p - 1)) / (tss / (n - 1)))
    adj_r_squared =paste("adjusted rsquared  ",round(adj_r_squared,6))
    adj_r_squared
    
  })
  output$text_simple=renderText({
    req(simple_model())
  return("model's performance on the test data")
  }
  )
  
  ## multiple linear regression
  
  
  output$matrix_plot <- renderPlot({
    req(input$mi_var_id, input$md_var_id)
    ggpairs(data_multiple()[c(input$md_var_id, input$mi_var_id)])
    
  })
  train_indices_multiple <- reactive({
    req(input$multiple_size, data_multiple())
    return(sample(nrow(data_multiple()), round(0.01*(as.numeric(input$multiple_size)) * nrow(data_multiple()))))
  })
  train_data_multiple <- reactive({
    req(data_multiple(),train_indices_multiple(),input$multiple_size)
    return(data_multiple()[train_indices_multiple(), ])
    
  })
  test_data_multiple <-  reactive({
    req(data_multiple(),train_indices_multiple())
    return(data_multiple()[-(train_indices_multiple()), ])
  })
  multiple_model <-reactive({
    req(input$md_var_id, input$mi_var_id)
    lm(formula = as.formula(paste(input$md_var_id, paste(input$mi_var_id, collapse = " + "), sep = " ~ ")), data = train_data_multiple())
  })
  
  output$summary_multi <- renderPrint({
    req(multiple_model())
    summary(multiple_model())
   
  })
  #predict 
  predict_multiple=reactive({
    req(multiple_model())
    predict(multiple_model(), newdata = test_data_multiple())
  })
   
  mse_multiple=reactive({
    req(multiple_model(),test_data_multiple(),input$md_var_id)
    y=input$md_var_id
    
   mean((as.numeric(unlist(test_data_multiple()[,y])) - predict_multiple())^2)
  })
  #performance of test dataset
     #mse
  output$mse_multiple <- renderText({
    
    return(paste("mean squared error ",round(mse_multiple(),4)))
   
  })
  #adjusted r2
  output$adjr2_multiple=renderText({
    req(input$md_var_id,multiple_model(),mse_multiple())
    y=input$md_var_id
    tss <- sum((test_data_multiple()[,y] - mean(test_data_multiple()[,y]))^2)
    r_squared <- 1 - (mse_multiple() / tss)
    n <- nrow(test_data_multiple())
    p <- length(multiple_model()$coefficients) - 1
    adj_r_squared <- 1 - ((mse_multiple() / (n - p - 1)) / (tss / (n - 1)))
    adj_r_squared =paste("adjusted rsquared  ",round(adj_r_squared,6))
    adj_r_squared
    
  })
  
  output$text_multiple=renderText({
    req(multiple_model())
    return("model's performance on the test data")
  }
  )
  ## logistic classification
 
  
  # output$plot_logistic <- renderPlot({
  #   req(input$li_var_id, input$ld_var_id)
  #   #pairs(data_log()[c(input$ld_var_id, input$li_var_id)])
  # })
  train_indices_logistic <- reactive({
    req(input$logistic_size, data_log())
    return(sample(nrow(data_log()), round(0.01*(as.numeric(input$logistic_size)) * nrow(data_log()))))
  })
  train_data_logistic <- reactive({
    req(data_log(),train_indices_logistic(),input$logistic_size)
    return(data_log()[train_indices_logistic(), ])
    
  })
  test_data_logistic <-  reactive({
    req(data_log(),train_indices_logistic())
    return(data_log()[-(train_indices_logistic()), ])
  })
  logistic.model=reactive({
    req(input$li_var_id, input$ld_var_id,input$logistic_size)
    glm(formula = as.formula(paste(input$ld_var_id, paste(input$li_var_id, collapse = " + "), sep = " ~ ")), data = train_data_logistic(),family = "binomial") 
    
  })
  output$summary_logistic <- renderPrint({
    req(logistic.model())
    summary(logistic.model())
  })
  #predict 
  predict_logistic=reactive({
    req(logistic.model())
    predicted_prob=predict(logistic.model(), newdata = test_data_logistic(),type="response")
    predicted_classes=ifelse(predicted_prob > 0.5, 1, 0)
    predicted_classes
  })
  #Accuracy
  output$logisticAccuracy=renderText({
    req(test_data_logistic())
    # Get the accuracy of model
    accuracy <- paste("Accuracy of logistic model is",round(mean(predict_logistic() == test_data_logistic()[,input$ld_var_id])*100,2),"%")
    accuracy
  })
  output$logisticCM=renderPrint({
    req(test_data_logistic(),input$ld_var_id)
    # Get the confusion matrix
    confusion <- table( actual=as.numeric(unlist(test_data_logistic()[,input$ld_var_id])),predicted=predict_logistic())
    confusion
  })
  output$text_logistic=renderText({
    req(logistic.model())
    return("model's performance on the test data")
  }
  )
  #KNN
  knn.model=reactive({
    req(input$file_knn_train,input$file_knn_test,input$k,input$knn_response_id,input$knn_pred_id)
    train.data=(data_knn_train())
    test.data=(data_knn_test())
    # print(data_knn_train())
    # View(data_knn_train())
    
    # Fit a KNN model with k
    knn.model <- knn(train =as.data.frame(scale(train.data[,input$knn_pred_id])), test = as.data.frame(scale(test.data[,input$knn_pred_id])), cl = train.data[,input$knn_response_id], k = input$k)
    return(knn.model)
  })
  output$knnAccuracy=renderText({
    req(data_knn_test(),input$knn_response_id)
    # Get the accuracy of model
    accuracy <- paste("Accuracy of KNN model with k=",input$k," is",round(mean(knn.model() == data_knn_test()[,input$knn_response_id])*100,2),"%")
    accuracy
  })
  output$knnCM=renderPrint({
    req(data_knn_test(),input$knn_response_id,knn.model())
    # Get the confusion matrix
    confusion <- table( actual=data_knn_test()[,input$knn_response_id],predicted=knn.model())
    confusion
  })
  
  #LDA
  lda.predict= reactive({
    req(input$file_lda_train,input$file_lda_test,input$lda_response_id,length(input$lda_pred_id)>=2)
    train.data=data_lda_train()
    test.data=data_lda_test()
    # Fit a LDA model
    lda.model <- lda(formula = as.formula(paste(input$lda_response_id, paste(input$lda_pred_id, collapse = " + "), sep = " ~ ")), data =train.data )
    # predict the class labels for the test data
    lda.predict <- predict(lda.model, newdata = as.data.frame(test.data[,input$lda_pred_id]))
    return(lda.predict)
  })
  output$ldaAccuracy=renderText({
    req(data_lda_test(),input$lda_response_id)
    # Get the accuracy of model
    accuracy <- paste("Accuracy of LDA model is ",round(mean(lda.predict()$class == data_lda_test()[,input$lda_response_id])*100,2),"%")
    accuracy
    
  })
  # Get the confusion matrix
  output$ldaCM=renderPrint({
    req(data_lda_test(),input$lda_response_id)
    confusion <- table( actual=data_lda_test()[,input$lda_response_id],predicted=lda.predict()$class)
    confusion
  })
  #QDA
  qda.predict= reactive({
    req(input$file_qda_train,input$file_qda_test,input$qda_response_id,length(input$qda_pred_id)>=2)
    train.data=data_qda_train()
    test.data=data_qda_test()
    # Fit a QDA model
    qda.model <- qda(formula = as.formula(paste(input$qda_response_id, paste(input$qda_pred_id, collapse = " + "), sep = " ~ ")), data =train.data )
    # predict the class labels for the test data
    qda.predict <- predict(qda.model, newdata = as.data.frame(test.data[,input$qda_pred_id]))
    return(qda.predict)
  })
  output$qdaAccuracy=renderText({
    req(data_qda_test(),input$qda_response_id)
    # Get the accuracy of model
    accuracy <- paste("Accuracy of QDA model is ",round(mean(qda.predict()$class == data_qda_test()[,input$qda_response_id])*100,2),"%")
    accuracy
    
  })
  # Get the confusion matrix
  output$qdaCM=renderPrint({
    req(data_qda_test(),input$qda_response_id)
    confusion <- table( actual=data_qda_test()[,input$qda_response_id],predicted=qda.predict()$class)
    confusion
  })
  
  #NB
  nb.predict= reactive({
    req(input$file_nb_train,input$file_nb_test,input$nb_response_id,length(input$nb_pred_id)>=2)
    train.data=data_nb_train()
    test.data=data_nb_test()
    # Fit a NB model
    nb.model <- naiveBayes(formula = as.formula(paste(input$nb_response_id, paste(input$nb_pred_id, collapse = " + "), sep = " ~ ")), data =train.data )
    # predict the class labels for the test data
    nb.predict <- predict(nb.model, newdata = as.data.frame(test.data[,input$nb_pred_id]))
    return(nb.predict)
  })
  output$nbAccuracy=renderText({
    req(data_nb_test(),input$nb_response_id)
    # Get the accuracy of model
    accuracy <- paste("Accuracy of Naive Bayes model is ",round(mean(nb.predict() == data_nb_test()[,input$nb_response_id])*100,2),"%")
    accuracy
    
  })
  # Get the confusion matrix
  output$nbCM=renderPrint({
    req(data_nb_test(),input$nb_response_id)
    confusion <- table( actual=data_nb_test()[,input$nb_response_id],predicted=nb.predict())
    confusion
  })
  
}

shinyApp(ui,server)
