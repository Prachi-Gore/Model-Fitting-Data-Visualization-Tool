library(tools)#to check file extension
library(readxl)
library(shiny)
library(DT)
library("dplyr")
library(ggplot2)
library(shinydashboard)

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
    menuItem("Statistics",tabName = "statistics",icon = icon("bar-chart"),
             menuSubItem("Summary", tabName = "Summary"),
             menuSubItem("Mean", tabName = "Mean"),
             menuSubItem("Variance", tabName = "Variance"),
             menuSubItem("Median", tabName = "Median"),
             menuSubItem("Correlation", tabName = "Correlation"),
             menuSubItem("Quantile", tabName = "Quantile")
    ),
    menuItem("Model", tabName="model",icon = icon("list-alt"),
             menuItem("Regression", tabName = "regression",icon = icon("chart-line"),
                      menuSubItem("Simple", tabName = "Simple"),
                      menuSubItem("Multiple", tabName = "Multiple"),
                      menuSubItem("Polynomial", tabName = "Polynomial"),
                      menuSubItem("Logistic", tabName = "Logistic")
             ),
             menuItem("Classification", tabName = "classification",icon=icon("sitemap"),
                      menuSubItem("Decision Tree", tabName = "Decision-Tree"),
                      menuSubItem("Random Forest", tabName = "Random-Forest"),
                      menuSubItem("Naive Bayes", tabName = "Naive-Bayes")
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
simple_layout=sidebarPanel(file_input("file_simple"),select_input("dependent_var_id", "Select Dependent Variable"),
                           select_input("independent_var_id", "Select Independent Variable"))
multi_layout=sidebarPanel(file_input,select_input)
poly_layout=sidebarPanel(file_input,select_input)
log_layout=sidebarPanel(file_input,select_input)
mean_layout=sidebarPanel(file_input,select_input)
var_layout=sidebarPanel(file_input,select_input)
cor_layout=sidebarPanel(file_input,select_input)
med_layout=sidebarPanel(file_input,select_input)
qua_layout=sidebarPanel(file_input,select_input)

dec_layout=sidebarPanel(file_input,select_input)
ran_layout=sidebarPanel(file_input,select_input)
naive_layout=sidebarPanel(file_input,select_input)

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
summary_ui=fluidPage(title="",sidebarLayout(summary_layout,mainPanel(verbatimTextOutput("result"))))
barchart_ui=fluidPage(title="bar graph",sidebarLayout(barchart_layout,mainPanel (plotOutput("barchart"))))
simple_ui=fluidPage(title=" ",sidebarLayout(simple_layout,mainPanel(plotOutput("plot_simple"),verbatimTextOutput("summary_simple"))))
boxplot_ui=fluidPage(title="bar line plot",sidebarLayout(boxplot_layout,mainPanel (plotOutput("boxplot"))))
line_ui=fluidPage(title="line-chart",sidebarLayout(line_layout,mainPanel (plotOutput("lineplot"))))




# multi_ui=fluidPage(
#   title="line-chart",
#   #titlePanel(h3("Data Visualization")),
#   sidebarLayout(
#     multi_layout,
#     mainPanel ()
#   )
# )
# multi_ui=fluidPage(title="",sidebarLayout(sidebarPanel(fileInput("datafile", "Choose CSV file",
#                                                  selectInput("response", "Select response variable:", ""),
#                                                  selectInput("predictors", "Select predictor variables:", "", multiple = TRUE),
#                                                  actionButton("run", "Run regression"),
#                                                  hr(),
#                                                  h4("Regression results:")),
#                                                  mainPanel(
#                                                  verbatimTextOutput("results"),
#                                                  plotOutput("plot"))
#                               )))
                             
poly_ui=fluidPage(
  title="line-chart",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    poly_layout,
    mainPanel ()
  )
)
log_ui=fluidPage(
  title="line-chart",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    log_layout,
    mainPanel ()
  )
)

  
med_ui=fluidPage(
  title="line-chart",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    med_layout,
    mainPanel ()
  )
)
var_ui=fluidPage(
  title="line-chart",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    var_layout,
    mainPanel ()
  )
)
cor_ui=fluidPage(
  title="line-chart",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    cor_layout,
    mainPanel ()
  )
)
qua_ui=fluidPage(
  title="line-chart",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    qua_layout,
    mainPanel ()
  )
)
dec_ui=fluidPage(
  title="line-chart",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    dec_layout,
    mainPanel ()
  )
)
ran_ui=fluidPage(
  title="line-chart",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    ran_layout,
    mainPanel ()
  )
)
naive_ui=fluidPage(
  title="line-chart",
  #titlePanel(h3("Data Visualization")),
  sidebarLayout(
    naive_layout,
    mainPanel ()
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
    # tabItem("Multiple",multi_ui),
    # tabItem("Polynomial",poly_ui),
    # tabItem("Logistic",log_ui),
     tabItem("Summary",summary_ui)
    # tabItem("Median",med_ui),
    # tabItem("Variance",var_ui),
    # tabItem("Quantile",qua_ui),
    # tabItem("Correlation",cor_ui),
    # tabItem("Decision-Tree",dec_ui),
    # tabItem("Random-Forest",ran_ui),
    # tabItem("Naive-Bayes",naive_ui)
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
    df=read_excel(input$file_simple$datapath)
    
  }
  else{
    df <- read.csv(input$file_simple$datapath )
    
  }
  return(select_if(df, is.numeric))
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
observe(update_input("hist_var_id","choose Variable",data_hist))
observe(update_input("scatter_var1_id",label="select X variable",data_scatter))
observe(update_input("scatter_var2_id",label="select Y variable",data_scatter))
observe(update_input("bar_var1_id","choose Numerical Variable",data_bar_numeric))
observe(update_input("bar_var2_id","choose Categorical Variable",data_bar_categorical))
observe(update_input("boxplot_var1_id",label="select  variable",data_boxplot_categorical))
observe(update_input("boxplot_var2_id",label="select  variable",data_boxplot_numeric))
observe(update_input("line_var1_id",label="select X variable",data_line))
observe(update_input("line_var2_id",label="select Y variable",data_line))
observe({update_input("summary_var","Select Variable",data_summary)})
observe({update_input("dependent_var_id","Select Dependent Variable",data_simple)})
observe({update_input("independent_var_id","Select Independent Variable",data_simple)})

output$contents <-renderDataTable(data_view())
output$histogram <- renderPlot({
 
    x = as.numeric(unlist(data_hist()[,input$hist_var_id]))
    # print(is.numeric(x))
    # print(x)
  
  bins <- seq(min(x), max(x), length.out = input$bins + 1)
  hist(x,xlab=input$hist_var_id, breaks=bins,col="#75AADB",border = "white",main = paste("Histogram of" ,input$hist_var_id))
  }
)
output$scatter <- renderPlot({
  
  x = as.numeric(unlist(data_scatter()[,input$scatter_var1_id]))
  y=as.numeric(unlist(data_scatter()[,input$scatter_var2_id]))
  plot(x,y,xlab=input$scatter_var1_id,ylab=input$scatter_var2_id,main = paste("Scatter plot of" ,input$scatter_var1_id,"vs",input$scatter_var2_id))
}
)
output$barchart <- renderPlot({
  
  y=data_barchart()[,input$bar_var1_id]
   x=data_barchart()[,input$bar_var2_id]
  # print(x)
  # print(length(x))
  # print(class(x))
 # barplot(height = y,names.arg=rep("A",length(y)))
  ggplot(data=data_barchart(), aes(x=x, y=y)) +
    theme(panel.background = element_blank())+
    geom_bar(stat="identity",fill="#75AADB" )+
    xlab(input$bar_var2_id) +
    ylab(input$bar_var1_id) +
    ggtitle(paste("Barchart of" ,input$bar_var2_id,"vs",input$bar_var1_id))+
    theme(plot.title = element_text(hjust = 0.5))
    
}
)
output$boxplot <- renderPlot({
  
  x = data_boxplot()[,input$boxplot_var1_id]
  y=data_boxplot()[,input$boxplot_var2_id]
  boxplot(y~x,xlab=input$boxplot_var1_id,ylab=input$boxplot_var2_id,main = paste("Boxplot of" ,input$boxplot_var1_id," vs ",input$boxplot_var2_id))
}
)
output$lineplot <- renderPlot({
  
  x = data_line()[,input$line_var1_id]
  y=data_line()[,input$line_var2_id]
  plot(x,y,type = "l",xlab=input$line_var1_id,ylab=input$line_var2_id,main = paste("Line plot of" ,input$line_var1_id,"vs",input$line_var2_id))
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
  
  req(input$dependent_var_id, input$independent_var_id, data_simple())
  x = as.numeric(unlist(data_simple()[,input$independent_var_id]))
  y = as.numeric(unlist(data_simple()[,input$dependent_var_id]))
  # Create a scatter plot of the data
  plot(x, y,xlab = input$independent_var_id, ylab = input$dependent_var_id)
  
  # Add the regression line to the plot
  
  abline(lm(y~x,data=data_simple()))
})

# Set up the summary simple output
output$summary_simple <- renderPrint({
  req(input$dependent_var_id, input$independent_var_id, data_simple())
  x = as.numeric(unlist(data_simple()[,input$independent_var_id]))
  y = as.numeric(unlist(data_simple()[,input$dependent_var_id]))
  
  # Fit a linear regression model
  lm_model <- lm(y~x,data=data_simple())
  
  # Print the summary of the model and the regression equation
  summary(lm_model)
 
})
 
# # multiple linear regression
# data_multi <- reactive({
#   req(input$datafile)
#   read.csv(input$datafile$datapath)
# })
# observe({
#   req(data_multi())
#   updateSelectInput(session, "response", label = "Select response variable:",
#                     choices = names(data_multi()))
#   updateSelectInput(session, "predictors", label = "Select predictor variables:",
#                     choices = names(data_multi()), selected = names(data_multi())[1])
# })
# # Run regression and display results
# regression <- reactive({
#   req(input$run)
#   lm(as.formula(paste(input$response, "~", paste(input$predictors, collapse = "+"))), data = data_multi())
# })
# output$results <- renderPrint({
#   req(regression())
#   summary(regression())
# })
# # Generate scatterplot
# output$plot <- renderPlot({
#   req(data_multi(), input$response, input$predictors)
#   ggplot(data_multi(), aes_string(x = input$predictors[1], y = input$response)) +
#     geom_point() +
#     geom_smooth(method = "lm", formula = as.formula(paste(input$response, "~", paste(input$predictors, collapse = "+"))))
# })
}

shinyApp(ui,server)


