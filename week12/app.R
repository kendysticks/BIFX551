library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("CSV Scatterplot and LM Analysis"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File:",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            # input title on scatterplot
            textInput(inputId = "title",
                      label = "Plot Title:",
                      value = "Scatterplot of various Variables"),
            # input which column from csv you would like to be the x axis
            numericInput(inputId = "column_1",
                      label = "Column A (X Axis) Selection:",
                      value = "1"),
            # input which column from csv you would like to be the y axis
            numericInput(inputId = "column_2",
                      label = "Column B (Y Axis) Selection",
                      value = "2"),
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot linear model and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("plot")),
                        tabPanel("Linear Model", verbatimTextOutput("lm")),
                        tabPanel("Table", tableOutput("contents"))
            ),
            
            #Saves graph
            actionButton("savePlot", "Save Plot")
          
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep)
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
    plotdata <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep)
        # create scatterplot using the user input for which columns they would like to compare
        
        g <- ggplot(df, aes(x=df[,input$column_1],y=df[,input$column_2])) + geom_point(colour="blue") 
        
        # user input for setting the title and both axis labels
        g <- g + labs(title = input$title,
                      x = names(df[input$column_1]),
                      y = names(df[input$column_2]))
        
        g
    })
    
    lmdata <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep)
        
        # linear modal prediction using the user input for column selection
        lm(formula = df[,input$column_1]~df[,input$column_2], data = df)
    })
    
    output$plot <- renderPlot({
        #using the reactive above, display the scatterplot with all user specifications
        g <- plotdata()
        
        g
    })
    
    output$lm <- renderPrint({
        #using the reactive above, display the linear model with user column specifications
        lm <- lmdata()
        lm
    })
    
    observeEvent(input$savePlot,{
        
        #save the plot based on user specifications
        g <- plotData()
        ggsave("myPlot.png",plot = g, device="png",path="~/")
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)