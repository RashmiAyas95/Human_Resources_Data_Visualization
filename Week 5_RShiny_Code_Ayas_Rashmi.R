#Loading the necessary packages and libraries
install.packages("shiny")
library(shiny)
library(ggplot2)

#Loading the HR Data Set
getwd()
setwd("C:\\Users\\Dell\\Desktop\\Visualization and Communication\\dashboard")
hrdata <- read.csv('HRDataset.csv')
d<-hrdata

#Histogram that controls the number of bins
ui <- fluidPage(
    
    # App title
    titlePanel("Human Resources Dashboard Visualization"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar panel for inputs 
        sidebarPanel(
            
            sliderInput(inputId = "bins",
                        label = "Number of Observations:",
                        min = 5,
                        max = 50,
                        value = 30),
            selectInput(inputId = "bincolor",
                        label = "Please Select a Color:",
                        choices = colors( ),
                        selected = "red"),
            
            checkboxInput(inputId = "addmean",
                          label = "Do you want to add a mean line?",
                          value = FALSE),
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Histogram", plotOutput("distPlot")),  # The original Histogram
                        
                        tabPanel("Bar Plot", plotOutput("barChart")),
                        
                        tabPanel("Box Plot", plotOutput("facetChart")),
                        
                        tabPanel("Scatter Plot", plotOutput("scatter")),
                        
                        tabPanel("Summary", verbatimTextOutput("summary")), #Histogram Summary 
                        
                        tabPanel("Histogram Description",
                                 a("The Histogram gives an idea what the pay scale distribution in the company is and average pay given to its employees."),
                                 a("We can say that the President and CEO of the company have the highest salry of $80/hour and production technicians are paid the least of $14/hour."),
                                 a("The average pay scale provided by the company is $31.28")),
                        
                        tabPanel("Bar Chart Description",
                                 a("The Bar chart depicts gender diversification in the Dental Magic Company. From this we can understand that number of male and female employees working in different departs are not equally balanced."),
                                 a("Most women are working in the production department and men are allocated in the IT department."),
                                 a("The Executive Office department has only one female employee. Thus, the company needs to hire more employees equally to strike a balance.")
                      
     )
    )
  )
 )
)

#Executing server side logic
server <- shinyServer(function(input, output) {
    #Histogram
    output$distPlot <- renderPlot({
        x    <- d$PayRate
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        #For Summary Statistics
        output$summary <- renderPrint({
            x <- hrdata$PayRate             # Define x again
            summary(x)
        })
        
        hist(x, breaks = bins, col = input$bincolor, border = "black",
             xlab = "PayRate",
             main = "The PayRate Distribution of Employees in Dental Magic")
        
        if(input$addmean) {  # If the addmean box is checked...
            
            # Add a vertical line at the mean of x
            abline(v = mean(x),
                   lwd = 2,      # Thickness
                   lty = 2)      # Dashed line
            
        } # close if statement
        
        #Bar plot to depict gender diversification in Dental Magic
        output$barChart <- renderPlot({
          ggplot(hrdata,aes(x=factor(Department)))+geom_bar(aes(fill=factor(Sex)),position = "dodge",alpha=0.5)+
            xlab("Departments")+ylab("Number of employees")+ggtitle("Department-wise Gender Diversification")
          
        })
        
        #Box Plot
        output$facetChart <- renderPlot({
          ggplot(hrdata, aes(x=factor(CitizenDesc), y=PayRate, fill = factor(CitizenDesc))) + geom_boxplot() + facet_grid(~Sex) + xlab("Citizenship")+ylab("PayRate")+ggtitle("Average Pay Scale of US Citizens")
          
        })
        
        #Scatter Plot
        output$scatter <- renderPlot({
          ggplot(hrdata, aes(x=factor(RaceDesc), y=PayRate)) + geom_point(size = 3) + xlab("Human Race")+ylab("Pay Rate")+ggtitle("Do all Human Races have equal opportunities in the company?")
          
        })
        
    })
        
    })

#Code for running the RShiny Application
shinyApp(ui, server)
