#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Variabile aleatoare discrete si continue"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            useShinyjs(),
            radioButtons("var",
                        "Alegeti o repartitie:",
                        choices =list("beta" = 1,
                                      "var2" = 2,
                                      "var3" = 3),
                        selected = 1
                        
                        ),
            helpText(h3("A si B")),
            numericInput("A", h4("A"), value = 0),
            numericInput("B", h4("B"), value= 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           box(id = "beta", width = '1000px',
               numericInput("alpha_",h3("alpha"),min=0.1, value = 0.1, step=0.1),
               numericInput("beta_", h3("beta"),min=0.1, value = 0.1, step=0.1)),
           plotOutput("functii")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # il folosim pentru a retine ultima variabila aleasa
    lastChoice <- "null"
    observeEvent(input$var, {
      if(input$var == 1){
        if(lastChoice != "null")
          shinyjs::hide(id=lastChoice)
        shinyjs::show(id="beta")
        lastChoice <<- "beta"
        output$functii <- renderPlot({par(mfrow = c(1,2))
          curve(dbeta(x, shape1 = input$alpha_, shape2 = input$beta_), 0, 1, ylim = c(0, 2.5),
                ylab = "f(x)", col = "red", lwd = 2, main = "Functia de densitate")
          curve(pbeta(x, shape1 = input$alpha_, shape2 = input$beta_),0,1,ylim = c(0,1), type = "l", main = "Functia de repartitie", ylab="F(x)", lwd = 2, col = "red")
          })
          
      }
      if(input$var==2){
        shinyjs::hide(id=lastChoice)
        
        cat(file=stderr(), paste0("lastChoice: ",lastChoice, "\n"))
        shinyjs::show(id="var2")
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
