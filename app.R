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
# library(shinydashboard)

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
            numericInput("A", h1("A"), value = 0),
            numericInput("B", h1("B"), value= 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           box(id = "beta", width = '800px',
               numericInput("alpha_",h3("alpha"),min=0.1, value = 0.1, step=0.1),
               numericInput("beta_", h3("beta"),min=0.1, value = 0.1, step=0.1)),
           plotOutput("functii"),
           fluidRow(
             column(6,
                    div(style = "height:20px; font-size:25px; color: rgb(9, 233, 1); margin-bottom: 20px; margin-top: 140px; font-weight: bold;", textOutput("Px_a_val")),
                    div(style = "height:20px; font-size:25px; color: orange; margin-bottom: 20px; font-weight: bold;", textOutput("Px_b_val")),
                    div(style = "height:20px; font-size:25px; color: blue; font-weight: bold;", textOutput("Px_a_b_val"))
             ),
             column(6,
                    plotOutput("Px_a")
             )
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # il folosim pentru a retine ultima variabila aleasa
    lastChoice <- "null"
    #input$var e var din radioButtons si returneza nr variabilei alese
    observeEvent(input$var, {
      if(input$var == 1){
        
        updateNumericInput(session, "A", label = "A", 
                          value=0, min = 0, max = 1, step = 1
        )
        updateNumericInput(session, "B", label = "B", 
                           value=0, min = 0, max = 1, step = 1
        )
        if(lastChoice != "null")
          shinyjs::hide(id=lastChoice)
        shinyjs::show(id="beta")
        # <<- se foloseste pentru a accesa variabile globale(in server)
        lastChoice <<- "beta"
        # f si F
        # input$alpha_ si input$beta_ sunt cele din mainPanel din box-ul "beta"
        # ylab = label pentru Oy
        # ylim = val de pe Oy
        output$functii <- renderPlot({par(mfrow = c(1,2))
          curve(dbeta(x, shape1 = input$alpha_, shape2 = input$beta_), 0, 1, ylim = c(0, 2.5),
                ylab = "f(x)", col = "black", lwd = 2, main = "Functia de densitate")
          curve(pbeta(x, shape1 = input$alpha_, shape2 = input$beta_), 0 , 1, ylim = c(0,1), type = "l", main = "Functia de repartitie", ylab="F(x)", lwd = 2, col = "black")
          })
        output$Px_a_val <- renderText({
          paste("P(X <= A = P(X <=", input$A, ") =", round(pbeta(input$A, shape1= input$alpha_, shape2 = input$beta_), digits = 3))
        })
        output$Px_b_val <- renderText({
          paste("P( X> = B = P(X >=", input$B, ") =", 1- round(pbeta(input$B, shape1= input$alpha_, shape2=input$beta_), digits = 3))
        })
        output$Px_a_b_val <- renderText({
          paste("P( A <= X <= B = P(", input$A, "<= X <=", input$B, ") =", round((pbeta(input$B, shape1 = input$alpha_, shape2=input$beta_) - pbeta(input$A, shape1=input$alpha_, shape2=input$beta_)), digits = 3))
        })

        tryCatch(expr = {
         # mai facem un grafic pentru functia de repartitie
          output$Px_a <- renderPlot({curve(pbeta(x, shape1=input$alpha_, shape2=input$beta_),0,1,ylim=c(0,1), type="l", main="P(X <= A), P(X >= B) si P(A <= X <= B)", ylab="F(x)", lwd = 2, col = "black")
          
         
        # coloram portiunile de sub grafic
          x <- seq(0, input$A, by =0.01)
          x1 <- c(x, seq(input$A, 0, by =-0.01))
          y <- c(c(pbeta(x, shape1 = input$alpha_, shape2= input$beta_)), seq(0,0,length = length(x)))
          polygon(x1, y, col = "green")
  
          cat(file=stderr(), paste0("lastChoice: ",lastChoice, "\n"))
  
  
          x <- seq(input$B, 1, by =0.01)
          x1 <- c(x, seq(1, input$B, by =-0.01))
          y <- c(c(pbeta(x, shape1= input$alpha_, shape2 = input$beta_)), seq(0,0, length = length(x)))
          polygon(x1, y, col = "orange")
  
  
          x <- seq(input$A, input$B, by =0.01)
          x1 <- c(x, seq(input$B, input$A, by =-0.01))
          y <- c(c(pbeta(x, shape1 = input$alpha_, shape2 = input$beta_)), seq(0,0, length = length(x)))
          polygon(x1, y, col = "blue")
          })
        },
        
          error = function(e){
            message('Valoare incorecta pentru A sau B')
            print(e)
          },
          warning = function(w){
            message('Test')
            print(w)
          }
        )
        
        
        
      }
      
      
      
      if(input$var==2){
        plot.new();
        shinyjs::hide(id=lastChoice)
        
        cat(file=stderr(), paste0("lastChoice: ",lastChoice, "\n"))
        shinyjs::show(id="var2")
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
