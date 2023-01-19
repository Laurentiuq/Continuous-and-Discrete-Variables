library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyjs)

# Define the user interface
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    selectInput("dist", "Select a Distribution:", 
                c("Normal" = "norm", "Uniform" = "unif", "Exponential" = "exp", "Poisson" = "pois", "Binomial" = "binom", "Geometric" = "geom", "Gamma" = "gamma", "Beta" = "beta", "Chi-Squared" = "chisq", 
                  "F" = "f", "t" = "t", "Cauchy" = "cauchy", "Laplace" = "laplace", "Log-Normal" = "lnorm", "Weibull" = "weibull"))
    


    
   
  ),
  
  dashboardBody(
   useShinyjs(),
   hidden(
      div(id = "alphaBeta", 
          box(
            title = "Alpha",
            sliderInput("alpha_", "alpha_", 0.1, 1, 0.1)
          ),
          box(
            title = "Beta",
            sliderInput("beta_", "beta_", 0.1, 1, 0.1),
          )
      )
    ),
   
   
  
   
    fluidRow(
      box(plotOutput("density", height=450)),
      box(plotOutput("distribution", height = 450)),
      box(plotOutput("distributionWithProb"), height=450),
     
    ),
   sliderInput("A", "A", 0, 10, 0, 0.1),
   
   
   
   sliderInput("B", "B", 0, 10, 1, 0.1),
  )
)

server <- function(input, output, session) {
  observe({
    if(input$dist == "beta"){
        # hide("alphaBeta")
        show("alphaBeta")
        output$density <-renderPlot({
          x <- seq(0, 1, length = 100)
          a <- input$alpha_
          b <- input$beta_
          density <- dbeta(x, shape1 = a, shape2 = b)
          ggplot() + 
            geom_line(aes(x=x,y=density), color="blue")+
            ggtitle(paste0("Densitatea a = ", a, " b = ", b))+
            xlab("X") + ylab("Density")
        })
        
        output$distribution <-renderPlot({
          x <- seq(0, 1, length = 100)
          a <- input$alpha_
          b <- input$beta_
          distribution <- pbeta(x, shape1 = a, shape2 = b)
          ggplot() +
            geom_line(aes(x=x, y=distribution), color = "red")
            # polygon(x = seq(0.2,0.5,0.01), y=distribution, col="green")
            # geom_area(aes(x=c(0.3,0.5), y=distribution), fill="green", alpha=0.3)
            # ggtitle(paste0("Repartitia a = ", a, "b = ", b))+
            # xlab("X") + ylab("Densitatea")
            
        })
    }
    if(input$dist == "norm"){
      show("alphaBeta")
      updateSliderInput(session, "alpha_", label = "alpha_",value=1, 
                         1, 100, 0.1
      )
      updateSliderInput(session, "beta_", label = "beta_", value=1,
                         1, 100, 0.1
      )
      # hide("alphaBeta")
      output$density<-renderPlot({
        x <- seq(0, 10, length = 1000)
        a <- input$alpha_
        b <- input$beta_
        density <- dnorm(x, mean = a, sd = b)
        ggplot() + 
          geom_line(aes(x=x,y=density), color="blue")+
          
          xlab("X") + ylab("Density")
      })
      
      
      output$distribution <-renderPlot({
        x <- seq(0, 1, length = 1000)
        a <- input$alpha_
        b <- input$beta_
        distribution <- pnorm(x, mean = a, sd = b)
        
        
        ggplot() +
          geom_line(aes(x=x, y=distribution), color = "red") +
          xlab("X") + ylab("Distribution")
       
        
      
        
        
        
        

        
       
        
      })
      output$distributionWithProb<-renderPlot({
        x <- seq(0, 1, length = 1000)
        a <- input$alpha_
        b <- input$beta_
        distribution <- pnorm(x, mean = a, sd = b)
        xs <- seq(0, input$A, by = 0.01)
        xs1 <- c(xs, seq(input$A, 0, by =-0.01))
        ys <- c(c(pnorm(xs, mean = a, sd= b)), seq(0,0,length = length(xs)))
        
        xt <- seq(input$B, 1, by = 0.01)
        xt1 <- c(xt, seq(1, input$B, by = -0.01))
        yt <- c(c(pnorm(xt, mean = a, sd= b)), seq(0,0,length = length(xt)))
        
        
        xf <- seq(input$A, input$B, by =0.01)
        xf1 <- c(xf, seq(input$B, input$A, by =-0.01))
        yf <- c(c(pnorm(xf, mean = a, sd = b)), seq(0,0, length = length(xf)))
        polygon(xf1, yf, col = "blue")
        
        
        
        ggplot() +
          geom_line(aes(x=x, y=distribution), color = "red")+
          geom_polygon(aes(x= xs1, y=ys), colour = "purple", fill="purple")+
          geom_polygon(aes(x= xt1, y=yt), colour = "blue", fill="blue")+
          geom_polygon(aes(x= xf1, y=yf), colour = "grey", fill="grey")
      })
    }
  })
    
}






# Run the shiny app
shinyApp(ui, server)



