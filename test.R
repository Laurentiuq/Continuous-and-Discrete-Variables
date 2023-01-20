library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyjs)
library(r2symbols)



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
      box(title = "P", height = 450,
            h4(id = "P1", textOutput("P1")),
            h4(id = "P2", textOutput("P2")),
            h4(id = "P3", textOutput("P3"))
          )
      
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
      
      updateSliderInput(session, "alpha_", label = symbol("alpha"),value=1, 
                        0, 10, 0.1
      )
      updateSliderInput(session, "beta_", label = HTML("beta"), value=1,
                        0, 10, 0.1
      )
      
      output$density <-renderPlot({
        x <- seq(0, 1, length = 1000)
        a <- input$alpha_
        b <- input$beta_
        density <- dbeta(x, shape1 = a, shape2 = b)
        ggplot() + 
          geom_line(aes(x=x,y=density), color="blue")+
          ggtitle(paste0("Densitatea a = ", a, " b = ", b))+
          xlab("X") + ylab("Density")
      })
      
      output$distribution <-renderPlot({
        x <- seq(0, 1, length = 1000)
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
      
      
      output$distributionWithProb<-renderPlot({
        x <- seq(0, 1, length = 1000)
        a <- input$alpha_
        b <- input$beta_
        A <- input$A
        B <- input$B
        distribution <- pbeta(x, shape1 = a, shape2 = b)
        
        
        xs <- seq(0, A, by = 0.01)
        xs1 <- c(xs, seq(A, 0, by =-0.01))
        ys <- c(c(pbeta(xs, shape1 = a, shape2 = b)), seq(0,0,length = length(xs)))
        
        xt <- seq(B, 1, by = 0.01)
        xt1 <- c(xt, seq(1, B, by = -0.01))
        yt <- c(c(pbeta(xt, shape1 = a, shape2 = b)), seq(0,0,length = length(xt)))
        
        
        xf <- seq(A, B, by =0.01)
        xf1 <- c(xf, seq(B, A, by =-0.01))
        yf <- c(c(pbeta(xf, shape1 = a, shape2 = b)), seq(0,0, length = length(xf)))
        polygon(xf1, yf, col = "blue")
        
        
        
        
        output$P1<-renderText(paste0(HTML("P(X ≤ A) = "), round(pbeta(A, shape1 = a, shape2 = b), digits = 2)))
        output$P2<-renderText(paste0(HTML("P( X ≥ B) = "), round(1 - pbeta(B, shape1 = a, shape2 = b), digits = 2)))
        output$P3<-renderText(paste0(HTML("P(A ≤ X ≤ B) = "), round(pbeta(B, shape1 = a, shape2 = b) - pbeta(A, shape1 = a, shape2 = b), digits= 2)))
        
        
        ggplot() +
          geom_line(aes(x=x, y=distribution), color = "red")+
          geom_polygon(aes(x= xs1, y=ys), colour = "purple", fill="purple")+
          geom_polygon(aes(x= xt1, y=yt), colour = "blue", fill="blue")+
          geom_polygon(aes(x= xf1, y=yf), colour = "grey", fill="grey")
      })
      
      
    }
    
    
    
    
    if(input$dist == "norm"){
      show("alphaBeta")
      updateSliderInput(session, "alpha_", label = "Media",value=1, 
                        0, 100, 0.1
      )
      updateSliderInput(session, "beta_", label = "Deviatia standard", value=1,
                        0, 100, 0.1
      )
      # hide("alphaBeta")
      output$density<-renderPlot({
        x <- seq(0, 1, length = 1000)
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
        A <- input$A
        B <- input$B
        distribution <- pnorm(x, mean = a, sd = b)
        xs <- seq(0, A, by = 0.01)
        xs1 <- c(xs, seq(A, 0, by =-0.01))
        ys <- c(c(pnorm(xs, mean = a, sd= b)), seq(0,0,length = length(xs)))
        
        xt <- seq(B, 1, by = 0.01)
        xt1 <- c(xt, seq(1, B, by = -0.01))
        yt <- c(c(pnorm(xt, mean = a, sd= b)), seq(0,0,length = length(xt)))
        
        
        xf <- seq(A, B, by =0.01)
        xf1 <- c(xf, seq(B, A, by =-0.01))
        yf <- c(c(pnorm(xf, mean = a, sd = b)), seq(0,0, length = length(xf)))
        polygon(xf1, yf, col = "blue")
        

        
        
        output$P1<-renderText(paste0(HTML("P(X ≤ A) = "), round(pnorm(A, mean = a, sd = b), digits = 2)))
        output$P2<-renderText(paste0(HTML("P( X ≥ B) = "), round(1 - pnorm(B, mean = a, sd = b), digits = 2)))
        output$P3<-renderText(paste0(HTML("P(A ≤ X ≤ B) = "), round(pnorm(B, mean = a, sd = b) - pnorm(A, mean = a, sd = b), digits= 2)))
        
        
        ggplot() +
          geom_line(aes(x=x, y=distribution), color = "red")+
          geom_polygon(aes(x= xs1, y=ys), colour = "purple", fill="purple")+
          geom_polygon(aes(x= xt1, y=yt), colour = "blue", fill="blue")+
          geom_polygon(aes(x= xf1, y=yf), colour = "grey", fill="grey")
      })
    }
      
    
    if(input$dist == "unif"){
      show("alphaBeta")
      updateSliderInput(session, "alpha_", label = "Min",value=0.5, 
                        0, 1, 0.01
      )
      updateSliderInput(session, "beta_", label = "Max", value=0.7,
                        0, 1, 0.01
      )
      # hide("alphaBeta")
      output$density<-renderPlot({
        x <- seq(0, 1, length = 1000)
        a <- input$alpha_
        b <- input$beta_
        density <- dunif(x, min = a,  max = b, log = FALSE)
        ggplot() + 
          geom_line(aes(x=x,y=density), color="blue")+
          
          xlab("X") + ylab("Density")
      })
      
      
      output$distribution <-renderPlot({
        x <- seq(0, 1, length = 1000)
        a <- input$alpha_
        b <- input$beta_
        distribution <- punif(x, min = a, max = b)
        
        
        ggplot() +
          geom_line(aes(x=x, y=distribution), color = "red") +
          xlab("X") + ylab("Distribution")
        
        
      })
      output$distributionWithProb<-renderPlot({
        x <- seq(0, 1, length = 1000)
        a <- input$alpha_
        b <- input$beta_
        A <- input$A
        B <- input$B
        distribution <- punif(x, min = a, max = b)
        
        xs <- seq(0, A, by = 0.01)
        xs1 <- c(xs, seq(A, 0, by =-0.01))
        ys <- c(c(punif(xs, min = a, max= b)), seq(0,0,length = length(xs)))
        
        xt <- seq(B, 1, by = 0.01)
        xt1 <- c(xt, seq(1, B, by = -0.01))
        yt <- c(c(punif(xt, min = a, max= b)), seq(0,0,length = length(xt)))
        
        
        xf <- seq(A, B, by =0.01)
        xf1 <- c(xf, seq(B, A, by =-0.01))
        yf <- c(c(punif(xf, min = a, max = b)), seq(0,0, length = length(xf)))
        polygon(xf1, yf, col = "blue")
        
        
        
        
        output$P1<-renderText(paste0(HTML("P(X ≤ A) = "), round(punif(A, min = a, max = b), digits = 2)))
        output$P2<-renderText(paste0(HTML("P( X ≥ B) = "), round(1 - punif(B, min = a, max = b), digits = 2)))
        output$P3<-renderText(paste0(HTML("P(A ≤ X ≤ B) = "), round(punif(B, min = a, max = b) - punif(A, min = a, max = b), digits= 2)))
        
        
        ggplot() +
          geom_line(aes(x=x, y=distribution), color = "red", size=2)+
          geom_polygon(aes(x= xs1, y=ys), colour = "purple", fill="purple")+
          geom_polygon(aes(x= xt1, y=yt), colour = "blue", fill="blue")+
          geom_polygon(aes(x= xf1, y=yf), colour = "grey", fill="grey")
      })
    
    
    }
    
    
    
    
  })
  
}






# Run the shiny app
shinyApp(ui, server)



