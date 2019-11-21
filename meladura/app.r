library(shiny)

library(lubridate)

setwd("C:/Users/Antonio/Desktop/Material Cursos/CursoLU")

datos = read.csv("lu.csv",header=T,stringsAsFactors = F)
datos$FECHA = as.POSIXlt(datos$DATA, format="%Y-%m-%d %H:%M:%S")
datos$valorDia = as.numeric(datos$VALOR_DIA)

fechaini = as.POSIXlt("2018-11-11",format="%Y-%m-%d")
fechafin = as.POSIXlt("2019-05-09",format="%Y-%m-%d")
datos$DIAZAFRA = (as.numeric(datos$FECHA - fechaini, units="days") + 1)*((datos$FECHA >= fechaini)&(datos$FECHA<= fechafin)))
diaszafra = max(datos$DIAZAFRA)

ui <- fluidPage(
  
  # App title ----
  titlePanel(h1("Meladura",align="center")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      
      selectInput("variable", "Variable:",
                  c("Pol" = "POLMELB",
                    "Brix" = "BRIMELB",
                     "pH" = "PHMELB",
                     "Pureza" = "PZAMELB")),
      
      sliderInput("diazafra", "Dias de Zafra:",
                  min = 1, max = diaszafra,
                  value = c(1,diaszafra)),
      
      # Input: Checkbox for whether outliers should be included ----
      numericInput("minspec", "Espec Min", 45),
      numericInput("maxspec", "Espec Max", 60)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("text_calc")),
      
      h3(textOutput("text_calc2")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("meladuraPlot"),
      
      plotOutput("meladuraHist"),
      
      plotOutput("meladuraRun")
      
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  output$text_calc <- renderText({
    valores = datos$valorDia[(datos$VARIA==input$variable)&(datos$DIAZAFRA!=0)]
    val1 <- valores[input$diazafra[1]:input$diazafra[2]]
    fuera <- sum((val1<input$minspec)|(val1>input$maxspec))
    perout <- 100*fuera/length(val1)
    paste("% de datos fuera de specs =",round(perout,2))
  })
  
  output$text_calc2 <- renderText({
    valores = datos$valorDia[(datos$VARIA==input$variable)&(datos$DIAZAFRA!=0)]
    val2 <- valores[input$diazafra[1]:input$diazafra[2]]
    media <- mean(val2)
    desv <- sd(val2)
    paste("Media = ",round(media,2),"   Desv Est =", round(desv,2))
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$meladuraPlot <- renderPlot({
    valores = datos$valorDia[(datos$VARIA==input$variable)&(datos$DIAZAFRA!=0)]
    boxplot(valores[input$diazafra[1]:input$diazafra[2]],col="brown",main="Meladura")
    abline(h=c(input$minspec,input$maxspec),col="green")
    abline(h=mean(valores[input$diazafra[1]:input$diazafra[2]]),col="red",lwd=2)
  })
  
  output$meladuraHist <- renderPlot({
    valores = datos$valorDia[(datos$VARIA==input$variable)&(datos$DIAZAFRA!=0)]
    hist(valores[input$diazafra[1]:input$diazafra[2]],col="brown",main="Meladura",xlab=input$variable)
    abline(v=c(input$minspec,input$maxspec),col="green")
    abline(v=mean(valores[input$diazafra[1]:input$diazafra[2]]),col="red",lwd=2)
  })
  
  output$meladuraRun <- renderPlot({
    valores = datos$valorDia[(datos$VARIA==input$variable)&(datos$DIAZAFRA!=0)]
    plot(input$diazafra[1]:input$diazafra[2],valores[input$diazafra[1]:input$diazafra[2]],type="l",col="brown",xlab="Dia de Zafra",ylab=input$variable)
    abline(h=c(input$minspec,input$maxspec),col="green")
    abline(h=mean(valores[input$diazafra[1]:input$diazafra[2]]),col="red",lwd=2)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
