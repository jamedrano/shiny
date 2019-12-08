library(shiny)
library(lubridate)
library(ggplot2)

setwd("C:/Users/Antonio/Desktop/Material Cursos/CursoLU")

nomvars = read.csv("VARS.csv",header=T,stringsAsFactors = F)
datos = read.csv("lu.csv",header=T,stringsAsFactors = F)
datos$FECHA = as.POSIXlt(datos$DATA, format="%Y-%m-%d %H:%M:%S")

fechaini = as.POSIXlt("2018-11-11",format="%Y-%m-%d")
fechafin = as.POSIXlt("2019-05-09",format="%Y-%m-%d")

zafra = datos[((datos$FECHA >= fechaini)&(datos$FECHA<= fechafin)),]
rm(datos)

zafra$DIAZAFRA = (as.numeric(zafra$FECHA - fechaini, units="days") + 1)
zafra$valorDia = as.numeric(zafra$VALOR_DIA)
zafra$MESAN = format(as.Date(zafra$FECHA),"%Y-%m")

diaszafra = max(zafra$DIAZAFRA)
semanaszafra = ceiling(as.numeric(difftime(fechafin, fechaini, units = "weeks")))


ui <- fluidPage(

  # App title ----
  titlePanel(h1(textOutput("nomvar"),align="center")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      
      textInput("variable","Variable",value="PZAJCLB"),
      
      
      sliderInput("diazafra", "Dias de Zafra:",
                  min = 1, max = diaszafra,
                  value = c(1,diaszafra)),
      
      numericInput("minspec", "Espec Min", 45),
      numericInput("maxspec", "Espec Max", 60)
    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      
      
      h3(textOutput("text_calc")),
      
      verbatimTextOutput("sum"),
      
      h4(textOutput("text_calc3")),
      
      
      plotOutput("varPlot"),
      
      plotOutput("varHist"),
      
      plotOutput("varRun"),
      
      plotOutput("varBox"),
      
      plotOutput("varViolin")
      
    )
  )
)


server <- function(input, output) {
  
  output$text_calc <- renderText({
    valores = zafra$valorDia[(zafra$VARIA==input$variable)]
    val <- valores[input$diazafra[1]:input$diazafra[2]]
    fuera <- sum((val<input$minspec)|(val>input$maxspec))
    perout <- 100*fuera/length(val)
    paste("% de datos fuera de specs =",round(perout,2))
  })
  
  
  
  output$text_calc3 <- renderText({
    valores = zafra$valorDia[(zafra$VARIA==input$variable)]
    val <- valores[input$diazafra[1]:input$diazafra[2]]
    desv <- sd(val)
    paste("Desv Est =", round(desv,2))
  })
  
  output$sum <- renderPrint({
    valores = zafra$valorDia[(zafra$VARIA==input$variable)]
    val <- valores[input$diazafra[1]:input$diazafra[2]]
    summary(val)
  })
  
  output$nomvar = renderText({
    nomvars$DESCRICAO[nomvars$CODIGO==input$variable]
    
  })
  
  
  output$varPlot <- renderPlot({
    valores = zafra$valorDia[(zafra$VARIA==input$variable)]
    boxplot(valores[input$diazafra[1]:input$diazafra[2]],col="brown")
    abline(h=c(input$minspec,input$maxspec),col="green")
    abline(h=mean(valores[input$diazafra[1]:input$diazafra[2]]),col="red",lwd=2)
  })
  
  output$varHist <- renderPlot({
    valores = zafra$valorDia[(zafra$VARIA==input$variable)]
    hist(valores[input$diazafra[1]:input$diazafra[2]],col="brown",xlab=input$variable)
    abline(v=c(input$minspec,input$maxspec),col="green")
    abline(v=mean(valores[input$diazafra[1]:input$diazafra[2]]),col="red",lwd=2)
  })
  
  output$varRun <- renderPlot({
    valores = zafra$valorDia[(zafra$VARIA==input$variable)]
    plot(input$diazafra[1]:input$diazafra[2],valores[input$diazafra[1]:input$diazafra[2]],type="l",col="brown",xlab="Dia de Zafra",ylab=input$variable)
    abline(h=c(input$minspec,input$maxspec),col="green")
    abline(h=mean(valores[input$diazafra[1]:input$diazafra[2]]),col="red",lwd=2)
  })
  
  output$varBox <- renderPlot({
    zafraRango = zafra[(zafra$VARIA==variable1)&(zafra$DIAZAFRA>=input$diazafra[1])&(zafra$DIAZAFRA<=input$diazafra[2]),]
    boxplot(valorDia ~ MESAN, data=zafraRango, col=rainbow(6))
    abline(h=c(input$minspec,input$maxspec),col="green")
    abline(h=mean(zafraRango$valorDia),col="red",lwd=2)
  })
  
  output$varViolin <- renderPlot({
    zafraRango = zafra[(zafra$VARIA==variable1)&(zafra$DIAZAFRA>=input$diazafra[1])&(zafra$DIAZAFRA<=input$diazafra[2]),]
    ggplot(data=zafraRango,aes(x=MESAN,y=valorDia,fill=MESAN))+geom_violin()
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
