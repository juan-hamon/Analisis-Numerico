#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Prueba modelo SI"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Cambia el valor de los parametros del modelo"),
            selectInput("tipoModelo",
                        label = "Seleccione el modelo con el que quiere trabajar",
                        choices = list("rk4",
                                       "euler"),
                        selected = "euler"),
            sliderInput("beta",
                        "Valor beta (tasa de infeccion):",
                        min = 0.001,
                        max = 0.232,
                        value = 0.150),
            sliderInput("Suceptibles",
                        "Valor de total de suceptibles: ",
                        min = 1,
                        max = 5,
                        value = 4),
            sliderInput("Infectados",
                        "Valor de total de infectados: ",
                        min = 1,
                        max = 5,
                        value = 3),
            sliderInput("ValHoras",
                        "Horas evaluadas",
                        min = 1,
                        max = 15,
                        value = 10),
            sliderInput("ValParticion",
                        "Escala de horas:",
                        min = 0.1,
                        max = 1,
                        value = 0.5),
            selectInput("datosAMostrar",
                        "Seleccione la poblacion de la que quiere ver los datos en la tabla",
                        choices = list("Suceptibles",
                                       "Infectados"),
                        selected = "Suceptibles")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            dataTableOutput("Table"),
            plotOutput("Error"),
            dataTableOutput("TablaError")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        #Se limpian los elementos creados con anterioridad
        #rm(list=ls())
        #Se limpia la consola para una mejor visualizacion 
        cat("\014")
        library(deSolve)
        library(EpiDynamics)
        #Modelo basado en "On the Performance of Internet Worm Scanning Strategies"
        
        SI <- function(t, x, parametros){
            with(as.list(c(parametros, x)),{
                dS <- - beta*S*I
                dI <- + beta*S*I
                derivadas <- c(dS, dI)
                return(list(derivadas))
            })
        } 
        
        parametros <- c(beta=input$beta)
        
        v_iniciales <- c(S=input$Suceptibles, I=input$Infectados)
        
        dt <- seq(0, input$ValHoras, input$ValParticion)
        
        sol = ode(y=v_iniciales, times=dt, func=SI,parms=parametros, method = input$tipoModelo)
        
        paramm <- c(beta=input$beta, gamma = 0)
        
        v_0 <- c(S=input$Suceptibles, I=input$Infectados, R = 0)
        
        result = SIR(pars = paramm, init = v_0, time = dt)
        
        simulacion.si <- as.data.frame(sol)
        attach(simulacion.si)
        N <- sum(v_iniciales)
        plot(dt, S, type="l", col="blue", ylim=c(0,sum(v_iniciales)),xlab="Tiempo (en Horas)", ylab="Numero de individuos")
        lines(dt, I, type="l", col="red")
        lines(dt, result$results[,2], type = "l", col = "brown")
        lines(dt, result$results[,3], type = "l", col = "orange")
        title("Modelo SI:")
        legend((input$ValHoras)/2, N/1.5,
               legend=c("Susceptibles por ODE", "Infectados por ODE", "Suceptibles por EpiDynamics", "Infectados por EpiDynamics"), col=c("blue", "red", "brown", "orange"), lty=rep(1, 2)) 
    })
    
    output$Table <-renderDataTable({
        #Se limpian los elementos creados con anterioridad
        #rm(list=ls())
        #Se limpia la consola para una mejor visualizacion 
        cat("\014")
        library(deSolve)
        
        #Modelo basado en "On the Performance of Internet Worm Scanning Strategies"
        
        SI <- function(t, x, parametros){
            with(as.list(c(parametros, x)),{
                dS <- - beta*S*I
                dI <- + beta*S*I
                derivadas <- c(dS, dI)
                return(list(derivadas))
            })
        } 
        
        parametros <- c(beta=input$beta)
        
        v_iniciales <- c(S=input$Suceptibles, I=input$Infectados)
        
        dt <- seq(0, input$ValHoras, input$ValParticion)
        
        sol = ode(y=v_iniciales, times=dt, func=SI,parms=parametros, method = input$tipoModelo)
        
        N <- sum(v_iniciales)
        
        if( input$datosAMostrar == "Suceptibles" ){
            tabla = cbind(dt, round((sol[,2]/N)*100,1))
            colnames(tabla) = c("Tiempo", "Suceptibles (% de poblacion)")
            tabla
        }else if(input$datosAMostrar == "Infectados"){
            tabla = cbind(dt, round((sol[,3]/N)*100,1))
            colnames(tabla) = c("Tiempo", "Infectados(% de poblacion)")
            tabla
        }
    })
    
    output$Error <- renderPlot({
        #Se limpian los elementos creados con anterioridad
        #rm(list=ls())
        #Se limpia la consola para una mejor visualizacion 
        cat("\014")
        library(deSolve)
        library(EpiDynamics)
        #Modelo basado en "On the Performance of Internet Worm Scanning Strategies"
        
        SI <- function(t, x, parametros){
            with(as.list(c(parametros, x)),{
                dS <- - beta*S*I
                dI <- + beta*S*I
                derivadas <- c(dS, dI)
                return(list(derivadas))
            })
        } 
        
        parametros <- c(beta=input$beta)
        
        v_iniciales <- c(S=input$Suceptibles, I=input$Infectados)
        
        dt <- seq(0, input$ValHoras, input$ValParticion)
        
        sol = ode(y=v_iniciales, times=dt, func=SI,parms=parametros, method = input$tipoModelo)
        
        paramm <- c(beta=input$beta, gamma = 0)
        
        v_0 <- c(S=input$Suceptibles, I=input$Infectados, R = 0)
        
        result = EpiDynamics::SIR(pars = paramm, init = v_0, time = dt)
        
        if( input$datosAMostrar == "Suceptibles" ){
            valODESuceptibles = c(sol[,2])
            valEpiSuceptibles = c(result$results[,2])
            errores = c()
            i = 1
            max = length(valODESuceptibles)
            while(i <= max){
                errorActual =(abs(valODESuceptibles[i] - valEpiSuceptibles[i] )/valEpiSuceptibles[i]) 
                errores[i] = errorActual
                i = i + 1
            }
            plot(dt, errores, type="l", col= "red", xlab = "Tiempo (en Horas)", ylab = "Error relativo")
            title("Error relativo para Suceptibles")
        }else if( input$datosAMostrar == "Infectados"){
            valODEInfectados = c(sol[,3])
            valEpiInfectados = c(result$results[,3])
            errores = c()
            i = 1
            max = length(valODEInfectados)
            while(i <= max){
                errorActual = (abs(valODEInfectados[i] - valEpiInfectados[i] )/valEpiInfectados[i])
                errores[i] = errorActual
                i = i + 1
            }
            plot(dt, errores, type="l", col= "blue", xlab = "Tiempo (en Horas)", ylab = "Error relativo")
            title("Error relativo para Infectados")
        }
    })
    
    output$TablaError <- renderDataTable({
        #Se limpian los elementos creados con anterioridad
        #rm(list=ls())
        #Se limpia la consola para una mejor visualizacion 
        cat("\014")
        library(deSolve)
        library(EpiDynamics)
        #Modelo basado en "On the Performance of Internet Worm Scanning Strategies"
        
        SI <- function(t, x, parametros){
            with(as.list(c(parametros, x)),{
                dS <- - beta*S*I
                dI <- + beta*S*I
                derivadas <- c(dS, dI)
                return(list(derivadas))
            })
        } 
        
        parametros <- c(beta=input$beta)
        
        v_iniciales <- c(S=input$Suceptibles, I=input$Infectados)
        
        dt <- seq(0, input$ValHoras, input$ValParticion)
        
        sol = ode(y=v_iniciales, times=dt, func=SI,parms=parametros, method = input$tipoModelo)
        
        paramm <- c(beta=input$beta, gamma = 0)
        
        v_0 <- c(S=input$Suceptibles, I=input$Infectados, R = 0)
        
        result = SIR(pars = paramm, init = v_0, time = dt)
        
        if( input$datosAMostrar == "Suceptibles" ){
            valODESuceptibles = c(sol[,2])
            valEpiSuceptibles = c(result$results[,2])
            errores = c()
            i = 1
            max = length(valODESuceptibles)
            while(i <= max){
                errorActual =(abs(valODESuceptibles[i] - valEpiSuceptibles[i] )/valEpiSuceptibles[i]) 
                errores[i] = errorActual
                i = i + 1
            }
            tabla = cbind(dt, errores)
            colnames(tabla) = c("Tiempo", "Error relativo")
            tabla
        }else if( input$datosAMostrar == "Infectados"){
            valODEInfectados = c(sol[,3])
            valEpiInfectados = c(result$results[,3])
            errores = c()
            i = 1
            max = length(valODEInfectados)
            while(i <= max){
                errorActual = (abs(valODEInfectados[i] - valEpiInfectados[i] )/valEpiInfectados[i])
                errores[i] = errorActual
                i = i + 1
            }
            tabla = cbind(dt, errores)
            colnames(tabla) = c("Tiempo", "Error relativo")
            tabla
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
