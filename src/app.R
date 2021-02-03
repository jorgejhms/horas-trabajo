# Aplicación para la gestión de horas de trabajo

#Carga de librerías
library(shiny)
library(tidyverse)
library(lubridate)
library(flexdashboard)
# library(zoo)

# Carga de data

    db <- read.csv2("../example/horas_trabajo.csv", sep = ";") 
    db$Inicio <- as.POSIXct(db$Inicio, format = "%d/%m/%Y %H:%M:%S")
    db$Fin <- as.POSIXct(db$Fin, format = "%d/%m/%Y %H:%M:%S")

# Interfaz de usuario
ui <- fluidPage(
    
    # Título del App
    titlePanel("Horas de trabajo"),
    
    sidebarLayout(
        # Barra lateral
        sidebarPanel( 
                      radioButtons(
                          inputId = "IniSem",
                          label = "La semana empieza en:",
                          choices = c("Domingo", "Lunes")
                          )
        ),

        # Panel principal
        mainPanel(
            h2("Horas de trabajo en la semana"),
            gaugeOutput("hSem"),
            
            h2("Horas de trabajo en el día"),
            gaugeOutput("hDia"),
            
            h2("Horas diarias"),
            plotOutput("hdiarias"),
            h2("Horas en el último mes"),
            h2("Horas en la última semana"),
            h2("Promedio de trabajo por día"),
            h2("Promedio de trabajo por día durante la última semana")
            )
    )
)

# Funciones del servidor
server <- function(input, output) {
    
    #Definición de inicio de semana para los gráficos
    SemIni <- reactive({        
        switch(input$IniSem,
            Domingo = 0,
            Lunes = 1)
        options("lubridate.week.start" = SemIni)
        
    }
        
    ) 

    
    # Total de horas en la semana
    output$hSem <- renderGauge ({
        
        tHsem <- db %>%
            mutate(isoweek = isoweek(Inicio)) %>%
            filter(isoweek == last(isoweek)) %>%
            select(Fin, Inicio) %>%
            summarize(Tiempo = sum(difftime(Fin, Inicio, units = "hours"))) %>%
            pull()
        
        gauge(
            round(tHsem, 2),
            min = 0,
            max = 35,
            symbol = "h",
            gaugeSectors(
                success = c(30, 35),
                warning = c(21, 30),
                danger = c(0, 20)
            ))
                })
    
    # Total de horas en el día
    output$hDia <- renderGauge({
        
        tHdia <- db %>%
            mutate(d = date(Inicio)) %>%
            filter(d == today()) %>%
            select(Fin, Inicio) %>%
            summarize(Tiempo = sum(difftime(Fin, Inicio, units = "hours"))) %>%
            pull()
        
        gauge(
            round(tHdia, 2),
            min = 0,
            max = 7,
            symbol = "h",
            gaugeSectors(
                success = c(6, 7),
                warning = c(3, 6),
                danger = c(0, 3)
            )
        )
    })
    
    #Horas diarias
    output$hdiarias <- renderPlot({
        db %>%
            mutate(fecha = date(Inicio)) %>%
            group_by(fecha) %>%
            summarize(Tiempo = sum(difftime(Fin, Inicio, units = "hours"))) %>%
            ggplot(aes(x = fecha, y = Tiempo)) +
            geom_col() +
            geom_hline(yintercept = 6.6,
                       linetype = "dashed",
                       color = "red") +
            theme(legend.position = "none") +
            labs (x = element_blank(), y = element_blank(), title = element_blank())
    })
    
    
}


# Lanzamiento de la aplicación
shinyApp(ui = ui, server = server)
