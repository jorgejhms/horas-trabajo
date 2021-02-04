# Aplicación para la gestión de horas de trabajo

#Carga de librerías
library(shiny)
library(tidyverse)
library(lubridate)
library(flexdashboard)
# library(zoo)

options("lubridate.week.start" = 1)

# Carga de data

db <- read.csv2("../example/horas_trabajo.csv", sep = ";")
db$Inicio <- as.POSIXct(db$Inicio, format = "%d/%m/%Y %H:%M:%S")
db$Fin <- as.POSIXct(db$Fin, format = "%d/%m/%Y %H:%M:%S")


# Interfaz de usuario
ui <- fluidPage(# Título del App
    titlePanel("Horas de trabajo"),
    
    sidebarLayout(
        # Barra lateral
        
        
        sidebarPanel(
            dateRangeInput(inputId = "dates",
                           label = "Fechas",
                           start = today() - 30,
                           end = today(),
                           min = min(db$Inicio),
                           max = today()
                           ),
           # sliderInput(
            #    inputId = "dias",
             #   label = "Número de días:",
              #  min = 1,
               # max = 365,
                #value = 30
            #),
            
            selectInput(
                inputId = "filtro",
                label = "Escoge un filtro:",
                choices = c("Mes", "Semana")
            ),
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
            plotOutput("hultimMon"),
            
            h2("Horas en la última semana"),
            plotOutput("hultimSem"),
            
            h2("Promedio de trabajo por día"),
            plotOutput("promDia"),
            
            h2("Promedio de trabajo por día durante la última semana"),
            plotOutput("promSem")
        )
    ))



# Funciones del servidor
server <- function(input, output) {
    filtro <- reactive({
        switch(input$filtro,
               "Mes" = month,
               "Semana" = isoweek)
    })
    
    # Total de horas en la semana
    output$hSem <- renderGauge ({
        tHsem <- db %>%
            filter(isoweek(Inicio) == last(isoweek(Inicio))) %>%
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
            )
        )
    })
    
    # Total de horas en el día
    output$hDia <- renderGauge({
        tHdia <- db %>%
            filter(date(Inicio) == today()) %>%
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
            filter(fecha > input$dates[1] & fecha < input$dates[2]) %>%
            #filter(fecha >= today() - input$dias) %>%
            ggplot(aes(x = fecha, y = Tiempo)) +
            geom_col() +
            geom_hline(yintercept = 6.6,
                       linetype = "dashed",
                       color = "red") +
            theme(legend.position = "none") +
            labs (x = element_blank(),
                  y = element_blank(),
                  title = element_blank())
    })
    output$hultimMon <- renderPlot ({
        filtro <- filtro()
        
        db %>%
            filter(filtro(Inicio) == last(filtro(Inicio))) %>%
            group_by(Trabajo) %>%
            summarize(Tiempo = sum(difftime(Fin, Inicio, units = "hours"))) %>%
            ggplot(aes(
                x = Tiempo,
                y = Trabajo,
                fill = Trabajo
            )) +
            geom_col() +
            geom_label(aes(label = round(Tiempo, 2)), fill = "white") +
            theme(legend.position = "none") +
            labs (x = element_blank(),
                  y = element_blank(),
                  title = element_blank())
    })
    
    output$hultimSem <- renderPlot({
        db %>%
            mutate(isoweek = isoweek(Inicio)) %>%
            filter(isoweek == last(isoweek)) %>%
            group_by(Trabajo) %>%
            summarize(Tiempo = sum(difftime(Fin, Inicio, units = "hours"))) %>%
            ggplot(aes(
                x = Tiempo,
                y = Trabajo,
                fill = Trabajo
            )) +
            geom_col() +
            geom_label(aes(label = round(Tiempo, 2)), fill = "white") +
            theme(legend.position = "none") +
            labs (x = element_blank(),
                  y = element_blank(),
                  title = element_blank())
        
        
    })
    
    output$promDia <- renderPlot({
        db %>%
            tibble(
                dia = wday(Inicio, label = TRUE, abbr = FALSE),
                isoweek = isoweek(Inicio)
            ) %>%
            group_by(dia, isoweek) %>%
            summarize(Tiempo = sum(difftime(Fin, Inicio, units = "hours"))) %>%
            group_by(dia) %>%
            summarize(Tiempo = mean(Tiempo)) %>%
            ggplot(aes(
                x = dia,
                y = Tiempo,
                fill = dia
            )) +
            geom_col() +
            geom_label(aes(label = round(Tiempo, 2)), fill = "white") +
            theme(legend.position = "none") +
            labs (x = element_blank(),
                  y = element_blank(),
                  title = element_blank())
    })
    
    output$promSem <- renderPlot({
        db %>%
            tibble(
                dia = wday(Inicio, label = TRUE, abbr = FALSE),
                isoweek = isoweek(Inicio)
            ) %>% #identifica el número de semana
            filter(isoweek == last(isoweek)) %>% #filtra última semana
            group_by(dia) %>%
            summarize(Tiempo = sum(difftime(Fin, Inicio, units = "hours"))) %>%
            ggplot(aes(
                x = dia,
                y = Tiempo,
                fill = dia
            )) +
            geom_col() +
            geom_label(aes(label = round(Tiempo, 2)), fill = "white") +
            theme(legend.position = "none") +
            labs (x = element_blank(),
                  y = element_blank(),
                  title = element_blank())
    })
}


# Lanzamiento de la aplicación
shinyApp(ui = ui, server = server)
