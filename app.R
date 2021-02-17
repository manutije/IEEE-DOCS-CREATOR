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
    titlePanel("Generador de documentos IEEE"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "ID_Nombre",
                      label = "Ingresa el nombre del Archivo",
            ),
            textInput(inputId = "ID_Titulo",
                      label = "Ingresa el titulo del Documento",
            ),
            textInput(inputId = "ID_autor",
                      label = "Ingresa el autor del Documento",
            ),
            sliderInput(inputId = "ID_Tamaño",label = "Tamaño de letra"
                        ,min = 10,max = 12,value = 10,step = 1
            ),
            selectInput(inputId = "ID_Tipo_Papel",label = "Selecciona el tamaño de papel"
                        ,choices = c("a4paper","letterpaper"),selected = "a4paper"
            ),
            selectInput(inputId = "ID_Clase",label = "Selecciona la clase de documento"
                        ,choices = c("article","report","book","slide")
                        ,selected = "article"
            ),
            radioButtons(inputId = "ID_Columnas",label = "Se requieren 2 columnas"
                         ,choices = c("SI","NO"),selected = "NO"
            ),
            radioButtons(inputId = "ID_Hoja_Titulo",label = "Se requiere una hoja para el titulo"
                         ,choices = c("SI","NO"),selected = "NO"
            ),
            radioButtons(inputId = "ID_Hoja_Indice",label = "Se requiere una hoja para el indice"
                         ,choices = c("SI","NO"),selected = "NO"
            ),
            actionButton(inputId = "ID_boton_1",
                         label = "CREAR"
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Crear Nueva Seccion"),
            textInput(inputId = "ID_Seccion",
                      label = "Ingresa el nombre de la Seccion",
                      value = "Recuerda crear primero el documento"
            ),
            textAreaInput(inputId = "Id_Texto_seccion",
                          label = "Ingresa el texto de la seccion nueva",
                          value = "Primero Crea una nueva Seccion",
                          width = 500,
                          height = 500),
            actionButton(inputId = "ID_boton_2",
                         label = "AGREGAR SECCION"),
            actionButton(inputId = "ID_boton_4",
                         label = "AGREGAR PARRAFO"),
            actionButton(inputId = "ID_boton_3",
                         label = "TERMINAR DOCUMENTO"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$ID_boton_1,{
        nombre <- isolate(input$ID_Nombre)
        titulo <- isolate(input$ID_Titulo)
        autor <- isolate(input$ID_autor)
        tamaño <- isolate(input$ID_Tamaño)
        papel <- isolate(input$ID_Tipo_Papel)
        clase <- isolate(input$ID_Clase)
        columnas <- isolate(input$ID_Columnas)
        hojas_titulo <- isolate(input$ID_Hoja_Titulo)
        hojas_indice <- isolate(input$ID_Hoja_Indice)
        
        nombre_archivo <- sprintf("%s.tex",nombre)
        titulo_completo <- sprintf("\\title{%s}",titulo)
        autor_completo <- sprintf("\\author{%s}",autor)
        if(columnas =="NO"){
            if(hojas_titulo =="NO"){
                clase_completa <- sprintf("\\documentclass[%spt,%s,notitlepage]{%s}",tamaño,papel,clase)
            }
            if(hojas_titulo =="SI"){
                clase_completa <- sprintf("\\documentclass[%spt,%s,titlepage]{%s}",tamaño,papel,clase)
            }
        }
        if(columnas =="SI"){
            if(hojas_titulo =="NO"){
                clase_completa <- sprintf("\\documentclass[%spt,%s,twocolumn,notitlepage]{%s}",tamaño,papel,clase)
            }
            if(hojas_titulo =="SI"){
                clase_completa <- sprintf("\\documentclass[%spt,%s,twocolumn,titlepage]{%s}",tamaño,papel,clase)
            }
            
        }
        file.create(nombre_archivo)
        conn <- file(nombre_archivo,open = "a",encoding = "UTF-8")
        sink(conn,append = TRUE)
        cat(clase_completa)
        cat("\n")
        cat("\\usepackage[utf8]{inputenc}")
        cat("\n")
        cat("\\usepackage{latexsym}")
        cat("\n")
        cat("\\usepackage[activeacute,spanish]{babel}")
        cat("\n")
        cat(autor_completo)
        cat("\n")
        cat(titulo_completo)
        cat("\n")
        cat("\\frenchspacing")
        cat("\n")
        cat("\\begin{document}")
        cat("\n")
        cat("\\maketitle")
        cat("\n")
        cat("\\tableofcontents")
        cat("\n")
        if(hojas_indice =="SI"){
            cat("\\newpage")
            cat("\n")
        }
        sink()
        close(conn)
        updateTextInput(session,"ID_Titulo",value = "")
        updateTextInput(session,"ID_autor",value = "")
        updateTextInput(session,"ID_Seccion",value = "")
        
    })
    
    observeEvent(input$ID_boton_2,{
        nombre <- isolate(input$ID_Nombre)
        seccion <- isolate(input$ID_Seccion)
        nombre_archivo <- sprintf("%s.tex",nombre)
        titulo_seccion <- sprintf("\\section{%s}",seccion)
        conn <- file(nombre_archivo,open = "a",encoding = "UTF-8")
        sink(conn,append = TRUE)
        cat(titulo_seccion)
        cat("\n")
        sink()
        close(conn)
        updateTextInput(session,"ID_Seccion",value = " Ahora escribe abajo el parrafo")
        updateTextAreaInput(session,"Id_Texto_seccion",value = "")
    })
    
    observeEvent(input$ID_boton_3,{
        nombre <- isolate(input$ID_Nombre)
        nombre_archivo <- sprintf("%s.tex",nombre)
        conn <- file(nombre_archivo,open = "a",encoding = "UTF-8")
        sink(conn,append = TRUE)
        cat("\\end{document}")
        cat("\n")
        sink()
        close(conn)
        updateTextInput(session,"ID_Nombre",value = "")
        
    })
    observeEvent(input$ID_boton_4,{
        nombre <- isolate(input$ID_Nombre)
        nombre_archivo <- sprintf("%s.tex",nombre)
        texto <- isolate(input$Id_Texto_seccion)
        parrafo <- sprintf("%s\\\\",texto)
        conn <- file(nombre_archivo,open = "a",encoding = "UTF-8")
        sink(conn,append = TRUE)
        cat(parrafo)
        cat("\n")
        sink()
        close(conn)
        updateTextInput(session,"ID_Seccion",value = "")
        updateTextAreaInput(session,"Id_Texto_seccion",value = "")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
