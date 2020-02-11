#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),
                  tags$head(tags$script(HTML(jscode))),
                  shinyjs::useShinyjs(),
                  useShinyalert(),
                  shinyjs::inlineCSS(appCSS),
                  use_waiter(),
                  hidden(
                    div(id="pg",
                        navbarPage("GpMedia", 
                                   tabPanel("Calificar",
                                            jumbotron(div(img(src="gppolls.png", height="5%", width="5%", align="right"),
                                                          "Noticias SEP"),
                                                      "Calificar noticias", button = F),
                                            uiOutput("datos"),
                                            pickerInput("filtro", label = "Seleccionar tipo de medio", 
                                                        choices = c("Internet", "Periódico", "Radio", "Revista", "Televisión"),
                                                        selected = c("Internet", "Periódico", "Radio", "Revista", "Televisión"),
                                                        multiple = T,
                                                        options = list(`actions-box`=T,
                                                                       `deselect-all-text` = "Deseleccionar todo",
                                                                       `select-all-text` = "Seleccionar todo",
                                                                       `none-selected-text` = "Seleccione tipo de medio")),
                                            fluidRow(actionBttn("siguiente","Eliminar nota",
                                                                style = "minimal",block = T,color = "warning")),
                                            br(),
                                            hidden(
                                              div(id = "revision",
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      fluidRow(
                                                        radioGroupButtons(
                                                          inputId = "calificacion",
                                                          label = labelMandatory("Calificación de la información"),
                                                          choices = c("Negativa","Neutra","Positiva"),
                                                          status = "primary",
                                                          selected = "",
                                                          checkIcon = list(
                                                            yes = icon("ok", 
                                                                       lib = "glyphicon"),
                                                            no = icon("remove",
                                                                      lib = "glyphicon"))
                                                        )),
                                                      fluidRow( 
                                                        radioGroupButtons(
                                                          inputId = "postura",
                                                          label = labelMandatory("Neutra, a favor o en contra de EMB o SEP"),
                                                          choices = cross2(c("Negativa","Neutra","Positiva"),c("SEP","EMB")) %>% 
                                                            map(~paste(.x[[1]],.x[[2]])) %>% do.call(base::c, .),
                                                          status = "primary",
                                                          selected = "",
                                                          checkIcon = list(
                                                            yes = icon("ok", 
                                                                       lib = "glyphicon"),
                                                            no = icon("remove",
                                                                      lib = "glyphicon"))
                                                        )),
                                                      fluidRow(column(6,
                                                                      pickerInput("habla",label = labelMandatory("¿Quién habla en la nota?"),
                                                                                  choices = NULL, options = list(`live-search` = T,
                                                                                                                 `live-search-placeholder`="Buscar...",
                                                                                                                 `none-selected-text` = "Seleccione una opción",
                                                                                                                 `none-results-text` = "Opción no encontrada. Favor de agregarla")
                                                                      )),
                                                               column(6,
                                                                      actionBttn(inputId = "addHabla",
                                                                                 label = "Agregar",
                                                                                 color = "danger",
                                                                                 style = "material-circle",
                                                                                 icon = icon("plus"))
                                                               )),
                                                      fluidRow(column(6,
                                                                      pickerInput("refiere",label = labelMandatory("¿A quién o a qué se refiere la nota?"),
                                                                                  choices = NULL, options = list(`live-search` = T,
                                                                                                                 `live-search-placeholder`="Buscar...",
                                                                                                                 `none-selected-text` = "Seleccione una opción",
                                                                                                                 `none-results-text` = "Opción no encontrada. Favor de agregarla")
                                                                      )),
                                                               column(6,
                                                                      actionBttn(inputId = "addRefiere",
                                                                                 label = "Agregar",
                                                                                 color = "danger",
                                                                                 style = "material-circle",
                                                                                 icon = icon("plus"))
                                                               )
                                                      ),
                                                      fluidRow(
                                                        alignCenter(
                                                          radioGroupButtons(
                                                            inputId = "importancia",
                                                            label = labelMandatory("Importancia de la nota según su contenido y ubicación"),
                                                            choices = c("Referencial","Secundario","Principal"),
                                                            status = "primary",
                                                            selected = "",
                                                            checkIcon = list(
                                                              yes = icon("ok", 
                                                                         lib = "glyphicon"),
                                                              no = icon("remove",
                                                                        lib = "glyphicon"))
                                                          ))
                                                      ),
                                                      fluidRow(
                                                        textAreaInput("mencion", label = labelMandatory("Mención"))
                                                      ),
                                                      fluidRow(column(6,
                                                                      pickerInput("subtema",label = labelMandatory("Subtema"),
                                                                                  choices = NULL, options = list(`live-search` = T,
                                                                                                                 `live-search-placeholder`="Buscar...",
                                                                                                                 `none-selected-text` = "Seleccione una opción",
                                                                                                                 `none-results-text` = "Opción no encontrada. Favor de agregarla")
                                                                      )),
                                                               column(6,
                                                                      actionBttn(inputId = "addSubtema",
                                                                                 label = "Agregar",
                                                                                 color = "danger",
                                                                                 style = "material-circle",
                                                                                 icon = icon("plus"))
                                                               )
                                                      ),
                                                      fluidRow(
                                                        textAreaInput("observaciones", label = "Observaciones")
                                                      ),
                                                      fluidRow(column(10, offset = 1,
                                                                      disabled(actionButton(inputId = "guardar",label = "Guardar")
                                                                      )))
                                                    ),
                                                    mainPanel(uiOutput("visualizacion")), fluid = T
                                                  )
                                              )
                                            )
                                   ),
                                   tabPanel("Spam", id = "norelevante",
                                            hidden(
                                              div(id = "norelevantes",
                                                  tagList(
                                                    uiOutput("visualizacion2"),
                                                    br(),
                                                    DTOutput("nnrtabla",width = "100%"),
                                                    fluidRow(
                                                      column(4, offset = 1,
                                                             actionBttn(inputId = "agregar",label = "Calificar",block = T,style = "jelly",
                                                                        icon = icon(name = "newspaper"),color = "success")
                                                      ),
                                                      column(4, offset = 1,
                                                             actionBttn(inputId = "eliminar",label = "Desechar", block = T, style = "jelly",
                                                                        icon = icon(name = "trash"), color = "danger")
                                                      )
                                                    )
                                                  )
                                              )
                                            )
                                   ),
                                   tabPanel(
                                     "Editar/Revisar",
                                     h1("Todavía no sirve la opción de editar"),
                                     dateRangeInput("fechasVis","Seleccionar fechas",
                                                    start = today("America/Mexico_City")-1,
                                                    end = today(tzone = "America/Mexico_City"),
                                                    separator = "a",language = "es"),
                                         sidebarLayout(
                                           sidebarPanel(
                                             fluidRow(
                                               radioGroupButtons(
                                                 inputId = "calificacionEd",
                                                 label = labelMandatory("Calificación de la información"),
                                                 choices = c("Negativa","Neutra","Positiva"),
                                                 status = "primary",
                                                 selected = "",
                                                 checkIcon = list(
                                                   yes = icon("ok", 
                                                              lib = "glyphicon"),
                                                   no = icon("remove",
                                                             lib = "glyphicon"))
                                               )),
                                             fluidRow( 
                                               radioGroupButtons(
                                                 inputId = "posturaEd",
                                                 label = labelMandatory("Neutra, a favor o en contra de EMB o SEP"),
                                                 choices = cross2(c("Negativa","Neutra","Positiva"),c("SEP","EMB")) %>% 
                                                   map(~paste(.x[[1]],.x[[2]])) %>% do.call(base::c, .),
                                                 status = "primary",
                                                 selected = "",
                                                 checkIcon = list(
                                                   yes = icon("ok", 
                                                              lib = "glyphicon"),
                                                   no = icon("remove",
                                                             lib = "glyphicon"))
                                               )),
                                             fluidRow(column(6,
                                                             pickerInput("hablaEd",label = labelMandatory("¿Quién habla en la nota?"),
                                                                         choices = NULL, options = list(`live-search` = T,
                                                                                                        `live-search-placeholder`="Buscar...",
                                                                                                        `none-selected-text` = "Seleccione una opción",
                                                                                                        `none-results-text` = "Opción no encontrada. Favor de agregarla")
                                                             ))),
                                             fluidRow(column(6,
                                                             pickerInput("refiereEd",label = labelMandatory("¿A quién o a qué se refiere la nota?"),
                                                                         choices = NULL, options = list(`live-search` = T,
                                                                                                        `live-search-placeholder`="Buscar...",
                                                                                                        `none-selected-text` = "Seleccione una opción",
                                                                                                        `none-results-text` = "Opción no encontrada. Favor de agregarla")
                                                             ))
                                             ),
                                             fluidRow(
                                               alignCenter(
                                                 radioGroupButtons(
                                                   inputId = "importanciaEd",
                                                   label = labelMandatory("Importancia de la nota según su contenido y ubicación"),
                                                   choices = c("Referencial","Secundario","Principal"),
                                                   status = "primary",
                                                   selected = "",
                                                   checkIcon = list(
                                                     yes = icon("ok", 
                                                                lib = "glyphicon"),
                                                     no = icon("remove",
                                                               lib = "glyphicon"))
                                                 ))
                                             ),
                                             fluidRow(
                                               textAreaInput("mencionEd", label = labelMandatory("Mención"))
                                             ),
                                             fluidRow(column(6,
                                                             pickerInput("subtemaEd",label = labelMandatory("Subtema"),
                                                                         choices = NULL, options = list(`live-search` = T,
                                                                                                        `live-search-placeholder`="Buscar...",
                                                                                                        `none-selected-text` = "Seleccione una opción",
                                                                                                        `none-results-text` = "Opción no encontrada. Favor de agregarla")
                                                             ))
                                             ),
                                             fluidRow(
                                               textAreaInput("observacionesEd", label = "Observaciones")
                                             ),
                                             fluidRow(column(10, offset = 1,
                                                             disabled(actionButton(inputId = "editar",label = "Editar")
                                                             )))
                                           ),
                                           mainPanel(dataTableOutput("notasCalificadas")), fluid = T
                                         )
                                   ),
                                   tabPanel("Descargar base",
                                            dateRangeInput("fechas","Seleccionar fechas",
                                                           start = today("America/Mexico_City") %>% floor_date(unit = "month"),
                                                           end = today(tzone = "America/Mexico_City"),separator = "a"),
                                            downloadButton("descargaBase",label = "Descargar base calificada")
                                            )
                        )
                    )
                  )
))
