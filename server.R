shinyServer(function(input, output, session) {
  
  rv <- reactiveValues(row = 1, row2 = 1, link = NULL, link2 = NULL)
  bd <- reactiveValues(entrenamiento = NULL, prueba = NULL, 
                       preprueba = NULL, nnr = NULL, opciones = NULL,
                       backup = NULL)
  fecha <- reactiveValues(inicio = NULL, guardar = NULL)
  tareas <- reactiveValues(restantes = NULL,
                           pe = NULL,
                           tv = NULL,
                           rd = NULL)
  # leer preguntas y que sea reactivo por sesion
  
  # generar páginas
  observe({
    req(bd$prueba)
    if(nrow(bd$prueba) >= 0){
      rv$link <- req(bd$prueba) %>% slice(rv$row) %>% pull(link)
      output$visualizacion <- renderUI({
        iframe(width = "100%", height = "750",
               url_link = rv$link)
      }) 
    }
  })
  
  observe({
    req(bd$nnr)
    if(nrow(bd$nnr) >= max(rv$row2)){
      rv$link2 <- req(bd$nnr) %>% slice(rv$row2) %>% pull(link)
      output$visualizacion2 <- renderUI({
        iframe(width = "100%", height = "750",
               url_link = rv$link2[[length(rv$link2)]]) 
      })
    }
  })
  
  observe({
    if(is.null(input$nnrtabla_rows_selected)){
      shinyjs::disable(id = "agregar")
      shinyjs::disable(id = "eliminar")
    } else{
      shinyjs::enable(id = "agregar")
      shinyjs::enable(id = "eliminar")
    }
  })
  observe({
    output$nnrtabla <- renderDT(bd$nnr %>% select(texto, medio, tipoNota,tipoMedio), selection = list(selected = 1))
  })
  
  observe({
    if(!is.null(input$nnrtabla_rows_selected)){
      rv$row2 <- input$nnrtabla_rows_selected
    }
  })
  
  observeEvent(input$agregar,{
    # sprintf("UPDATE pruebaSepMedia SET noticiaRelevante = 'sí' WHERE link ='%s'",
    #         rv$link2) %>%
    #   dbGetQuery(pool, .)
    sprintf("UPDATE %s SET noticiaRelevante = 'sí' WHERE link in ('%s')",
            pruebabd,
            paste(rv$link2,collapse = "', '")) %>%
      dbGetQuery(pool, .)
    bd$prueba %<>% bind_rows(bd$nnr %>% slice(rv$row2))
    if((nrow(bd$prueba)) >= 0) {
      shinyjs::show(id = "revision")
    }
    
    
    tareas$restantes <- tareas$restantes + length(rv$row2)
    auxtm <- bd$nnr %>% slice(rv$row2) %>% count(tipoMedio)
    
    bd$nnr %<>% slice(-rv$row2)
    
    if(str_detect(string = auxtm %>% pull(tipoMedio),pattern = "Televisión")) tareas$tv <- tareas$tv + (auxtm %>% filter(tipoMedio == "Televisión") %>% pull(n))
    if(str_detect(string = auxtm %>% pull(tipoMedio),pattern = "Radio")) tareas$rd <- tareas$rd + (auxtm %>% filter(tipoMedio == "Radio") %>% pull(n))
    if(str_detect(string = auxtm %>% pull(tipoMedio),pattern = "Radio|Televisión", negate = T)) tareas$pe <- tareas$pe + (auxtm %>% filter(!tipoMedio %in% c("Televisión","Radio")) %>% pull(n) %>% sum)
    if(nrow(bd$nnr) == 0){shinyjs::hide(id = "norelevantes")}
    rv$row2 <- 1
  })
  
  observeEvent(input$eliminar,{
    shinyalert(title = "Confirme",
               text = "¿Desea eliminar por completo estas noticias del análisis?",
               type = "warning",showCancelButton = T,cancelButtonText = "No",
               confirmButtonText = "Sí",
               showConfirmButton = T,
               callbackR = function(x){
                 if(x){
                   saveData(tabla = entrenamientobd, matriz = bd$nnr %>% slice(rv$row2) %>% 
                              mutate(fechaGuardar = now(tzone = "America/Mexico_City"), fechaInicio = !! fecha$inicio))
                   removeData(tabla = pruebabd, valor = bd$nnr$link[rv$row2])
                   bd$nnr %<>% slice(-rv$row2)
                   rv$row2 <- 1
                   if(nrow(bd$nnr) < max(rv$row2)){
                     shinyjs::hide(id = "norelevantes")
                   }
                   fecha$inicio <- now(tzone = "America/Mexico_City")
                 }
               })
  })
  
  # modal para poner id
  query_modal <- modalDialog(
    title = "Analista",
    tagList(alignCenter(selectInput("id", label = "Nombre",choices = psw$usuario)),
            alignCenter(passwordInput("psw", label = "Contraseña")),
            alignCenter(fileInput(inputId = "preprueba",
                                  label = "Subir notas nuevas",
                                  accept = ".xlsx", buttonLabel = "Buscar archivo",
                                  placeholder = "Seleccionar archivo xlsx"))
    ),
    easyClose = F,
    fade = T, size = "l",
    footer = alignCenter(actionButton("run", "Continuar"))
  )
  
  query_modal4 <- modalDialog(
    title = "Nuevo",
    tagList(alignCenter(textInput("nuevoHabla", label = "Nuevo expositor de la noticia"))
    ),
    easyClose = T,
    fade = T, size = "l",
    footer = alignCenter(actionButton("agregarHabla", "Continuar"))
  )
  
  observeEvent(input$addHabla,{
    showModal(query_modal4)
  })
  
  observeEvent(input$agregarHabla,{
    nu <- tibble(habla = input$nuevoHabla, analista = input$id, fecha = now(tzone = "America/Mexico_City"))
    saveData("opcionesSepMedia",nu)
    bd$opciones <- bd$opciones %>% bind_rows(nu %>% mutate(fecha = as.character(fecha)))
    updateSelectInput(session = session, inputId = "habla", choices = bd$opciones$habla %>% na.omit %>% sort,selected = input$nuevoHabla)
    removeModal()
  })
  
  query_modal5 <- modalDialog(
    title = "Nuevo",
    tagList(alignCenter(textInput("nuevoRefiere", label = "Nueva referencia de noticia"))
    ),
    easyClose = T,
    fade = T, size = "l",
    footer = alignCenter(actionButton("agregarRefiere", "Continuar"))
  )
  
  observeEvent(input$addRefiere,{
    showModal(query_modal5)
  })
  
  observeEvent(input$agregarRefiere,{
    nu <- tibble(refiere = input$nuevoRefiere, analista = input$id, fecha = now(tzone = "America/Mexico_City"))
    saveData("opcionesSepMedia",nu)
    bd$opciones <- bd$opciones %>% bind_rows(nu %>% mutate(fecha = as.character(fecha)))
    updateSelectInput(session = session, inputId = "refiere",choices = bd$opciones$refiere %>% na.omit %>% sort,
                      selected = input$nuevoRefiere)
    removeModal()
  })
  
  query_modal6 <- modalDialog(
    title = "Nuevo",
    tagList(alignCenter(textInput("nuevoSubtema", label = "Nuevo subtema"))
    ),
    easyClose = T,
    fade = T, size = "l",
    footer = alignCenter(actionButton("agregarSubtema", "Continuar"))
  )
  
  observeEvent(input$addSubtema,{
    showModal(query_modal6)
  })
  
  observeEvent(input$agregarSubtema,{
    nu <- tibble(subtema = input$nuevoSubtema, analista = input$id, fecha = now(tzone = "America/Mexico_City"))
    saveData("opcionesSepMedia",nu)
    bd$opciones <- bd$opciones %>% bind_rows(nu %>% mutate(fecha = as.character(fecha)))
    updateSelectInput(session = session, inputId = "subtema",choices = bd$opciones$subtema %>% na.omit %>% sort,
                      selected = input$nuevoSubtema)
    removeModal()
  })
  
  # enseñar modal
  showModal(query_modal)   
  
  observeEvent(input$run,{
    if((psw %>% filter(usuario == input$id) %>% pull(contraseña)) == input$psw){
      removeModal()
      
      show_waiter(
        tagList(
          img(src = "gppolls.png", height="15%", width="15%"),
          spin_cube_grid()
        ), color = "#F4AA90"
        
      )
      
      
      bd$prueba <- loadData(pruebabd)
      bd$opciones <- loadData(opcionesbd)
      
      updateSelectInput(session = session, inputId = "habla",choices = bd$opciones$habla %>% na.omit %>% sort %>% unique, selected = " ")
      updateSelectInput(session = session, inputId = "refiere",choices = bd$opciones$refiere %>% na.omit %>% sort %>% unique, selected =" ")
      updateSelectInput(session = session, inputId = "subtema",choices = bd$opciones$subtema %>% na.omit %>% sort %>% unique, selected = " ")
      
      updateSelectInput(session = session, inputId = "hablaEd",choices = bd$opciones$habla %>% na.omit %>% sort %>% unique, selected = " ")
      updateSelectInput(session = session, inputId = "refiereEd",choices = bd$opciones$refiere %>% na.omit %>% sort %>% unique, selected =" ")
      updateSelectInput(session = session, inputId = "subtemaEd",choices = bd$opciones$subtema %>% na.omit %>% sort %>% unique, selected = " ")
      
      if(!is.null(input$preprueba)){

        bd$entrenamiento <- loadData(entrenamientobd)
        bd$preprueba <- read_xlsx(input$preprueba$datapath) %>% mutate(Fecha = as.character(floor_date(x = Fecha,unit= "day"),format = "%d/%m/%y"))
        bd$preprueba %<>% preprocesamiento()
        bd$preprueba <- revisarRepetidos(entrenamiento = bd$entrenamiento, 
                                         prueba = bd$prueba, 
                                         preprueba = bd$preprueba)
        bd$repetidas <- bd$preprueba %>% filter(existencia != "nueva")
        bd$preprueba %<>% filter(existencia == "nueva") %>% select(-existencia) 
        if(nrow(bd$preprueba) > 0){
          mod_entrena <- entrenar_nr(bd$entrenamiento)
          bd$preprueba %<>% 
            predecir_nr(prueba = ., modelo = mod_entrena) %>% 
            mutate(noticiaRelevante = noticiaRelevante_p) %>% 
            repartir(valores =psw$usuario, estratos = "noticiaRelevante",
                     proporcion=c(.2,.2,.2,.2,.2))
          # tibble(fecha=NA_character_, analista=NA_character_, minFecha=NA_character_, maxFecha=NA_character_, total=NA_integer_) %>%
          #   dbWriteTable(pool,"subidasSepMedia",.,overwrite=T)
          dbReadTable(pool,"subidasSepMedia") %>% add_row(fecha = as.character(now(tzone = "America/Mexico_City")),
                                                          analista = input$id, 
                                                          minFecha = as.character(min(bd$preprueba$fecha)),
                                                          maxFecha = as.character(max(bd$preprueba$fecha)),
                                                          total = nrow(bd$preprueba)) %>% 
            dbWriteTable(pool,"subidasSepMedia",.,overwrite = T)
          bd$preprueba %>% dbWriteTable(pool, name = pruebabd, ., append = T)
          
          bd$prueba %<>% bind_rows(bd$preprueba %>% mutate(fecha = as.character(fecha),
                                                           tiraje = as.numeric(tiraje))) 
        }
      }
      
      bd$prueba %<>% filter(analista == input$id) %>% arrange(fecha)
      bd$nnr <- bd$prueba %>% filter(noticiaRelevante == "no") %>% arrange(fecha)
      
      bd$prueba %<>% filter(noticiaRelevante == "sí")
      bd$backup <- bd$prueba
      if(nrow(bd$prueba) != 0) shinyjs::show(id = "revision")
      if(nrow(bd$nnr) != 0) shinyjs::show(id = "norelevantes")
      tareas$restantes <- nrow(bd$prueba)
      conteoTipoMedio <- bd$prueba %>% 
        mutate(tipoMedio=case_when(tipoMedio %in% c("Internet","Periódico","Revista")~"Prensa escrita",
                                   T~tipoMedio)) %>% count(tipoMedio)
      tareas$pe <- conteoTipoMedio %>% filter(tipoMedio == "Prensa escrita") %>% pull(n)
      tareas$tv<- conteoTipoMedio %>% filter(tipoMedio == "Televisión") %>% pull(n)
      tareas$rd<- conteoTipoMedio %>% filter(tipoMedio == "Radio") %>% pull(n)
      hide_waiter()
      shinyjs::show(id = "pg")
      fecha$inicio <- now(tzone = "America/Mexico_City")
    } else{
      shinyalert(title = "Contraseña equivocada",
                 text = "Intente nuevamente",type = "error",
                 showConfirmButton = T,closeOnEsc = T,closeOnClickOutside = T)
    }
  })
  
  
  observe({
    req(bd$backup)
    bd$prueba <- bd$backup %>% filter(tipoMedio %in% input$filtro)
    if(nrow(bd$prueba) > 0){
      rv$row <- 1
      shinyjs::show(id = "revision")
    } else{
      shinyjs::hide(id = "revision")
    }
    
  })
  # modal para poner id
  query_modal2 <- modalDialog(
    title = "Repetidas",
    dataTableOutput("repetidas"),
    easyClose = T,
    footer = downloadButton("descargar",label = "Descargar repetidas"),
    fade = T, size = "l"
  )
  
  
  # enseñar modal
  observe({
    req(bd$repetidas)
    if(nrow(bd$repetidas)>0){
      output$repetidas <- renderDataTable({
        count(bd$repetidas,tipoMedio) %>% arrange(desc(n))
      })
      showModal(query_modal2) 
    }
  })
  
  observeEvent("descargar",
               output$descargar <- downloadHandler(
                 filename = function() {
                   paste("repetidas-", now(tzone = "America/Mexico_City"), ".csv", sep="")
                 },
                 content = function(file) {
                   write_excel_csv(bd$repetidas, file)
                 }
               )
  )
  
  observeEvent("descargaBase",
               output$descargaBase <- downloadHandler(
                 filename = function() {
                   paste("calificaciones_", input$fechas[1],"_a_",input$fechas[2], ".csv", sep="")
                 },
                 content = function(file) {
                   tbl(pool, entrenamientobd) %>% mutate(fecha = as.Date(fecha)) %>% 
                     filter(fecha >= !!input$fechas[1], fecha <= !!input$fechas[2]) %>%
                     filter(noticiaRelevante == "sí") %>%
                     select(-row_names,-noticiaRelevante_p, -fechaGuardar, -fechaInicio, -noticiaRelevante) %>% 
                     collect() %>% 
                     write_excel_csv(file)
                 }
               )
  )
  observe({
    output$datos <- renderUI({
      
      list_group(tagList(list_item("Por calificar", badge_value = tareas$restantes),
                         list_item("Prensa escrita", badge_value = tareas$pe),
                         list_item("Radio", badge_value = tareas$rd),
                         list_item("Televisión", badge_value = tareas$tv)
      ))
      
    })
  })
  
  query_modal3 <- modalDialog(
    title = "Noticia no pertinente",
    selectInput(inputId="razon",label = NULL, choices = c("El testigo no abre", "La nota no tiene que ver con la agenda SEP", "El medio no corresponde a la lista")),
    easyClose = T,
    footer = actionButton("remover",label = "Hacer cambio"),
    fade = T, size = "l"
  )
  
  observeEvent(input$siguiente,{
    showModal(query_modal3)
  })
  
  observeEvent(input$remover,{
    fecha$guardar <- now(tzone = "America/Mexico_City")
    rem <- bd$prueba %>% slice(rv$row) %>%
      mutate(noticiaRelevante  = "no", razon = input$razon, fechaInicio = !!as.character(fecha$inicio), fechaGuardar = !!as.character(fecha$guardar))
    
    saveData(tabla = entrenamientobd, matriz = rem)
    removeData(tabla = pruebabd, valor = rem$link)
    
    
    if(str_detect(string = rem %>% pull(tipoMedio),pattern = "Televisión")) tareas$tv <- tareas$tv - 1
    if(str_detect(string = rem %>% pull(tipoMedio),pattern = "Radio")) tareas$rd <- tareas$rd - 1
    if(str_detect(string = rem %>% pull(tipoMedio),pattern = "Radio|Televisión", negate = T)) tareas$pe <- tareas$pe - 1
    # tareas$saltadas <- tareas$saltadas + 1
    bd$prueba %<>% filter(id != rem$id)
    bd$backup %<>% filter(id != rem$id)
    tareas$restantes <- tareas$restantes - 1
    bd$opciones <- loadData(opcionesbd)
    removeModal()
    if(nrow(bd$prueba) == 0){shinyjs::hide(id = "revision")}
    fecha$inicio <- now(tzone = "America/Mexico_City")
  })
  
  observe({
    mandatoryFilled <- variables %>% grep(pattern = "observaciones",value = T,invert = T) %>% map_lgl(function(x) {
      !is.null(input[[x]]) && ((input[[x]] %>% str_squish()) != "") && input[[x]] != 0
    })
    
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "guardar", condition = mandatoryFilled)
    
  })
  
  observeEvent(input$guardar,{
    fecha$guardar <- now(tzone = "America/Mexico_City")
    valores <- variables %>% map(~input[[.x]]) %>% reduce(c)
    
    auu <- bd$prueba %>% slice(rv$row) %>% mutate(noticiaRelevante  = "sí", razon = NA, fechaInicio = !!as.character(fecha$inicio), fechaGuardar = !!as.character(fecha$guardar))
    auu[variables] <- valores
    saveData(tabla = entrenamientobd, matriz = auu)
    removeData(tabla = pruebabd, valor = auu$link)
    tareas$restantes <- tareas$restantes - 1
    if(str_detect(string = auu %>% pull(tipoMedio),pattern = "Televisión")) tareas$tv <- tareas$tv - 1
    if(str_detect(string = auu %>% pull(tipoMedio),pattern = "Radio")) tareas$rd <- tareas$rd - 1
    if(str_detect(string = auu %>% pull(tipoMedio),pattern = "Radio|Televisión", negate = T)) tareas$pe <- tareas$pe - 1
    
    bd$prueba %<>% filter(id != auu$id)
    bd$backup %<>% filter(id != auu$id)
    shinyjs::reset(id = "revision")
    updateRadioGroupButtons(session = session, inputId = "calificacion", selected = "")
    updateRadioGroupButtons(session = session, inputId = "postura", selected = "")
    updateRadioGroupButtons(session = session, inputId = "importancia", selected = "")
    if(nrow(bd$prueba) == 0){shinyjs::hide(id = "revision")}
    fecha$inicio <- now(tzone = "America/Mexico_City")
  })
  
  edicion <- reactiveValues(bd = NULL)
  
  observe({
    output$notasCalificadas <- renderDT({
      edicion$bd <- tbl(pool, entrenamientobd) %>% 
        # mutate(fechaGuardar = as.Date(fechaGuardar)) %>%
        filter(fechaGuardar >= !!input$fechasVis[1]) %>% 
        filter(fechaGuardar < !!(input$fechasVis[2]+1)) %>% 
        filter(noticiaRelevante == "sí", analista == !!input$id) %>%
        collect()
      if(!is.null(edicion$bd) | nrow(edicion$bd) > 0){
       edicion$bd %>% arrange(desc(fechaGuardar)) %>% select(`Fecha calificada` = fechaGuardar, Título = texto, Medio = medio)
      }

    }, selection = list(mode="single",selected=1))
  })
  
  observe({
    req(edicion$bd)
    if(!is.null(input$notasCalificadas_rows_selected)){
      updateRadioGroupButtons(session = session, inputId = "calificacionEd",
                              selected = edicion$bd %>% 
                                slice(as.numeric(input$notasCalificadas_rows_selected)) %>% 
                                pull(calificacion))
      updateRadioGroupButtons(session = session, inputId = "posturaEd", 
                              selected = edicion$bd %>% 
                                slice(as.numeric(input$notasCalificadas_rows_selected)) %>% 
                                pull(postura))
      updatePickerInput(session = session,inputId = "hablaEd",
                        selected = edicion$bd %>% 
                          slice(as.numeric(input$notasCalificadas_rows_selected)) %>% 
                          pull(habla))
      updatePickerInput(session = session,inputId = "refiereEd",
                        selected = edicion$bd %>% 
                          slice(as.numeric(input$notasCalificadas_rows_selected)) %>% 
                          pull(refiere))
      updateRadioGroupButtons(session = session, inputId = "importanciaEd", 
                              selected =edicion$bd %>% 
                                slice(as.numeric(input$notasCalificadas_rows_selected)) %>% 
                                pull(importancia))
      # 
      updateTextAreaInput(session = session, inputId = "mencionEd",
                          value = edicion$bd %>% 
                            slice(as.numeric(input$notasCalificadas_rows_selected)) %>% 
                            pull(mencion))
      updatePickerInput(session = session,inputId = "subtemaEd",
                        selected = edicion$bd %>% 
                          slice(as.numeric(input$notasCalificadas_rows_selected)) %>% 
                          pull(subtema))
      updateTextAreaInput(session = session, inputId = "observacionesEd",
                          value = edicion$bd %>% 
                            slice(as.numeric(input$notasCalificadas_rows_selected)) %>% 
                            pull(observaciones))
    }})
  
  observe({
    mandatoryFilled2 <- variables %>% grep(pattern = "observaciones",value = T,invert = T) %>% 
      paste0("Ed") %>% 
      map_lgl(function(x) {
      !is.null(input[[x]]) && ((input[[x]] %>% str_squish()) != "") && input[[x]] != 0
    })
    
    mandatoryFilled2 <- all(mandatoryFilled2)
    
    shinyjs::toggleState(id = "editar", condition = mandatoryFilled2)
    
  })
})
