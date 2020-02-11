preprocesamiento <- function(tb, v_calificacion=c("calificacion","postura","habla", "refiere","importancia","mencion", "subtema", 
                                                  "observaciones")){
  # Aplicar reglas fijas
  tb %<>% filter(Estado =="CDMX")
                 # , !TipoMedio %in% c("Televisión", "Radio"))
  # Seleccionar variables
  tb %<>% select(medio=Medio,seccion=Sección,autor=Autor,tipoNota=TipoNota,
                 tipoMedio=TipoMedio,tamano=Tamaño, 
                 costo=Costo, tiraje=TirajeRating, 
                 fecha=Fecha, id=IDNOTA, link=Link, texto=Titulo)
  # Corregir variables
  # tamaño
  tb$tamano[is.na(tb$tamano)] <- "N/D"
  # fecha, id
  tb %<>% mutate(fecha=lubridate::dmy(fecha),
                 link=gsub(pattern = "http", replacement = "https",x = link),
                 id=gsub("*&temaid=10003098", "",link) %>% gsub(".*=","",.)) %>% 
    mutate(link=gsub(pattern = "httpss", replacement = "https",x = link)) %>% 
    distinct(id, .keep_all = T)
  col_extra <-map(.x=v_calificacion ,.f = ~{
    bd <- tibble(x=rep(NA_character_, nrow(tb)))
    names(bd) <- .x    
    return(bd)
  }) %>% reduce(bind_cols)
  tb %<>% bind_cols(col_extra)
  # extraer texto
  # tb %<>% discursera_media()
   
  return(tb)
} 
entrenar <- function(tb){
  # Re-escalar y transformar variables continuas
  aux<- predict(tb %>% preProcess(method=c("center", "scale", "YeoJohnson")),newdata = tb)
  # Quitar variables continuas con varianza cero o cuasi-cero
  aux <- aux[,-nearZeroVar(aux)]
  # Frecuencia de palabras por id # Filtrar?
  conteo <- discursera_frec(aux, grupos = c("id","Palabra"), unnested = F,
                            token="Palabra", sw=sw, top=150)  
  # Realizar
  matriz<- conteo %>% bind_tf_idf(term = Palabra, document = id,n = n) %>% 
    cast_dtm(id, term = Palabra, value = tf_idf) 
  ids_orden <- dimnames(matriz)[[1]]
  matriz  %<>% as.matrix() %>% as_tibble() %>% mutate(id=ids_orden)
  aux3 <- inner_join(matriz, aux2 %>% select(Medio,Sección,Autor,TipoNota, TipoMedio, Tamaño, Costo, TirajeRating,
                                             `Calificación de la información`,id), by="id")
  
  p3 <- predict(aux3 %>% preProcess(method=c("center", "scale", "YeoJohnson")),newdata = aux3)
  p3$Tamaño[is.na(p3$Tamaño)] <- "N/D"
  # p3 <- predict(dummyVars(`Calificación de la información`~., data = p3), p3)%>% as.tibble()
  nzv3 <- nearZeroVar(p3)
  p3 <- p3[,-nzv3]
  p3 <- predict(dummyVars(`Calificación de la información`~., data = p3), p3)%>% as_tibble()
  nzv3 <- nearZeroVar(p3)
  p3 <- p3[,-nzv3]
  p3 %<>% mutate(cali=aux3$`Calificación de la información`)
}

entrenar_nr <- function(entrenamiento, pclave=100){
  t1 <- lubridate::now()
  # Seleccionar las variables
  aux1 <- entrenamiento %>% select(medio, 
                                   autor, tamano,
                                   tipoNota, tipoMedio, 
                        noticiaRelevante,fecha,id,link, texto)
  clave <- discursera_clave(aux1, grupos = "noticiaRelevante", unnested = F,
                            sw=sw, top = pclave) %>% pull(Palabra) %>% unique()
  conteo <- discursera_frec(aux1, grupos = c("id","Palabra"), unnested = F, token="Palabra",
                            sw=sw) %>% filter(Palabra %in% clave) 
  # Un mínimo de p
  matriz<- conteo %>% bind_tf_idf(term = Palabra, document = id,n = n) %>% 
    cast_dtm(id, term = Palabra, value = n) 
  notass <- dimnames(matriz)[[1]]
  matriz  %<>% as.matrix() %>% as_tibble()
  matriz %<>% mutate(id=notass)
  aux1 <- left_join(aux1 %>% select(-texto) %>% replace(is.na(.), ""), matriz, by="id") %>%
    replace(is.na(.), 0)
  # Convertir todos los character a factores
  aux1 %<>% mutate_if(is.character, as.factor)
  # Centrar y transformar variables continuas
  # p <- predict(aux1 %>% preProcess(method=c("center", "scale", "YeoJohnson")),
  #              newdata = aux1 %>% select(-fecha, -link, -id))
  p <-  aux1 %>% select(-fecha, -link, -id)
  # Eliminar variables cuasi-cero
  # nzv <- nearZeroVar(p)
  # if(length(nzv)>0) p <- p[,-nzv]
  # Variables categóricas
  p <- predict(dummyVars(noticiaRelevante~., data = p), newdata = p)%>% as_tibble() 
  # Eliminar variables cuasi-cero
  nzv <- nearZeroVar(p) %>% setdiff(which(names(p) %in% clave))
  if(length(nzv)>0) p <- p[,-nzv]
  # Declarar tipo de entrenamiento
  # fitControl <- trainControl(## 10-fold CV
  #   method = "repeatedcv",
  #   number = 10,
  #   ## repeated ten times
  #   repeats = 3,
  #   classProbs = TRUE)
  set.seed(825)
  # Entrenar con RF
  p %<>% mutate(y=as.factor(aux1$noticiaRelevante), 
                fecha=aux1$fecha, 
                id=aux1$id,
                link=aux1$link)
  # return(p)
  rf_mod <- train(x = p %>% select(-y, -fecha,-id,-fecha,-link) ,
                  y = p$y,
                  method = "ranger",
                  trControl=trainControl(method = "none", classProbs = T),
                  importance = "impurity",
                  tuneGrid = expand.grid(mtry=round(sqrt(ncol(p %>% select(-y, -fecha,-id,-link)))),
                                         splitrule="gini", min.node.size=1))
                  # trControl = fitControl)
  t2 <- lubridate::now()
  return(list(modelo=rf_mod, clave=clave, t=t2-t1))
}

entrenar_nrP <- function(entrenamiento, pclave=1, factor=1){
  t1 <- lubridate::now()
  # Seleccionar las variables
  aux1 <- entrenamiento %>% select(medio, autor, tipoNota, tipoMedio, tamano,
                                   noticiaRelevante,fecha,id,link, texto)
  clave <- discursera_clave(aux1, grupos = "noticiaRelevante", unnested = F,
                            sw=sw, top = pclave) %>% pull(Palabra) %>% unique()
  conteo <- discursera_frec(aux1, grupos = c("id","Palabra"), unnested = F, token="Palabra",
                            sw=sw) %>% filter(Palabra %in% clave) 
  # Un mínimo de p
  matriz<- conteo %>% bind_tf_idf(term = Palabra, document = id,n = n) %>% 
    cast_dtm(id, term = Palabra, value = n) 
  notass <- dimnames(matriz)[[1]]
  matriz  %<>% as.matrix() %>% as_tibble()
  matriz %<>% mutate(id=notass)
  aux1 <- left_join(aux1 %>% select(-texto) %>% replace(is.na(.), ""), matriz, by="id") %>%
    replace(is.na(.), 0)
  # Convertir todos los character a factores
  aux1 %<>% mutate_if(is.character, as.factor)
  # Centrar y transformar variables continuas
  # p <- predict(aux1 %>% preProcess(method=c("center", "scale", "YeoJohnson")),
  #              newdata = aux1 %>% select(-fecha, -link, -id))
  p <-  aux1 %>% select(-fecha, -link, -id)
  # Eliminar variables cuasi-cero
  # nzv <- nearZeroVar(p)
  # if(length(nzv)>0) p <- p[,-nzv]
  # Variables categóricas
  p <- predict(dummyVars(noticiaRelevante~., data = p), newdata = p)%>% as_tibble() 
  # Eliminar variables cuasi-cero
  nzv <- nearZeroVar(p) %>% setdiff(which(names(p) %in% clave))
  if(length(nzv)>0) p <- p[,-nzv]
  # Declarar tipo de entrenamiento
  # fitControl <- trainControl(## 10-fold CV
  #   method = "repeatedcv",
  #   number = 10,
  #   ## repeated ten times
  #   repeats = 3,
  #   classProbs = TRUE)
  set.seed(825)
  # Entrenar con RF
  p %<>% mutate(y=as.factor(aux1$noticiaRelevante), 
                fecha=aux1$fecha, 
                id=aux1$id,
                link=aux1$link)
  # return(p)
  rf_mod <- train(x = p %>% select(-y, -fecha,-id,-fecha,-link) ,
                  y = p$y,
                  method = "svmRadialCost",
                  trControl = fitControl)
  t2 <- lubridate::now()
  return(list(modelo=rf_mod, clave=clave, t=t2-t1))
}

predecir_nr <- function(prueba, modelo, umbral=.2){
  # Predice noticiaRelevante
  # Seleccionar las variables
  aux1 <- prueba %>% select(medio, seccion, autor, tipoNota, tipoMedio, tamano,
                        # costo, tiraje,
                        fecha,id,link, texto)
  conteo <- discursera_frec(aux1, grupos = c("id","Palabra"), unnested = F, token="Palabra",
                            sw=sw) %>% filter(Palabra %in% modelo$clave) 
  
  # Matriz de frecuencia por documento
  matriz<- conteo %>% bind_tf_idf(term = Palabra, document = id,n = n) %>% 
    cast_dtm(id, term = Palabra, value = n) 
  notass <- dimnames(matriz)[[1]]
  matriz  %<>% as.matrix() %>% as_tibble()
  matriz %<>% mutate(id=notass)
  aux1 <- left_join(aux1 %>% select(-texto) %>% replace(is.na(.), ""), matriz, by="id") %>%
    replace(is.na(.), 0)
  # Convertir todos los caracteres a factores
  aux1 %<>% mutate_if(is.character, as.factor)
  # Seleccionar variables
  p <-  aux1 %>% select(-fecha, -link, -id)
  # Variables categóricas
  p <- predict(dummyVars(~., data = p), newdata = p)%>% as_tibble() 
  # Eliminar variables cuasi-cero, excepto palabras clave
  variables <- predictors(modelo$modelo) %>% union(modelo$clave)
  p <- p[,variables %>% intersect(names(p))]
  col_extra <-map(.x=variables %>% setdiff(names(p)) ,.f = ~{
    bd <- tibble(x=rep(0, nrow(p)))
    names(bd) <- .x    
    return(bd)
  }) %>% reduce(bind_cols)
  p %<>% bind_cols(.,col_extra)
  # Entrenar con RF
  p %<>% mutate(fecha=aux1$fecha, 
                id=aux1$id,
                link=aux1$link)
  prueba %<>% mutate(ppertencia=predict(modelo$modelo, p,"prob")[[2]], 
                    noticiaRelevante_p=if_else(condition = ppertencia>=umbral, true = "sí", false = "no"),
                    noticiaRelevante=noticiaRelevante_p,
                    umbral) %>% select(-ppertencia,-umbral)
  return(prueba)
  
  # Hace trampa en calificacion, postura, habla, refiere, importancia, mencion, subtema,
  # observaciones
  
  # regresa base de datos con calificaciones
  
}

discursera_media <- function(tb, hipervinculo="link"){
  hipervinculo <- tb %>% pull(!! rlang::parse_expr(hipervinculo))
  lectura <- function(hipervinculo){
    pagina <- readLines(hipervinculo, encoding = "CP1252")
    inicio <- pagina %>% grep(pattern = 'id=\"notaTexto\"')
    fin <- pagina %>% grep(pattern = "<hr>")
    fin <- fin[(fin > inicio)] %>% min()
    pagina[inicio:fin] %>% paste(collapse = "") %>%  
      gsub(pattern='<span id=\"notaTexto\">|<p>|</p>|</span>|<span>|<hr>', replacement="")
    
  }
  lectura %<>% possibly(otherwise=NA)
  tb %<>% mutate(texto=map_chr(.x=hipervinculo, .f = ~lectura(.x)))
}

discursera_frec <- function(tb, relativo=F, sw=NULL, grupos="", unnested=T, token=NULL,
                            filtrar_base="", filtrar_resultado="", ordenar="", top=0){
  if(filtrar_base != "") tb %<>% filter(!! rlang::parse_expr(filtrar_base)) 
  if(! unnested){
    token_ingles<-switch (token,
                          "Palabra" = "words", "Enunciado" = "sentences"
    )
    tb %<>% unnest_tokens_(output = "Palabra", input = "texto", token = token_ingles)
  }
  if(! sw %>% is.null()) tb %<>% anti_join(sw)
  tb %<>% group_by(!!! rlang::parse_exprs(grupos))
  tb %<>% summarise(n=n())
  if(relativo) tb %<>% mutate(n=round(100*n/sum(n),2))
  if(filtrar_resultado != "") tb %<>% filter(!! rlang::parse_expr(filtrar_resultado)) 
  if(ordenar != "") tb %<>% arrange(!!! rlang::parse_exprs(ordenar)) 
  if(top > 0) tb %<>% top_n(n=top, wt=n) 
  return(tb)
}

discursera_clave <-function(tb, sw=NULL, grupos="", unnested=T, token=NULL, filtrar_base="",
                            filtrar_resultado="", top=0, ordenar=""){
  if(filtrar_base != "") tb %<>% filter(!! rlang::parse_expr(filtrar_base)) 
  if(unnested==F){
    tb %<>% unnest_tokens_(output = "Palabra", input = "texto", token = "words")
  }
  if(! sw %>% is.null()) tb %<>% anti_join(sw) %>% filter(!str_detect(Palabra, "[0-9]"))
  aux1 <- tb %>% group_by(!!! rlang::parse_exprs(grupos)) %>% group_by(Palabra, add = T) %>%
    summarise(o=n()) 
  aux2 <- tb %>%  group_by(!!! rlang::parse_exprs(grupos)) %>% summarise(n=n())
  # minimo <- aux2 %>% filter(n>10) %>% pull(Palabra)
  aux3 <-tb %>% group_by(Palabra) %>% summarise(e=n()) %>% mutate(e=e/sum(e))
  tb <- left_join(aux1, aux2) %>% left_join(aux3) %>%
    mutate(e=e*n) %>% select(-n) %>% mutate(G2=-2*o*log(e/o)) %>%  
    # filter(Palabra %in% minimo) %>% 
    mutate(Rango=rank(-G2,ties.method = "min"))
  if( top > 0) tb %<>% top_n(n=top, wt = G2)
  if(ordenar != "") tb %<>% arrange(!!! rlang::parse_exprs(ordenar))   
  if(filtrar_resultado != "") tb %<>% filter(!! rlang::parse_expr((filtrar_resultado))) 
  return(tb)
}
