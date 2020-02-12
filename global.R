library(magrittr)
library(tidyverse)
library(shiny)
library(shinyBS)
library(shinyLP)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(DBI)
library(pool)
library(waiter)
library(DT)
library(slickR)
library(tidytext)
library(caret)
library(lubridate)
library(tm)
library(e1071)
library(ranger)
library(readxl)

# Globales -----------------------------------------------------------------------

variables <- c("calificacion", "postura", "habla", "refiere", "importancia", "mencion", "subtema",
               "observaciones","prioridades","origen")

appCSS <- ".mandatory_star { color: red; }"

# Java + CSS --------------------------------------------------------------------

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

jscode <- '
$(document).keyup(function(event) {
if ((event.keyCode == 13)) {
$("#run").click();
}
});
'

# Queries -----------------------------------------------------------------

loadData <- function(nombre) {
  data <- dbReadTable(pool, nombre)
}

saveData <- function(tabla, matriz) {
  # sprintf(
  #   "INSERT INTO %s (%s) VALUES ('%s')",
  #   tabla, 
  #   paste(variables, collapse = ", "),
  #   paste(valores, collapse = "', '")
  # ) %>% pool::dbGetQuery(pool, .)
  dbWriteTable(pool, tabla, matriz,
               append = T)
}

removeData <- function(tabla, valor) {
  sprintf(
    "DELETE FROM %s WHERE link in ('%s')",
    tabla, 
    paste(valor,collapse = "', '")
  ) %>% pool::dbGetQuery(pool, statement = .)
}


# Pool --------------------------------------------------------------------


pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = "db_gp",
  host = "mysql.c21k45zgpk1y.us-east-2.rds.amazonaws.com",
  username = "root",
  password = "9Blw33caY",
  port = 3306
)

onStop(function() {
  poolClose(pool)
})

# BDs ---------------------------------------------------------------------
opcionesbd <- "opcionesII"
pruebabd <- "pruebaII"
entrenamientobd <- "entrenamientoII"
usuariosbd <- "usuariosII"

psw <- dbReadTable(pool, usuariosbd)
sw <- read_csv("stopwords_corta.csv")
# Funciones ---------------------------------------------------------------
# setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Productos/Media/Calificador/")
source("funciones.R")
revisarRepetidos <- function(entrenamiento, prueba, preprueba){
  preprueba %>% mutate(existencia = case_when((link %in% entrenamiento$link) | (link %in% prueba$link)~"repetida",
                                          T~"nueva"
                                          ))
}

repartir <- function(bd, valores, estratos=NULL, proporcion){
  bd %>% mutate(tipoMedio2 = case_when(tipoMedio %in% c("Internet","Periódico","Revista")~"Prensa escrita",
                                       T~tipoMedio)) %>% 
    group_by(!!! rlang::parse_exprs(c("tipoMedio2",estratos))) %>% 
    mutate(analista = sample(valores,size = n(), replace = T, prob = proporcion)) %>% ungroup %>% 
    select(-tipoMedio2)
}

# bd %>% mutate(filtrada = sample(c(T,F),size = n(),replace = T,prob = c(.9,.1))) %>% 
#   repartir(valores = c("yo","tu"), estratos = "filtrada", proporcion = c(.9,.1)) %>%
#   count(filtrada, tipoMedio2,analista)
