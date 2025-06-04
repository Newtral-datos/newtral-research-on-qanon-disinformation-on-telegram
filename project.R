# CARGAR LIBRERÍAS ----------

library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(writexl)

# CARGAR LOS DATOS ----------

  ## Esta es la carpeta donde almacenamos todos los .xlsx de cada cuenta de Telegram.

ruta_carpeta <- "TU_RUTA_AQUI/raw_data"

archivos <- list.files(path = ruta_carpeta, pattern = "\\.xlsx$", full.names = TRUE)

datos_brutos <- lapply(archivos, read_excel) %>%
  bind_rows()

  ## Damos formato fecha a la fecha.

datos_brutos$fecha_formateada <- format(datos_brutos$fecha, "%d-%m-%Y")
datos_brutos$año <- format(datos_brutos$fecha, "%Y")

# LIMPIEZA DE DATOS ----------

fecha_limite <- as.Date("2017-10-01") ## Limitamos los datos a la fecha en la que comenzó Qanon.

datos <- datos_brutos %>%
  filter(!is.na(texto),
         fecha > fecha_limite)

# FILTRADO DE DATOS ----------

palabras <- c("q(u?)an[o0]n", "17anon", "qdrop(s?)", "\\bcabal\\b", "wwg1wga", "pizzagate", "pizzagater(s?)", "pedogate") ## Términos relacionados con Qanon que vamos a buscar.

patron <- paste(palabras, collapse = "|")  ## Patrón para filtrar las filas que contienen al menos un término.

terminos_qanon <- datos %>%
  mutate(
    texto2 = str_to_lower(texto),
    qanon = ifelse(str_detect(texto2, "q(u?)an[o0]n"), 1, 0),
    non17 = ifelse(str_detect(texto2, "17anon"), 1, 0),
    qdrops = ifelse(str_detect(texto2, "qdrop(s?)"), 1, 0),
    cabal = ifelse(str_detect(texto2, "\\bcabal\\b"), 1, 0),
    wwg1wga = ifelse(str_detect(texto2, "wwg1wga"), 1, 0),
    pizzagate = ifelse(str_detect(texto2, "pizzagate|pizzagater(s?)|pedogate"), 1, 0)
  ) %>%
  filter(grepl(patron, texto2, ignore.case = TRUE)) %>%
  rowwise() %>%
  mutate(qanon_general = if_any(c(qanon, non17, qdrops, cabal, wwg1wga, pizzagate), ~ . == 1) %>%
           as.numeric()) %>%
  ungroup()

# LIMPIEZA DE NUEVOS DATOS ----------

terminos_qanon$fecha_formateada <- as.Date(terminos_qanon$fecha_formateada, format = "%d-%m-%Y")
terminos_qanon <- terminos_qanon %>% select(-id, -texto2, -id_emisor, -fecha)

# GRÁFICO EVOLUCIÓN DE MENSAJES TOTALES ----------

evolucion_mensajes_totales <- datos %>%
  group_by(fecha_formateada) %>%
  summarise(conteo = n())

evolucion_mensajes_totales$fecha_formateada <- as.Date(evolucion_mensajes_totales$fecha_formateada,
                                     format = "%d-%m-%Y")

evolucion <- ggplot(evolucion_mensajes_totales, aes(x = fecha_formateada,
                                  y = conteo)) +
  geom_point(color = "#01f3b3") +
  theme_minimal() +
  labs(x = "Fecha",
       y = "Publicaciones") +
  scale_x_date(date_labels = "%d-%m-%Y",
               date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

ggplotly(evolucion)

# EVOLUCIÓN MENSAJES PONDERADOS ----------

mensajes_dia <- datos %>%
  group_by(fecha_formateada) %>%
  summarise(conteo = n())

canales_dia <- datos %>%
  group_by(fecha_formateada) %>%
  distinct(canal) %>%
  summarise(canales = n())

mensajes_ponderados <- left_join(mensajes_dia, canales_dia, by = "fecha_formateada") %>%
  mutate(ratio_mensajes = conteo / canales,
         fecha = as.Date(fecha_formateada, format = "%d-%m-%Y"))

evolucion_mensajes_ponderados <- ggplot(mensajes_ponderados, mapping = aes(x = fecha, y = ratio_mensajes)) +
  geom_point(color = "#01f3b3") +
  theme_minimal() +
  labs(x = "Fecha",
       y = "Publicaciones") +
  scale_x_date(date_labels = "%d-%m-%Y",
               date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

ggplotly(evolucion_mensajes_ponderados)

# EVOLUCIÓN DEL NÚMERO DE CUENTAS ACTIVAS ----------

datos$fecha_formateada <- as.Date(datos$fecha, format = "%d-%m-%Y")
datos <- datos %>% arrange(fecha_formateada)

unico <- datos %>%
  group_by(canal) %>%
  summarise(fecha = min(fecha_formateada), .groups = "drop")

evolucion_cuentas <- unico %>%
  group_by(fecha) %>%
  summarise(nuevos = n(), .groups = "drop") %>%
  arrange(fecha) %>%
  mutate(acumulado = cumsum(nuevos))

  ## Gráfico de la evolución del número de cuentas.

evolucion_cuentas <- ggplot(evolucion_cuentas, aes(x = fecha, y = acumulado)) +
  geom_line(color = "#01f3b3") +
  theme_minimal() +
  labs(x = "Fecha", y = "Canales acumulados") +
  scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(evolucion_cuentas)

# EVOLUCIÓN TEMPORAL TÉRMINOS DE QANON ----------

terminos_qanon$mes_año <- format(terminos_qanon$fecha_formateada, "%Y-%m")

evolucion_terminos_qanon <- terminos_qanon %>%
  group_by(mes_año) %>%
  summarise(
    Qanon = sum(qanon),
    non_17 = sum(non17),
    Qdrops = sum(qdrops),
    Cabal = sum(cabal),
    WWG1WGA = sum(wwg1wga),
    Pizzagate = sum(pizzagate)
  )

# CANALES CON MAYOR IMPACTO EN SUS PUBLICACIONES (POPULARIDAD) ----------

  ## En número de seguidores.

seguidores <- terminos_qanon %>%
  distinct(canal, followers)

  ## Información sobre los canales.

info_canales <- datos %>%
  group_by(canal) %>%
  summarise(
    visualizaciones = sum(vistas),
    reenvios = sum(reenvios),
    publicaciones = n())

  ## Mensajes sobre QAnon

mencionQ <- terminos_qanon %>%
  group_by(canal) %>%
  summarise(mencionQ = sum(qanon_general))

  ## Total de publicaciones en la base de datos.

n_publicaciones <- datos %>%
  summarise(n_publicaciones = n()) %>%
  pull(n_publicaciones)

  ## Unir los df.

canales_impacto <- left_join(seguidores, info_canales, by = "canal") %>%
  left_join(mencionQ, by = "canal") %>%
  mutate(visu_publicacion = visualizaciones / publicaciones,
         reenvi_publicacion = reenvios / publicaciones,
         perc_publicaciones = (publicaciones / n_publicaciones) %>%
           round(4) * 100,
         canal = str_replace(canal, "^@", ""),
         mencionesQ_por_mensaje = mencionQ / n_publicaciones) %>%
  filter(mencionQ > 0)

# COMPARACIÓN SEIS MESES ANTERIORES Y POSTERIORES DE LA VICTORIA DE TRUMP ----------

  ## Antes de las elecciones.

fecha_inicio <- as.Date("2024-05-05")
fecha_fin <- as.Date("2024-11-05")

antes_trump <- terminos_qanon %>%
  filter(between(fecha_formateada, fecha_inicio, fecha_fin)) %>%
  summarise(
    Qanon = sum(qanon),
    "17non" = sum(non17),
    Qdrops = sum(qdrops),
    Cabal = sum(cabal),
    WWG1WGA = sum(wwg1wga),
    Pizzagate = sum(pizzagate)
  ) %>%
  mutate(periodo = "antes")

  ## Después de las elecciones.

fecha_inicio2 <- as.Date("2024-11-06")
fecha_fin2 <- as.Date("2025-5-06")

despues_trump <- terminos_qanon %>%
  filter(between(fecha_formateada, fecha_inicio2, fecha_fin2)) %>%
  summarise(
    Qanon = sum(qanon),
    "17non" = sum(non17),
    Qdrops = sum(qdrops),
    Cabal = sum(cabal),
    WWG1WGA = sum(wwg1wga),
    Pizzagate = sum(pizzagate)
  ) %>%
  mutate(periodo = "después")

  ## Unión de las dos.

comparacion_trump <- bind_rows(antes_trump, despues_trump)

  ## Crear gráfico.

comparacion_trump_largo <- pivot_longer(comparacion_trump, 
                         cols = -periodo, 
                         names_to = "Termino", 
                         values_to = "Frecuencia")

grafico_comparacion_trump <- ggplot(comparacion_trump_largo, aes(x = Termino, y = Frecuencia, fill = periodo)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("antes" = "#01f3b3", "después" = "#aaaaaa")) +
  theme_minimal() +
  labs(
    title = "Frecuencia de términos antes y después",
    x = NULL,
    y = NULL,
    fill = "Periodo"
  )

ggplotly(grafico_comparacion_trump)

# EXPORTAR DATAFRAMES A EXCEL ----------

#write_xlsx(mensajes_ponderados, "TU_RUTA_AQUI/evolucion_canales_qanon.xlsx")
#write_xlsx(evolucion_terminos_qanon, "TU_RUTA_AQUI/evolucion_terminos_qanon.xlsx")
#write_xlsx(canales_impacto, "TU_RUTA_AQUI/popularidad_canales_qanon.xlsx")
#write_xlsx(evolucion_cuentas, "TU_RUTA_AQUI/evolucion_numero_canales.xlsx")
#write_xlsx(comparacion_trump, "TU_RUTA_AQUI/comparacion_trump.xlsx")
#write_xlsx(terminos_qanon, "TU_RUTA_AQUI/terminos_qanon.xlsx")
#write.csv(datos_brutos, file = "TU_RUTA_AQUI/datos_brutos.csv", row.names = FALSE)
#write.csv(datos, file = "TU_RUTA_AQUI/datos_limpios.csv", row.names = FALSE)







