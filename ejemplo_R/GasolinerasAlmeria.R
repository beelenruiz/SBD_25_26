# 0. CARGA DE LIBRERÍAS
library(jsonlite)
library(tidyverse) # Incluye dplyr, stringr, etc.
library(sf)
library(mapSpain)
library(leaflet)
library(stringi)   # Para limpiar acentos en los nombres

# 1. DESCARGA DE DATOS (API Ministerio)
url <- "https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/"
datos_raw <- fromJSON(url)
lista_gasolineras <- datos_raw$ListaEESSPrecio

# 1B. FUNCIÓN PARA CALCULAR PRECIO MÍNIMO
calcular_precio_minimo <- function(df, cadena) {
  
  # Creamos un nombre automático para la columna resultado (ej. "Precio_Min_95")
  nombre_col <- paste0("Precio_Min_", gsub(" ", "_", cadena))
  
  df_resultado <- df %>%
    # Limpiar y convertir a numérico las columnas que contienen la cadena
    mutate(across(
      contains(cadena),
      ~ as.numeric(gsub(",", ".", na_if(., "")))
    )) %>%
    
    # Calcular el mínimo por fila
    rowwise() %>%
    mutate(
      !!sym(nombre_col) := min(c_across(contains(cadena)), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    
    # Corregir los casos de Infinito a NA
    mutate(
      !!sym(nombre_col) := ifelse(is.infinite(!!sym(nombre_col)), NA, !!sym(nombre_col))
    )
  
  return(df_resultado)
}

# Aplicamos la función para Gasolina 95
tipo <- "95"
lista_gasolineras <- calcular_precio_minimo(lista_gasolineras, tipo)

# 2. PROCESADO DE DATOS POR MUNICIPIO
gasolineras_municipio <- lista_gasolineras %>%
  as_tibble() %>%
  mutate(
    # La columna ya es numérica gracias a nuestra función
    Precio = Precio_Min_95, 
    
    # NORMALIZACIÓN: Quitamos acentos, pasamos a mayúsculas y quitamos espacios extra
    Municipio_Join = toupper(stri_trans_general(Municipio, "Latin-ASCII")) %>% trimws()
  ) %>%
  filter(!is.na(Precio)) %>%
  group_by(Municipio_Join) %>%
  summarise(
    PrecioMedio = mean(Precio, na.rm = TRUE),
    NumGasolineras = n(),
    .groups = 'drop'
  )

# 3. OBTENCIÓN DE GEOMETRÍAS (SOLO ALMERÍA)
# Usamos region = "Almeria" para no sobrecargar el mapa con toda España
municipios_sf <- esp_get_munic(region = "Almeria") %>%
  st_transform(4326) %>% 
  mutate(
    Municipio_Join = toupper(stri_trans_general(name, "Latin-ASCII")) %>% trimws()
  )

# 4. UNIÓN (JOIN) POR NOMBRE NORMALIZADO
mapa_municipios <- municipios_sf %>%
  left_join(gasolineras_municipio, by = "Municipio_Join")

# 5. MAPA INTERACTIVO
# Usamos una paleta "Quantile" para que los colores se repartan mejor 
paleta <- colorBin(palette = "YlOrRd", domain = mapa_municipios$PrecioMedio, bins = 7)

leaflet(mapa_municipios) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -2.46, lat = 36.84, zoom = 9) %>% # Centrado en Almería por defecto
  addPolygons(
    fillColor = ~paleta(PrecioMedio),
    color = "white",
    weight = 0.5, # Línea más fina porque hay muchos municipios
    fillOpacity = 0.7,
    label = ~paste0(name, ": ", round(PrecioMedio, 3), " €/L (", NumGasolineras, " gasolineras)"),
    highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)
  ) %>%
  addLegend(pal = paleta, values = ~PrecioMedio, title = "Precio G95 por Municipio")

# Descubrir qué municipios con gasolineras NO se han dibujado en el mapa
municipios_perdidos <- gasolineras_municipio %>%
  anti_join(municipios_sf, by = "Municipio_Join")

# Ver la lista en la consola (puedes abrir 'municipios_perdidos' en tu entorno para verlos todos)
print(municipios_perdidos$Municipio_Join)
