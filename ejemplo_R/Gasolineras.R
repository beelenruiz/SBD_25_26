#install.packages("sf")
library(sf)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("mapSpain")
library(mapSpain)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("leaftlet")
library(leaflet)
library(jsonlite)

# Sacamos la URL
urlCsv <- "https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/"

# Ahora los datos
rawData <- fromJSON(urlCsv)

# Sacamos la lista de gasolineras de los datos
lista_gasolineras <- rawData$ListaEESSPrecio

# Acotamos la parte que nos interesa, precio y municipio
gasolinera_municipio <- lista_gasolineras %>%
  as_tibble() %>%
  mutate(
    Precio95 = as.numeric(gsub(",",".",`Precio Gasolina 95 E5`)),
    IDMunicipio = str_pad(IDMunicipio,5,pad = 0)
  ) %>%
  filter(!is.na(Precio95)) %>%
  group_by(IDMunicipio) %>%
  summarise(PrecioMedio = mean(Precio95, na.rm = TRUE), .groups = 'drop')

# Obtenemos los municipios
municipios <- esp_get_munic() %>% st_transform(4326)

# Ubicación municipios
mapa_data <- municipios %>%
  left_join(gasolinera_municipio, by=c("LAU_CODE" = "IDMunicipio"))

# Paleta para el mapa
paleta <- colorBin("YlOrRd", domain=mapa_data$PrecioMedio, bins=7)

# Genarmos el mapa
leaflet(mapa_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-3.7, lat=40.4, zoom=6) %>%
  addPolygons(
    fillColor = ~paleta(PrecioMedio),
    color = "white", weight = 0.5, fillOpacity = 0.7,
    label = ~paste0(name,":",round(PrecioMedio,3),"€/l."),
    highlightOptions = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)
  ) %>%
  addLegend(pal = paleta, values = ~PrecioMedio, title = "Gasolina 95 €/l.")



