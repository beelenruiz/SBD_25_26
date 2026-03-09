library(rmarkdown)
library(DT)
library(ggplot2)
library(plotly)
library(tidyr)

# Cargamos la URL con los datos de ejemplo
urlCsv <- "https://drive.google.com/uc?id=1vzWghwFEtUnFwhwYLRAkXaKUUPpamGYd&export=download"

# Creamos el data set con lo que hay en el csv
datos_clase <- read.csv(urlCsv)

# Calculamos la nota media
datos_clase$Nota_Media <- rowMeans(datos_clase[ , 2:9])

# Y redondeamos la nota
datos_clase$Nota_Media <- round(datos_clase$Nota_Media, 2)

datos_violin <- pivot_longer(
  datos_clase,
  cols = -Alumno, 
  names_to = "Asignatura", 
  values_to = "Nota"
  )

# Ponemos aptos y no aptos
datos_clase$Estado <- ifelse(datos_clase$Nota_Media >= 5, "Aprobado", "Suspenso")

# Contamos cuantos hay de cada tipo
conteo_estados <- as.data.frame(table(datos_clase$Estado))
colnames(conteo_estados) <- c("Estado", "Cantidad") 

# Creamos la web
render("ejemploClase.rmd")

#Guardamos el set de datos
write.csv(datos_clase, "notas_finales_procesadas.csv", row.names = FALSE)
