rm(list = ls()) #limpio entorno
install.packages("tidyverse")
library(tidyverse)
url <- "https://raw.githubusercontent.com/rominicky/materiales/main/assets/Provincias.csv"
destino <- "provincias.csv"
download.file(url, destfile = destino, mode = "wb")
#como se pide en el documento primeros analissi en r studio que nos mandaste, descargue el archivo
provincias <- read_csv(destino)
#ahora voy a limpiar y usar tydeverse
#voy a Calcular el porcentaje de personas en situación de calle relativo a la población total y filtrar aquellas provincias que tienen datos registrados
analisis_provincias <- provincias %>%
  # Renombrar columnas largas (como se sugiere en el archivo que subiste)
  rename(
    provincia = Nombre.de.provincia,
    situacion_calle = Personas.en.situación.de.calle..vía.pública.,
    poblacion_2022 = Población..2022.
  ) %>%  
  mutate(tasa_por_100k = (situacion_calle / poblacion_2022) * 100000) %>%
  filter(situacion_calle > 0) %>%  
names(provincias)
#me daba error, entonces para ver bien el nombre, (lo que me había equivocado la vez anterior), ejecuté el comando names
analisis_provincias <- provincias %>%
  rename(
    # Nuevo_nombre = "Nombre_Original_en_tu_archivo"
    provincia = "Nombre de provincia", 
    situacion_calle = "Personas en situación de calle (vía pública)",
    poblacion_2022 = "Población (2022)"
  ) %>%
  # Ahora que ya tienen nombres simples, hacemos los cálculos
  mutate(tasa_por_100k = (situacion_calle / poblacion_2022) * 100000) %>%
  filter(situacion_calle > 0) %>%
  arrange(desc(tasa_por_100k))
#verifico
head(analisis_provincias)
#ahora voy a visualizar con ggplot, como dice en el documento
ggplot(analisis_provincias, aes(x = reorder(provincia, tasa_por_100k), 
                                y = tasa_por_100k, 
                                fill = tasa_por_100k)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  scale_fill_viridis_c(option = "magma") + 
  labs(
    title = "Incidencia de situación de calle por Provincia",
    subtitle = "Tasa cada 100.000 habitantes (Censo 2022)",
    x = "Provincia",
    y = "Tasa (personas x 100k hab.)",
    fill = "Tasa"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#en plots se puede ver el grafico con los nombres de las provincias