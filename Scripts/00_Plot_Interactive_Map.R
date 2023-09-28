
rm(list = ls())

#' =============================================================================
#' ============================== LIBRERIAS DE R ===============================
#' =============================================================================

library(transformeR) # para transformaciones
library(rasterVis)
library(climate4R.datasets)
library(ggplot2)
library(leaflet)
library(htmlwidgets)



#' =============================================================================
#' =========================== FUNCIONES AUXILIARES ============================
#' =============================================================================

#' Cargamos las funciones auxiliares que son necesarias a lo largo del código
#' -----------------------------------------------------------------------------

# Crea una función para cargar un archivo .Rdata y extraer el objeto deseado
load_and_extract <- function(path) {
  
  # Variable que contendrá los datos
  data_load <- NULL
  
  # Obtenemos las variables que existen previamente a cargar los datos
  preview_data <- c(ls(), "preview_data")
  load(path) # Carga el archivo .Rdata
  
  # La carga del archivo ha añadido una nueva variable al entorno
  name_data_load <- setdiff(ls(), preview_data) # Nombre de la variable (string)
  
  if (length(get(name_data_load)) == 1) {
    data_load <- get(name_data_load)[[names(get(name_data_load))]]
  } else {
    data_load <- get(name_data_load)
  }
  
  # Eliminamos las variables que no necesitamos
  rm(list = name_data_load)
  gc()
  
  return(data_load)
}

#' =============================================================================
#' ============================= CUERPO DEL CÓDIGO =============================
#' =============================================================================

setwd(file.path(getwd(), "4. MASTER DATA UC", "Master_2022","M1985_TFM"))
path_Datos <- file.path(getwd(), "Datos")

# Cargar el dataset EOBS_Iberia_pr
obs <- load_and_extract(file.path(path_Datos, "iberia01", "tas_iberia01_1986-2005.Rdata"))

# Extraer los valores de precipitación (primer día)
values <- climatology(obs)$Data[1, , , drop = FALSE]

# Obtener las coordenadas de latitud y longitud
lon <- obs$xyCoords$x
lat <- obs$xyCoords$y

# Crear un data frame con las coordenadas y valores
data_df <- data.frame(
  lon = rep(lon, each = length(lat)),
  lat = rep(lat, times = length(lon)),
  value = as.vector(values)
)

# Crear el mapa utilizando ggplot2
ggplot(data_df, aes(x = lon, y = lat, color = value)) +
  geom_point() +
  scale_color_viridis_c() +
  labs(
    title = paste0("Mapa de ",  attr(obs$Variable, "longname")),
    subtitle = "Dataset iberia01",
    x = "Longitud", y = "Latitud",
    color = paste0( attr(obs$Variable, "longname")[1], " (", getGridUnits(obs), ")")
  )

leaflet(data = data_df) %>%
  addTiles() %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = 5,
    color = ~ colorNumeric(palette = viridis::viridis(10), domain = data_df$value)(value),
    fillOpacity = ~ ifelse(is.na(value), 0, 0.8), # Ajusta la opacidad para NA
    stroke = FALSE, # Desactivar el borde
    popup = ~ paste("Latitud: ", lat, "<br>Longitud: ", lon, paste0("<br>", getVarNames(obs), ": "), round(value, 2), getGridUnits(obs)),
    label = ~value
  ) %>%
  addLegend("bottomright",
            pal = colorNumeric(palette = viridis::viridis(10), domain = data_df$value),
            values = ~value, title = paste0(getVarNames(obs), " (", getGridUnits(obs), ")"), opacity = 1
  )

# Filtrar los valores NA
data_df <- data_df[!is.na(data_df$value), ]

# Crear un mapa interactivo con leaflet
map_circles <- leaflet(data = data_df) %>%
  addTiles() %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = 9,
    color = ~ colorNumeric(palette = viridis::viridis(10), domain = data_df$value)(value),
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~ paste("Latitud: ", lat, "<br>Longitud: ", lon, paste0("<br>", getVarNames(obs), ": "), round(value, 2), getGridUnits(obs)),
    label = ~value
  ) %>%
  addLegend("bottomright",
            pal = colorNumeric(palette = viridis::viridis(10), domain = data_df$value),
            values = ~value, title = paste0(getVarNames(obs), " (", getGridUnits(obs), ")"), opacity = 1
  )

# Mostramos el mapa en Rstudio
map_circles

# Guardar el mapa en un archivo HTML
saveWidget(map_circles, file = paste0("mapa_circulos_", getVarNames(obs), ".html"))


# Crear un mapa interactivo con leaflet con cuadrados
map_square <- leaflet(data = data_df) %>%
  addTiles() %>%
  addRectangles(
    lng1 = ~ lon - attr(obs$xyCoords, "resX")/2, lat1 = ~ lat - attr(obs$xyCoords, "resY")/2, # Esquina superior izquierda del cuadrado
    lng2 = ~ lon + attr(obs$xyCoords, "resX")/2, lat2 = ~ lat + attr(obs$xyCoords, "resY")/2, # Esquina inferior derecha del cuadrado
    color = ~ colorNumeric(palette = viridis::viridis(10), domain = data_df$value)(value),
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~ paste("Latitud: ", lat, "<br>Longitud: ", lon, paste0("<br>", getVarNames(obs), ": "), round(value, 2), getGridUnits(obs)),
    label = ~value
  ) %>%
  addLegend("bottomright",
            pal = colorNumeric(palette = viridis::viridis(10), domain = data_df$value),
            values = ~value, title = paste0(getVarNames(obs), " (", getGridUnits(obs), ")"), opacity = 1
  )

# Mostramos el mapa en Rstudio
map_square

# Guardar el mapa en un archivo HTML
saveWidget(map_square, file = paste0("mapa_cuadrados_", getVarNames(obs), ".html"))

