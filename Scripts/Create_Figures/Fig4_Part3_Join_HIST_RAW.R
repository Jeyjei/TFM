#' =============================================================================
#' ======================== PAQUETES Y DATOS CLIMÁTICOS ========================
#' =============================================================================

#' Limpiamos el entorno de trabajo
#' -----------------------------------------------------------------------------

rm(list = ls())
gc()


#' Declaramos los paths de los códigos y datos
#' -----------------------------------------------------------------------------

# setwd(file.path(getwd(), "4. MASTER DATA UC", "Master_2022", "M1985_TFM"))
setwd(file.path("/home", "juanjose", "Documentos", "Mis_proyectos"))
path_Datos <- file.path(getwd(), "Datos") # file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") #
path_Datos_iberia_metadatos <- file.path(getwd(), "Datos")
path_Codigo <- file.path(getwd(), "Codigo")
path_Datos_save <- file.path(getwd(), "Datos") #  file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") #
  
# Carpeta donde se encuentran los datos de modelos que se van a utilizar en este scritp
folder_data <- "CordexDev"
folder_data_ref <- "iberia01"

# Carpeta donde se van a guardar los datos dentro del 'path_Datos_save' (se crea si no existe)
folder_saved <- "Figures_season"
path_filename_data <- file.path(path_Datos_save, folder_saved, "Fig4")


#' Cargamos los paquetes necesarios del entorno
#' -----------------------------------------------------------------------------

# Paquetes generales
library(dplyr) # Para ejecutar %>%
library(abind)
library(stringr) # Dividimos los nombres

# Para pintar mapas
library(maps)
library(maptools)
library(RColorBrewer)

# Paqutes de Climate4R
library(loadeR) # para leer datos (función loadGridData)
library(visualizeR) # para generar figuras (función spatialPlot)
library(transformeR) # para transformaciones
library(convertR)



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


# Dibuja un mapa en función del grid dado
plot_custom_map <- function(grid, name_title = "Title", min.value = NULL, max.value = NULL, name.palette = NULL, rev.colors = FALSE) {
  
  # Definición línea de costa sobre el grid
  # x_lim <- c(floor(min(grid$xyCoords$x) - attr(grid$xyCoords, "resX")),
  #            ceiling(max(grid$xyCoords$x) + attr(grid$xyCoords, "resX")))
  # y_lim <- c(floor(min(grid$xyCoords$y) - attr(grid$xyCoords, "resY")),
  #            ceiling(max(grid$xyCoords$y) + attr(grid$xyCoords, "resY")))
  #
  # x_lim <- c(floor(min(grid$xyCoords$x)),
  #           ceiling(max(grid$xyCoords$x)))
  # y_lim <- c(floor(min(grid$xyCoords$y) ),
  #            ceiling(max(grid$xyCoords$y)))
  
  x_lim <- c(
    (min(grid$xyCoords$x) - attr(grid$xyCoords, "resX")),
    (max(grid$xyCoords$x) + attr(grid$xyCoords, "resX"))
  )
  y_lim <- c(
    (min(grid$xyCoords$y) - attr(grid$xyCoords, "resY")),
    (max(grid$xyCoords$y) + attr(grid$xyCoords, "resY"))
  )
  
  
  coast <- map("world",
               xlim = x_lim,
               ylim = y_lim,
               fill = FALSE, plot = FALSE
  )
  coast <- map2SpatialLines(coast) # coastlines (for maps)
  
  # Dibujado de mapas
  # mxvalue <- suppressMessages(aggregateGrid((grid), aggr.spatial = list(FUN = "max", na.rm = TRUE))$Data[1])
  # mnvalue <- suppressMessages(aggregateGrid((grid), aggr.spatial = list(FUN = "min", na.rm = TRUE))$Data[1])
  mxvalue <- suppressMessages(aggregateGrid((grid), aggr.spatial = list(FUN = "quantile", probs = c(0.95), na.rm = TRUE))$Data[1])
  mnvalue <- suppressMessages(aggregateGrid((grid), aggr.spatial = list(FUN = "quantile", probs = c(0.05), na.rm = TRUE))$Data[1])
  qnvalue <- suppressMessages(aggregateGrid(climatology(grid), aggr.spatial = list(FUN = "quantile", probs = c(1, 0.95, 0.5, 0.05, 0), na.rm = TRUE))$Data)
  print(qnvalue)
  
  if (abs(diff(c(mxvalue, mnvalue))) <= 1) {
    max_value <- mxvalue
    min_value <- mnvalue
  } else {
    max_value <- ceiling(mxvalue)
    min_value <- floor(mnvalue)
  }
  message("\nSUGGEST: min_value:", min_value, "\tmax_value:", max_value)
  
  if (!(is.null(min.value))) {
    min_value <- min.value
  }
  if (!(is.null(max.value))) {
    max_value <- max.value
  }
  
  # Mostramos los límites del colorbar
  message("USED: min_value:", min_value, "\tmax_value:", max_value)
  
  N_int <- abs(diff(c(max_value, min_value)))
  
  if (N_int > 20) {
    N_int <- 10
    cuts <- N_int + 1
    seq_used <- round(seq(min_value, max_value, length.out = cuts))
  } else {
    if (N_int > 13) { # Más de 15 enteros entre el máximo y el mínimo
      seq_used <- round(seq(min_value, max_value, by = 2))
      cuts <- N_int
    } else { # Menos de 15 enteros entre el máximo y el mínimo
      if (N_int < 10) {
        N_int <- 10
      }
      cuts <- N_int + 1
      seq_used <- seq(min_value, max_value, length.out = cuts)
    }
  }
  figw <- 10
  
  # Definir la paleta de colores
  if (is.null(name.palette)) {
    if (grid$Variable$varName %in% c("tas", "tg", "tasmax", "tasmin")) {
      name_palette <- "YlOrRd"
      rev_colors <- FALSE
    } else if (grid$Variable$varName %in% c("pr")) {
      name_palette <- "Blues"
      rev_colors <- FALSE
    } else {
      name_palette <- "Spectral"
      rev_colors <- TRUE
    }
  } else {
    name_palette <- name.palette
    rev_colors <- rev.colors
  }
  color_palette <- colorRampPalette(brewer.pal(9, name_palette))(cuts + 1)
  
  spatialPlot(
    grid = grid,
    sp.layout = list(list(coast, first = FALSE)),
    scales = list(draw = TRUE),
    main = list(label = name_title, cex = 0.9),
    names.attr = c("sub1", "sub2"),
    par.strip.text = list(cex = 0.9),
    as.table = TRUE,
    layout = c(1, 1),
    set.min = min_value, set.max = max_value, # Rango de temperaturas
    xlim = x_lim, ylim = y_lim,
    col.regions = color_palette,
    cuts = cuts,
    rev.colors = rev_colors,
    colorkey = list(
      labels = list(at = seq_used), # Etiquetas del colorbar
      right = list(
        fun = "draw.colorkey",
        args = list(
          key = list(
            at = seq_used, # Posiciones de los marcadores en el colorbar
            col = color_palette, # Paleta de colores
            width = figw / 1.75
          )
        )
      )
    )
  )
}


# Cargamos el fichero de bias de los modelos raw
data_raw <- load_and_extract(file.path(path_Datos, folder_saved, "Fig4", "fig4_relative_bias_S4-10_RAW.Rdata"))
data_corr <- load_and_extract(file.path(path_Datos, folder_saved, "Fig4", "fig4_relative_bias_S4-10.Rdata"))

# data_join <- list()
# for (i in (names(data_raw))) {
#   names(data_raw[[i]]) <- c("a1 MPI-CCLM4 (RAW)", "b1 MPI-RCA4 (RAW)", "c1 HadGeM2-CCLM4 (RAW)", "d1 HadGeM2-RCA4 (RAW)" )
#   data_join[[i]] <- append(data_raw[[i]], data_corr[[i]])
# }

# Eliminamos las letras
names_corr_sin <- sub("^[^ ]* ", "", names(data_corr[[1]]))
names_raw_sin <- sub("^[^ ]* ", "", names(data_raw[[1]]))

# Creamos un vector ordenado con los nombres
names_join <- names_corr_sin
names_join <- append(names_join, names_raw_sin[[1]], after = 0)
names_join <- append(names_join, names_raw_sin[[2]], after = 6)
names_join <- append(names_join, names_raw_sin[[3]], after = 12)
names_join <- append(names_join, names_raw_sin[[4]], after = 18)
subtitle_plot <- names_join

# Reasignamos los nombres sin letra a los datos
for (i in names(data_corr)) {
  names(data_raw[[i]]) <- names_raw_sin
  names(data_corr[[i]]) <- names_corr_sin
}


# Unimos lso datos en una única lista
data_join <- list()
for (i in (names(data_raw))) {
  data_join[[i]] <- append(data_raw[[i]], data_corr[[i]])
}

# Creamos una nueva lista con los nombres ordenados alfabeticamente
data_grid <- list()
for (i in names(data_join)) {
  for (j in 1:length(names(data_join[[i]]))) {
    data_grid[[i]][[paste0(letters[j], " ", names_join[j])]] <- data_join[[i]][[names_join[j]]]
  }
}

# Guardamos el archivo .Rdata
save(data_grid, file = file.path(path_filename_data, "fig4_relative_bias_s4-9_C1_JOIN.Rdata"))

# Seleccionamos la variable
variable.i <- "tasmax"

model_order <- c("model_4", "model_21", "model_3", "model_20") # number_model_data
sort_names <- c("MPI-CCLM4", "MPI-RCA4", "HadGeM2-CCLM4", "HadGeM2-RCA4")

grid_plot <- data_grid[[variable.i]]

# Obtenemos los límites de la variable
grid_list_arg <- data_raw[[variable.i]]
names(grid_list_arg) <- NULL
grid_list_arg[["skip.temporal.check"]] <- TRUE
grid <- do.call(makeMultiGrid, grid_list_arg)
clim_max <- ceiling(quantile(as.vector(grid$Data), probs = c(0.98), na.rm = T))
clim_min <- ceiling(quantile(as.vector(grid$Data), probs = c(0.02), na.rm = T))
message(clim_min, " -- ", clim_max)
# rm(grid, grid_list_arg)
gc()

# Argumentos necesarios para la figura 4
modelos.i <- model_order #c("model_20") # c("model_20", "model_21", "model_3", "model_4")
methodList.i <- c("raw", "eqm", "pqm", "qdm", "mbcr", "mbcn")

methodList <- rep(methodList.i, times = length(modelos.i))
modelosList <- rep(modelos.i, each = length(modelos.i) * length(methodList.i))

# length(indices.i) = nº de gráficas
indices.i <- c(variable.i) # c("pr", "tas", "tasmax", "tasmin")
indices <- rep(indices.i, each = length(modelos.i) * length(methodList.i))

colours.aux <- brewer.pal(n = 12, name = "Paired")
colours <- colours.aux[seq(2, length(colours.aux), 2)[1:6]]
coloursList <- rep(colours, times = length(modelos.i))

# Definimos el texto de Y
if (variable.i %in% c("pr")) {
  y_name <- "%"
  # Definimos los máximos y mínimos del eje Y
  y_lim_min <- clim_min
  y_lim_max <- clim_max
}
if (variable.i %in% c("tasmin", "tasmax", "tas")) {
  y_name <- "ºC"
  # Definimos los máximos y mínimos del eje Y
  y_lim_min <- -4
  y_lim_max <- 1.5
}

fig1 <- violinPlot(grid_plot,
           group.index = indices,
           violin = FALSE,
           fill = colours, # Aunque funciona, debería ir 'coloursList' que ya tiene el orden marcado
           h.lines = 0,
           # v.lines = 10.5, # lineas verticales para separar y línea horizontal en el 0 marcando cero bias
           bwplot.custom = list(
             notch = FALSE,
             do.out = FALSE,
             as.table = TRUE,
             ylab = list(label = y_name),
             xlab = list(label = ""),
             scales = list(
               x = list(rot = 45, labels = subtitle_plot), # Volvemos a añadir los nombres a los grid
               y = list(relation = "free", limits = c(y_lim_min, y_lim_max), rot = 0)
             ), # límites distintos en cada panel
             ylim = c(y_lim_min, y_lim_max),
             key = list( # Leyenda
               space = "bottom", cex = 1,
               text = list(toupper(methodList.i)),
               columns = 3,
               points = list(col = colours, pch = 19)
             )
           )
)


fig1

# Guardamos la imagen
season <- getSeason(grid_plot[[1]])
img.ext = ".pdf"
base_name <- paste0("Fig4_Relative-Bias-iberia01_", variable.i, "_S", paste0(c(min(season, na.rm = T), max(season, na.rm = T)), collapse = "-"), "_V1_JOIN.Rdata")
filename_fig1 <- file.path(path_filename_data, gsub(".Rdata", img.ext, base_name))

if (img.ext == ".png") {
  png(filename_fig1)
} else {
  pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
}
print(fig1)
dev.off()
message(paste0("\nSe guarda la imagen: ", filename_fig1))
fig1
rm(fig1, filename_fig1)
gc()
