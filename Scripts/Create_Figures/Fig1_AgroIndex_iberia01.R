#' =============================================================================
#' ======================== PAQUETES Y DATOS CLIMÁTICOS ========================
#' =============================================================================

#' Limpiamos el entorno de trabajo
#' -----------------------------------------------------------------------------

rm(list = ls())
gc()


#' Declaramos los paths de los códigos y datos
#' -----------------------------------------------------------------------------

# setwd(file.path("/home", "juanjose", "Documentos", "Mis_proyectos"))
setwd(file.path(getwd(), "4. MASTER DATA UC", "Master_2022", "M1985_TFM"))
path_Datos <- file.path(getwd(), "Datos") # file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") #
path_Datos_iberia_metadatos <- file.path(getwd(), "Datos")
path_Codigo <- file.path(getwd(), "Codigo")
path_Datos_save <- file.path(getwd(), "Datos") #  file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") #

# Carpeta donde se encuentran los datos de modelos que se van a utilizar en este scritp
folder_data <- "iberia01"

# Carpeta donde se van a guardar los datos dentro del 'path_Datos_save' (se crea si no existe)
folder_saved <- "Figures"


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
#' =========================== VARIABLES NECESARIAS ============================
#' =============================================================================

#' Lista de las variables que se van a cargar y utilizar para calcular el índice
#' -----------------------------------------------------------------------------

# Modelos a tener en cuenta
model_folder <- file.path(path_Datos_iberia_metadatos, "iberia01")

# Nombre de la función atómica del ínidice agroclimático
index.code <- list("HI", "BEDD", "BBLI")

# Tipo de CV: "WarmCold" o NULL
CV_used_list <- c("none", "WarmCold") # "WarmCold"
BC_used <- NULL

# Valores mínimos y máximos del índice
min.value <- NULL
max.value <- NULL

# Formato de las imágenes
img.ext <- ".pdf" # pdf ó png
plot_indexmap <- FALSE

# Opción para pasar las variables a otra unidad previamente de calcular el índice
option_convert <- TRUE



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
plot_custom_map <- function(grid, name_title = "Title", min.value = NULL, max.value = NULL, name.palette = NULL, rev.colors = FALSE, n.bins = 20) {

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

  clim_min <- ceiling(quantile(as.vector(grid$Data), probs = c(0.02), na.rm = T)) # extremes (upper and lower) for a zero-centered colormap
  clim_max <- ceiling(quantile(as.vector(grid$Data), probs = c(0.98), na.rm = T))
  cmap.nbins <- n.bins # number of bins in the colorbar
  cmap.delta <- rev(colorRampPalette(brewer.pal(9, "RdBu"))(cmap.nbins)) # color palette for biases (red-white-blue)
  seq_used <- round(seq(clim_min, clim_max, abs(clim_min - clim_max) / cmap.nbins), 0)


  cmap.delta <- colorRampPalette(brewer.pal(9, name_palette))(cmap.nbins)
  if (isTRUE(rev.colors)) {
    cmap.delta <- rev(cmap.delta)
  }

  spatialPlot(
    grid = grid,
    sp.layout = list(list(coast, first = FALSE)),
    scales = list(draw = TRUE),
    main = list(label = name_title, cex = 2),
    par.strip.text = list(cex = 0.75),
    as.table = TRUE,
    layout = c(1, 1),
    set.min = clim_min, set.max = clim_max, # Rango
    # xlim = x_lim, ylim = y_lim,
    col.regions = cmap.delta,
    at = seq_used,
    # cuts = cuts,
    # rev.colors = TRUE,
    colorkey = list(
      labels = list(at = seq_used, cex = 1.3), # Etiquetas del colorbar
      right = list(
        fun = "draw.colorkey",
        args = list(
          key = list(
            at = seq_used, # Posiciones de los marcadores en el colorbar
            col = cmap.delta, # Paleta de colores
            width = 1 / 1.75
          )
        )
      )
    )
  )
}



#' =============================================================================
#' ============================ ARCHIVO DE ERRORES =============================
#' =============================================================================

#' Se genera un archivo de errores
#' -----------------------------------------------------------------------------

# Ruta del archivo de texto
path_errores <- file.path(path_Datos_save, folder_data, "errores_ejecucion.txt")

# Verificar si el archivo existe
if (!file.exists(path_errores)) {
  # Crear el archivo si no existe
  file.create(path_errores)
}

# Abrir el archivo en modo de escritura
archivo_errores <- file(path_errores, open = "a")

# Escribir la fecha de ejecución del archivo
cat(paste0(
  "\n\n=======================================================================================================================\n",
  "================================================= ", as.character(format(as.POSIXct(Sys.time()) + as.difftime(2, units = "hours"))), " =================================================\n",
  "======================================================================================================================="
), file = archivo_errores, "\n", append = TRUE)
close(archivo_errores)


# Generamos (si no existe) la carpeta que contendrá las figuras
path_filename_data <- file.path(path_Datos_save, folder_saved)
if (!file.exists(path_filename_data)) {
  dir.create(path_filename_data)
  message(paste0("==> Se ha creado la carpeta: ", (path_filename_data)))
}

# Generamos (si no existe) la carpeta que contendrá las figuras
path_filename_data <- file.path(path_Datos_save, folder_saved, "Fig1")
if (!file.exists(path_filename_data)) {
  dir.create(path_filename_data)
  message(paste0("==> Se ha creado la carpeta: ", (path_filename_data)))
}

# Lista con los datos
data_fig1 <- list()

for (CV_used in CV_used_list) {

  # Lista que almacena los datos para la CV usada
  data_fig1[[CV_used]] <- list()


  #' =============================================================================
  #' ========================= AÑOS CÁLIDOS Y AÑOS FRÍOS =========================
  #' =============================================================================

  if (CV_used == "none") {
    int_years <- (1986:2005)
  }

  if (CV_used == "WarmCold") {
    # 2 - fold, uno de los años más cálidos y el otro de los años más fríos según
    # la temperatura media de Iberia01

    # Path de la temperatura media de Iberia01
    path_tas <- file.path(path_Datos_iberia_metadatos, "iberia01", "tas_iberia01_1986-2005.Rdata")
    data <- load_and_extract(path_tas)

    # Valor de tempeartura media por año (aplando temporalmente el tiempo y espacialmente)
    tas_by_year <- aggregateGrid(data, aggr.y = list(FUN = "mean", na.rm = TRUE), aggr.spatial = list(FUN = "mean", na.rm = TRUE))
    rm(data)
    gc()

    # Creamos un df con los años y sus valores
    df <- data.frame(values = tas_by_year$Data, years = getYearsAsINDEX(tas_by_year))

    # Ordenamos de forma decreciente el dataframe en función de los valores
    df_ordenado <- df[order(df$values, decreasing = TRUE), ]

    # Obtenemos los años calientes y fríos
    years_WARM <- df_ordenado[1:(nrow(df_ordenado) %/% 2), "years"]
    years_COLD <- df_ordenado[(nrow(df_ordenado) %/% 2 + 1):nrow(df_ordenado), "years"]

    # Intervalo de tiempo seleccionado para utilizar en el plot
    int_years <- years_WARM

    # Borramos las variables y se limpia la memoria
    rm(tas_by_year, df)
    gc()
  } else {
    int_years <- (1986:2005)
  }

  browser()


  #' =============================================================================
  #' ========================== PARA CADA ÍNDICE ... =============================
  #' =============================================================================

  # Para caso de Iberia01
  path.model.i <- model_folder[[1]]

  for (index.code.i in index.code) {

    # Lista que almacena los datos para el índice usado
    data_fig1[[CV_used]][[index.code.i]] <- list()


    tryCatch(
      {
        message(paste0("\n==================================== ", basename((path.model.i)), " ===================================="))
        message("==================================================================================")

        # Lista de archivos .Rdata en el directorio de "agroindex_..." donde se encuentran los archivos de los índices
        filesRdata <- list.files(file.path(path.model.i, paste0("agroindex_", basename(path.model.i))), pattern = "\\.Rdata$", full.names = TRUE)

        if (length(filesRdata) == 0) {
          # Pasamos al siguiente modelo histórico
          message(paste0("No hay archivos .Rdata en la carpeta para el método '", basename(path_filename_data), "'"))
          next
        }

        # Operamos con cada índice
        message(paste0("\n======================= ", index.code.i, " ======================="))

        # Obtenemos el path del índice (uno por índice)

        # pattern <- paste0("^", index.code.i, ".*", BC_used, ".*", CV_used)
        pattern <- paste0("^", index.code.i, ".*", BC_used)
        index.files <- subset(filesRdata, grepl(pattern, basename(filesRdata)) & !grepl("_list", basename(filesRdata)))

        #' Cargamos los datos de la variable del modelo
        #' -----------------------------------------------------------------------------

        # Carga y combina los datos de los archivos .Rdata en un solo marco de datos
        data.list <- lapply(index.files, load_and_extract)


        #' Unimos los datos de la variable en un único archivo si fuera necesario
        #' -----------------------------------------------------------------------------

        # Datos sin nombre
        ls <- unname(data.list)
        len.data.list <- length(data.list)
        rm(data.list)
        gc()


        if (len.data.list == 1) {
          data_all <- ls[[1]]

          #' Reformateamos la fecha del los datos por si se encontrara en formato POSIXlt
          #' ----------------------------------------------------------------------------

          data_all$Dates$start <- format(data_all$Dates$start, format = "%Y-%m-%d %H:%M:%S %Z")
          data_all$Dates$end <- format(data_all$Dates$end, format = "%Y-%m-%d %H:%M:%S %Z")
        } else {
          data_all <- redim(bindGrid(ls, dimension = "time"), drop = TRUE)

          #' Reformateamos la fecha del los datos por si se encontrara en formato POSIXlt
          #' ----------------------------------------------------------------------------

          data_all$Dates$start <- format(data_all$Dates$start, format = "%Y-%m-%d %H:%M:%S %Z")
          data_all$Dates$end <- format(data_all$Dates$end, format = "%Y-%m-%d %H:%M:%S %Z")
        }

        # Eliminamos las variables innecesarias
        rm(ls)
        gc()

        # Seleccionamos el intervalo de tiempo de interés
        data.index <- subsetGrid(data_all,
          years = int_years
          # season = season,
          # lonLim = c(-6, -1), # c(-10, 5),
          # latLim = c(41, 43.8) # c(35, 44))
        )
        rm(data_all)
        gc()

        # Lista que almacena los datos para el índice usado
        data_fig1[[CV_used]][[index.code.i]] <- data.index
        rm(data.index)
        gc()



        if (isTRUE(plot_indexmap)) {
          message(paste0("Se pinta el mapa de '", index.code.i, "' para ", basename(path.model.i)))

          # Guardamos la imagen
          base_name <- paste0("Fig1_", basename(path.model.i), "_", index.code.i, paste0(c("", BC_used, CV_used), collapse = "_"), ".Rdata")
          filename_fig1 <- file.path(path_filename_data, gsub(".Rdata", img.ext, base_name))
          fig1 <- plot_custom_map(climatology(data_fig1[[CV_used]][[index.code.i]]),
            name_title = paste0(basename(path.model.i), " - ", index.code.i, paste0(c("", BC_used, CV_used), collapse = " - ")),
            min.value = min.value, max.value = max.value
          )
          if (img.ext == ".png") {
            png(filename_fig1)
          } else {
            pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
          }
          print(fig1)
          dev.off()
          message(paste0("\nSe guarda la imagen: ", filename_fig1))
          rm(fig1, filename_fig1)
          gc()
        }
      },
      error = function(e3) {
        message("\nERROR: ", conditionMessage(e3), "\n\n")

        archivo_errores <- file(path_errores, open = "a")
        cat(paste("Error en el modelo", basename((path.model.i))),
          file = archivo_errores, "\n", append = TRUE
        )
        cat(paste0("==> ERROR: ", conditionMessage(e3)),
          file = archivo_errores, "\n", append = TRUE
        )
        close(archivo_errores)
      }
    )
  }
}


#' =============================================================================
#' =========================== DIBUJAMOS LOS ÍNDICES ===========================
#' =============================================================================

#' Dibujamos la figura una vez que tenemos todos los datos cargados.
#' -----------------------------------------------------------------------------

# Agrupamos los grid en un MultiGrid (en función del índice)
index.code.i <- "BBLI"

grid1 <- climatology(data_fig1[["none"]][[index.code.i]])
grid2 <- climatology(data_fig1[["WarmCold"]][[index.code.i]])
grid <- makeMultiGrid(grid1,
  grid2,
  skip.temporal.check = TRUE
) # para el caso de 2 grids

name_title <- index.code.i
min.value <- NULL
max.value <- NULL

# Para dibujar
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
mxvalue <- suppressMessages(aggregateGrid(climatology(grid), aggr.spatial = list(FUN = "max", na.rm = TRUE))$Data[1])
mnvalue <- suppressMessages(aggregateGrid(climatology(grid), aggr.spatial = list(FUN = "min", na.rm = TRUE))$Data[1])
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
  N_int <- 13
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
if (index.code.i %in% c("HI", "BEDD")) {
  name_palette <- "Spectral"
  rev_colors <- TRUE
} else if (index.code.i %in% c("BBLI")) {
  name_palette <- "Spectral"
  rev_colors <- FALSE
}

color_palette <- colorRampPalette(brewer.pal(9, name_palette))(cuts + 1)

# Como no me funciona la opción de rev_colors, invierto el vector de colores
# manualmente
if (isTRUE(rev_colors)) {
  color_palette <- rev(color_palette)
  rev_colors <- FALSE
}


spatialPlot(
  grid = grid,
  sp.layout = list(list(coast, first = FALSE)),
  scales = list(draw = TRUE),
  main = list(label = name_title, cex = 2),
  names.attr = c("1986-2005", "Años cálidos"),
  par.strip.text = list(cex = 1.3),
  as.table = TRUE,
  layout = c(1, 2),
  set.min = min_value, set.max = max_value, # Rango de temperaturas
  xlim = x_lim, ylim = y_lim,
  col.regions = color_palette,
  cuts = cuts,
  rev.colors = TRUE,
  colorkey = list(
    labels = list(at = seq_used, cex = 1.3), # Etiquetas del colorbar
    right = list(
      fun = "draw.colorkey",
      args = list(
        key = list(
          at = seq_used, # Posiciones de los marcadores en el colorbar
          col = color_palette, # Paleta de colores
          width = 1 / 1.75
        )
      )
    )
  )
)


# Guardamos la imagen
fig1 <- plot_custom_map(subsetGrid(grid = data_fig1[["none"]][[index.code.i]], years = 2005), name_title = paste(index.code.i, "2005"), name.palette = "Spectral", rev.colors = FALSE, n.bins = 10)

img.ext <- ".pdf"
base_name <- paste0("Fig1_Iberia01_", index.code.i, "_V1.Rdata")
filename_fig1 <- file.path(dirname(path_filename_data), gsub(".Rdata", img.ext, base_name))

if (img.ext == ".png") {
  png(filename_fig1)
} else {
  pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
}
print(fig1)
dev.off()
message(paste0("\nSe guarda la imagen: ", filename_fig1))

rm(fig1, filename_fig1)
gc()
