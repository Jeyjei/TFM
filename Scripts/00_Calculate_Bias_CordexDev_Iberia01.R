# Solo funciona para los modelos de CordexDev, que se interpolaron en conjuntos
# de años del modo: "variable_nºdelConjunto_interpol_Dev.Rdata"



#' =============================================================================
#' ======================== PAQUETES Y DATOS CLIMÁTICOS ========================
#' =============================================================================

#' Limpiamos el entorno de trabajo
#' -----------------------------------------------------------------------------

rm(list = ls())
graphics.off()
gc()


#' Declaramos los paths de los códigos y datos
#' -----------------------------------------------------------------------------

# setwd(file.path(getwd(), "4. MASTER DATA UC", "Master_2022", "M1985_TFM"))
path_Datos <- file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") # file.path(getwd(), "Datos") # file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos")
path_Datos_iberia_metadatos <- file.path(getwd(), "Datos")
path_Codigo <- file.path(getwd(), "Codigo")
path_Datos_save <- file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") # file.path(getwd(), "Datos") # file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos")

# Carpeta donde se encuentran los datos de modelos que se van a utilizar en este scritp
folder_data <- "CordexDev"

# Carpeta donde se van a guardar los datos dentro del 'path_Datos_save' (se crea si no existe)
folder_saved <- "CordexDev"


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
library(climate4R.UDG) # para acceder a datos remotos
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
plot_custom_map <- function(grid, name_title = "Title") {

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
  mxvalue <- suppressMessages(aggregateGrid(climatology(grid), aggr.spatial = list(FUN = "max", na.rm = TRUE))$Data[1])
  mnvalue <- suppressMessages(aggregateGrid(climatology(grid), aggr.spatial = list(FUN = "min", na.rm = TRUE))$Data[1])
  if (abs(diff(c(mxvalue, mnvalue))) <= 1) {
    max_value <- mxvalue
    min_value <- mnvalue
  } else {
    max_value <- ceiling(mxvalue)
    min_value <- floor(mnvalue)
  }
  N_int <- abs(diff(c(max_value, min_value)))

  if (N_int > 15) { # Más de 15 enteros entre el máximo y el mínimo
    seq_used <- round(seq(min_value, max_value, by = 2))
    cuts <- N_int
  } else { # Menos de 15 enteros entre el máximo y el mínimo
    if (N_int < 10) {
      N_int <- 10
    }
    cuts <- N_int + 1
    seq_used <- seq(min_value, max_value, length.out = cuts)
  }
  figw <- 10

  # Definir la paleta de colores
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



#' =============================================================================
#' =========================== VARIABLES NECESARIAS ============================
#' =============================================================================

#' Lista de las variables que se van a cargar y utilizar para calcular el índice
#' -----------------------------------------------------------------------------

var.name <- c("tas", "tasmax", "tasmin", "pr")

# Formato de las imágenes
img.ext <- ".pdf" # pdf ó png

# Opción para pasar las variables a otra unidad previamente de calcular el índice
option_convert <- TRUE


# Lista de modelos a evaluar (todos los que aparece en la carpeta CordexDev)
model_folder <- list.dirs(file.path(path_Datos, folder_data), recursive = FALSE)
model_folder <- model_folder[grepl(paste0("^", "model_"), basename(model_folder), ignore.case = TRUE)] # ("^" ,"model_1", "$")

# Variable de Iberia01 (observación)
# model_folder <- c(file.path(path_Datos_iberia_metadatos, "iberia01"), model_folder)



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



#' =============================================================================
#' ============================ PREPARAR LAS LISTAS ============================
#' =============================================================================


# Imprimir los nombres de los modelos
message("Los modelos que se van a utilizar son:")
for (model_i in model_folder) {
  message(paste0("+ ", basename(model_i)))
}


#' ===========================================================================
#' ========================== PARA CADA MODELO ... ===========================
#' ===========================================================================

# Seleccionamos el modelo
for (path.model.i in model_folder) {
  tryCatch(
    {
      message(paste0("\n==================================== ", basename((path.model.i)), " ===================================="))
      message("==================================================================================")

      # Generamos (si no existe) la carpeta que contendrá los datos del modelo
      path_filename_data <- file.path(path_Datos_save, folder_saved, basename(path.model.i))
      if (!file.exists(path_filename_data)) {
        dir.create(path_filename_data)
        message(paste0("==> Se ha creado la carpeta: ", basename(path.model.i)))
      }

      # Generamos (si no existe) la carpeta que contendrá las imágenes asociadas al modelo
      path_filename_fig1 <- file.path(path_filename_data, paste0("plots_", basename(path.model.i)))
      if (!file.exists(path_filename_fig1)) {
        dir.create(path_filename_fig1)
        message(paste0("==> Se ha creado la carpeta: ", basename(path_filename_data), "/plots_", basename(path.model.i)))
      }

      # Lista de archivos .Rdata en el directorio del método de correción de bias
      filesRdata <- list.files(path.model.i, pattern = "\\.Rdata$", full.names = TRUE)

      if (length(filesRdata) == 0) {
        # Pasamos al siguiente modelo histórico
        message(paste0("No hay archivos .Rdata en la carpeta para el método '", basename(path_filename_data), "'"))
        next
      }


      #' Obtenemos los paths de las variables.
      #' Una variable puede tener varios archivos .Rdata asociados
      #' -------------------------------------------------------------------------

      var.files <- list()
      for (var.name.i in var.name) {
        pattern <- paste0("^", var.name.i, "_\\d+_.*")
        var.files[[var.name.i]] <- filesRdata[grepl(pattern, basename(filesRdata))]
      }


      #' =========================================================================
      #' ============== PARA CADA SECCIÓN DE TIEMPO DEL MODELO ... ===============
      #' =========================================================================

      # Lista donde se guardan los valores de 1 índice (tendrá varios argumentos str() si
      # las variables están disgregadas en varios archivos .Rdata)
      list.arg <- list()

      # Todas las variables deben tener el mismo número de archivos asociados!!!
      if (length(var.files[[var.name[1]]]) != 0) {
        for (var.i in 1:length(var.name)) {
          
          # Lista de las variables del modelo tras el pre-procesado.
          list.units <- list()

          #' Cargamos los datos de la variable del modelo
          #' -----------------------------------------------------------------------------

          # Carga y combina los datos de los archivos .Rdata en un solo marco de datos
          data.list <- lapply(var.files[[var.name[[var.i]]]], load_and_extract)
          message(paste0("Se han cargado los .Rdata (", length(var.files[[var.name[[var.i]]]]), ") de la variable '", var.name[[var.i]], "'"))

          #' Unimos los datos de la variable en un único archivo
          #' -----------------------------------------------------------------------------

          # Datos sin nombre
          ls <- unname(data.list)
          len.data.list <- length(data.list)
          rm(data.list)
          gc()

          # Se unen los datos y se guardan unidos (comentado)
          if (len.data.list == 1) {
            data_all <- ls[[1]]
            dir_model <- dirname(var.files[[var.name[[var.i]]]][1])
            filename <- gsub(paste0(var.name[[var.i]], "_\\d+_"), paste0("complete_", var.name[[var.i]], "_"), basename(var.files[[var.name[[var.i]]]][1]))
            path_filename_save <- file.path(dir_model, filename)
            # save(data_all, file = path_filename_save)
            message("Se ha guardado los datos unidos en un archivo .Rdata: ", path_filename_save)
          } else {
            data_all <- redim(bindGrid(ls, dimension = "time"), drop = TRUE)
            dir_model <- dirname(var.files[[var.name[[var.i]]]][1])
            filename <- gsub(paste0(var.name[[var.i]], "_\\d+_"), paste0("complete_", var.name[[var.i]], "_"), basename(var.files[[var.name[[var.i]]]][1]))
            path_filename_save <- file.path(dir_model, filename)
            save(data_all, file = path_filename_save)
            message("Se ha guardado los datos unidos en un archivo .Rdata: ", path_filename_save)
          }

          # Eliminamos las variables innecesarias
          rm(ls)
          gc()

          # # Guardamos las variables unidas en la lista
          # data_all <- data_all

          # # Eliminamos las variables
          # rm(data_all)
          # gc()

          # Obtenemos los años de los datos unidos
          int_years <- unique(getYearsAsINDEX(data_all))
          message(paste0("\n--> Los datos pertenecen al intervalo: ", int_years[1], " - ", int_years[length(int_years)]))

          # Ploteamos la imagen de los datos
          filename_fig1 <- file.path(path_filename_fig1, paste0("00_", var.name[[var.i]], "_", int_years[1], "-", int_years[length(int_years)], "_01_original", img.ext))
          fig1 <- plot_custom_map(suppressMessages(climatology(data_all)),
            name_title = paste0(basename(path.model.i), ": ", var.name[[var.i]], " (", attr(data_all$Variable, "units"), ") de ", int_years[1], " a ", int_years[length(int_years)])
          )
          if (img.ext == ".png") {
            png(filename_fig1)
          } else {
            pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
          }
          print(fig1)
          dev.off()
          message(paste0("\nSe guarda la imagen: ", filename_fig1, "\n"))
          rm(fig1, filename_fig1)
          gc()


          #' Reescalar los daots del modelo a las unidades de la observación
          #' -------------------------------------------------------------------------
          
          # Convertimos las dimensiones de las variables a ºC y kg m-2 si así lo queremos
          if (option_convert) {

            # Temperaturas  attr(data_all$Variable, "units")
            if (var.name[var.i] %in% c("tasmin", "tasmax", "tas")) {
              if (attr(data_all$Variable, "units") == "K") {
                message(paste0("\t==> Variable '", var.name[var.i], "': units "), attr(data_all$Variable, "units"), " to ºC")
                data_all <- udConvertGrid(data_all, "degree_Celsius") # bindGrid es una función de transforeR que vuelve a unir todos los días en un grid de climate4R
                list.units[[var.name[var.i]]] <- "degreeC"
              } else {
                list.units[[var.name[var.i]]] <- attr(data_all$Variable, "units")
              }
            }

            # precipitación
            if (var.name[var.i] %in% c("pr")) {
              if (attr(data_all$Variable, "units") == "kg m-2 s-1") {
                message(paste0("\t==> Variable ", var.name[var.i], ": units "), attr(data_all$Variable, "units"), " to kg m-2")
                nt <- length(getRefDates(data_all))
                ls <- lapply(1:nt, function(i) {
                  timei <- subsetDimension(data_all, dimension = "time", indices = i) # separa día a día
                  gridArithmetics(timei, 86400, operator = "*") # multiplica los segundos de un día para pasar a kg m-2
                })
                data_all <- NULL
                data_all <- bindGrid(ls, dimension = "time") # bindGrid es una función de transforeR que vuelve a unir todos los días en un grid de climate4R
                rm(ls, nt)
                list.units[[var.name[var.i]]] <- "kg m-2"
                # Asignamos al atributo del grid las nuevas unidades
                attr(data_all$Variable, "units") <- "kg m-2"
              } else {
                list.units[[var.name[var.i]]] <- attr(data_all$Variable, "units")
              }
            }
          }

          # Ploteamos la imagen de los datos
          filename_fig1 <- file.path(path_filename_fig1, paste0("00_", var.name[[var.i]], "_", int_years[1], "-", int_years[length(int_years)], "_02_Units", img.ext))
          fig1 <- plot_custom_map(suppressMessages(climatology(data_all)),
            name_title = paste0(basename(path.model.i), ": ", var.name[[var.i]], " (", attr(data_all$Variable, "units"), ") de ", int_years[1], " a ", int_years[length(int_years)])
          )

          if (img.ext == ".png") {
            png(filename_fig1)
          } else {
            pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
          }
          print(fig1)
          dev.off()
          message(paste0("\nSe guarda la imagen: ", filename_fig1, "\n"))
          rm(fig1, filename_fig1)


          #' Calculamos el bias entre los datos del modelo y las observaciones
          #' -----------------------------------------------------------------------------

          # Comprobamos si los datos pertenecen al periodo de observaciones
          if (all(int_years >= 1980 & int_years <= 2005)) { # ... & (len.data.list > 1)
            same_years_obs <- TRUE

            # Path de los datos de Iberia01
            path_data <- file.path(path_Datos_iberia_metadatos, "iberia01")

            # Variable de Iberia01 (observación)
            message(paste0("\nSe cargan las observaciones (", basename(path_data), "): ", file.path(path_data, paste0(var.name[var.i], "_iberia01_1986-2005.Rdata"))))
            data_iberia01 <- load_and_extract(file.path(path_data, paste0(var.name[var.i], "_iberia01_1986-2005.Rdata")))
            message(paste0("\t==> Se ha cargado el archivo: ", var.name[var.i], "_iberia01_1986-2005.Rdata\n"))

            # Seleccionamos el intervalo de tiempo de interés
            obs <- subsetGrid(data_iberia01,
              years = int_years
              # season = season,
              # lonLim = c(-6, -1), # c(-10, 5),
              # latLim = c(41, 43.8) # c(35, 44))
            )
            rm(data_iberia01)
            gc()

            # Cálculo del bias entre modelo corregido y la observación
            message(paste0("Se calcula el bias entre '", basename(path.model.i), "' e Iberia01"))
            bias <- suppressMessages(gridArithmetics(climatology(data_all), climatology(obs), operator = "-"))

            # Guardamos la imagen
            base_name <- gsub("^complete", "00_Bias", basename(path_filename_save))
            filename_fig1 <- file.path(path_filename_fig1, gsub(".Rdata", img.ext, base_name))
            fig1 <- plot_custom_map(bias,
              name_title = paste0(
                basename(path.model.i), ": Sesgo de ", var.name[var.i], " (", attr(data_all$Variable, "units"), ") de ",
                int_years[1], " a ", int_years[length(int_years)], "\n Sin corregir"
              )
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

            # Guardamos el Bias
            save(bias, file = file.path(dirname(path_filename_save), base_name))
            message("\nSave '", file.path(dirname(path_filename_save), base_name), "'\n")
            rm(bias, base_name)
            gc()
          }

          # Eliminamos las variables
          rm(data_all)
          gc()

        }
      } else {
        message(paste("No hay archivos .Rdata para la variable", var.name[1], "y quizás en ninguna otra"))
      }
    },
    error = function(e) {
      message("\nERROR:", conditionMessage(e), "\n\n")

      archivo_errores <- file(path_errores, open = "a")
      cat(paste0("\nModelo '", basename(path.model.i), "'"), file = archivo_errores, "\n", append = TRUE)
      cat(paste0("ERROR: ", conditionMessage(e)), file = archivo_errores, "\n", append = TRUE)
      close(archivo_errores)
    }
  )
}
