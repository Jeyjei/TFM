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
# setwd(file.path(getwd(), "4. MASTER DATA UC", "Master_2022","M1985_TFM"))
path_Datos <- file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") # file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") 
path_Codigo <- file.path(getwd(), "Scripts") # file.path(getwd(), "Codigo")
path_Datos_save <- file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") # path_Datos


#' Cargamos los paquetes necesarios del entorno
#' -----------------------------------------------------------------------------

# Paquetes generales
library(dplyr) # Para ejecutar %>%
library(abind)

# Paqutes de Climate4R
library(climate4R.UDG) # para acceder a datos remotos
library(loadeR) # para leer datos (función loadGridData)
library(visualizeR) # para generar figuras (función spatialPlot)
library(downscaleR) # para bias correction (función biasCorrection)
library(transformeR) # para transformaciones
library(climate4R.indices)


#' =============================================================================
#' ============================ ARCHIVO DE ERRORES =============================
#' =============================================================================

#' Se genera un archivo de errores
#' -----------------------------------------------------------------------------

# Ruta del archivo de texto
path_errores <- file.path(path_Datos_save, "CordexDev", "errores_ejecucion.txt")

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
#' =========================== VARIABLES NECESARIAS ============================
#' =============================================================================

#' Lista de las variables que se van a cargar y utilizar para calcular el índice
#' -----------------------------------------------------------------------------

# Formato de las imágenes
img.ext <- ".pdf" # pdf ó png

# Nombre de la función atómica del ínidice agroclimático
index.code <- list("HI", "BEDD", "BBLI") # "GDD_WI", "BEDD"

# Lista con los argumentos añadidos para calcular los índices
list.arg.any <- list()

# Lista donde se guarda el cálculo del índice agroclimático
list.agroindex <- list()

# Lista de modelos a evaluar (todos los que aparece en la carpeta CordexDev)
model_folder <- list.dirs(file.path(path_Datos, "CordexDev"), recursive = FALSE)
model_folder <- model_folder[grepl(paste0("^" ,"model_"), basename(model_folder), ignore.case = TRUE)] # ("^" ,"model_1", "$")

# Archivos Rdata del conjunto de datos de Iberia01 (observación)
path_iberia <- file.path(getwd(), "Datos", "iberia01", "agroindex_iberia01")
filesRdata_ib01 <- list.files(path_iberia, pattern = "\\.Rdata$", full.names = TRUE)


# Imprimir los nombres de los modelos
message("Los modelos que se van a utilizar son:")
for (model_i in model_folder) {
  message(paste0("+ ", basename(model_i)))
}

#' Metadatos de los modelos 
#' ---------------------------------------------------------------------------

path_metadata_models <- file.path(path_Datos, "CordexDev", "Metadatos")

list.metadata.cordex <- file.path(path_Datos, "CordexDev", "list_models_Dev.txt") %>%
  read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    na.strings = ""
  )


#' ===========================================================================
#' ========================== PARA CADA MODELO ... ===========================
#' ===========================================================================

# Seleccionamos el modelo
for (path.model.i in model_folder) {
  
  #  Añadir que sean históricos
  
  tryCatch(
    {
      message(paste0("\n==================================== ", basename((path.model.i)), " ===================================="))
      message("==================================================================================")

      # Generamos (si no existe) la carpeta que contendrá las imágenes asociadas al modelo
      path_filename_fig1 <- file.path(path.model.i, paste0("plots_", basename(path.model.i)))
      if (!file.exists(path_filename_fig1)) {
        dir.create(path_filename_fig1)
        message("==> Se ha creado la carpeta:", paste0(basename(path.model.i), "/plots_", basename(path.model.i)) )
      }

      # Carpeta que contiene los datos de los índices agroclimáticos asociados al modelo: "agroindex_..."
      path_filename_indice <- list.dirs(path.model.i, recursive = FALSE)
      path_filename_indice <- path_filename_indice[grepl(paste0("^", "agroindex_"), basename(path_filename_indice), ignore.case = TRUE)]
      # path_filename_indice <- path_filename_indice[grepl("agroindex_", path_filename_indice, ignore.case = TRUE)]
      # path_filename_indice <- file.path(path.model.i, paste0("agroindex_", basename((path.model.i))))

      if (file.exists(path_filename_indice)) {
        # Lista de archivos .Rdata en la carpeta agroindex
        filesRdata <- list.files(path_filename_indice, pattern = "\\.Rdata$", full.names = TRUE)

        #' =============================================================================
        #' =========================== PARA CADA ÍNDICE ... ============================
        #' =============================================================================

        for (index.code.i in index.code) {
          message(paste0("\n======================= ", index.code.i, " ======================="))

          if (length(filesRdata) != 0) {

            # El archivo completo (y unido) para cada modelo se nombró como "nombreIndex.Rdata"
            # Obtenemos el path de los datos del índice para el modelo i: es un archivo .Rdata
            path_agroindex.i <- filesRdata[grepl(paste0(index.code.i, ".Rdata"), filesRdata)]
            # Obtenemos el path del archivo del índice para iberia01: es un archivo .Rdata
            path_agroindex.ib01 <- filesRdata_ib01[grepl(paste0(index.code.i, ".Rdata"), filesRdata_ib01)]

            # Todas las variables deben tener el mismo número de archivos asociados!!!
            if (length(path_agroindex.i) == 1 & length(path_agroindex.ib01) == 1) {

              # Cargamos los datos del indice agroclimático del modelo
              message(paste("Se carga:", path_agroindex.i))
              preview_data <- c(ls(), "preview_data")
              load(path_agroindex.i) # Se carga la variable
              name_data_load <- setdiff(ls(), preview_data) # Nombre de la variable

              indice.mod <- get(name_data_load)

              # Eliminamos las variables que no necesitamos
              rm(list = name_data_load)
              gc()

              # Cargamos los datos del indice agroclimático de iberia01
              message(paste("Se carga:", path_agroindex.ib01))
              preview_data <- c(ls(), "preview_data")
              load(path_agroindex.ib01) # Se carga la variable
              name_data_load <- setdiff(ls(), preview_data) # Nombre de la variable

              indice.obs <- get(name_data_load)

              # Eliminamos las variables que no necesitamos
              rm(list = name_data_load)
              gc()

              # Tenemos las variables necesarias para el cálculo del índice, para la sección de tiempo i.part
              # y para el modelo path.model.i
              tryCatch(
                {
                  int_years <- unique(getYearsAsINDEX(indice.obs))
                  message(paste0("--> Intervalo: ", int_years[1], " to ", int_years[length(int_years)]))


                  #' Represento el sesgo (bias) del modelo
                  #' -----------------------------------------------------------------------

                  bias <- gridArithmetics(climatology(indice.mod), climatology(indice.obs), operator = "-")
                  rm(indice.mod, indice.obs)

                  # Se guarda el bias calculado para el modelo
                  message(paste0("\nComienza el guardado del Bias para el índice ", index.code.i, " en archivo .Rdata"))
                  filename <- file.path(path_filename_indice, paste0("Bias_", index.code.i, ".Rdata"))
                  save(bias, file = filename)
                  message("Se ha guardado satisfactoriamente el archivo.\n")

                  # Ploteamos la imagen de los datos
                  filename_fig1 <- file.path(path_filename_fig1, paste0("04_", "Bias_", index.code.i, img.ext))
                  fig1 <- spatialPlot(bias,
                    backdrop.theme = "coastline", rev.colors = TRUE,
                    main = paste0("Sesgo de ", index.code.i, " asociado a ", basename((path.model.i)), " [", int_years[1], " : ", int_years[length(int_years)],"]")
                  )
                  if (img.ext == ".png") {
                    png(filename_fig1)
                  } else {
                    pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                  }
                  print(fig1)
                  dev.off()
                  message(paste0("Se guarda la imagen: ", filename_fig1, "\n"))

                  # Se eliminan las variables que ya no se necesitan
                  rm(fig1, filename_fig1, bias, filename)
                },
                error = function(e) {
                  message("\nERROR: ", conditionMessage(e), "\n\n")
                  archivo_errores <- file(path_errores, open = "a")
                  cat(paste0("\nModelo: ", basename((path.model.i))), file = archivo_errores, "\n", append = TRUE)
                  cat(paste0("\nÍndice climático: ", index.code.i), file = archivo_errores, "\n", append = TRUE)
                  cat(paste0("ERROR: ", conditionMessage(e)), file = archivo_errores, "\n", append = TRUE)
                  close(archivo_errores)
                }
              ) # Terminó el TryCath
            } else {
              message(paste("No hay archivos .Rdata para la variable", var.name[1], "y quizás en ninguna otra"))
            }
          } else {
            message(paste0("La carpeta 'agroindex_", basename(path.model.i), "' está vacía."))
          }
        }
      } else {message(paste("La carpeta", basename(path.model.i), "está vacía."))}
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
