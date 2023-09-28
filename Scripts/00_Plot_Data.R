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
path_Datos <- file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") # file.path(getwd(), "Datos") #   
path_Codigo <- file.path(getwd(), "Scripts") # file.path(getwd(), "Codigo")


#' Cargamos los paquetes necesarios del entorno
#' -----------------------------------------------------------------------------

# Paquetes generales
library(dplyr) # Para ejecutar %>%
library(abind)

# Paquetes de Climate4R
library(climate4R.UDG) # para acceder a datos remotos
library(loadeR) # para leer datos (función loadGridData)
library(visualizeR) # para generar figuras (función spatialPlot)
library(downscaleR) # para bias correction (función biasCorrection)
library(transformeR) # para transformaciones
# library(climate4R.indices)


#' =============================================================================
#' ============================ ARCHIVO DE ERRORES =============================
#' =============================================================================

#' Se genera un archivo de errores
#' -----------------------------------------------------------------------------

# Ruta del archivo de texto
path_errores <- file.path(path_Datos, "CordexDev", "errores_ejecucion.txt")

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

#' Lista de las variables que se van a cargar y utilizar en el script
#' -----------------------------------------------------------------------------

# Nombre de las variables (se encuentran en el nombre de los archivos .Rdata)
var.name <- c("tasmin", "tasmax", "tas", "pr")

# Lista de modelos a evaluar (todos los que aparece en la carpeta CordexDev)
model_folder <- list.dirs(file.path(path_Datos, "CordexDev"), recursive = FALSE)
model_folder <- model_folder[grepl("model_", model_folder, ignore.case = TRUE)]

# Lista de modelos a evaluar (todos los que aparece en la carpeta CordexDev)
model_folder <- c(file.path(getwd(), "Datos", "iberia01"), model_folder)



#' ===========================================================================
#' ========================== PARA CADA MODELO ... ===========================
#' ===========================================================================

# Seleccionamos el modelo
for (path.model.i in model_folder) {
  message(paste0("\n=============================== ", basename((path.model.i)), " ==============================="))

  # Generamos (si no existe) la carpeta que contendrá las imágenes asociadas al modelo
  path_filename_fig1 <- file.path(path.model.i, paste0("plots_", basename((path.model.i))))
  if (!file.exists(path_filename_fig1)) {
    dir.create(path_filename_fig1)
    message(paste0("==> Se ha creado la carpeta: plots_", basename((path.model.i))))
  }

  # Lista de archivos .Rdata en el directorio
  filesRdata <- list.files(path.model.i, pattern = "\\.Rdata$", full.names = TRUE)

  # Obtenemos los paths de las variables que intervienen en el índice
  var.files <- list()
  for (var.name.i in var.name) {
    if (length(filesRdata[grepl(paste0(var.name.i, "_"), filesRdata)]) != 0) {
      var.files[[var.name.i]] <- filesRdata[grepl(paste0(var.name.i, "_"), filesRdata)]
    }
  }


  #' =========================================================================
  #' ============== PARA CADA SECCIÓN DE TIEMPO DEL MODELO ... ===============
  #' =========================================================================

  # Lista donde se guardan los valores de 1 índice (tendrá varios argumentos str() si
  # las variables están disgregadas en varios archivos .Rdata)
  list.agroindex <- list()

  # Para aquellos modelos que los datos de cada variable se encuentra en varios archivos
  for (var.i in names(var.files)) {
    tryCatch(
      {
        for (i.part in 1:length(var.files[[var.i]])) {

          # Lista de argumentos para cada sección (años) del modelo
          list.arg <- list()
          name_variable <- var.i
          
          message(paste("Se carga:", var.files[[name_variable]][i.part]))
          preview_data <- c(ls(), "preview_data")
          load(var.files[[name_variable]][i.part]) # Se carga la variable
          name_data_load <- setdiff(ls(), preview_data) # Nombre de la variable
          if (length(get(name_data_load)) == 1) {
            list.arg[[name_variable]] <- get(name_data_load)[[names(get(name_data_load))]]
          } else {
            list.arg[[name_variable]] <- get(name_data_load)
          }

          # message(str(list.arg))
          # Eliminamos las variables que no necesitamos
          rm(list = name_data_load)
          gc()


          int_years <- unique(getYearsAsINDEX(list.arg[[name_variable]]))
          message(paste0("--> Intervalo: ", int_years[1], " to ", int_years[length(int_years)]))
          message(paste0("--> Variable ", name_variable, ": units "), getGridUnits(list.arg[[name_variable]])) # degC para temperaturas, mm ó kg*m-2 para precipitación


          # Guardamos la figura en una imagen PNG
          filename_fig1 <- file.path(path_filename_fig1, basename(sub("\\.Rdata$", ".png", var.files[[name_variable]][i.part])))
          fig1 <- spatialPlot(climatology(list.arg[[name_variable]]),
            backdrop.theme = "coastline", rev.colors = TRUE,
            main = paste0(basename((path.model.i)), ": ", name_variable, " (", getGridUnits(list.arg[[name_variable]]), ") de ", int_years[1], " a ", int_years[length(int_years)])
          )
          png(filename_fig1)
          print(fig1)
          dev.off()
          message(paste0("--> Se guarda el PNG: ", filename_fig1, "\n"))

          # Se eliminan las variables que ya no se necesitan
          rm(fig1, filename_fig1, list.arg, name_variable)
          gc()
        }
      },
      error = function(e) {
        message("\nERROR: ", conditionMessage(e), "\n\n")
        archivo_errores <- file(path_errores, open = "a")
        cat(paste0("\nModelo: ", basename((path.model.i))), file = archivo_errores, "\n", append = TRUE)
        cat(paste0("\nVariable: ", name_variable), file = archivo_errores, "\n", append = TRUE)
        cat(paste0("ERROR: ", conditionMessage(e)), file = archivo_errores, "\n", append = TRUE)
        close(archivo_errores)
      }
    )
  }
}

