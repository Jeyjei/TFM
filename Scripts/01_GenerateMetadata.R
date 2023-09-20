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
path_Datos <- file.path(getwd(), "Datos")
path_Codigo <- file.path(getwd(), "Codigo")
path_Datos_save <- path_Datos

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
# library(climate4R.indices)

#' =============================================================================
#' ======================= CARPETAS Y ARCHIVO DE ERRORES =======================
#' =============================================================================

#' Se genera la carpeta que contendrá todos los archivos creados por este script
#' -----------------------------------------------------------------------------

dir_codexdev <- file.path(path_Datos)
if (!file.exists(path_Datos)) {
  dir.create(path_Datos)
  message("==> Se ha creado la carpeta: Datos")
}

dir_codexdev <- file.path(path_Datos_save, "CordexDev")
if (!file.exists(dir_codexdev)) {
  dir.create(dir_codexdev)
  message("==> Se ha creado la carpeta: CordexDev")
}
dir_codexdev_met <- file.path(path_Datos_save, "CordexDev", "Metadatos")
if (!file.exists(dir_codexdev_met)) {
  dir.create(dir_codexdev_met)
  message("==> Se ha creado la carpeta: CordexDev/Metadatos")
}

#' Se genera un archivo de errores
#' -----------------------------------------------------------------------------
#' # Ruta del archivo de texto
path_errores <- file.path(path_Datos_save, "CordexDev", "errores_ejecucion.txt")

# Verificar si el archivo existe
if (!file.exists(path_errores)) {
  # Crear el archivo si no existe
  file.create(path_errores)
}

# Abrir el archivo en modo de escritura
archivo_errores <- file(path_errores, open = "a")

# Escribir la fecha de ejecución del archivo
cat(paste0("\n\n=======================================================================================================================\n",
           "================================================= ", as.character(format(as.POSIXct(Sys.time()) + as.difftime(2, units = "hours"))), " =================================================\n",
           "======================================================================================================================="), 
    file = archivo_errores, "\n", append = TRUE)
close(archivo_errores)


#' =============================================================================
#' =========================== VARIABLES NECESARIAS ============================
#' =============================================================================

#' Lista de las variables que se quieren comprobar
#' -----------------------------------------------------------------------------

# Nombre de las variables utilizados en los .Rdata y modelos CORDEX
list.variables <- c("pr", "tasmin", "tasmax", "tas", "sftlf")


#' =============================================================================
#' ============================= MODELOS DE CORDEX =============================
#' =============================================================================

#' Generamos los METADATOS de los modelos de CORDEX
#' -----------------------------------------------------------------------------

# Se obtiene el DataFrame con la información de los modelos
# df_m <- read.csv(file.path(path_Datos, "CordexDev", "Metadatos", "inventory.csv")) # inventario con las rutas corregidas

url <- "https://data.meteo.unican.es/inventory.csv"
path_local <- file.path(path_Datos, "CordexDev", "Metadatos", "inventory.csv")


# Función para verificar si la URL es accesible
url_accesible <- function(url) {
  resultado <- tryCatch({
    con <- read.csv(url)
    resultado <- TRUE
  }, error = function(e) {
    resultado <- FALSE
  })
  return(resultado)
}

# Intentar cargar desde la URL
if (url_accesible(url)) {
  # Descargar el archivo desde la URL
  # download.file(url, destfile = path_local, mode = "wb")
  message("\n\t==> Se utiliza la URL")
  df_m <- read.csv(url)
} else {
  # Cargar desde la ruta local
  message("\n\t==> Se utiliza el archivo CSV previamente descargado")
  df_m <- read.csv(path_local)
}


# Se añade una nueva columna que indica el nombre completo de cada modelo
# df_m$longname <- paste(as.character(df_m$activity),
#   as.character(df_m$domain),
#   as.character(df_m$model),
#   as.character(df_m$experiment),
#   as.character(df_m$ensemble),
#   as.character(df_m$rcm),
#   as.character(df_m$rcm_version),
#   sep = "_"
# )
df_m$longname <- paste(as.character(df_m$activity),
  as.character(df_m$domain),
  as.character(df_m$institution),
  as.character(df_m$model),
  as.character(df_m$experiment),
  as.character(df_m$ensemble),
  as.character(df_m$rcm),
  as.character(df_m$rcm_version),
  sep = "_"
)
# df_m$longname <- as.factor(df_m$longname)

# Obtenemos los modelos asociados a cada variable de interés.
# Tanto en el periodo historico ("historical") como escenario "rcp85"
list.historical <- list()
list.rcp85 <- list()
for (variable_i in list.variables) {
  list.historical[[variable_i]] <- subset(df_m, activity == "CORDEX" &
    domain == "EUR-11" &
    variable == variable_i &
    experiment == "historical")
  list.rcp85[[variable_i]] <- subset(df_m, activity == "CORDEX" &
    domain == "EUR-11" &
    variable == variable_i &
    experiment == "rcp85")
}

# Unimos dicho conjunto de modelos en un único DataFrame
df.historical.all <- do.call(rbind, list.historical)
df.rcp85.all <- do.call(rbind, list.rcp85)
df.all <- rbind(df.historical.all, df.rcp85.all)
df.historical.all <- data.frame(df.historical.all, row.names = NULL)
df.rcp85.all <- data.frame(df.rcp85.all, row.names = NULL)
df.all <- data.frame(df.all, row.names = NULL)

# Lista de los nombres únicos de los modelos
uniq.historical.model_name <- unique(df.historical.all$longname)
uniq.rcp.model_name <- unique(df.rcp85.all$longname)
uniq.all.model_name <- unique(df.all$longname)
df.historical.all$longname <- as.factor(df.historical.all$longname)
df.rcp85.all$longname <- as.factor(df.rcp85.all$longname)
df.all$longname <- as.factor(df.all$longname)

# Lista para guardar los modelos válidos
list.df.model <- list()

# Contador de modelos válidos
j <- 0

# DataFrame de los metadatos breves
results <- data.frame()

# Asociamos cada conjunto de variables con su grid tierra-mar
for (i in 1:length(uniq.all.model_name)) {
  cordex.model.i <- uniq.all.model_name[i]


  tryCatch(
    {
      # Seleccionamos todas las filas que pertenezcan al modelo (longname)
      df.model <- subset(df.all, longname == cordex.model.i)
      list.var <- as.character(df.model$variable)
      

      if (!all(list.var %in% "sftlf")) {

        # Actualizamos el contador
        j <- j + 1

        message(paste0("\n------ Modelo ", j, ": ", cordex.model.i, " ------"))
        message("-------------------------------------------------------------------------------------\n")

        message(paste("- Lista de variables asociadas al DataFrame del modelo:", paste(list.var, collapse = ", "), "\n"))

        if (all(list.variables %in% list.var)) {
          df.model <- data.frame(df.model, row.names = NULL)

          # Guardamos el dataframe del modelo
          list.df.model[[paste0("model_", j)]] <- df.model

          # Comprobamos que variables dispone este modelo
          status_list_variables <- as.numeric(list.variables %in% df.model$variable)

          # Obtenemos el rango temporal de una variable del modelo
          data_range_i <- NULL
          for (variable_i in as.character(df.model$variable)) {
            if (is.null(data_range_i)) {
              url_data <- as.character(subset(df.model, variable == variable_i)$location)
              inventori <- dataInventory(url_data)
              data_range_i <- inventori[[variable_i]]$Dimensions$time$Date_range
            }
          }

          # If the time range has not been found...
          if (is.null(data_range_i)) {
            data_range_i <- "Not_appear"
          }

          # Generamos un archivo breve de metadatos
          results_model_i <- as.data.frame(t(c(paste0("model_", j), cordex.model.i, status_list_variables, data_range_i)))
          names(results_model_i) <- c("N_model", "name_model", list.variables, "data_range")

          # Save the dataframe
          results <- rbind(results, results_model_i)
        } else {

          # puede que sea r0i0p0: se busca sin ensemble
          df.model.sftlf <- subset(df.all, variable == "sftlf" &
            institution == as.character(df.model$institution) &
            model == as.character(df.model$model[1]) &
            experiment == as.character(df.model$experiment[1]) &
            rcm == as.character(df.model$rcm[1]) &
            rcm_version == as.character(df.model$rcm_version[1]))

          # puede que el modelo sea cpr85: buscamos historical y ensemble definido
          if (nrow(df.model.sftlf) == 0) {
            df.model.sftlf <- subset(df.all, variable == "sftlf" &
              institution == as.character(df.model$institution) &
              model == as.character(df.model$model[1]) &
              experiment == "historical" &
              ensemble == as.character(df.model$ensemble[1]) &
              rcm == as.character(df.model$rcm[1]) &
              rcm_version == as.character(df.model$rcm_version[1]))
          }

          # puede que el modelo sea cpr85: buscamos historical y sin ensemble
          if (nrow(df.model.sftlf) == 0) {
            df.model.sftlf <- subset(df.all, variable == "sftlf" &
              institution == as.character(df.model$institution) &
              model == as.character(df.model$model[1]) &
              experiment == "historical" &
              rcm == as.character(df.model$rcm[1]) &
              rcm_version == as.character(df.model$rcm_version[1]))
          }
          if (nrow(df.model.sftlf) == 0) {
            message("  ==> No se ha encontrado la variable 'sftlf' asociada al modelo.\n")
          }

          # Unimos en un único DataFrame toda la información asociada al modelo
          df.model <- rbind(df.model, df.model.sftlf)
          df.model <- data.frame(df.model, row.names = NULL)

          # Guardamos el dataframe del modelo
          list.df.model[[paste0("model_", j)]] <- df.model

          # Comprobamos que variables dispone este modelo
          status_list_variables <- as.numeric(list.variables %in% df.model$variable)

          # Obtenemos el rango temporal de una variable del modelo
          data_range_i <- NULL
          for (variable_i in as.character(df.model$variable)) {
            if (is.null(data_range_i)) {
              url_data <- as.character(subset(df.model, variable == variable_i)$location)
              inventori <- dataInventory(url_data)
              data_range_i <- inventori[[variable_i]]$Dimensions$time$Date_range
            }
          }

          # If the time range has not been found...
          if (is.null(data_range_i)) {
            data_range_i <- "Not_appear"
          }

          # Generamos un archivo breve de metadatos
          results_model_i <- as.data.frame(t(c(paste0("model_", j), cordex.model.i, status_list_variables, data_range_i)))
          names(results_model_i) <- c("N_model", "name_model", list.variables, "data_range")

          # Save the dataframe
          results <- rbind(results, results_model_i)
        }
      }
    },
    error = function(e) {
      message("\nERROR:", conditionMessage(e), "\n\n")

      archivo_errores <- file(path_errores, open = "a")
      cat(paste0("\nModelo ", j, ": ", cordex.model.i), file = archivo_errores, "\n", append = TRUE)
      cat(paste0("ERROR: ", conditionMessage(e)), file = archivo_errores, "\n", append = TRUE)
      close(archivo_errores)

    }
  )
}

# Guardamos los archivos
path_metadata_short <- file.path(path_Datos, "CordexDev", "Metadatos", "list_metadata_models_DEV")
path_metadata_df <- file.path(path_Datos, "CordexDev", "Metadatos", "df_metadata_models_DEV")

# Write the dataframe to a .txt and .Rdata file in the specified format
write.table(results, file = paste0(path_metadata_short, ".txt"), sep = ";", quote = FALSE, row.names = FALSE)
save(list.df.model, file = paste0(path_metadata_df, ".Rdata"))
