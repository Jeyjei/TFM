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
path_Datos <- file.path(getwd(), "Datos")
path_Codigo <- file.path(getwd(), "Codigo")
path_Datos_save <- file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos")


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


#' Cargamos las funciones definidas en los archivos .R de mi carpeta local sincronizadas en el GIT:
#' -----------------------------------------------------------------------------
#
# # Definir la ruta de la carpeta local que contiene los archivos .R
# ruta_git <- "C:/Users/juan-/Documents/4. Git_TFM/climate4R.indices"
# # Obtener la lista de archivos .R en la carpeta local y sus subcarpetas
# archivos_git <- list.files(path = ruta_git, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
# # Cargar cada archivo y todas sus funciones usando la función lapply() y source()
# lapply(archivos_git, source)


#' Cargamos las funciones modificadas sin sincronizar en el GIT
#' -----------------------------------------------------------------------------

file_funciones_mod <- file.path(path_Codigo, "00_funciones_modificadas.R")
lapply(file_funciones_mod, source)


#' =============================================================================
#' ============================= MODELOS DE CORDEX =============================
#' =============================================================================

#' Generamos los METADATOS de los modelos de CORDEX
#' -----------------------------------------------------------------------------

# Obtenemos la lista de simulaciones de EURO-CORDEX a resolución 0.11ºx0.11º
# list.cordex11 <- UDG.datasets("CORDEX-EUR-11")

# # Seleccionar los modelos históricos
# list.historical.cordex11 <- list.cordex11$CORDEX[grep("_historical_", list.cordex11$CORDEX)]

# # Seleccionamos los modelos "rcp85"
# list.rcp85.cordex11 <- list.cordex11$CORDEX[grep("_rcp85_", list.cordex11$CORDEX)]

# # Obtenemos los metadatos de los modelos y verificamos si existen las variables de interés
# results <- metadata_model(
#   list_models = c(list.historical.cordex11, list.rcp85.cordex11),
#   list_variables = c("pr", "tas", "tasmax", "tasmin", "sftlf"),
#   save_file = "off"
# )

# Directorio de los datos referenciado al path de este archivo
# path_metadata_models <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
#                          "Datos", "Cordex", "Metadatos")
# path_metadata_models <- file.path(path_Datos, "Cordex", "Metadatos")

# filename <- file.path(path_metadata_models, "list_metadata_models_excluidos")

# # Escribir el data frame en un archivo .txt con el formato especificado
# write.table(results,
#             file = paste0(filename, ".txt"), sep = ";", quote = FALSE,
#             row.names = FALSE
# )


#' Cargamos los metadatos de los modelos de interés
#' -----------------------------------------------------------------------------

# path_metadata_cordex <- file.path(
#   dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
#   "Datos", "Cordex"
# )

# Abrimos el archivo de metadatos de los modelos de interés
path_metadata_models <- file.path(path_Datos, "CordexDev", "Metadatos")
list.metadata.cordex <- file.path(path_metadata_models, "list_metadata_models_DEV.txt") %>%
  read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    na.strings = ""
  )

# Tenemos simulaciones históricas y sus correspondientes RCP8.5, que se
# forman con la combinación de 7 RCMs anidados de distinta manera a 5 GCMs.

# Lista de modelos con las variables de interés: "pr", "tas", "tasmax", "tasmin", "c"
list.model.cordex <- subset(
  list.metadata.cordex,
  pr == 1 & tas == 1 & tasmax == 1 & tasmin == 1 & sftlf == 1
)

#' Guardamos el DataFrame en un archivo .txt con los modelos que cumplen las
#' condiciones
#' -----------------------------------------------------------------------------
filename <- file.path(path_Datos_save, "CordexDev", "list_models_Dev")
write.table(list.model.cordex,
  file = paste0(filename, ".txt"), sep = ";", quote = FALSE,
  row.names = FALSE
)


#' =============================================================================
#' ============================ ARCHIVO DE ERRORES =============================
#' =============================================================================

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
cat(paste0(
  "\n\n=======================================================================================================================\n",
  "================================================= ", as.character(format(as.POSIXct(Sys.time()) + as.difftime(2, units = "hours"))), " =================================================\n",
  "======================================================================================================================="
),
file = archivo_errores, "\n", append = TRUE
)
close(archivo_errores)


#' =============================================================================
#' =========================== VARIABLES NECESARIAS ============================
#' =============================================================================

#' Lista de las variables que se van a cargar y utilizar para calcular el índice
#' -----------------------------------------------------------------------------

# Formato de las imágenes
img.ext <- ".png" # pdf ó png

# Nombre de las variables utilizados en los .Rdata y modelos CORDEX
list.variables <- c("pr", "tasmin", "tasmax", "tas")

# Nombre de dichas variables según los argumentos de la función 'indexGrid'
names.array.indexGrid <- c("pr", "tn", "tx", "tm") # c("tn", "tx", "tm", "pr")


#' =============================================================================
#' =========================== PARA CADA MODELO ... ============================
#' =============================================================================

#' Leemos solo el dominio que incluya la Península Ibérica y todos nuestros
#' años de estudio
#' -----------------------------------------------------------------------------

for (index.model in 1:nrow(list.model.cordex)) { # nrow(list.model.cordex) # c(25, 26, 27, 28, 30)

  # Cargamos la lista de DataFrames de los modelos
  load(file.path(path_metadata_models, "df_metadata_models_DEV.Rdata"))

  # Seleccionamos el modelo
  N_model.i <- list.model.cordex$N_model[index.model]
  df.i <- list.df.model[[N_model.i]]
  cordex.model.i <- as.character(df.i$longname[1])

  rm(list.df.model)
  gc()

  # Generamos (si no existe) la carpeta que contendrá los datos del modelo
  folder <- file.path(path_Datos_save, "CordexDev", N_model.i)
  if (!file.exists(folder)) {
    dir.create(folder)
    message(paste0("==> Se ha creado la carpeta: ", N_model.i))
  }

  # Generamos (si no existe) la carpeta que contendrá las imágenes asociadas al modelo
  path_filename_fig1 <- file.path(path_Datos_save, "CordexDev", N_model.i, paste0("plots_", N_model.i))
  if (!file.exists(path_filename_fig1)) {
    dir.create(path_filename_fig1)
    message(paste0("==> Se ha creado la carpeta: ", N_model.i, "/plots_", N_model.i))
  }



  message("\n\n---- ", N_model.i, ": ", cordex.model.i, " ----")
  message("--------------------------------------------------------------------------------------------\n")

  tryCatch(
    {
      #' ===========================================================================
      #' ================= CALCULAR LA MÁSCARA DEL MODELO CORDEX ===================
      #' ===========================================================================

      #' Calculamos la máscara de mar-tierra del modelo
      #' ---------------------------------------------------------------------------

      # Los modelos tienen datos en el mar y en la tierra pero las observaciones
      # solo en tierra. Hay que aplicar una máscara de mar-tierra (variable sftlf
      # de cada modelo) para garantizar que no corregimos un punto de mar de modelo
      # con la observación.
      # Los modelos rcp85 no contienen la variable sftlf, por eso utilizamos sus
      # correspondientes en históricos para generar la máscara mar-tierra
      if (grepl("rcp85", cordex.model.i)) { # rcp85 models

        # Metemos todo el código....
        # si no se puede guardar en la variable grid_p, mejor guardar
        # diferentes dataframes y usar un for...
        URL_sftlf <- as.character(subset(df.i, variable == "sftlf")$location)
        mask.model.i <- loadGridData(URL_sftlf,
          var = "sftlf",
          lonLim = c(-10, 5),
          latLim = c(35, 44)
        )

        # OJO! puede que en el algún modelo venga en % en lugar de tanto por 1.
        # spatialPlot(mask.model.i) # porcentaje de tierra de cada pixel
        mask.model.i$Data[mask.model.i$Data < 0.4 * max(na.omit(mask.model.i$Data))] <- NA
        mask.model.i$Data[mask.model.i$Data >= 0.4 * max(na.omit(mask.model.i$Data))] <- 1

        # message(str(mask.model.i))
        message(paste0("\n==> Máscara tierra-mar del modelo ha sido creada: max = ", max(na.omit(mask.model.i$Data)), "\n"))


        # Como en estos modelos son muchos años, se aborda por secciones.
        # Intervalo de años de interés
        if (index.model %in% c(18, 23, 29)) {
          all_years <- 2006:2099
        } else {
          all_years <- 2006:2100
        }


        # Secciones de esos años para poder operar y trabajar en memoria
        c_part <- 9
        sub_years <- split(all_years, cut(all_years, breaks = c_part, labels = FALSE))

        # Obtenemos un grid para cada sección de años
        # grid_p <- lapply(1:c_part, function(i.part) {
        # })

        #' Preprocesamos cada variable de forma individual
        #' ---------------------------------------------------------------------------
        #' 1º Aplicamos la máscara tierra-mar del modelo.
        #' 2º Aplicamos el interpolado con la variable observada (en este caso Iberia01).
        #' 3º Aplicamos la máscara tierra-mar de la variable observada sobre la variable
        #'    interpolada del modelo.

        for (variable_i in list.variables) {
          for (i.part in 1:c_part) {

            # Obtenemos la sección de años con la que vamos a operar
            int_years <- sub_years[[i.part]]
            message(paste0("\n========> Variable: ", variable_i, " - ", int_years[1], " to ", int_years[length(int_years)], " <========"))

            # Lista de las variables del modelo tras el pre-procesado.
            list.arg.model <- list()

            # Cargamos los datos de la variable del modelo
            URL_variable <- as.character(subset(df.i, variable == variable_i)$location)
            variable_i.model <- loadGridData(URL_variable,
              var = variable_i,
              years = int_years,
              lonLim = c(-10, 5),
              latLim = c(35, 44)
            )
            # Ploteamos la imagen de los datos
            filename_fig1 <- file.path(path_filename_fig1, paste0("02_", variable_i, "_", i.part, "_01_original", img.ext))

            fig1 <- spatialPlot(climatology(variable_i.model),
              backdrop.theme = "coastline", rev.colors = TRUE,
              main = paste0(N_model.i, ": ", variable_i, " (", getGridUnits(variable_i.model), ") de ", int_years[1], " a ", int_years[length(int_years)])
            )
            if (img.ext == ".png") {
              png(filename_fig1)
            } else {
              pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
            }
            print(fig1)
            dev.off()
            message(paste0("Se guarda la imagen: ", filename_fig1))
            rm(fig1, filename_fig1)



            #' Multiplicamos los datos del modelo por su máscara tierra-mar.
            #' -------------------------------------------------------------------------

            # Pondrá NA en los puntos de mar. Se hace una multiplicación día a día de
            # los datos del modelo por la máscara.
            time <- getRefDates(variable_i.model)
            nt <- length(time)
            ls <- lapply(1:nt, function(i) {
              timei <- subsetDimension(variable_i.model, dimension = "time", indices = i) # separa día a día
              gridArithmetics(timei, mask.model.i, operator = "*") # multiplica los dos grids
            })
            rm(variable_i.model, time, nt)
            gc()

            # `bindGrid` es una función de transforeR que vuelve a unir todos los días
            # en un grid de climate4R
            variable_i.model.masked <- bindGrid(ls, dimension = "time")

            # Eliminamos las variables que no usaremos
            rm(ls)
            gc()

            message("\n==> Máscara tierra-mar del modelo aplicada sobre la variable.\n")

            # Ploteamos la imagen de los datos
            fig1 <- spatialPlot(climatology(variable_i.model.masked),
              backdrop.theme = "coastline", rev.colors = TRUE,
              main = paste0(N_model.i, ": ", variable_i, " (", getGridUnits(variable_i.model.masked), ") de ", int_years[1], " a ", int_years[length(int_years)])
            )

            filename_fig1 <- file.path(path_filename_fig1, paste0("02_", variable_i, "_", i.part, "_02_mask_model", img.ext))
            if (img.ext == ".png") {
              png(filename_fig1)
            } else {
              pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
            }
            print(fig1)
            dev.off()
            message(paste0("Se guarda la imagen: ", filename_fig1))
            rm(fig1, filename_fig1)


            #' =========================================================================
            #' ======= INTERPOLACIÓN DEL MODELO DE CORDEX CON LAS OBSERVACIONES ========
            #' =========================================================================

            #' Cargamos la variable observada
            #' -------------------------------------------------------------------------

            # Directorio de los datos referenciado al path de este archivo
            # path_data <- file.path(
            #   dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
            #   "Datos", "iberia01"
            # )
            path_data <- file.path(path_Datos, "iberia01")

            # Variable de Iberia01 (observación)
            message(paste0("\n==> Cargamos: ", file.path(path_data, paste0(variable_i, "_iberia01_1986-2005.Rdata"))))
            load(file.path(path_data, paste0(variable_i, "_iberia01_1986-2005.Rdata")))

            message(paste0("==> Se ha cargado el archivo: ", variable_i, "_iberia01_1986-2005.Rdata\n"))

            obs <- data
            rm(data)
            gc()
            # str(obs)

            #' Interpolar los datos del modelo a la malla de la observación
            #' -------------------------------------------------------------------------
            variable_i.model.interp <- interpGrid(variable_i.model.masked,
              new.coordinates = getGrid(obs)
            )
            # Eliminamos las variables que no necesitamos
            rm(variable_i.model.masked)
            gc()
            message("\n==> Se ha interpolado los datos del modelo a la malla de observación")

            # Ploteamos la imagen de los datos
            fig1 <- spatialPlot(climatology(variable_i.model.interp),
              backdrop.theme = "coastline", rev.colors = TRUE,
              main = paste0(N_model.i, ": ", variable_i, " (", getGridUnits(variable_i.model.interp), ") de ", int_years[1], " a ", int_years[length(int_years)])
            )

            filename_fig1 <- file.path(path_filename_fig1, paste0("02_", variable_i, "_", i.part, "_03_interp_obs", img.ext))
            if (img.ext == ".png") {
              png(filename_fig1)
            } else {
              pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
            }
            print(fig1)
            dev.off()
            message(paste0("Se guarda la imagen: ", filename_fig1))
            rm(fig1, filename_fig1)


            #' =========================================================================
            #' ===== APLICAR LA MÁSCARA DE LAS OBSERVACIONES AL MODELO INTERPOLADO =====
            #' =========================================================================

            #' Calculamos la máscara de mar-tierra de la variable observada
            #' ---------------------------------------------------------------------------

            # Necesitamos la máscara de tierra-mar de las observaciones para filtrar los
            # datos de modelo después de interpolarlos
            mask.obs <- climatology(obs)
            mask.obs$Data[!is.na(mask.obs$Data)] <- 1

            # Eliminamos las variables que no necesitamos
            rm(obs)
            gc()

            # Filtrar los puntos de la máscara de la observación mask.obs (para que no
            # salgan Francia y Marruecos, donde no hay observación)
            time <- getRefDates(variable_i.model.interp)
            nt <- length(time)
            ls <- lapply(1:nt, function(i) {
              timei <- subsetDimension(variable_i.model.interp, dimension = "time", indices = i)
              gridArithmetics(timei, mask.obs, operator = "*")
            })
            rm(variable_i.model.interp, time, nt, mask.obs)
            gc()

            message("\n==> Se ha aplicado la máscara de observaciones al modelo interpolado")

            # Lista con las variables del modelo interpoladas a las observaciones y
            # con la malla de las observaciones aplicada
            list.arg.model[[variable_i]] <- bindGrid(ls, dimension = "time")
            message("==> Se guarda el restuldado en la variable list.arg.model")

            # Eliminamos las variables que no necesitamos
            rm(ls)
            gc()

            # Ploteamos la imagen de los datos
            fig1 <- spatialPlot(climatology(list.arg.model[[variable_i]]),
              backdrop.theme = "coastline", rev.colors = TRUE,
              main = paste0(N_model.i, ": ", variable_i, " (", getGridUnits(list.arg.model[[variable_i]]), ") de ", int_years[1], " a ", int_years[length(int_years)])
            )

            filename_fig1 <- file.path(path_filename_fig1, paste0("02_", variable_i, "_", i.part, "_04_Interp_mask_obs", img.ext))
            if (img.ext == ".png") {
              png(filename_fig1)
            } else {
              pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
            }
            print(fig1)
            dev.off()
            message(paste0("Se guarda la imagen: ", filename_fig1))
            rm(fig1, filename_fig1)

            # Guardamos la variable interpolada
            message("\nComienza el guardado de dicha variable en archivo .Rdata")

            filename <- file.path(folder, paste0(variable_i, "_", i.part, "_interpol_Dev"))
            save(list.arg.model, file = paste0(filename, ".Rdata"))
            rm(list.arg.model)
            gc()

            message(paste0("===> Se ha guardado satisfactoriamente el archivo: ", filename, ".Rdata"))
          }
        }

        # Eliminamos las variables que no necesitamos
        rm(mask.model.i)
        gc()

        # `bindGrid` es una función de transforeR que vuelve a unir todos los días
        # en un grid de climate4R
        # variable_i.model.masked <- bindGrid(grid_p, dimension = "time")
      }

      # historical models
      else {
        URL_sftlf <- as.character(subset(df.i, variable == "sftlf")$location)
        mask.model.i <- loadGridData(URL_sftlf,
          var = "sftlf",
          lonLim = c(-10, 5),
          latLim = c(35, 44)
        )

        # OJO! puede que en el algún modelo venga en % en lugar de tanto por 1.
        # spatialPlot(mask.model.i) # porcentaje de tierra de cada pixel
        mask.model.i$Data[mask.model.i$Data < 0.4 * max(na.omit(mask.model.i$Data))] <- NA
        mask.model.i$Data[mask.model.i$Data >= 0.4 * max(na.omit(mask.model.i$Data))] <- 1

        # message(str(mask.model.i))
        message(paste0("\n==> Máscara tierra-mar del modelo ha sido creada: max = ", max(na.omit(mask.model.i$Data)), "\n"))

        all_years <- 1986:2005

        # Secciones de esos años para poder operar y trabajar en memoria
        c_part <- 2
        if (c_part == 1) {
          sub_years <- list()
          sub_years[[1]] <- all_years
        } else {
          sub_years <- list()
          sub_years <- split(all_years, cut(all_years, breaks = c_part, labels = FALSE))
        }

        #' Preprocesamos cada variable de forma individual
        #' ---------------------------------------------------------------------------
        #' 1º Aplicamos la máscara tierra-mar del modelo.
        #' 2º Aplicamos el interpolado con la variable observada (en este caso Iberia01).
        #' 3º Aplicamos la máscara tierra-mar de la variable observada sobre la variable
        #'    interpolada del modelo.

        for (variable_i in list.variables) {
          for (i.part in 1:c_part) {

            # Obtenemos la sección de años con la que vamos a opearar
            int_years <- sub_years[[i.part]]

            message(paste0("\n========> Variable: ", variable_i, " - ", int_years[1], " to ", int_years[length(int_years)], " <========"))
            # Lista de las variables del modelo tras el pre-procesado.
            list.arg.model <- list()

            URL_variable <- as.character(subset(df.i, variable == variable_i)$location)
            variable_i.model <- loadGridData(URL_variable,
              var = variable_i,
              years = int_years,
              lonLim = c(-10, 5),
              latLim = c(35, 44)
            )

            # Ploteamos la imagen de los datos
            filename_fig1 <- file.path(path_filename_fig1, paste0("02_", variable_i, "_", i.part, "_01_original", img.ext))

            fig1 <- spatialPlot(climatology(variable_i.model),
              backdrop.theme = "coastline", rev.colors = TRUE,
              main = paste0(N_model.i, ": ", variable_i, " (", getGridUnits(variable_i.model), ") de ", int_years[1], " a ", int_years[length(int_years)])
            )
            if (img.ext == ".png") {
              png(filename_fig1)
            } else {
              pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
            }
            print(fig1)
            dev.off()
            message(paste0("Se guarda la imagen: ", filename_fig1))
            rm(fig1, filename_fig1)

            #' Multiplicamos los datos del modelo por su máscara tierra-mar.
            #' -------------------------------------------------------------------------

            # Pondrá NA en los puntos de mar. Se hace una multiplicación día a día de
            # los datos del modelo por la máscara.
            time <- getRefDates(variable_i.model)
            nt <- length(time)
            ls <- lapply(1:nt, function(i) {
              timei <- subsetDimension(variable_i.model, dimension = "time", indices = i) # separa día a día
              gridArithmetics(timei, mask.model.i, operator = "*") # multiplica los dos grids
            })
            rm(variable_i.model, time, nt)
            gc()

            # `bindGrid` es una función de transforeR que vuelve a unir todos los días
            # en un grid de climate4R
            variable_i.model.masked <- bindGrid(ls, dimension = "time")

            # Eliminamos las variables que no usaremos
            rm(ls)
            gc()

            message("\n==> Máscara tierra-mar del modelo aplicada sobre la variable.\n")

            # Ploteamos la imagen de los datos
            fig1 <- spatialPlot(climatology(variable_i.model.masked),
              backdrop.theme = "coastline", rev.colors = TRUE,
              main = paste0(N_model.i, ": ", variable_i, " (", getGridUnits(variable_i.model.masked), ") de ", int_years[1], " a ", int_years[length(int_years)])
            )

            filename_fig1 <- file.path(path_filename_fig1, paste0("02_", variable_i, "_", i.part, "_02_mask_model", img.ext))
            if (img.ext == ".png") {
              png(filename_fig1)
            } else {
              pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
            }
            print(fig1)
            dev.off()
            message(paste0("Se guarda la imagen: ", filename_fig1))
            rm(fig1, filename_fig1)


            #' =========================================================================
            #' ======= INTERPOLACIÓN DEL MODELO CON LA MALLA DE LA OBSERVACIONES =======
            #' =========================================================================

            #' Cargamos la variable observada
            #' -------------------------------------------------------------------------

            # Directorio de los datos referenciado al path de este archivo
            # path_data <- file.path(
            #   dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
            #   "Datos", "iberia01"
            # )
            path_data <- file.path(path_Datos, "iberia01")

            # Variable de Iberia01 (observación)
            message(paste0("\n==> Cargamos: ", file.path(path_data, paste0(variable_i, "_iberia01_1986-2005.Rdata"))))
            load(file.path(path_data, paste0(variable_i, "_iberia01_1986-2005.Rdata")))

            message(paste0("==> Se ha cargado el archivo: ", paste0(variable_i, "_iberia01_1986-2005.Rdata\n")))

            obs <- data
            rm(data)
            gc()
            # str(obs)

            #' Interpolar los datos del modelo a la malla de la observación
            #' -------------------------------------------------------------------------
            variable_i.model.interp <- interpGrid(variable_i.model.masked,
              new.coordinates = getGrid(obs)
            )
            # Eliminamos las variables que no necesitamos
            rm(variable_i.model.masked)
            gc()
            message("\n==> Se ha interpolado los datos del modelo a la malla de observación")

            # Ploteamos la imagen de los datos
            fig1 <- spatialPlot(climatology(variable_i.model.interp),
              backdrop.theme = "coastline", rev.colors = TRUE,
              main = paste0(N_model.i, ": ", variable_i, " (", getGridUnits(variable_i.model.interp), ") de ", int_years[1], " a ", int_years[length(int_years)])
            )

            filename_fig1 <- file.path(path_filename_fig1, paste0("02_", variable_i, "_", i.part, "_03_interp_obs", img.ext))
            if (img.ext == ".png") {
              png(filename_fig1)
            } else {
              pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
            }
            print(fig1)
            dev.off()
            message(paste0("Se guarda la imagen: ", filename_fig1))
            rm(fig1, filename_fig1)


            #' =========================================================================
            #' ===== APLICAR LA MÁSCARA DE LAS OBSERVACIONES AL MODELO INTERPOLADO =====
            #' =========================================================================

            #' Calculamos la máscara de mar-tierra de la variable observada
            #' ---------------------------------------------------------------------------

            # Necesitamos la máscara de tierra-mar de las observaciones para filtrar los
            # datos de modelo después de interpolarlos
            mask.obs <- climatology(obs)
            mask.obs$Data[!is.na(mask.obs$Data)] <- 1

            # Eliminamos las variables que no necesitamos
            rm(obs)
            gc()

            # Filtrar los puntos de la máscara de la observación mask.obs (para que no
            # salgan Francia y Marruecos, donde no hay observación)
            time <- getRefDates(variable_i.model.interp)
            nt <- length(time)
            ls <- lapply(1:nt, function(i) {
              timei <- subsetDimension(variable_i.model.interp, dimension = "time", indices = i)
              gridArithmetics(timei, mask.obs, operator = "*")
            })
            rm(variable_i.model.interp, time, nt, mask.obs)
            gc()

            message("\n==> Se ha aplicado la máscara de observaciones al modelo interpolado")

            # Lista con las variables del modelo interpoladas a las observaciones y
            # con la malla de las observaciones aplicada
            list.arg.model[[variable_i]] <- bindGrid(ls, dimension = "time")
            message("==> Se guarda el resultado en la variable list.arg.model")

            # Eliminamos las variables que no necesitamos
            rm(ls)
            gc()

            # Ploteamos la imagen de los datos
            fig1 <- spatialPlot(climatology(list.arg.model[[variable_i]]),
              backdrop.theme = "coastline", rev.colors = TRUE,
              main = paste0(N_model.i, ": ", variable_i, " (", getGridUnits(list.arg.model[[variable_i]]), ") de ", int_years[1], " a ", int_years[length(int_years)])
            )

            filename_fig1 <- file.path(path_filename_fig1, paste0("02_", variable_i, "_", i.part, "_04_Interp_mask_obs", img.ext))
            if (img.ext == ".png") {
              png(filename_fig1)
            } else {
              pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
            }
            print(fig1)
            dev.off()
            message(paste0("Se guarda la imagen: ", filename_fig1))
            rm(fig1, filename_fig1)

            # Guardamos la variable interpolada
            message("\nComienza el guardado de dicha variable en archivo .Rdata")

            filename <- file.path(folder, paste0(variable_i, "_", i.part, "_interpol_Dev"))
            save(list.arg.model, file = paste0(filename, ".Rdata"))
            rm(list.arg.model)
            gc()

            message(paste0("===> Se ha guardado satisfactoriamente el archivo: ", filename, ".Rdata"))
          }
        }

        # Eliminamos las variables que no necesitamos
        rm(mask.model.i)
        gc()

        # Renombramos los nombres de las listas en función de los argumentos de "indexGrid"
        # names(list.arg.model) <- names.array.indexGrid

        # Guardamos las variables interpoladas
        # filename <- file.path(path_Datos, "Cordex", paste0("Interp_M_", index.model))
        # save(list.arg.model, file = paste0(filename, ".Rdata"))
        # rm(list.arg.model)
      }
    },
    error = function(e) {
      message("\nERROR: ", conditionMessage(e), "\n\n")

      archivo_errores <- file(path_errores, open = "a")
      cat(paste0("\n", N_model.i, ": ", cordex.model.i),
        file = archivo_errores, "\n", append = TRUE
      )
      close(archivo_errores)

      tryCatch(
        {
          archivo_errores <- file(path_errores, open = "a")
          cat(paste0(variable_i, "(", int_years[1], "-", int_years[length(int_years)], ")"),
            file = archivo_errores, "\n", append = TRUE
          )
          close(archivo_errores)
        },
        error = function(e2) {
          archivo_errores <- file(path_errores, open = "a")
          cat("Error al importar la máscara tierra-mar del modelo",
            file = archivo_errores, "\n", append = TRUE
          )
          close(archivo_errores)
        }
      )

      archivo_errores <- file(path_errores, open = "a")
      cat(paste0("ERROR: ", conditionMessage(e)),
        file = archivo_errores, "\n", append = TRUE
      )
      close(archivo_errores)
    }
  )
}

# Cerrar el archivo
close(archivo_errores)


# Guardamos los resultados
# filename <- file.path(path_Datos, "Cordex", paste0("indice_", index.code.i))
# save(list.agroindex, file = paste0(filename, ".Rdata"))
# rm(list.agroindex)
