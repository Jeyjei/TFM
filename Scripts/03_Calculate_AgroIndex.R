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
path_Datos <- file.path(getwd(), "Datos") # file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") 
path_Codigo <- file.path(getwd(), "Scripts") # file.path(getwd(), "Codigo")
path_Datos_save <- path_Datos # file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") # path_Datos 


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

# Opción para pasar las variables a otra unidad previamente de calcular el índice
option_convert <- TRUE

# Nombre de la función atómica del ínidice agroclimático
index.code <- list("HI", "BEDD", "BBLI") # "GDD_WI", "BEDD"

# Lista con los argumentos añadidos para calcular los índices
list.arg.any <- list()

# Lista donde se guarda el cálculo del índice agroclimático
list.agroindex <- list()

# Lista de modelos a evaluar (todos los que aparece en la carpeta CordexDev)
model_folder <- list.dirs(file.path(path_Datos, "CordexDev"), recursive = FALSE)
model_folder <- model_folder[grepl(paste0("^" ,"model_1", "$"), basename(model_folder), ignore.case = TRUE)] # ("^" ,"model_1", "$")

# Variable de Iberia01 (observación)
model_folder <- c(file.path(getwd(), "Datos", "iberia01"), model_folder)

# Imprimir los nombres de los modelos
message("Los modelos que se van a utilizar son:")
for (model_i in model_folder) {
  message(paste0("+ ", basename(model_i)))
}

# Leemos los metadatos de los índices agroclimáticos
metadata <- indexShow()


#' ===========================================================================
#' ========================== PARA CADA MODELO ... ===========================
#' ===========================================================================

# Seleccionamos el modelo
for (path.model.i in model_folder) {
  tryCatch(
    {
      message(paste0("\n==================================== ", basename((path.model.i)), " ===================================="))
      message("==================================================================================")

      # Generamos (si no existe) la carpeta que contendrá las imágenes asociadas al modelo
      path_filename_fig1 <- file.path(path.model.i, paste0("plots_", basename((path.model.i))))
      if (!file.exists(path_filename_fig1)) {
        dir.create(path_filename_fig1)
        message(paste0("==> Se ha creado la carpeta:", basename(path.model.i), "/plots_", basename(path.model.i)))
      }

      # Generamos (si no existe) la carpeta que contendrá los datos asociadas a los índices
      path_filename_indice <- file.path(path.model.i, paste0("agroindex_", basename((path.model.i))))
      if (!file.exists(path_filename_indice)) {
        dir.create(path_filename_indice)
        message(paste0("==> Se ha creado la carpeta:", basename(path.model.i), "/agroindex_", basename(path.model.i)))
      }

      # Lista de archivos .Rdata en el directorio
      filesRdata <- list.files(path.model.i, pattern = "\\.Rdata$", full.names = TRUE)
      # message(path.model.i)
      # message(filesRdata)

      #' =============================================================================
      #' =========================== PARA CADA ÍNDICE ... ============================
      #' =============================================================================

      for (index.code.i in index.code) {
        message(paste0("\n======================= ", index.code.i, " ======================="))

        # Metadatos del índice agroclimático i
        df_i <- subset(metadata, code == index.code.i)
        var.name <- c("tasmin", "tasmax", "tas", "pr", "any")[which(as.logical(df_i[, 4:8]))]
        var.s <- c("tn", "tx", "tm", "pr", "any")[which(as.logical(df_i[, 4:8]))]
        message(paste("Variables utilizadas:", paste(var.name, collapse = ", ")))

        if (length(filesRdata) != 0) {

          # Obtenemos los paths de las variables que intervienen en el índice.
          # Una variable puede tener varios archivos .Rdata asociados
          var.files <- list()
          for (var.name.i in var.name) {
            var.files[[var.name.i]] <- filesRdata[grepl(paste0("^", var.name.i, "_"), basename(filesRdata))]
          }


          #' =========================================================================
          #' ============== PARA CADA SECCIÓN DE TIEMPO DEL MODELO ... ===============
          #' =========================================================================

          # Lista donde se guardan los valores de 1 índice (tendrá varios argumentos str() si
          # las variables están disgregadas en varios archivos .Rdata)
          list.agroindex <- list()

          # Todas las variables deben tener el mismo número de archivos asociados!!!
          if (length(var.files[[var.name[1]]]) != 0) {

            # Para aquellos modelos que los datos de cada variable se encuentra en varios archivos
            for (i.part in 1:length(var.files[[var.name[1]]])) {

              # Lista de argumetos para una sección i de años
              list.arg <- list()
              list.units <- list()

              # Generamos la lista de argumentos de la función indexGrid
              for (var.i in 1:length(var.name)) {
                # message(paste("Se carga:", var.files[[var.name[var.i]]][i.part]))
                preview_data <- c(ls(), "preview_data")
                load(var.files[[var.name[var.i]]][i.part]) # Se carga la variable
                name_data_load <- setdiff(ls(), preview_data) # Nombre de la variable (string)
                if (length(get(name_data_load)) == 1) {
                  list.arg[[var.s[var.i]]] <- get(name_data_load)[[names(get(name_data_load))]]
                } else {
                  list.arg[[var.s[var.i]]] <- get(name_data_load)
                }

                # message(str(list.arg))
                # Eliminamos las variables que no necesitamos
                rm(list = name_data_load)
                gc()


                if (var.i == 1) {
                  int_years <- unique(getYearsAsINDEX(list.arg[[var.s[var.i]]]))
                  message(paste0("\n--> Intervalo: ", int_years[1], " to ", int_years[length(int_years)]))
                }

                # Convertimos las dimensiones de las variables a ºC y kg m-2 si así lo queremos
                if (option_convert) {

                  # Temperaturas
                  if (var.name[var.i] %in% c("tasmin", "tasmax", "tas")) {
                    if (getGridUnits(list.arg[[var.s[var.i]]]) == "K") {
                      message(paste0("\t==> Variable ", var.name[var.i], ": units "), getGridUnits(list.arg[[var.s[var.i]]]), " to ºC")
                      nt <- length(getRefDates(list.arg[[var.s[var.i]]]))
                      ls <- lapply(1:nt, function(i) {
                        timei <- subsetDimension(list.arg[[var.s[var.i]]], dimension = "time", indices = i) # separa día a día
                        gridArithmetics(timei, 273.15, operator = "-") # Restamos 273.15 K para pasar a ºC
                      })
                      list.arg[[var.s[var.i]]] <- NULL
                      list.arg[[var.s[var.i]]] <- bindGrid(ls, dimension = "time") # bindGrid es una función de transforeR que vuelve a unir todos los días en un grid de climate4R
                      rm(ls, nt)
                      list.units[[var.s[var.i]]] <- "degreeC"
                    } else {
                      list.units[[var.s[var.i]]] <- getGridUnits(list.arg[[var.s[var.i]]])
                    }
                  }

                  # precipitación
                  if (var.name[var.i] %in% c("pr")) {
                    if (getGridUnits(list.arg[[var.s[var.i]]]) == "kg m-2 s-1") {
                      message(paste0("\t==> Variable ", var.name[var.i], ": units "), getGridUnits(list.arg[[var.s[var.i]]]), " to kg m-2")
                      nt <- length(getRefDates(list.arg[[var.s[var.i]]]))
                      ls <- lapply(1:nt, function(i) {
                        timei <- subsetDimension(list.arg[[var.s[var.i]]], dimension = "time", indices = i) # separa día a día
                        gridArithmetics(timei, 86400, operator = "*") # multiplica los segundos de un día para pasar a kg m-2
                      })
                      list.arg[[var.s[var.i]]] <- NULL
                      list.arg[[var.s[var.i]]] <- bindGrid(ls, dimension = "time") # bindGrid es una función de transforeR que vuelve a unir todos los días en un grid de climate4R
                      rm(ls, nt)
                      list.units[[var.s[var.i]]] <- "kg m-2"
                    } else {
                      list.units[[var.s[var.i]]] <- getGridUnits(list.arg[[var.s[var.i]]])
                    }
                  }
                }

                # Ploteamos la imagen de los datos
                filename_fig1 <- file.path(path_filename_fig1, basename(sub("\\.Rdata$", paste0("_UNITS", img.ext), var.files[[var.name[var.i]]][i.part])))
                # filename_fig1 <- file.path(path_filename_fig1, paste0("03_", basename(sub("\\.Rdata$", paste0("_UNITS", img.ext),  var.files[[var.name[var.i]]][i.part]))))

                fig1 <- spatialPlot(climatology(list.arg[[var.s[var.i]]]),
                  backdrop.theme = "coastline", rev.colors = TRUE,
                  main = paste0(var.name[var.i], " [", list.units[[var.s[var.i]]], "] de ", int_years[1], " a ", int_years[length(int_years)])
                )
                if (img.ext == ".png") {
                  png(filename_fig1)
                } else {
                  pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                }
                print(fig1)
                dev.off()
                message(paste0("Se guarda la imagen: ", (filename_fig1), "\n"))
                rm(fig1, filename_fig1)
              }

              # Tenemos las variables necesarias para el cálculo del índice, para la sección de tiempo i.part
              # y para el modelo path.model.i
              tryCatch(
                {


                  # Resto de argumentos para el cálculo del índice agroclimático
                  list.arg[["time.resolution"]] <- "year"
                  list.arg[["index.code"]] <- index.code.i
                  list.arg <- c(list.arg, list.arg.any)

                  #' Calculamos el índice con la función indexGrid
                  #' -----------------------------------------------------------------------
                  
                  list.agroindex[[paste0("part_", i.part)]] <- do.call("indexGrid", list.arg)

                  # Eliminamos las variables que no necesitamos
                  rm(list.arg)
                  gc()

                  # Ploteamos la imagen de los datos
                  filename_fig1 <- file.path(path_filename_fig1, paste0("03_", "AgroIndex_", index.code.i, "_", i.part, img.ext))

                  fig1 <- spatialPlot(climatology(list.agroindex[[paste0("part_", i.part)]]),
                    backdrop.theme = "coastline", rev.colors = TRUE,
                    main = paste0(basename((path.model.i)), ": ", index.code.i, " de ", int_years[1], " a ", int_years[length(int_years)])
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
                },
                error = function(e) {
                  message("\nERROR: ", conditionMessage(e), "\n\n")
                  archivo_errores <- file(path_errores, open = "a")
                  cat(paste0("\nModelo: ", basename((path.model.i))), file = archivo_errores, "\n", append = TRUE)
                  cat(paste0("ERROR: ", conditionMessage(e)), file = archivo_errores, "\n", append = TRUE)
                  close(archivo_errores)

                  tryCatch(
                    {
                      int_years <- unique(getYearsAsINDEX(list.arg[[var.s[var.i]]]))
                      archivo_errores <- file(path_errores, open = "a")
                      cat(paste0("Error en el intervalo ", i.part, " de la variable ", variable_i, " (", int_years[1], "-", int_years[length(int_years)], ")"),
                        file = archivo_errores, "\n", append = TRUE
                      )
                      close(archivo_errores)
                    },
                    error = function(e2) {
                      message("\nERROR: ", conditionMessage(e2), "\n\n")
                      archivo_errores <- file(path_errores, open = "a")
                      cat(paste("No sé que ha pasado en el intervalo", i.part),
                        file = archivo_errores, "\n", append = TRUE
                      )
                      close(archivo_errores)
                    }
                  )
                }
              ) # Terminó el TryCath
            } # Terminó el bucle de los i.part


            # Tras pasar por todos los intervalos de tiempo, guardamos la lista del índice calculado para el modelo
            message(paste0("\nComienza el guardado del índice ", index.code.i, " en archivo .Rdata"))

            # Se guarda el índice con los intervalos en listas individuales
            filename <- file.path(path_filename_indice, paste0(index.code.i, "_list.Rdata"))
            save(list.agroindex, file = filename)

            # Se guarda el índice con los intervalos unidos
            ls <- unname(list.agroindex)
            if (length(list.agroindex) == 1) {
              agroindex_all <- ls[[1]]
              filename <- file.path(path_filename_indice, paste0(index.code.i, ".Rdata"))
              save(agroindex_all, file = filename)
            } else {
              agroindex_all <- redim(bindGrid(ls, dimension = "time"), drop = TRUE)
              filename <- file.path(path_filename_indice, paste0(index.code.i, ".Rdata"))
              save(agroindex_all, file = filename)
            }

            message("Se ha guardado satisfactoriamente el archivo.\n")

            # Ploteamos la imagen de los datos
            filename_fig1 <- file.path(path_filename_fig1, paste0("03_", "AgroIndex_", index.code.i, "_ALL", img.ext))

            fig1 <- spatialPlot(climatology(agroindex_all),
              backdrop.theme = "coastline", rev.colors = TRUE,
              main = paste0(basename((path.model.i)), ": ", index.code.i, " con ", paste(var.name, collapse = ", "), " [", paste(list.units, collapse = ", "), "]")
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

            # Eliminamos las variables que no necesitamos
            rm(list.agroindex, var.files, ls, agroindex_all)
            gc()

            # Unimos temporalmente las secciones de tiempo, obteniendo un único ClimGrid para el índice
            # # Opción 1
            # ls <- unname(list.agroindex)

            # # Opción 2
            # ls <- lapply(names(list.agroindex), function(i){
            #   data.subset <- list.agroindex[[i]]
            #   return(data.subset) #si no pones el return guarda en la lista el último producto
            # })
            #
            # # Unimos las listas por la dimensión time
            # data <- redim(bindGrid(ls, dimension = "time"), drop=TRUE)
            # data2 <- bindGrid(ls, dimension = "time")
            # rm(ls)
          } else {
            message(paste("No hay archivos .Rdata para la variable", var.name[1], "y quizás en ninguna otra"))
          }
        } else {
          paste("La carpeta", basename((path.model.i)), "está vacía.")
        }
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
