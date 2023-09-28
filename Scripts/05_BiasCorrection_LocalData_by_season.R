#' =============================================================================
#' ======================== PAQUETES Y DATOS CLIMÁTICOS ========================
#' =============================================================================

#' Limpiamos el entorno de trabajo
#' -----------------------------------------------------------------------------

rm(list = ls())
graphics.off()
gc()

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
library(transformeR) # para transformaciones
library(visualizeR) # para generar figuras (función spatialPlot)
library(downscaleR) # para bias correction (función biasCorrection)
# library(climate4R.indices)
library(convertR)
library(MBC)


#' Declaramos los paths de los códigos y datos
#' -----------------------------------------------------------------------------


for (j_sesion in seq(1, 12, by = 1)) {

  # setwd(file.path("/home", "juanjose", "Documentos", "Mis_proyectos"))
  path_Datos <- file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") #file.path(getwd(), "Datos") # file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos") 
  path_Datos_iberia_metadatos <- file.path(getwd(), "Datos")
  path_Codigo <- file.path(getwd(), "Scripts") # file.path(getwd(), "Codigo")
  path_Datos_save <- file.path("/lustre", "gmeteo", "WORK", "velascohj", "Datos")  # file.path(getwd(), "Datos") 
  folder_saved <- "CodexDev_univariados_season"

  
  #' Cargamos las funciones definidas en los archivos .R de mi carpeta local sincronizadas en el GIT:
  #' -----------------------------------------------------------------------------
  
  # # Definir la ruta de la carpeta local que contiene los archivos .R
  # ruta_git <- file.path(getwd(), "downscaleR")
  # # Obtener la lista de archivos .R en la carpeta local y sus subcarpetas
  # archivos_git <- list.files(path = ruta_git, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  # # archivos_git <- archivos_git[!grepl("biasCorrection.R", archivos_git)]
  # # Cargar cada archivo y todas sus funciones usando la función lapply() y source()
  # lapply(archivos_git, source)
  # 
  

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
  
  #' Lista de las variables que se van a cargar y utilizar
  #' -----------------------------------------------------------------------------
  
  # Variables que se van a corregir con el método "Bias Correction".
  # El nombre de las variables es el utilizado en los .Rdata y modelo CORDEX
  list.variables.bias <- list("tas", "tasmin", "tasmax", "pr") # Para MBC methods: list(c("tas", "tasmin", "tasmax", "pr"))
  
  # Nombre del método de "Bias correction" empleado para corregir los sesgos
  bias_method <- "qdm"
  
  # Tipo de Cross-Validación que se va a utilizar: "none", "kfold_WarmCold" o "kfold_Random"
  cross_val <- "kfold_Random"
  
  # Caso de uso: "same.historical" o "diff.historical" o "rcp"
  type_newdata <- "same.historical"
  
  # Años que queremos cargar del periodo de calibración
  years_to_find.c <- 1986:2005
  
  # Años que queremos cargar del periodo de proyección a futuro
  years_to_find.p <- 2070:2080
  
  # Mes del año para corregir los meses de forma individual. season <- NULL implica utilizar todos
  season <- j_sesion
  
  # Cuantiles a utilizar. n.quantiles <- NULL implica utilizar todos los datos
  n.quantiles <- NULL
  
  # Opción para usar los años que aparecen en CSV de CMIP5_Atlas_WarmingLevels 
  # (escenario X4_rpc85) para el periodo de proyección.
  # Únicamente sirve cuando: type_newdata <- "rcp"
  useWarmingLevels <- TRUE
  
  # Opción para retornar los datos del periodo de calibración corregidos
  return_grid_c <- FALSE
  
  # Formato de las imágenes
  img.ext <- ".pdf" # pdf ó png
  
  # Opción para pasar las variables a otra unidad previamente de los cálculos
  option_convert <- TRUE
  
  
  #' =============================================================================
  #' ============================= MODELOS DE CORDEX =============================
  #' =============================================================================
  
  #' Se genera la carpeta que contendrá todos los archivos creados por este script
  #' -----------------------------------------------------------------------------
  
  dir_bias <- file.path(path_Datos_save, folder_saved)
  if (!file.exists(dir_bias)) {
    dir.create(dir_bias)
    message("==> Se ha creado la carpeta: ", folder_saved)
  }
  
  
  #' Cargamos los modelos que se encuentran en local
  #' -----------------------------------------------------------------------------
  
  # Lista de modelos a evaluar (todos los que aparece en la carpeta CordexDev)
  model_folder <- list.dirs(file.path(path_Datos, "CordexDev"), recursive = FALSE)
  model_folder_hist <- model_folder[grepl(paste0("^", "model_", "(3|4|20|21)$"), basename(model_folder), ignore.case = TRUE)] # (3|4|20|21)
  
  # Imprimir los nombres de los modelos
  message("Los modelos que se van a utilizar son:")
  for (model_i in model_folder_hist) {
    message(paste0("+ ", basename(model_i)))
  }
  
  #' Cargamos los metadatos de todos modelos CORDEX, incluido los modelos en local
  #' -----------------------------------------------------------------------------
  
  # Abrimos el archivo de metadatos de los modelos CORDEX
  path_metadata_models <- file.path(path_Datos_iberia_metadatos, "CordexDev", "Metadatos")
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
  rm(list.metadata.cordex)
  
  
  #' Guardamos el DataFrame en un archivo .txt con los modelos que cumplen las condiciones
  #' -----------------------------------------------------------------------------
  
  filename <- file.path(path_Datos_save, folder_saved, "list_models_bias")
  write.table(list.model.cordex,
    file = paste0(filename, ".txt"), sep = ";", quote = FALSE,
    row.names = FALSE
  )
  
  
  #' Cargamos los períodos de tiempo en los que se alcanzan niveles de calentamiento
  #' global transitorio (GWL) de +1,5, +2, +3 y +4 grados (con respecto al valor
  #' medio preindustrial de 1850-1900) calculados para los datos CMIP5
  #' -----------------------------------------------------------------------------
  
  url <- "https://github.com/IPCC-WG1/Atlas/raw/main/warming-levels/CMIP5_Atlas_WarmingLevels.csv"
  path_CMIP5_Atlas <- file.path(path_Datos, "CordexDev", "Metadatos", "CMIP5_Atlas_WarmingLevels.csv")
  
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
    message("\n\t==> Se utiliza la URL para descargar el CSV 'CMIP5_Atlas_WarmingLevels.csv'")
    df_Atlas <- read.csv(url)
  } else {
    # Cargar desde la ruta local
    message("\n\t==> Se utiliza el archivo CSV 'CMIP5_Atlas_WarmingLevels.csv' previamente descargado")
    df_Atlas <- read.csv(path_CMIP5_Atlas)
  }
  
  # Dividimos el nombre del modelo para poder identificarlo con los metadatos
  df_Atlas$model <- str_split(df_Atlas$model_run, "_", simplify = TRUE)[, 1]
  df_Atlas$ensemble <- str_split(df_Atlas$model_run, "_", simplify = TRUE)[, 2]
  
  
  #' =============================================================================
  #' ============================ ARCHIVO DE ERRORES =============================
  #' =============================================================================
  
  #' Se genera un archivo de errores
  #' -----------------------------------------------------------------------------
  
  # Ruta del archivo de texto
  path_errores <- file.path(path_Datos_save, folder_saved, "errores_ejecucion.txt")
  
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
  #' ================= VALIDACIONES DE LAS VARIABLES ESCOGIDAS ===================
  #' =============================================================================
  
  if (cross_val == "kfold_Random") {
    # 2 - fold construidos de forma aleatoria
  
    # Rango de años del periodo histórico y observaciones
    rango_anios <- years_to_find.c
  
    # Calcular la mitad de los años en el rango
    set.seed(1)
    mitad_anios <- length(rango_anios) %/% 2
  
    # Crear un vector aleatorio de índices para seleccionar la mitad de los años
    indices_aleatorios <- sample(length(rango_anios), length(rango_anios) %/% 2)
  
    # Crear el 1º fold
    fold1 <- sort(rango_anios[indices_aleatorios])
  
    # Crear el 2º fold, siendo los años restantes que no aparecen en el 1º fold
    fold2 <- setdiff(rango_anios, fold1)
  
    # Lista de vectores para el agumento 'folds' de 'biasCorrection'
    folds <- list(fold1, fold2)
    cross_val.arg <- "kfold"
  } else if (cross_val == "kfold_WarmCold") {
    # 2 - fold, uno de los años más cálidos y el otro de los años más fríos según
    # la temperatura media de Iberia01
  
    # Path de la temperatura media de Iberia01
    path_tas <- file.path(path_Datos_iberia_metadatos, "iberia01", "tas_iberia01_1986-2005.Rdata")
    load(path_tas)
  
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
  
    # Lista de vectores para el agumento 'folds' de 'biasCorrection'
    folds <- list(years_WARM, years_COLD)
    cross_val.arg <- "kfold"
  } else {
    folds <- NULL
    cross_val.arg <- "none"
  }
  
  if (type_newdata == "same.historical") {
    years_to_find.p <- years_to_find.c
  }
  
  if (is.null(season)) {
    season_i <- NULL
  } else {
    season_i <- season
  }
  
  if (is.null(n.quantiles)) {
    n_quantiles <- NULL
  } else {
    n_quantiles <- n.quantiles
  }
  
  #' Comprobaciones de las variables seleccionadas por el usuario
  #' -----------------------------------------------------------------------------
  
  # Comprobación de que se han introducido bien los parámetros para el método de
  # "Bias correction"
  meets_conditions <- TRUE
  isMBC <- FALSE
  if (bias_method %in% c("mbcr", "mbcp", "mbcn")) {
    isMBC <- TRUE
    for (variables.bias in list.variables.bias) {
      if (length(variables.bias) <= 1) {
        message("Has escogido un método multivariable de correción de sesgo (MBC)")
        message(paste0("No puedes corregir únicamente '", variables.bias, "' con estos métodos."))
        meets_conditions <- FALSE
      }
    }
  } else {
    for (variables.bias in list.variables.bias) {
      if (length(variables.bias) != 1) {
        message("Has escogido un método univariable de correción de sesgo.")
        message(paste0("No puedes corregir de forma conjunta las variables: ", paste(variables.bias, collapse = ", ")))
        meets_conditions <- FALSE
      }
    }
  }
  
  if (!(type_newdata %in% c("same.historical", "diff.historical", "rcp"))) {
    message("No ha elegido una opción válida. Seleccione:")
    message("\t- 'same.historical' para que los datos del periodo de calibración sean los mismos del periodo de proyección.")
    message("\t- 'diff.historical' para que los datos del periodo de proyección sean distintos al del periodo de calibración, pero pertenezcan al mismo modelo histórico.")
    message("\t- 'rcp' para utilizar los datos del modelo RPC (asociado del modelo histórico) en el periodo de proyección.")
    meets_conditions <- FALSE
  }
  
  if (!(cross_val %in% c("none", "kfold_WarmCold", "kfold_Random"))) {
    message("No ha elegido una opción válida de Cross-Validation. Seleccione:")
    message("\t- 'none' para no hacer Cross-Validation")
    message("\t- 'kfold_WarmCold' para hacer 2 folds con los años más cálidos y los años más frios según Iberia01.")
    message("\t- 'kfold_Random' para hacer 2 folds generados de forma aleatoria.")
    meets_conditions <- FALSE
  }
  
  if ((type_newdata %in% c("diff.historical", "rcp")) & (cross_val %in% c("kfold_WarmCold", "kfold_Random"))) {
    message(paste0("'", type_newdata, "' implica utilizar un periodo de proyección distinto al de calibración."))
    message(paste0("Y, por el contrario, has escogido CV, que no permite usar un periodo de proyección distinto."))
    message("Con CV solo puedes escoger el caso de uso 'same.historical'.")
    meets_conditions <- FALSE
  }
  
  message(paste0("\nHas elegido el caso de uso: '", bias_method, "', '", type_newdata, "', '", cross_val, "' con q=", n.quantiles, ' y s=', season))
  
  
  #' =============================================================================
  #' =========================== PARA CADA MODELO ... ============================
  #' =============================================================================
  
  #' Leemos solo el dominio que incluya la Península Ibérica y todos nuestros
  #' años de estudio
  #' -----------------------------------------------------------------------------
  
  if (meets_conditions) {
    for (path.model.i in model_folder_hist) {
  
      # Seleccionamos el modelo histórico
      N_model.hist.i <- basename((path.model.i))
  
      # Cargamos la lista de DataFrames de los modelos y seleccionamos el asociado
      # al modelo "N_model.hist.i"
      load(file.path(path_metadata_models, "df_metadata_models_DEV.Rdata"))
      df.hist.i <- list.df.model[[N_model.hist.i]]
  
      # Nombre completo del modelo histórico
      cordex.model.hist.i <- as.character(df.hist.i$longname[1])
  
      # Los métodos de "Bias correction" trabajan comparando los datos de modelos y
      # con las observaciones en el periodo de calibración (historical).
      # Por ello, en este bucle solo se entra si:
      # - Se trata de un modelo histórico.
      # - Se cumple con las condiciones de BiasCorrection
      if (grepl("historical", cordex.model.hist.i)) {
        message("\n\n---- ", N_model.hist.i, ": ", cordex.model.hist.i, " ----")
        message("--------------------------------------------------------------------------------------------\n")
  
        # Lista de archivos .Rdata en el directorio del modelo histórico (calibración)
        filesRdata.hist <- list.files(path.model.i, pattern = "\\.Rdata$", full.names = TRUE)
  
        if (length(filesRdata.hist) == 0) {
          # Pasamos al siguiente modelo histórico
          message(paste0("No hay archivos .Rdata en la carpeta del modelo histórico ", N_model.hist.i))
          next
        }
  
        # Si type_newdata == "rcp" implica que se quiere aplicar la corrección hallada
        # en el periodo de calibración (histórico) sobre los datos del modelo a futuro (rcp)
        if (type_newdata == "rcp") {
          # Nombre completo del modelo RCP asociado al modelo histórico
          cordex.model.rcp.i <- gsub("historical", "rcp85", cordex.model.hist.i)
          df.meta.rcp.i <- subset(list.model.cordex, name_model == cordex.model.rcp.i)
          
          # Comprobamos que existe un modelo RCP asociado al modelo histórico
          if (nrow(df.meta.rcp.i) == 0) {
            # Pasamos al siguiente modelo histórico
            next
          }
  
          # Si existe el modelo RCP, obtenemos sus metadatos
          N_model.rcp.i <- df.meta.rcp.i$N_model
          df.rcp.i <- list.df.model[[N_model.rcp.i]]
  
          # Path local del modelo RCP asociado al modelo histórico
          path.model.rcp.i <- model_folder[which(basename(model_folder) == N_model.rcp.i)]
          if (length(path.model.rcp.i) == 0) {
            # Pasamos al siguiente modelo histórico
            message(paste0("El modelo ", N_model.rcp.i, " no se encuentra entre las carpetas locales"))
            next
          }
  
          # Lista de archivos .Rdata en el directorio del modelo histórico (calibración)
          filesRdata.rcp <- list.files(path.model.rcp.i, pattern = "\\.Rdata$", full.names = TRUE)
  
          if (length(filesRdata.rcp) == 0) {
            # Pasamos al siguiente modelo histórico
            message(paste0("No hay archivos .Rdata en la carpeta del modelo RCP ", N_model.rcp.i))
            next
          }
  
          # Generamos (si no existe) la carpeta que contendrá los datos del modelo
          path_filename_data.rcp <- file.path(path_Datos_save, folder_saved, N_model.rcp.i)
          if (!file.exists(path_filename_data.rcp)) {
            dir.create(path_filename_data.rcp)
            message(paste0("==> Se ha creado la carpeta: ", N_model.rcp.i))
          }
  
          # Generamos (si no existe) la carpeta que contendrá las imágenes asociadas al modelo
          path_filename_fig1.rcp <- file.path(path_Datos_save, folder_saved, N_model.rcp.i, paste0("plots_", N_model.rcp.i))
          if (!file.exists(path_filename_fig1.rcp)) {
            dir.create(path_filename_fig1.rcp)
            message(paste0("==> Se ha creado la carpeta: ", N_model.rcp.i, "/plots_", N_model.rcp.i))
          }
  
          # Comprobamos si en el CSV de CMIP5 Altas con los niveles de calentamiento
          # existe el modelo 'rcp' y tienen años asociados al escenario RCP8.5
          if (useWarmingLevels) {
            warming_mean <- 4
            warming_level <- paste0("X", warming_mean, "_rcp85")
            year_Atlas_rcp85 <- df_Atlas[sapply(1:nrow(df_Atlas), function(i) {
              grepl(df_Atlas$model[i], cordex.model.rcp.i) & grepl(df_Atlas$ensemble[i], cordex.model.rcp.i)
            }), warming_level]
            if ((length(year_Atlas_rcp85) == 0) | is.null(year_Atlas_rcp85)) {
              # No se encuentra el modelo RCP en el CSV de CMIP5 Altas
              message("No se ha encontrado referncia del modelo RCP en el CSV de CMIP5 Altas")
              message("Se utilizará los años definidos en 'years_to_find.p'")
            } else {
              message(paste0("Se va a utilizar el periodo de proyección (para el RCP): ", year_Atlas_rcp85[1] - 9, " al ", year_Atlas_rcp85[1] + 10))
              years_to_find.p <- seq(year_Atlas_rcp85[1] - 9, year_Atlas_rcp85[1] + 10, by = 1)
            }
          }
        }
        else {
  
          # Si type_newdata != "rcp", entonces utilizamos para el periodo de proyección los datos del modelo "historical"
          filesRdata.rcp <- filesRdata.hist
        }
  
        # Generamos (si no existe) la carpeta que contendrá los datos del modelo histórico corregido
        path_filename_data.hist <- file.path(path_Datos_save, folder_saved, N_model.hist.i)
        if (!file.exists(path_filename_data.hist)) {
          dir.create(path_filename_data.hist)
          message(paste0("==> Se ha creado la carpeta: ", N_model.hist.i))
        }
  
        # Generamos (si no existe) la carpeta que contendrá las imágenes asociadas al modelo
        path_filename_fig1.hist <- file.path(path_Datos_save, folder_saved, N_model.hist.i, paste0("plots_", N_model.hist.i))
        if (!file.exists(path_filename_fig1.hist)) {
          dir.create(path_filename_fig1.hist)
          message(paste0("==> Se ha creado la carpeta: ", N_model.hist.i, "/plots_", N_model.hist.i))
        }
  
        # Eliminamos las variables que no necesitamos
        rm(list.df.model)
        gc()
  
        for (variables.bias.i in list.variables.bias) {
  
          # Seleccionamos el "pack" de variables a corregir de forma conjunta:
          # pueden ser una o varias variables en función del método de Bias Correction
          # escogido.
  
          #' Comprobamos que el modelo histórico tiene las variables solicitadas y
          #' todas tienen el mismo número de archivos .Rdata
          #' -----------------------------------------------------------------------
  
          var.hist.files <- list()
          for (var.name.i in variables.bias.i) {
            var.hist.files[[var.name.i]] <- filesRdata.hist[grepl(paste0("^", var.name.i, "_"), basename(filesRdata.hist))]
          }
  
          # Todas las variables deben tener el mismo número de archivos asociados!!!
          # Y distinto de 0
          length.hist.files <- sapply(var.hist.files, function(k) length(k))
          if ((all(length.hist.files)) & (length.hist.files[[1]] != 0)) {
            x_hist_conditions <- TRUE
          } else {
            x_hist_conditions <- FALSE
          }
  
          #' Comprobamos que el modelo RCP (si se ha elegido) tiene las variables
          #' solicitadas y todas tienen el mismo número de archivos .Rdata
          #' -----------------------------------------------------------------------
  
          var.rcp.files <- list()
          for (var.name.i in variables.bias.i) {
            var.rcp.files[[var.name.i]] <- filesRdata.rcp[grepl(paste0("^", var.name.i, "_"), basename(filesRdata.rcp))]
          }
  
          # Todas las variables deben tener el mismo número de archivos asociados!!!
          # Y distinto de 0
          length.rcp.files <- sapply(var.rcp.files, function(k) length(k))
          if ((all(length.rcp.files)) & (length.rcp.files[[1]] != 0)) {
            newdata_rcp_conditions <- TRUE
          } else {
            newdata_rcp_conditions <- FALSE
          }
  
  
          if (x_hist_conditions & newdata_rcp_conditions) {
            tryCatch(
              {
                # Argumentos (y, x, newdata) de la función biasCorrection() de climate4R
                y_list <- list()
                x_list <- list()
                newdata_list <- list()
                mask.obs <- list()
  
                # Si unique es 1 valor (type_newdata = "historical"), solo se entra
                # una vez en el bucle. Si type_newdata = "rcp" se entra dos y en la segunda
                # iteración del bucle se cargan los archivos RCP asociados a newdata
                for (i.mode in unique(c("same.historical", type_newdata))) {
  
                  # Lista con los PATH de las variables
                  var.files <- list()
  
                  if (i.mode != "rcp") { # type_newdata = "historical"
  
                    #' Con el siguiente código solo cargamos los archivos .Rdata
                    #' que contienen los datos de interés para el periodo de calibración
                    #' ----------------------------------------------------------------------------------
  
                    # Todos los años que dura el periodo histórico
                    all_years.hist <- 1986:2005
  
                    # El número de archivos .Rdata que se han capturado en la carpeta
                    # se crearon dividiendo "all_years.hist" en N partes de la
                    # siguiente forma:
                    N_files <- length(var.hist.files[[1]])
                    if (N_files == 1) {
                      sub_years <- list()
                      sub_years[[1]] <- all_years.hist
                    } else {
                      sub_years <- list()
                      sub_years <- split(all_years.hist, cut(all_years.hist, breaks = N_files, labels = FALSE))
                    }
  
                    # Busqueda de los "years_to_find.hist"
                    if (i.mode == "same.historical") {
                      years_to_find.hist <- years_to_find.c
                    }
                    if (i.mode == "diff.historical") {
                      years_to_find.hist <- years_to_find.p
                    }
                    found_parts <- NULL
                    if (N_files == 1) {
                      found_parts <- 1
                    } else {
                      for (i in 1:length(sub_years)) {
                        if (any(years_to_find.hist %in% sub_years[[i]])) {
                          found_parts <- c(found_parts, i)
                        }
                      }
                    }
                    message(paste("Los años del", years_to_find.hist[1], "a", years_to_find.hist[length(years_to_find.hist)], "se encuentran en la(s) parte(s):", paste(found_parts, collapse = ", ")))
  
  
                    # Obtenemos la sección de años con la que vamos a operar
                    int_years <- years_to_find.hist
  
                    # DataFrame de los datos a utilizar
                    df.i <- df.hist.i
  
                    # Ruta de las figuras
                    path_filename_fig1 <- path_filename_fig1.hist
  
                    # Ruta de la carpeta del modelo
                    path_filename_data <- path_filename_data.hist
  
                    # # Lista de PATH de las variables
                    # var.files <- var.hist.files
  
                    # Lista de PATH de los años de interés de las variables
                    # Patrón construido con "found_parts"
                    patterns <- paste0("_", found_parts)
  
                    # Utilizar lapply para seleccionar rutas que cumplen el patrón
                    var.files <- lapply(names(var.hist.files), function(sublist_name) {
                      sublist <- var.hist.files[[sublist_name]]
                      matching_paths <- sublist[grep(paste(patterns, collapse = "|"), basename(sublist))]
                      return(matching_paths)
                    })
  
                    # Convertir los resultados a una lista con nombres
                    names(var.files) <- names(var.hist.files)
                  }
                  else { # type_newdata = "rcp"
  
                    #' Con el siguiente código solo cargamos los archivos .Rdata
                    #' que contienen los datos de interés para el modelo rcp
                    #' ----------------------------------------------------------------------------------
  
                    # Periodo de tiempo de los modelos rcp
                    all_years.rcp <- 2006:2100
  
                    # El número de archivos .Rdata que se han capturado en la carpeta
                    # se crearon dividiendo "all_years.rcp" en N partes de la
                    # siguiente forma:
                    N_files <- length(var.rcp.files[[1]])
  
                    if (N_files == 1) {
                      sub_years <- list()
                      sub_years[[1]] <- all_years.rcp
                    } else {
                      sub_years <- list()
                      sub_years <- split(all_years.rcp, cut(all_years.rcp, breaks = N_files, labels = FALSE))
                    }
  
                    # Busqueda de los "years_to_find.rcp"
                    years_to_find.rcp <- years_to_find.p
                    found_parts <- NULL
  
                    if (N_files == 1) {
                      found_parts <- 1
                    } else {
                      for (i in 1:length(sub_years)) {
                        if (any(years_to_find.rcp %in% sub_years[[i]])) {
                          found_parts <- c(found_parts, i)
                        }
                      }
                    }
                    message(paste("Los años del", years_to_find.rcp[1], "a", years_to_find.rcp[length(years_to_find.rcp)], "se encuentran en la(s) parte(s):", paste(found_parts, collapse = ", ")))
  
                    # Obtenemos la sección de años con la que vamos a operar
                    int_years <- years_to_find.rcp
  
                    # DataFrame de los datos a utilizar
                    df.i <- df.rcp.i
  
                    # Ruta de las figuras
                    path_filename_fig1 <- path_filename_fig1.rcp
  
                    # Ruta de la carpeta del modelo
                    path_filename_data <- path_filename_data.rcp
  
                    # # Lista de PATH de las variables
                    # var.files <- var.rcp.files
  
                    # Lista de PATH de los años de interés de las variables
                    # Patrón construido con "found_parts"
                    patterns <- paste0("_", found_parts)
  
                    # Utilizar lapply para seleccionar rutas que cumplen el patrón
                    var.files <- lapply(names(var.rcp.files), function(sublist_name) {
                      sublist <- var.rcp.files[[sublist_name]]
                      matching_paths <- sublist[grep(paste(patterns, collapse = "|"), basename(sublist))]
                      return(matching_paths)
                    })
  
                    # Convertir los resultados a una lista con nombres
                    names(var.files) <- names(var.rcp.files)
                  }
  
                  # Preparamos cada variable de forma individual:
                  # Como las variables ya se encuentran interpoladas con sus máscaras aplicadas y
                  # con la máscara de la observación aplicada, se lleva a cabo:
                  #   - Se cargan todos los archivos .Rdata asociado a la variable
                  #   - Se unen los datos temporalmente
                  #   - Se modifican sus unidades si fuera necesario.
                  #   - Se escoge el intervalo de tiempo de interés
  
                  for (variable_i in variables.bias.i) {
                    message(paste0("\n========> Variable: ", variable_i, " - ", int_years[1], " to ", int_years[length(int_years)], " <========"))
  
                    # Lista de las variables del modelo tras el pre-procesado.
                    list.arg.model <- list()
                    list.units <- list()
  
                    #' Cargamos los datos de la variable del modelo
                    #' -----------------------------------------------------------------------------
  
                    # Carga y combina los datos de los archivos .Rdata en un solo marco de datos
                    data.list <- lapply(var.files[[variable_i]], load_and_extract)
  
                    #' Unimos los datos de la variable en un único archivo
                    #' -----------------------------------------------------------------------------
  
                    # Datos sin nombre
                    ls <- unname(data.list)
                    len.data.list <- length(data.list)
                    rm(data.list)
  
                    # Se unen los datos y se guardan unidos (comentado)
                    if (len.data.list == 1) {
                      data_all <- ls[[1]]
                      filename <- file.path(path_filename_data, paste0("complete_interpolate_", variable_i, ".Rdata"))
                      # save(data_all, file = filename)
                    } else {
                      data_all <- redim(bindGrid(ls, dimension = "time"), drop = TRUE)
                      filename <- file.path(path_filename_data, paste0("complete_interpolate_", variable_i, ".Rdata"))
                      # save(data_all, file = filename)
                    }
                    rm(ls)
                    gc()
  
  
                    #' Seleccionamos el intervalo de tiempo de interés de los datos unidos
                    #' -----------------------------------------------------------------------------
                    
                    variable_i.model <- subsetGrid(data_all,
                      years = int_years,
                      season = season
                      # lonLim = c(-6, -1), # c(-10, 5),
                      # latLim = c(41, 43.8) # c(35, 44))
                    )
                    rm(data_all)
                    gc()
                    
  
                    # Ploteamos la imagen de los datos
                    filename_fig1 <- file.path(path_filename_fig1, paste0("05_", variable_i, "_", int_years[1], "-", int_years[length(int_years)], "_01_original", paste0(c("", season_i), collapse = "_S"), img.ext))
                    # fig1 <- spatialPlot(climatology(variable_i.model),
                    #   backdrop.theme = "coastline", rev.colors = TRUE,
                    #   main = paste0(N_model.hist.i, ": ", variable_i, " (", getGridUnits(variable_i.model), ") de ", int_years[1], " a ", int_years[length(int_years)])
                    # )
                    fig1 <- plot_custom_map(climatology(variable_i.model),
                      name_title = paste0(N_model.hist.i, ": ", variable_i, " (", getGridUnits(variable_i.model), ") de ", int_years[1], " a ", int_years[length(int_years)], paste0(c("", season_i), collapse = "\n Season "))
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
  
  
                    #' Reescalar los datos del modelo a las unidades de la observación
                    #' -------------------------------------------------------------------------
  
                    # Convertimos las dimensiones de las variables a ºC y kg m-2 si así lo queremos
                    if (option_convert) {
                      message("\nSe cambian las unidades de las variables del modelo para coincidir con las unidades de la observación (iberia01):")
  
                      # Temperaturas
                      if (variable_i %in% c("tasmin", "tasmax", "tas")) {
                        if (getGridUnits(variable_i.model) == "K") {
                          message(paste0("\t==> Variable ", variable_i, ": units "), getGridUnits(variable_i.model), " to ºC")
                          variable_i.model <- udConvertGrid(variable_i.model, "degree_Celsius")
                          list.units[[variable_i]] <- "degreeC"
                        } else {
                          list.units[[variable_i]] <- getGridUnits(variable_i.model)
                        }
  
                        # Ploteamos la imagen de los datos
                        filename_fig1 <- file.path(path_filename_fig1, paste0("05_", variable_i, "_", int_years[1], "-", int_years[length(int_years)], "_02_newUnits", paste0(c("", season_i), collapse = "_S"), img.ext))
                        # fig1 <- spatialPlot(climatology(variable_i.model),
                        #   backdrop.theme = "coastline", rev.colors = TRUE,
                        #   main = paste0(N_model.hist.i, ": ", variable_i, " (", getGridUnits(variable_i.model), ") de ", int_years[1], " a ", int_years[length(int_years)])
                        # )
                        fig1 <- plot_custom_map(climatology(variable_i.model),
                          name_title = paste0(N_model.hist.i, ": ", variable_i, " (", getGridUnits(variable_i.model), ") de ", int_years[1], " a ", int_years[length(int_years)], paste0(c("", season_i), collapse = "\n Season "))
                        )
  
                        if (img.ext == ".png") {
                          png(filename_fig1)
                        } else {
                          pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                        }
                        print(fig1)
                        dev.off()
                        message(paste0("Se guarda la imagen: ", filename_fig1, "\n"))
                        rm(fig1, filename_fig1)
                      }
  
                      # Precipitación
                      if (variable_i %in% c("pr")) {
                        if (getGridUnits(variable_i.model) == "kg m-2 s-1") {
                          message(paste0("\t==> Variable ", variable_i, ": units "), getGridUnits(variable_i.model), " to kg m-2")
                          nt <- length(getRefDates(variable_i.model))
                          ls <- lapply(1:nt, function(i) {
                            timei <- subsetDimension(variable_i.model, dimension = "time", indices = i) # separa día a día
                            gridArithmetics(timei, 86400, operator = "*") # multiplica los segundos de un día para pasar a kg m-2
                          })
                          rm(variable_i.model)
                          variable_i.model <- bindGrid(ls, dimension = "time") # bindGrid es una función de transforeR que vuelve a unir todos los días en un grid de climate4R
                          rm(ls, nt)
                          gc()
                          list.units[[variable_i]] <- "kg m-2"
                          # Asignamos al atributo del grid las nuevas unidades
                          attr(variable_i.model$Variable, "units") <- "kg m-2"
                        } else {
                          list.units[[variable_i]] <- getGridUnits(variable_i.model)
                        }
  
                        # Ploteamos la imagen de los datos
                        filename_fig1 <- file.path(path_filename_fig1, paste0("05_", variable_i, "_", int_years[1], "-", int_years[length(int_years)], "_02_newUnits", paste0(c("", season_i), collapse = "_S"), img.ext))
                        # fig1 <- spatialPlot(climatology(variable_i.model),
                        #   backdrop.theme = "coastline", rev.colors = TRUE,
                        #   main = paste0(N_model.hist.i, ": ", variable_i, " (", getGridUnits(variable_i.model), ") de ", int_years[1], " a ", int_years[length(int_years)])
                        # )
                        fig1 <- plot_custom_map(climatology(variable_i.model),
                          name_title = paste0(N_model.hist.i, ": ", variable_i, " (", getGridUnits(variable_i.model), ") de ", int_years[1], " a ", int_years[length(int_years)], paste0(c("", season_i), collapse = "\n Season "))
                        )
                        if (img.ext == ".png") {
                          png(filename_fig1)
                        } else {
                          pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                        }
                        print(fig1)
                        dev.off()
                        message(paste0("Se guarda la imagen: ", filename_fig1, "\n"))
                        rm(fig1, filename_fig1)
                      }
                    }
  
                    # Guardamos los datos
                    if (i.mode == "same.historical") {
                      # Guardamos los datos del modelo en el periodo de calibración
                      x_list[[variable_i]] <- variable_i.model
                    }
                    else {
                      # Guardamos los datos del modelo en el escenario RCP ('rcp') o los del modelo histórico ('diff.historical')
                      newdata_list[[variable_i]] <- variable_i.model
                    }
                    rm(variable_i.model)
                    gc()
  
  
                    #' Cargamos la observación (iberia01) de la variable
                    #' -------------------------------------------------------------------------
  
                    if (i.mode == "same.historical") {
                      path_data <- file.path(path_Datos_iberia_metadatos, "iberia01")
  
                      # Variable de Iberia01 (observación)
                      message(paste0("\nSe cargan las observaciones (", basename(path_data), "): ", file.path(path_data, paste0(variable_i, "_iberia01_1986-2005.Rdata"))))
                      load(file.path(path_data, paste0(variable_i, "_iberia01_1986-2005.Rdata")))
                      message(paste0("\t==> Se ha cargado el archivo: ", variable_i, "_iberia01_1986-2005.Rdata\n"))
  
                      # Calculamos la máscara de mar-tierra de toda la variable observada
                      # mask.obs.var <- suppressMessages(climatology(data))
                      # mask.obs.var$Data[!is.na(mask.obs.var$Data)] <- 1
                      # mask.obs[[variable_i]] <- mask.obs.var
  
                      # Seleccionamos el intervalo de tiempo de interés
                      obs <- subsetGrid(data,
                        years = int_years,
                        season = season
                        # lonLim = c(-6, -1), # c(-10, 5),
                        # latLim = c(41, 43.8) # c(35, 44))
                      )
  
                      # Guardamos los datos de la observación en el periodo de calibración
                      y_list[[variable_i]] <- obs
                      rm(obs, data)
                      # rm(mask.obs.var, mask.obs)
                      gc()
                    }
                  }
                }
  
                # Guardamos el modelo de calibración como modelo a corregir
                if (type_newdata == "same.historical") {
                  newdata_list <- NULL
                }
  
                if ((bias_method %in% c("mbcr", "mbcp", "mbcn")) & length(variables.bias.i) != 1) {
                  message(paste0("Se comienza con el 'MBC' de las variables ", paste(variables.bias.i, collapse = ", ")))
                } else {
                  message(paste0("Se comienza con el 'UBC' de la variable ", paste(variables.bias.i, collapse = ", ")))
                  x_list <- x_list[[1]]
                  y_list <- y_list[[1]]
                  newdata_list <- newdata_list[[1]]
                }
                gc()
  
                #' ==================================================================================
                #' === BIAS CORRECTION + INTERPOLACIÓN DEL MODELO DE CORDEX CON LAS OBSERVACIONES ===
                #' ==================================================================================
  
                #' Aplicación de técnicas de "bias correction"
                #' -------------------------------------------------------------------------
  
                # Ajustamos los argumentos de la función biasCorrection
                precipitation <- FALSE
                wet.threshold <- 1 
                fitdistr.args <- list(densfun = "normal")
                
                if ("pr" %in% variables.bias.i) {
                  precipitation <- TRUE
                  if (bias_method == "pqm") {
                    fitdistr.args <- list(densfun = "gamma")
                  }
                } 
                
                if (!is.null(season)) {
                  if (isMBC) {
                    for (i in 1:length(y_list))
                    {
                      attr(y_list[[i]]$Dates, "season") <- season
                      attr(y_list[[i]]$Dates, "subset") <- 'subsetYears'
                      attr(x_list[[i]]$Dates, "season") <- season
                      attr(x_list[[i]]$Dates, "subset") <- 'subsetYears'
                      if (!is.null(newdata_list)) {
                        attr(newdata_list[[i]]$Dates, "season") <- season
                        attr(newdata_list[[i]]$Dates, "subset") <- 'subsetYears'
                      }
                    }
                  } else { # UBC methods
                      attr(y_list$Dates, "season") <- season
                      attr(y_list$Dates, "subset") <- 'subsetYears'
                      attr(x_list$Dates, "season") <- season
                      attr(x_list$Dates, "subset") <- 'subsetYears'
                      if (!is.null(newdata_list)) {
                        attr(newdata_list$Dates, "season") <- season
                        attr(newdata_list$Dates, "subset") <- 'subsetYears'
                      }
                  }
                }
                # Los datos del modelo se interpolan con la malla de la observación
                # en un proceso subyacente del comando 'biasCorrection'
                
                variable_i.model.bias.interp <- biasCorrection(
                  y = y_list,
                  x = x_list,
                  newdata = newdata_list,
                  precipitation = precipitation,
                  method = bias_method,
                  cross.val = cross_val.arg,
                  folds = folds,
                  fitdistr.args = fitdistr.args,
                  wet.threshold = wet.threshold,
                  n.quantiles = n.quantiles,
                  extrapolation = "constant",
                  return.grid.c = return_grid_c
                )
  
                message("\n==> Terminó el método de 'bias correction'")
                
  
                #' Ploteamos y guardamos los datos corregidos
                #' -------------------------------------------------------------------------
  
                # Univariable bias method
                if (!isMBC) {
                  name_var <- unique(names(var.files))
                  if (type_newdata == "same.historical") {
                    if (cross_val == "none") {
                      base_name <- paste0(name_var, "-c_", int_years[1], "-", int_years[length(int_years)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")", paste0(c("", season_i), collapse = "_S"))
                    } else { # Indicamos el método de CV utilizado
                      base_name <- paste0(name_var, "-c_", int_years[1], "-", int_years[length(int_years)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")_CV-", strsplit(cross_val, "_")[[1]][2], paste0(c("", season_i), collapse = "_S"))
                    }
                  } else {
                    base_name <- paste0(name_var, "-p_", int_years[1], "-", int_years[length(int_years)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")", paste0(c("", season_i), collapse = "_S"))
                  }
                  filename_save <- file.path(path_filename_data, base_name)
                  if (!exists("N_model.rcp.i")) {
                    N_model.rcp.i <- N_model.hist.i
                  }

                  # fig1 <- spatialPlot(climatology(variable_i.model.bias.interp),
                  #   backdrop.theme = "coastline", rev.colors = TRUE,
                  #   main = paste0(
                  #     N_model.hist.i, ": ", name_var, " (", attr(variable_i.model.bias.interp$Variable, "units"), ") de ",
                  #     int_years[1], " a ", int_years[length(int_years)], "\nCorregida utilizando ", toupper(bias_method)
                  #   )
                  # )
                  fig1 <- plot_custom_map(climatology(variable_i.model.bias.interp),
                    name_title = paste0(
                      N_model.rcp.i, ": ", name_var, " (", attr(variable_i.model.bias.interp$Variable, "units"), ") de ",
                      int_years[1], " a ", int_years[length(int_years)], "\nCorregida utilizando ", toupper(bias_method), paste0(c("", season_i), collapse = ". Season ")
                    )
                  )
  
                  filename_fig1 <- file.path(path_filename_fig1, paste0("05_", base_name, img.ext))
                  if (img.ext == ".png") {
                    png(filename_fig1)
                  } else {
                    pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                  }
                  print(fig1)
                  dev.off()
                  message(paste0("Se guarda la imagen: ", filename_fig1, "\n"))
                  rm(fig1, filename_fig1)
  
                  # Guardamos las variables corregidas
                  save(variable_i.model.bias.interp, file = paste0(filename_save, ".Rdata"))
                  message("[", Sys.time(), "] Save '", basename(filename_save), ".Rdata'\n")
                }
                # MBC method
                else {
                  if (!exists("path_filename_fig1.rcp")) {
                    path_filename_fig1.rcp <- path_filename_fig1.hist
                    path_filename_data.rcp <- path_filename_data.hist
                    N_model.rcp.i <- N_model.hist.i
                  }
                  
                  # MBC + "same.historical" implica que los datos del periodo de calibración vuelven
                  # tanto en 'mhat.c' como en 'mhat.p'. Pero si "return_grid_c" es FALSA solo vienen 
                  # en 'mhat.p', que por como está construido el siguiente código, no calcula el bias
                  if (!return_grid_c & (type_newdata == "same.historical")) {
                    names(variable_i.model.bias.interp) <- lapply(names(variable_i.model.bias.interp), function(nombre) gsub("\\.p$", ".c", nombre))
                  }
  
                  for (var.i in names(variable_i.model.bias.interp))
                  {
                    name.var <- strsplit(var.i, "_")[[1]][1]
                    if (grepl("mhat.c", var.i)) {
                      if (cross_val == "none") {
                        base_name <- paste0(name.var, "-c_", years_to_find.c[1], "-", years_to_find.c[length(years_to_find.c)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")", paste0(c("", season_i), collapse = "_S"))
                      } else { # Indicamos el método de CV utilizado
                        base_name <- paste0(name.var, "-c_", years_to_find.c[1], "-", years_to_find.c[length(years_to_find.c)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")_CV-", strsplit(cross_val, "_")[[1]][2], paste0(c("", season_i), collapse = "_S"))
                      }
                      filename_save <- file.path(path_filename_data.hist, base_name)
  
                      # fig1 <- spatialPlot(climatology(variable_i.model.bias.interp[[var.i]]),
                      #   backdrop.theme = "coastline", rev.colors = TRUE,
                      #   main = paste0(
                      #     N_model.hist.i, ": ", var.i, " (", attr(variable_i.model.bias.interp[[var.i]]$Variable, "units"), ") de ",
                      #     years_to_find.c[1], " a ", years_to_find.c[length(years_to_find.c)], "\nCorregida utilizando ", toupper(bias_method)
                      #   )
                      # )
                      fig1 <- plot_custom_map(climatology(variable_i.model.bias.interp[[var.i]]),
                        name_title = paste0(
                          N_model.hist.i, ": ", var.i, " (", attr(variable_i.model.bias.interp[[var.i]]$Variable, "units"), ") de ",
                          years_to_find.c[1], " a ", years_to_find.c[length(years_to_find.c)], "\nCorregida utilizando ", toupper(bias_method), paste0(c("", season_i), collapse = ". Season ")
                        )
                      )
  
                      filename_fig1 <- file.path(path_filename_fig1.hist, paste0("05_", base_name, img.ext))
                      if (img.ext == ".png") {
                        png(filename_fig1)
                      } else {
                        pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                      }
                      print(fig1)
                      dev.off()
                      message(paste0("Se guarda la imagen: ", filename_fig1, "\n"))
                      rm(fig1, filename_fig1)
                    }
  
                    # Ls datos corregidos tras BC sólo serán diferentes a los calculados en mhat.c
                    # cuado sea 'rcp' o 'diff.historical' (y por tanto, sin CV)
                    if ((grepl("mhat.p", var.i)) & (type_newdata %in% c("rcp", "diff.historical"))) {
                      base_name <- paste0(name.var, "-p_", years_to_find.p[1], "-", years_to_find.p[length(years_to_find.p)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), "_", years_to_find.c[1], "-", years_to_find.c[length(years_to_find.c)], ")", paste0(c("", season_i), collapse = "_S"))
                      filename_save <- file.path(path_filename_data.rcp, base_name)
  
                      # fig1 <- spatialPlot(climatology(variable_i.model.bias.interp[[var.i]]),
                      #   backdrop.theme = "coastline", rev.colors = TRUE,
                      #   main = paste0(
                      #     N_model.hist.i, ": ", var.i, " (", attr(variable_i.model.bias.interp[[var.i]]$Variable, "units"), ") de ",
                      #     years_to_find.p[1], " a ", years_to_find.p[length(years_to_find.p)], "\n Corregida utilizando ", toupper(bias_method)
                      #   )
                      # )
                      fig1 <- plot_custom_map(climatology(variable_i.model.bias.interp[[var.i]]),
                        name_title = paste0(
                          N_model.rcp.i, ": ", var.i, " (", attr(variable_i.model.bias.interp[[var.i]]$Variable, "units"), ") de ",
                          years_to_find.p[1], " a ", years_to_find.p[length(years_to_find.p)], "\n Corregida utilizando ", toupper(bias_method), paste0(c("", season_i), collapse = ". Season ")
                        )
                      )
  
                      filename_fig1 <- file.path(path_filename_fig1.rcp, paste0("05_", base_name, img.ext))
                      if (img.ext == ".png") {
                        png(filename_fig1)
                      } else {
                        pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                      }
                      print(fig1)
                      dev.off()
                      message(paste0("Se guarda la imagen: ", filename_fig1, "\n"))
                      rm(fig1, filename_fig1)
                    }
  
                    # Guardamos las variables corregidas
                    data_bias <- variable_i.model.bias.interp[[var.i]]
                    save(data_bias, file = paste0(filename_save, ".Rdata"))
                    message("[", Sys.time(), "] Save '", (filename_save), ".Rdata'\n")
                    rm(data_bias)
                    gc()
                  }
                }
                
  
                #' =====================================================================================
                #' ================= CALCULAMOS EL BIAS Y LA MEJORA TRAS LA CORRECCIÓN =================
                #' =====================================================================================
  
                message("\nPasamos a calcular la mejora tras el BC y el bias con la observación.")
  
                # Univariable bias method
                if (!isMBC) {
                  name_var <- unique(names(var.files))
  
                  #' Cálculo de la mejora sucedida entre el modelo corregido y el modelo sin corregir
                  #' --------------------------------------------------------------------------------
  
                  if (type_newdata == "same.historical") { # No existe new_list
                    if (cross_val == "none") {
                      base_name <- paste0("Mejora_", name_var, "-c_", int_years[1], "-", int_years[length(int_years)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")", paste0(c("", season_i), collapse = "_S"))
                    } else { 
                      # Indicamos el método de CV utilizado
                      base_name <- paste0("Mejora_", name_var, "-c_", int_years[1], "-", int_years[length(int_years)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")_CV-", strsplit(cross_val, "_")[[1]][2], paste0(c("", season_i), collapse = "_S"))
                    }
                    filename_save <- file.path(path_filename_data, base_name)
                              
                    # Cálculo de la mejora entre el modelo corregido y sin corregir
                  	BC_improve <- gridArithmetics(climatology(variable_i.model.bias.interp), climatology(x_list), operator = "-")
  
                  } else {
                    base_name <- paste0("Mejora_", name_var, "-p_", int_years[1], "-", int_years[length(int_years)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")", paste0(c("", season_i), collapse = "_S"))
                    filename_save <- file.path(path_filename_data, base_name)
					 
                    # Cálculo de la mejora entre el modelo corregido y sin corregir
                    BC_improve <- gridArithmetics(climatology(variable_i.model.bias.interp), climatology(newdata_list), operator = "-")
                  }

                  # Si no existe, entonces es que no se ha seleccionado 'rcp' y habrás corregido el modelo histórico
                  if (!exists("N_model.rcp.i")) {
                    N_model.rcp.i <- N_model.hist.i
                  }

                  # fig1 <- spatialPlot(bias,
                  #   backdrop.theme = "coastline", rev.colors = TRUE,
                  #   main = paste0(
                  #     N_model.hist.i, ": Sesgo de ", name_var, " (", attr(variable_i.model.bias.interp$Variable, "units"), ") de ",
                  #     int_years[1], " a ", int_years[length(int_years)], "\nCorregida utilizando ", toupper(bias_method)
                  #   )
                  # )
                  fig1 <- plot_custom_map(BC_improve,
                    name_title = paste0(
                      N_model.rcp.i, ": Mejora de ", name_var, " (", attr(variable_i.model.bias.interp$Variable, "units"), ") de ",
                      int_years[1], " a ", int_years[length(int_years)], "\nCorregida utilizando ", toupper(bias_method), paste0(c("", season_i), collapse = ". Season ")
                    )
                  )
                  filename_fig1 <- file.path(path_filename_fig1, paste0("05_", base_name, img.ext))
                  if (img.ext == ".png") {
                    png(filename_fig1)
                  } else {
                    pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                  }
                  print(fig1)
                  dev.off()
                  message(paste0("Se guarda la imagen: ", filename_fig1, "\n"))
                  rm(fig1, filename_fig1)
  
                  # Guardamos las variables corregidas
                  save(BC_improve, file = paste0(filename_save, ".Rdata"))
                  message("[", Sys.time(), "] Save '", (filename_save), ".Rdata'\n")
                  rm(BC_improve)
                  gc()
  
  
                  #' Cálculo del bias entre el modelo corregido y la observación
                  #' --------------------------------------------------------------------------------
  
                  # Los métodos UBC solo devuelven el periodo de proyección corregido, que en el
                  # caso de 'same.historical' es el mismo que el periodo de calibración
  
                  if (type_newdata == "same.historical") {
                    if (cross_val == "none") {
                      base_name <- paste0("Bias_", name_var, "-c_", int_years[1], "-", int_years[length(int_years)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")", paste0(c("", season_i), collapse = "_S"))
                    } else { # Indicamos el método de CV utilizado
                      base_name <- paste0("Bias_", name_var, "-c_", int_years[1], "-", int_years[length(int_years)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")_CV-", strsplit(cross_val, "_")[[1]][2], paste0(c("", season_i), collapse = "_S"))
                    }
                    filename_save <- file.path(path_filename_data, base_name)
  
                    # Cálculo del bias entre modelo corregido y la observación
                    bias <- gridArithmetics(climatology(variable_i.model.bias.interp), climatology(y_list), operator = "-")
  
                    # fig1 <- spatialPlot(bias,
                    #   backdrop.theme = "coastline", rev.colors = TRUE,
                    #   main = paste0(
                    #     N_model.hist.i, ": Sesgo de ", name_var, " (", attr(variable_i.model.bias.interp$Variable, "units"), ") de ",
                    #     int_years[1], " a ", int_years[length(int_years)], "\nCorregida utilizando ", toupper(bias_method)
                    #   )
                    # )
                    fig1 <- plot_custom_map(bias,
                      name_title = paste0(
                        N_model.hist.i, ": Sesgo de ", name_var, " (", attr(variable_i.model.bias.interp$Variable, "units"), ") de ",
                        int_years[1], " a ", int_years[length(int_years)], "\nCorregida utilizando ", toupper(bias_method), paste0(c("", season_i), collapse = ". Season ")
                      )
                    )
                    filename_fig1 <- file.path(path_filename_fig1, paste0("05_", base_name, img.ext))
                    if (img.ext == ".png") {
                      png(filename_fig1)
                    } else {
                      pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                    }
                    print(fig1)
                    dev.off()
                    message(paste0("Se guarda la imagen: ", filename_fig1, "\n"))
                    rm(fig1, filename_fig1)
  
                    # Guardamos las variables corregidas
                    save(bias, file = paste0(filename_save, ".Rdata"))
                    message("[", Sys.time(), "] Save '", (filename_save), ".Rdata'\n")
                    rm(bias)
                    gc()
                  }
                }
                # MBC method
                else {
                  if (!exists("path_filename_fig1.rcp")) {
                    path_filename_fig1.rcp <- path_filename_fig1.hist
                    path_filename_data.rcp <- path_filename_data.hist
                    N_model.rcp.i <- N_model.hist.i
                  }
  
                  for (var.i in names(variable_i.model.bias.interp))
                  {
                    name.var <- strsplit(var.i, "_")[[1]][1]
  
                    if (grepl("mhat.c", var.i)) {
                      if (cross_val == "none") {
                        base_name <- paste0("Mejora_", name.var, "-c_", years_to_find.c[1], "-", years_to_find.c[length(years_to_find.c)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")", paste0(c("", season_i), collapse = "_S"))
                      } else { # Indicamos el método de CV utilizado
                        base_name <- paste0("Mejora_", name.var, "-c_", years_to_find.c[1], "-", years_to_find.c[length(years_to_find.c)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")_CV-", strsplit(cross_val, "_")[[1]][2], paste0(c("", season_i), collapse = "_S"))
                      }
  
                      filename_save <- file.path(path_filename_data.hist, base_name)
  
                      # Cálculo de la mejora entre el modelo corregido y el modelo sin corregir
                      BC_improve <- gridArithmetics(climatology(variable_i.model.bias.interp[[var.i]]), climatology(x_list[[name.var]]), operator = "-")
  
                      # fig1 <- spatialPlot(bias,
                      #   backdrop.theme = "coastline", rev.colors = TRUE,
                      #   main = paste0(
                      #     N_model.hist.i, ": Sesgo de ", var.i, " (", attr(variable_i.model.bias.interp[[var.i]]$Variable, "units"), ") de ",
                      #     years_to_find.c[1], " a ", years_to_find.c[length(years_to_find.c)], "\nCorregida utilizando ", toupper(bias_method)
                      #   )
                      # )
                      fig1 <- plot_custom_map(BC_improve,
                        name_title = paste0(
                          N_model.hist.i, ": Mejora de ", var.i, " (", attr(variable_i.model.bias.interp[[var.i]]$Variable, "units"), ") de ",
                          years_to_find.c[1], " a ", years_to_find.c[length(years_to_find.c)], "\nCorregida utilizando ", toupper(bias_method), paste0(c("", season_i), collapse = ". Season ")
                        )
                      )
                      filename_fig1 <- file.path(path_filename_fig1.hist, paste0("05_", base_name, img.ext))
                      if (img.ext == ".png") {
                        png(filename_fig1)
                      } else {
                        pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                      }
                      print(fig1)
                      dev.off()
                      message(paste0("Se guarda la imagen: ", filename_fig1, "\n"))
                      rm(fig1, filename_fig1)
  
                      # Guardamos las variables corregidas
                      save(BC_improve, file = paste0(filename_save, ".Rdata"))
                      message("[", Sys.time(), "] Save '", basename(filename_save), ".Rdata'\n")
                      rm(BC_improve)
                      gc()
  
  
                      #' Cálculo del bias entre el modelo corregido y la observación
                      #' --------------------------------------------------------------------------------
  
                      if (cross_val == "none") {
                        base_name <- paste0("Bias_", name.var, "-c_", years_to_find.c[1], "-", years_to_find.c[length(years_to_find.c)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")", paste0(c("", season_i), collapse = "_S"))
                      } else { # Indicamos el método de CV utilizado
                        base_name <- paste0("Bias_", name.var, "-c_", years_to_find.c[1], "-", years_to_find.c[length(years_to_find.c)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "-"), ")_CV-", strsplit(cross_val, "_")[[1]][2], paste0(c("", season_i), collapse = "_S"))
                      }
  
                      filename_save <- file.path(path_filename_data.hist, base_name)
  
                      # Cálculo del bias entre el modelo corregido de calibración y la observación
                      bias <- gridArithmetics(climatology(variable_i.model.bias.interp[[var.i]]), climatology(y_list[[name.var]]), operator = "-")
  
                      # fig1 <- spatialPlot(bias,
                      #   backdrop.theme = "coastline", rev.colors = TRUE,
                      #   main = paste0(
                      #     N_model.hist.i, ": Sesgo de ", var.i, " (", attr(variable_i.model.bias.interp[[var.i]]$Variable, "units"), ") de ",
                      #     years_to_find.c[1], " a ", years_to_find.c[length(years_to_find.c)], "\nCorregida utilizando ", toupper(bias_method)
                      #   )
                      # )
                      fig1 <- plot_custom_map(bias,
                        name_title = paste0(
                          N_model.hist.i, ": Sesgo de ", var.i, " (", attr(variable_i.model.bias.interp[[var.i]]$Variable, "units"), ") de ",
                          years_to_find.c[1], " a ", years_to_find.c[length(years_to_find.c)], "\nCorregida utilizando ", toupper(bias_method), paste0(c("", season_i), collapse = ". Season ")
                        )
                      )
                      filename_fig1 <- file.path(path_filename_fig1.hist, paste0("05_", base_name, img.ext))
                      if (img.ext == ".png") {
                        png(filename_fig1)
                      } else {
                        pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                      }
                      print(fig1)
                      dev.off()
                      message(paste0("Se guarda la imagen: ", filename_fig1, "\n"))
                      rm(fig1, filename_fig1)
  
                      # Guardamos las variables corregidas
                      save(bias, file = paste0(filename_save, ".Rdata"))
                      message("[", Sys.time(), "] Save '", basename(filename_save), ".Rdata'\n")
                      rm(bias)
                      gc()
                    }
  
                    # Para el periodo de proyección nunca se podrá calcular el bias, aunque sea 'diff.historical'
                    # ya que los datos de y_list se han acotado con un subsetGrid
                    # Y la mejora tras el BC, solo será diferente a la calculada en mhat.c cuado sea
                    # 'rcp' o 'diff.historical'
                    if ((grepl("mhat.p", var.i)) & (type_newdata %in% c("rcp", "diff.historical"))) {
                      base_name <- paste0("Mejora_", name.var, "-p_", years_to_find.p[1], "-", years_to_find.p[length(years_to_find.p)], "_", toupper(bias_method), "(", paste(variables.bias.i, collapse = "_"), "_", years_to_find.c[1], "-", years_to_find.c[length(years_to_find.c)], ")", paste0(c("", season_i), collapse = "_S"))
                      filename_save <- file.path(path_filename_data.rcp, base_name)
  
                      # Cálculo de la mejora entre el modelo corregido y el modelo sin corregir
                      BC_improve <- gridArithmetics(climatology(variable_i.model.bias.interp[[var.i]]), climatology(newdata_list[[name.var]]), operator = "-")
  
                      # fig1 <- spatialPlot(bias,
                      #   backdrop.theme = "coastline", rev.colors = TRUE,
                      #   main = paste0(
                      #     N_model.hist.i, ": Sesgo de ", var.i, " (", attr(variable_i.model.bias.interp[[var.i]]$Variable, "units"), ") de ",
                      #     years_to_find.p[1], " a ", years_to_find.p[length(years_to_find.p)], "\n Corregida utilizando ", toupper(bias_method)
                      #   )
                      # )
                      fig1 <- plot_custom_map(BC_improve,
                        name_title = paste0(
                          N_model.rcp.i, ": Mejora de ", var.i, " (", attr(variable_i.model.bias.interp[[var.i]]$Variable, "units"), ") de ",
                          years_to_find.p[1], " a ", years_to_find.p[length(years_to_find.p)], "\n Corregida utilizando ", toupper(bias_method), paste0(c("", season_i), collapse = ". Season ")
                        )
                      )
  
                      filename_fig1 <- file.path(path_filename_fig1.rcp, paste0("05_", base_name, img.ext))
                      if (img.ext == ".png") {
                        png(filename_fig1)
                      } else {
                        pdf(filename_fig1, width = 7, height = 5, bg = "transparent")
                      }
                      print(fig1)
                      dev.off()
                      message(paste0("Se guarda la imagen: ", filename_fig1, "\n"))
                      rm(fig1, filename_fig1)
  
                      # Guardamos las variables corregidas
                      save(BC_improve, file = paste0(filename_save, ".Rdata"))
                      message("[", Sys.time(), "] Save '", basename(filename_save), ".Rdata'\n")
                      rm(BC_improve)
                      gc()
                    }
                  }
                }
  
                # Eliminamos las variables que no necesitamos
                rm(x_list, y_list, newdata_list, variable_i.model.bias.interp, mask.obs)
                gc()
                
              },
              error = function(e) {
                message("\nERROR: ", conditionMessage(e), "\n\n")
  
                archivo_errores <- file(path_errores, open = "a")
                cat(paste0("\n", N_model.hist.i, ": ", cordex.model.hist.i),
                  file = archivo_errores, "\n", append = TRUE
                )
  
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
          } else {
            if (!x_hist_conditions) {
              message(paste("No hay los mismos archivos .Rdata para las variables seleccionadas del modelo histórico."))
            }
            if (!newdata_rcp_conditions) {
              message(paste("No hay los mismos archivos .Rdata para las variables seleccionadas del modelo RCP."))
            }
          }
        }
      } else {
        message(paste0("El modelo ", N_model.hist.i, " corresponde con el escenario RCP8.5"))
      }
    }
  } else {
    message("No cumples las condiciones para ejecutar el método de 'Bias Correction' escogido.")
  }


#' Limpiamos el entorno de trabajo
#' -----------------------------------------------------------------------------

rm(list = ls())
graphics.off()
gc()


}
