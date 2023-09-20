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
folder_data <- "CordexDev_CV2_BC_season"
folder_data_ref <- "iberia01"

# Carpeta donde se van a guardar los datos dentro del 'path_Datos_save' (se crea si no existe)
folder_saved <- "Figures_season"


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
model_folder <- list.dirs(file.path(path_Datos, folder_data), recursive = FALSE)
model_folder <- model_folder[grepl(paste0("^", "model_"), basename(model_folder), ignore.case = TRUE)] # ("^" ,"model_1", "$")

# Añadimo Iberia01
# model_folder <- c(file.path(path_Datos_iberia_metadatos, "iberia01"), model_folder)
model_folder_iberia <- file.path(path_Datos_iberia_metadatos, "iberia01")


# Nombre de la función atómica del ínidice agroclimático
variable_list <- list(c("pr", "tas"), c("tasmax", "tasmin"), c("tas", "tasmax"))

# Tipo de CV: "WarmCold" o NULL
CV_used <- "Random" # "WarmCold", NULL
BC_methods <- list("eqm", "pqm", "qdm", "mbcr", "mbcn")

season <- c(4:10)

# Formato de las imágenes
img.ext <- ".png" # pdf ó png
plot_indexmap <- TRUE

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



#' =============================================================================
#' ========================= AÑOS CÁLIDOS Y AÑOS FRÍOS =========================
#' =============================================================================

if (!is.null(CV_used)) {
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
} else {
  int_years <- (1986:2005)
}


#' =============================================================================
#' ========================= METADATOS DE LOS MODELOS ==========================
#' =============================================================================

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
path_filename_data <- file.path(path_Datos_save, folder_saved, "Fig5")
if (!file.exists(path_filename_data)) {
  dir.create(path_filename_data)
  message(paste0("==> Se ha creado la carpeta: ", (path_filename_data)))
}



#' =============================================================================
#' ========================== PARA CADA MODELO ... =============================
#' =============================================================================

var_corr_list <- list()

for (pack_variable.i in variable_list) {
  
  name_pack_var <- paste0(pack_variable.i, collapse = "_")
  
  # Generamos (si no existe) la carpeta que contendrá las figuras
  path_filename_data <- file.path(path_Datos_save, folder_saved, "Fig5", name_pack_var)
  if (!file.exists(path_filename_data)) {
    dir.create(path_filename_data)
    message(paste0("==> Se ha creado la carpeta: ", (path_filename_data)))
  }
  
  # Contendrá los datos del modelo
  var_corr_list[[name_pack_var]] <- list()

  # Guardará los nombres y su orden de aparición
  name_model_data <- c()
  number_model_data <- c()
  
  # Para cada modelo...
  for (path.model.i in model_folder) {
    
    # Modelo a evaluar
    # ----------------------------------------------------------------------------
    
    # Seleccionamos el modelo
    N_model.data.i <- basename(path.model.i)
    
    if (N_model.data.i != "iberia01") {
      # Cargamos la lista de DataFrames de los modelos
      list.df.model <- load_and_extract(file.path(path_metadata_models, "df_metadata_models_DEV.Rdata"))
      
      
      df.i <- list.df.model[[N_model.data.i]]
      cordex.model.i <- as.character(df.i$longname[1])
      
      # Eliminamos las variables
      rm(list.df.model)
      gc()
    } else {
      cordex.model.i <- "Iberia01"
    }
    
    # Creamos la lista que contendrá los datos asociados a los métodos BC
    var_corr_list[[name_pack_var]][[N_model.data.i]] <- list()

    # Guardamos el orden de los nombres y Nº de modelos
    name_model_data <- c(name_model_data, cordex.model.i)
    number_model_data <- c(number_model_data, N_model.data.i)
    
    tryCatch(
      {
        message(paste0("\n============================ ", N_model.data.i, " ============================="))
        message("==================================================================================")
        
        # Lista de archivos .Rdata en el directorio del modelo donde se encuentran los archivos de los índices
        filesRdata <- list.files(path.model.i, pattern = "\\.Rdata$", full.names = TRUE)
        
        if (length(filesRdata) == 0) {
          # Pasamos al siguiente modelo histórico
          message(paste0("No hay archivos .Rdata en la carpeta para el método '", basename(path.model.i), "'"))
          next
        }
        
        # Operamos con cada índice
        for (BC_used in BC_methods) {
          message(paste0("\n=================== ", name_pack_var, " + ", toupper(BC_used), " + ", CV_used, " ===================="))
          
          # Obtenemos los paths de las variables que intervienen.
          # Una variable puede tener varios archivos .Rdata asociados
          if (N_model.data.i != "iberia01") {
            var.files <- list()
            for (var.name.i in pack_variable.i) {
              if (is.null(CV_used)) {
                pattern <- paste0("^", var.name.i, "-.*", toupper(BC_used))
                var.files[[var.name.i]] <- subset(filesRdata, grepl(pattern, basename(filesRdata)) & !grepl("_CV-",  basename(filesRdata)) & !grepl("_complete",  basename(filesRdata)))
                
              } else {
                pattern <- paste0("^", var.name.i, "-.*", toupper(BC_used), ".*", CV_used)
                var.files[[var.name.i]] <- subset(filesRdata, grepl(pattern, basename(filesRdata)) & !grepl("_complete",  basename(filesRdata)))
              }
            }
          } else { # Caso Iberia01
            var.files <- list()
            for (var.name.i in pack_variable.i) {
              pattern <- paste0("^", var.name.i, "_.*")
              var.files[[var.name.i]] <- subset(filesRdata, grepl(pattern, basename(filesRdata)) & !grepl("_CV-",  basename(filesRdata)) & !grepl("_complete",  basename(filesRdata)))
            }
          }
          
          
          #' Ajustamos / cargamos las variables
          #' -------------------------------------------------------------------
          list.arg <- list()
          
          if (length(var.files[[var.name.i[1]]]) != 0) {
            for (var.name.i in pack_variable.i) {
              
              #' Cargamos los datos de la variable del modelo
              #' -----------------------------------------------------------------------------
              
              # Carga y combina los datos de los archivos .Rdata en un solo marco de datos
              data.list <- lapply(var.files[[var.name.i]], load_and_extract)
              message(paste0("\n==> Se han cargado los .Rdata (", length(var.files[[var.name.i]]), ") de la variable '", var.name.i, "'\n"))
              
              #' Unimos los datos de la variable en un único archivo
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
              
              
              
              #' Reescalar los datos del modelo a las unidades de la observación
              #' -------------------------------------------------------------------------
              
              # Convertimos las dimensiones de las variables a ºC y kg m-2 si así lo queremos
              if (option_convert) {
                message(paste0("Se evalúa si es necesario cambiar las unidades de las variables del '", N_model.data.i, "' para coincidir con las unidades de '", N_model.data.i, "':"))
                
                # Temperaturas
                if (var.name.i %in% c("tasmin", "tasmax", "tas")) {
                  if (attr(data_all$Variable, "units") == "K") {
                    message(paste0("\t==> Variable ", var.name.i, ": units "), attr(data_all$Variable, "units"), " to ºC")
                    data_all <- udConvertGrid(data_all, "degree_Celsius")
                  
                  
                    # Ploteamos la imagen de los datos
                    filename_fig1 <- file.path(path_filename_data, paste0("00_", var.name.i, "_", int_years[1], "-", int_years[length(int_years)], "_02_newUnits", img.ext))
                    
                    fig1 <- plot_custom_map(climatology(data_all),
                                            name_title = paste0(N_model.data.i, ": ", var.name.i, " (", attr(data_all$Variable, "units"), ") de ", int_years[1], " a ", int_years[length(int_years)]),
                                            name.palette = "RdBu"
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
                
                # Precipitación
                if (var.name.i %in% c("pr")) {
                  if (attr(data_all$Variable, "units") == "kg m-2 s-1") {
                    message(paste0("\t==> Variable ", var.name.i, ": units "), attr(data_all$Variable, "units"), " to kg m-2")
                    nt <- length(getRefDates(data_all))
                    ls <- lapply(1:nt, function(i) {
                      timei <- subsetDimension(data_all, dimension = "time", indices = i) # separa día a día
                      gridArithmetics(timei, 86400, operator = "*") # multiplica los segundos de un día para pasar a kg m-2
                    })
                    rm(data.all)
                    data_all <- bindGrid(ls, dimension = "time") # bindGrid es una función de transforeR que vuelve a unir todos los días en un grid de climate4R
                    rm(ls, nt)
                    gc()
                    
                    # Asignamos al atributo del grid las nuevas unidades
                    attr(data_all$Variable, "units") <- "kg m-2"
                    
                    # Ploteamos la imagen de los datos
                    filename_fig1 <- file.path(path_filename_data, paste0("00_", var.name.i, "_", int_years[1], "-", int_years[length(int_years)], "_02_newUnits", img.ext))
                    
                    fig1 <- plot_custom_map(climatology(data_all),
                                            name_title = paste0(N_model.data.i, ": ", var.name.i, " (", attr(data_all$Variable, "units"), ") de ", int_years[1], " a ", int_years[length(int_years)]),
                                            name.palette = "RdBu"
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
              }
              
              #' Se selecciona del 1 de abril al 30 de septiembre (periodo de los índices)
              #' ---------------------------------------------------------------------
              
              message("Se obtienen los datos definidos en 'season'")
              data_all <- subsetGrid(data_all,
                                     # years = int_years[]
                                     season = season,
                                     # lonLim = c(-6, -1), # c(-10, 5),
                                     # latLim = c(41, 43.8) # c(35, 44))
              ) 
              
              #' Se guarda la variable
              #' ---------------------------------------------------------------------
              
              list.arg[[var.name.i]] <- data_all
              rm(data_all)
              gc()
              
            }
            
            
            #' En este punto ya tenemos las variables preprocesadas en la lista "list.arg"
            #' -----------------------------------------------------------------------
            message("\nVariables cargadas. Pasamos a calcular la correlación entre ambas.")
            
            
            #' Calculamos la correlación entre variables
            #' -----------------------------------------------------------------------
            
            # Utiliza cor.method="pearson" para temperaturas y 
            #         cor.method="spearman" si interviene precipitación
            if ("pr" %in% pack_variable.i) {
              cor.method <- "spearman"
            } else {
              cor.method <- "pearson"
            }
            
            correlation <- suppressMessages(climatology(list.arg[[1]])) # fake C4R object to fill with correlation values (dims [1, lons, lats] )
            for(i in 1:dim(list.arg[[1]]$Data)[2]) { # Recorremos la latitud
              for(j in 1:dim(list.arg[[1]]$Data)[3]) { # Recorremos la longitud
                # mydata <- data.frame(list.arg[[1]]$Data[,i,j], list.arg[[2]]$Data[,i,j])
                corr.aux <- cor(list.arg[[1]]$Data[,i,j], list.arg[[2]]$Data[,i,j], 
                                use="pairwise.complete.obs", 
                                method = cor.method) # esto da un valor
                correlation$Data[,i, j] <- corr.aux
                rm(corr.aux)
              }  
            }
            # Modificamos atributos y nombres
            correlation$Variable$varName <- paste(cor.method, "correlation of", paste0(pack_variable.i, collapse = " and "))
            attr(correlation$Variable, "units") <- ""
            attr(correlation$Variable, "longname") <- paste(cor.method, "correlation of", paste0(pack_variable.i, collapse = " and "), "for", N_model.data.i, "-", toupper(BC_used))
            
            # Guardamos el grid de correlaciones
            var_corr_list[[name_pack_var]][[N_model.data.i]][[BC_used]] <- correlation
            rm(correlation)
            gc()
            
            if (isTRUE(plot_indexmap)) {
              message(paste0("Se pinta el mapa de '", name_pack_var, "' para ", basename(path.model.i)))
              
              # Guardamos la imagen
              base_name <- paste0("Fig5_Correlation_", basename(path.model.i), "_", name_pack_var, paste0(c("", toupper(BC_used), CV_used), collapse = "_"), ".Rdata")
              filename_fig1 <- file.path(path_filename_data, gsub(".Rdata", img.ext, base_name))
              fig1 <- plot_custom_map(climatology(var_corr_list[[name_pack_var]][[N_model.data.i]][[BC_used]]),
                                      name_title = paste0(
                                        basename(path.model.i), " - ", name_pack_var, paste0(c("", toupper(BC_used), CV_used), collapse = " - "),
                                        "\nCorrelación de ", cor.method
                                      ), name.palette = "RdBu", rev.colors = TRUE
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
            
          } else {
            message(paste("No hay archivos .Rdata para el conjunto ", name_pack_var, "y ", toupper(BC_used)))
          }
        } # for BC_methods
        
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
#' ===================== MODELOS DE REFERENCIA IBERIA 01 =======================
#' =============================================================================


for (pack_variable.i in variable_list) {
  
  name_pack_var <- paste0(pack_variable.i, collapse = "_")
  
  # Generamos (si no existe) la carpeta que contendrá las figuras
  path_filename_data <- file.path(path_Datos_save, folder_saved, "Fig5", name_pack_var)
  if (!file.exists(path_filename_data)) {
    dir.create(path_filename_data)
    message(paste0("==> Se ha creado la carpeta: ", (path_filename_data)))
  }
  
  
  # Para iberia01
  for (path.model.i in model_folder_iberia) {
    
    # Modelo a evaluar
    # ----------------------------------------------------------------------------
    
    # Seleccionamos el modelo
    N_model.data.i <- basename(path.model.i)
    cordex.model.i <- "Iberia01"
    
    tryCatch(
      {
        message(paste0("\n============================ ", N_model.data.i, " ============================="))
        message("==================================================================================")
        
        # Lista de archivos .Rdata en el directorio del modelo donde se encuentran los archivos de los índices
        filesRdata <- list.files(path.model.i, pattern = "\\.Rdata$", full.names = TRUE)
        
        if (length(filesRdata) == 0) {
          # Pasamos al siguiente modelo histórico
          message(paste0("No hay archivos .Rdata en la carpeta para el método '", basename(path.model.i), "'"))
          next
        }
        
        # Operamos con cada índice
          message(paste0("\n=================== ", name_pack_var, " + ", CV_used, " ===================="))
          
          # Obtenemos los paths de las variables que intervienen.
          # Una variable puede tener varios archivos .Rdata asociados
          # Caso Iberia01
          var.files <- list()
          for (var.name.i in pack_variable.i) {
            pattern <- paste0("^", var.name.i, "_.*")
            var.files[[var.name.i]] <- subset(filesRdata, grepl(pattern, basename(filesRdata)) & !grepl("_CV-",  basename(filesRdata)) & !grepl("_complete",  basename(filesRdata)))
          }
          
          
          
          #' Ajustamos / cargamos las variables
          #' -------------------------------------------------------------------
          list.arg <- list()
          
          if (length(var.files[[var.name.i[1]]]) != 0) {
            for (var.name.i in pack_variable.i) {
              
              #' Cargamos los datos de la variable del modelo
              #' -----------------------------------------------------------------------------
              
              # Carga y combina los datos de los archivos .Rdata en un solo marco de datos
              data.list <- lapply(var.files[[var.name.i]], load_and_extract)
              message(paste0("\n==> Se han cargado los .Rdata (", length(var.files[[var.name.i]]), ") de la variable '", var.name.i, "'\n"))
              
              #' Unimos los datos de la variable en un único archivo
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
              
              
              
              #' Reescalar los datos del modelo a las unidades de la observación
              #' -------------------------------------------------------------------------
              
              # Convertimos las dimensiones de las variables a ºC y kg m-2 si así lo queremos
              if (option_convert) {
                message(paste0("Se evalúa si es necesario cambiar las unidades de las variables del '", N_model.data.i, "':"))
                
                # Temperaturas
                if (var.name.i %in% c("tasmin", "tasmax", "tas")) {
                  if (attr(data_all$Variable, "units") == "K") {
                    message(paste0("\t==> Variable ", var.name.i, ": units "), attr(data_all$Variable, "units"), " to ºC")
                    data_all <- udConvertGrid(data_all, "degree_Celsius")
                    
                    
                    # Ploteamos la imagen de los datos
                    filename_fig1 <- file.path(path_filename_data, paste0("00_", var.name.i, "_", int_years[1], "-", int_years[length(int_years)], "_02_newUnits", img.ext))
                    
                    fig1 <- plot_custom_map(climatology(data_all),
                                            name_title = paste0(N_model.data.i, ": ", var.name.i, " (", attr(data_all$Variable, "units"), ") de ", int_years[1], " a ", int_years[length(int_years)]),
                                            name.palette = "RdBu"
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
                
                # Precipitación
                if (var.name.i %in% c("pr")) {
                  if (attr(data_all$Variable, "units") == "kg m-2 s-1") {
                    message(paste0("\t==> Variable ", var.name.i, ": units "), attr(data_all$Variable, "units"), " to kg m-2")
                    nt <- length(getRefDates(data_all))
                    ls <- lapply(1:nt, function(i) {
                      timei <- subsetDimension(data_all, dimension = "time", indices = i) # separa día a día
                      gridArithmetics(timei, 86400, operator = "*") # multiplica los segundos de un día para pasar a kg m-2
                    })
                    rm(data.all)
                    data_all <- bindGrid(ls, dimension = "time") # bindGrid es una función de transforeR que vuelve a unir todos los días en un grid de climate4R
                    rm(ls, nt)
                    gc()
                    
                    # Asignamos al atributo del grid las nuevas unidades
                    attr(data_all$Variable, "units") <- "kg m-2"
                    
                    # Ploteamos la imagen de los datos
                    filename_fig1 <- file.path(path_filename_data, paste0("00_", var.name.i, "_", int_years[1], "-", int_years[length(int_years)], "_02_newUnits", img.ext))
                    
                    fig1 <- plot_custom_map(climatology(data_all),
                                            name_title = paste0(N_model.data.i, ": ", var.name.i, " (", attr(data_all$Variable, "units"), ") de ", int_years[1], " a ", int_years[length(int_years)]),
                                            name.palette = "RdBu"
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
              }
              
              #' Se selecciona del 1 de abril al 30 de septiembre (periodo de los índices)
              #' ---------------------------------------------------------------------
              
              message("Se obtienen los datos definidos en 'season'")
              data_all <- subsetGrid(data_all,
                                     # years = int_years[]
                                     season = season,
                                     # lonLim = c(-6, -1), # c(-10, 5),
                                     # latLim = c(41, 43.8) # c(35, 44))
              ) 
              
              #' Se guarda la variable
              #' ---------------------------------------------------------------------
              
              list.arg[[var.name.i]] <- data_all
              rm(data_all)
              gc()
              
            }

            
            #' En este punto ya tenemos las variables preprocesadas en la lista "list.arg"
            #' -----------------------------------------------------------------------
            message("\nVariables cargadas. Pasamos a calcular la correlación entre ambas.")
            
            
            #' Calculamos la correlación entre variables
            #' -----------------------------------------------------------------------
            
            # Utiliza cor.method="pearson" para temperaturas y 
            #         cor.method="spearman" si interviene precipitación
            if ("pr" %in% pack_variable.i) {
              cor.method <- "spearman"
            } else {
              cor.method <- "pearson"
            }
            
            correlation <- suppressMessages(climatology(list.arg[[1]])) # fake C4R object to fill with correlation values (dims [1, lons, lats] )
            for(i in 1:dim(list.arg[[1]]$Data)[2]) { # Recorremos la latitud
              for(j in 1:dim(list.arg[[1]]$Data)[3]) { # Recorremos la longitud
                # mydata <- data.frame(list.arg[[1]]$Data[,i,j], list.arg[[2]]$Data[,i,j])
                corr.aux <- cor(list.arg[[1]]$Data[,i,j], list.arg[[2]]$Data[,i,j], 
                                use="pairwise.complete.obs", 
                                method = cor.method) # esto da un valor
                correlation$Data[,i, j] <- corr.aux
                rm(corr.aux)
              }  
            }
            # Modificamos atributos y nombres
            correlation$Variable$varName <- paste(cor.method, "correlation of", paste0(pack_variable.i, collapse = " and "))
            attr(correlation$Variable, "units") <- ""
            attr(correlation$Variable, "longname") <- paste(cor.method, "correlation of", paste0(pack_variable.i, collapse = " and "), "for", N_model.data.i)
            
            # Guardamos el grid de correlaciones
            var_corr_list[[name_pack_var]][[N_model.data.i]] <- correlation
            rm(correlation)
            gc()
            
            if (isTRUE(plot_indexmap)) {
              message(paste0("Se pinta el mapa de '", name_pack_var, "' para ", basename(path.model.i)))
              
              # Guardamos la imagen
              base_name <- paste0("Fig5_Correlation_", basename(path.model.i), "_", name_pack_var, paste0(c("", toupper(BC_used), CV_used), collapse = "_"), ".Rdata")
              filename_fig1 <- file.path(path_filename_data, gsub(".Rdata", img.ext, base_name))
              fig1 <- plot_custom_map(climatology(var_corr_list[[name_pack_var]][[N_model.data.i]]),
                                      name_title = paste0(
                                        basename(path.model.i), " - ", name_pack_var, paste0(c("", CV_used), collapse = " - "),
                                        "\nCorrelación de ", cor.method
                                      ), name.palette = "RdBu", rev.colors = TRUE
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
            
          } else {
            message(paste("No hay archivos .Rdata para el conjunto ", name_pack_var))
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

# Guardamos el orden de los nombres y Nº de modelos
name_model_data <- c(name_model_data, cordex.model.i)
number_model_data <- c(number_model_data, N_model.data.i)


#' =============================================================================
#' =========================== DIBUJAMOS LOS BOXPLOTS ==========================
#' =============================================================================



# Primeros los HadGeM2 (CCLM4 y RCA) y después los MPI
# model_order <- c("model_3", "model_20", "model_4", "model_21") # number_model_data
# sort_names <- c("HadGeM2 - CCLM4-8-17", "HadGeM2 - RCA4", "MPI - CCLM4-8-17", "MPI - RCA4")

model_order <- c("model_4", "model_21", "model_3", "model_20") # number_model_data
sort_names <- c("MPI-CCLM4", "MPI-RCA4", "HadGeM2-CCLM4", "HadGeM2-RCA4")

var_grid_list_arg <- list()


for (variable.i in names(var_corr_list)) {
  
  # Guardar datos
  grid_list_arg <- list()

  # Guardar nombres
  subtitle_plot <- c()

  m <- 1
  j <- 2
  
  # Añadimos iberia01 en la posición "a" letters[1]
  grid_list_arg[[paste0(letters[1], " ", "Iberia01")]] <- var_corr_list[[variable.i]][[N_model.data.i]]
  subtitle_plot <- c(subtitle_plot, "Iberia01")
  
  for (model_j in 1:length(model_order)) {
    # j <- 1
    for (BC_used in BC_methods) {
      grid_list_arg[[paste0(letters[j], " ", sort_names[model_j], " (", toupper(BC_used), ")")]] <- var_corr_list[[variable.i]][[model_order[model_j]]][[BC_used]]

      subtitle_plot <- c(subtitle_plot, paste0(sort_names[model_j], " (", toupper(BC_used), ")"))
      # names_plot <- c(names_plot, paste0(sort_names[model_j], " (", toupper(BC_used), ")"))
      j <- j + 1
    }
    m <- m + 1
  }
  
  # Añadimos a Iberia01 en la evolución temporal

  
  
  # Guardamos los datos para cada variable
  var_grid_list_arg[[variable.i]] <- grid_list_arg

  rm(grid_list_arg)
  gc()
}


#' Se guarda el archivo de datos
#' -----------------------------------------------------------------------------

# save(var_grid_list_arg, file = file.path(path_Datos_save, "Figures", "fig5_correlation.Rdata"))
save(var_grid_list_arg, file = file.path(path_Datos_save, folder_saved, "Fig5", paste0("fig5_correlation", "_S", paste0(c(min(season, na.rm = T), max(season, na.rm = T)), collapse = "-"), ".Rdata")))
message("Se ha guardado", file.path(path_Datos_save, folder_saved, "Fig5", paste0("fig5_correlation", "_S", paste0(c(min(season, na.rm = T), max(season, na.rm = T)), collapse = "-"), ".Rdata")))



#' Seleccionamos la variable para la figura
#' -----------------------------------------------------------------------------

variable.i <- names(var_grid_list_arg)[1]
message("Se ha seleccionado: ", variable.i)

# ------------------------------------------------------------------------------
# ----------------------------- Figure 4 ---------------------------------------

# Datos de la variable
grid_plot <- var_grid_list_arg[[variable.i]]

# Obtenemos el percentil 50 de iberia para colocar la línea horizontal
h_line <- quantile(as.vector(grid_plot[[1]]$Data), probs = c(0.5), na.rm = T)


# Obtenemos los límites de la variable
grid_list_arg <- var_grid_list_arg[[variable.i]]

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
methodList.i <- BC_methods #c("eqm", "pqm", "qdm", "mbcr", "mbcn")

methodList <- rep(methodList.i, times = length(modelos.i)) # añadir iberia01
modelosList <- rep(modelos.i, each = length(modelos.i) * length(methodList.i))

# length(indices.i) = nº de gráficas
parts <- unlist(strsplit(variable.i, "_"))
indices.i <- c(paste("(", parts[1], ", ", parts[2], ")", sep = ""))
indices <- rep(indices.i, each = length(modelos.i) * length(methodList.i))

colours.aux <- brewer.pal(n = 12, name = "Paired")
colours <- colours.aux[seq(2, length(colours.aux), 2)[1:5]]
coloursList <- rep(colours, times = length(modelos.i))

# Añadimos a iberia
indices <- c(indices.i, indices) # Añadimos el que le correspondería a iberia
coloursList <- c("#939290", coloursList) 
colours <- c("#939290", colours)
methodList.i <- c("Iberia01", toupper(methodList.i))

# Definimos los máximos y mínimos del eje Y
y_lim_min <- -0.5 # clim_min
y_lim_max <- 0.5 # clim_max

fig1 <- violinPlot(grid_plot,
           group.index = indices,
           violin = FALSE,
           fill = coloursList,
           h.lines = h_line,
           v.lines = 1, # lineas verticales para separar y línea horizontal en el 0 marcando cero bias
           bwplot.custom = list(
             notch = FALSE,
             do.out = FALSE,
             as.table = TRUE,
             ylab = list(label = "Correlación"),
             xlab = list(label = ""),
             scales = list(
               x = list(rot = 45, labels = subtitle_plot), # Volvemos a añadir los nombres a los grid
               y = list(relation = "free", limits = c(y_lim_min, y_lim_max), rot = 0)
             ), # límites distintos en cada panel
             ylim = c(y_lim_min, y_lim_max),
             key = list( # Leyenda
               space = "bottom", cex = 1,
               text = list((methodList.i)),
               columns = 3,
               points = list(col = colours, pch = 19)
             )
           )
)

message(paste0("Fig5_", variable.i))
fig1

# Guardamos la imagen
if (is.null(season)) {
  season <- c(0:12)
}
img.ext = ".pdf"
base_name <- paste0("Fig5_Corrleacion_", variable.i, "_S", paste0(c(min(season, na.rm = T), max(season, na.rm = T)), collapse = "-"), "_V1.Rdata")
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
