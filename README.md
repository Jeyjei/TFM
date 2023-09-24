<p align="center">
<img src="logo.svg"/>
</p>

## Entorno Mamba
Se ha utilizado [Mambaforge](https://github.com/conda-forge/miniforge#mambaforge) para llevar a cabo este proyecto, creando un entorno donde se han instalado todos los paquetes necesarios. Con el objetivo de facilitar la reproducibilidad del trabajo, se exportó el entorno mamba en un archivo `.yml` que se encuentra en el directorio principal. En función del sistema operativo que vaya usted a utilizar exite:
- Para Ubuntu: `ubuntu_env_meteo3.yml`
- Para Windows: `win_env_climate4r3.yml`

Una vez instalado Mambaforge, se crea un entorno conda/mamba a partir del archivo `.yml` con el [siguiente comando](https://iq.opengenus.org/clone-conda-environment/) sobre la terminal de Mambaforge. En el siguiente ejemplo se crea el entorno _clim4R_ utilizando el archivo de instalación de Ubuntu:
```bash
conda env create --name clim4R --file ubuntu_env_meteo3.yml
```
Si se prefiere no utilizar el archivo de instalación `.yml`, se puede crear un entorno manualmente y descargar las librerías de [climate4R](https://github.com/SantanderMetGroup/climate4R) de la siguiente forma sobre la terminal de Mambaforge:
```bash
conda create -n clim4R
conda activate clim4R
conda install --override-channels -c conda-forge r-base r-devtools rstudio-desktop
conda install -y -c conda-forge -c r -c defaults -c santandermetgroup r-loader r-transformer r-visualizer r-loader.java r-climate4r.udg r-downscaler r-convertr
```
<div style="background-color: #FFDAB9; color: black; padding: 10px;">
<strong>Aviso:</strong> A priori, puede seleccionar <i>mamba ...</i> o <i>conda ...</i> indistíntamente. Personalmente, utilizo <i>mamba ...</i> para <strong>Windows</strong> y <i>conda ...</i> para <strong>Ubuntu</strong> tras sufrir algunos problemas en la instalación y conflictos de dependencias.
</div> <br>


Ahora ya se encuentran instalados los paquetes y librerías básicas para comenzar con el proyecto. Para continuar asegurando la reproducibilidad del proyecto, se recomienda instalar las mismas versiones de los paquetes de [climate4R](https://github.com/SantanderMetGroup/climate4R) que se emplearon en este proyecto y el resto de librerias necesarias (índices climáticos y métodos de correción de sesgo multivariable). 

Una vez activado el entorno que se ha creado, se actualizan las versiones específicas de los paquetes de [climate4R](https://github.com/SantanderMetGroup/climate4R) desde el repositorio de GitHub. 

Para __Ubuntu__, desde la _terminal de R_:
```R
install.packages("remotes")
remotes::install_github("SantanderMetGroup/visualizeR@v1.6.3")
remotes::install_github("SantanderMetGroup/downscaleR@v3.3.4")
remotes::install_github("SantanderMetGroup/convertR@v0.2.1")
remotes::install_github("SantanderMetGroup/loadeR@v1.8.1")
remotes::install_github("SantanderMetGroup/transformeR@v2.1.5")
remotes::install_github("SantanderMetGroup/climate4R.UDG@devel")
remotes::install_github("Jeyjei/climate4R.indices")
remotes::install_github("Jeyjei/downscaleR@devel-JJVelasco")
```

Para __Windows__, si no lo has hecho antes, recomiendo instalar previamente el paquete [loadeR](https://github.com/SantanderMetGroup/loadeR) desde la terminal de Mambaforge (dentro del entorno creado):
```bash
mamba activate clim4R
mamba install -y -c conda-forge -c r -c defaults -c santandermetgroup r-loader
```
Desde la _terminal de R_:
```R
install.packages("remotes", type = "win.binary")
remotes::install_github("SantanderMetGroup/visualizeR@v1.6.3")
remotes::install_github("SantanderMetGroup/downscaleR@v3.3.4")
remotes::install_github("SantanderMetGroup/convertR@v0.2.1")
remotes::install_github("SantanderMetGroup/loadeR@v1.8.1")
remotes::install_github("SantanderMetGroup/transformeR@v2.1.5")
remotes::install_github("SantanderMetGroup/climate4R.UDG@devel")
remotes::install_github("Jeyjei/climate4R.indices")
remotes::install_github("Jeyjei/downscaleR@devel-JJVelasco")
```

Si aparece algún error a la hora de instalar las librerías de [climate4R](https://github.com/SantanderMetGroup/climate4R) desde la terminal de R, por favor, primero intente instalar la librería desde la terminal de Mambaforge utilizando los canales principales (como se ha hecho previamente con el paquete _loadeR_):
```bash 
mamba install -y -c conda-forge -c r -c defaults -c santandermetgroup r-loader
```
o si quieres más paquetes
```bash
conda install -y -c conda-forge -c r -c defaults -c santandermetgroup r-loader r-transformer r-visualizer r-loader.java r-climate4R.UDG r-downscaleR
```
<div style="background-color: #FFDAB9; color: black; padding: 10px;">
<strong>Aviso:</strong> A priori, puede seleccionar <i>mamba ...</i> o <i>conda ...</i> indistíntamente. Personalmente, utilizo <i>mamba ...</i> para <strong>Windows</strong> y <i>conda ...</i> para <strong>Ubuntu</strong> tras sufrir algunos problemas en la instalación y conflictos de dependencias.
</div> <br>

Posteriormente, intente actualizar dicho paquete desde la terminal en R con _remotes_ o _devtools_:
```R
install.packages("devtools", type = "win.binary") # Para Windows
install.packages("devtools") # Para Ubuntu
devtools::install_github(c("SantanderMetGroup/visualizeR@v1.6.3", "SantanderMetGroup/downscaleR@v3.3.4", "SantanderMetGroup/convertR@v0.2.1", "SantanderMetGroup/loadeR@v1.8.1", "SantanderMetGroup/transformeR@v2.1.5", "SantanderMetGroup/climate4R.UDG@devel"))
# devtools::install_github(c('SantanderMetGroup/climate4R.UDG','SantanderMetGroup/loadeR'))
```

Para comprobar que se han instalado las versiones correspondientes, utilizar el siguiente comando reemplazando *nombre_del_paquete* por el nombre de la librería que desea verificar:
```R
packageVersion("nombre_del_paquete")
```

Ahora, desde la terminal de R, instalamos el resto de librerías necesarias:
```R
install.packages(c("MBC", "maptools"), type = "win.binary") # Para Windows
install.packages(c("MBC", "maptools")) # Para Windows
```
Como se ha recomendado anteriormente, si sufre algún problema con esta instalación, inténtela desde la terminal de Mambaforge:
```bash
conda activate clim4R
conda install -c conda-forge -c r r-maptools r-MBC
```

## Códigos .R

A continuación explicaremos cada archivo .R utilizado, su funcionalidad y los archivos que genera:

### Archivos principales

- `01_GenerateMetadata.R`: Este archivo recopila los metadatos de los modelos climáticos de interés a través de una URL incluida en el código o del archivo `inventory.csv`. En el código se debe especificar las características de los modelos y los datos que se quieren de ellos. El código está diseñado para generar:
    - El archivo `list_metadata_models_DEV.txt` que indica los metadatos a nivel usuario de los modelos de interés, indicando con un 1 si la variable solicitada está disponible o con un 0 si no fuera el caso. 
    - El archivo `df_metadata_models_DEV.Rdata` que contiene metadatos más detallados y las rutas de los datos asociados a cada modelo de interés.

- `02_Generate_Model_Interpol.R`: A través de los ficheros de metadatos generados por el código `01GenerateMetadataCORDEX_DEV.R`, este archivo carga las variables asociadas a cada modelo y las interpola haciendo uso de los datos de la carpeta __ibera01__. El código está diseñado para crear una capeta con el nombre del modelo en el directorio de __CordexDEV__ y almacenar en ella sus variables  interpoladas asociadas en formato .Rdata para su futura utilización. Los datos de _iberia01_ empleados corresponden a la península ibérica, motivo por el cuál, el código acota los datos en esta zona del mapa. Además, genera en __CordexDEV__ el archivo `list_models_Dev.txt` que indica qué modelos tienen disponible las variables necesarias, seleccionando así los que cumplen con las exigencias.

<div style="background-color: #FFDAB9; color: black; padding: 10px;">
<strong>Aviso:</strong> Como estamos tratando de periodos temporales grandes, en el caso del <i>histórico</i> de 1985 a 2005 y en el caso del escenario <i>rcp85</i> de 2006 a 2100, no podemos operar con todos los datos a la vez. Por ello, el código divide estos intervalos de tiempo en secciones más pequeñas que emplea para cargar las variables e interpolarlas, generando así varios archivos .Rdata para cada variable. Todas las variables tendrán asociadas el mismo número de archivos con formato .Rdata.
</div> <br>

- `03_Calculate_AgroIndex.R`: Este archivo calcula los índices agroclimáticos para cada uno de los modelos que habitan en __CordexDev__. De forma automática y utilizando los metadatos del índices agroclimáticos, carga las variables que son necesarias para calcular cada uno de los índices seleccionados. Como dichos índices utilizan la temperatura en grados centígrados ($ºC$) y la precipitación en $kg/m^2$, previo al cálculo, se cambia de unidades a las variables que no cumplan con estas especificaciones. El código está diseñado para crear una carpeta, **agroindex_...**, dentro de cada carpeta de modelo y almacenar en ella los índices calculados en formato .Rdata. Además, en la carpeta **plots_...** se guardan las representaciones gráficas en formato .png o .pdf de los índices calculados.

- `04_Calculate_Bias_of_AgroIndex.R`: Este archivo genera el valor (mapa) del _bias_ resultante al comparar/restar el valor promedio del índice agroclimático calculado utilizando el modelo interpolado y el valor calculado a partir de los datos observados de _Iberia01_. El índice agroclimático (en la carpeta **agroindex_...**) se calcula anualmente, y se determina el promedio de todos los años para obtener un único mapa del índice agroclimático. Para obtener el mapa de _bias_, se realiza la resta entre el mapa resultante del índice de Iberia01 y el mapa del modelo.

$$bias_{HI} = HI_{model} - HI_{Iberia01} $$ 

- `05_BiasCorrection_LocalData_all_year.R`: Este archivo aplica un método de correción de _bias_ a una variable (univariable) o a un conjunto de variables (multivariable) climáticas para cada uno de los modelos históricos que habitan en __CordexDev__. Para aplicar los métodos de correción de _bias_ (BC) es necesario definir un periodo de calibración (involucra a los modelos históricos y a las observaciones) y un periodo de proyección a futuro (que puede involucrar al modelo histórico del periodo de calibración o su modelo RCP asociado). Por este motivo, se recorren únicamente los modelos históricos (y se hacen uso de sus modelos RCP asociados si es necesario). Se pueden definir dos casos de uso:
    - El periodo de calibración y el periodo de proyección se encuentran ambos dentro del rango de datos del modelo histórico y de las observaciones. Solo se hace uso del modelo histórico y de los datos observados. De esta forma, se puede hacer un análisis estadístico de los resultados del periodo de proyección al compararlos con datos observados.
    - El periodo de proyección se define en un intervalo de tiempo donde no existen datos observados. Se hace uso de las observaciones y un modelo histórico para el periodo de calibración y su modelo RCP asociado para el periodo de proyección. 

    El código está diseñado para crear una capeta con el nombre del modelo en el directorio de __CordexDEV_bias__ (que se crea si no existe) y almacenar en ella las variables corregidas en formato .Rdata para su futura utilización. Además, se calcula el mapa de _bias_ para cada variable, comparando su versión sin corregir y su versión corregida.

    En el caso de utilizar un método de BC multivariable, se obtiene corregido las variables del periodo de calibración y las del periodo de proyección. Los archivos .Rdata del periodo de calibración se guardan en la carpeta del modelo utilizado para dicho periodo. Los archivos .Rdata del periodo de proyección (RCP o el mismo modelo histórico) se guardan en la carpeta del modelo utilizado para dicho periodo. 

- `05_BiasCorrection_LocalData_by_season.R`: Este archivo es una copia del archivo `05_BiasCorrection_LocalData_all_year.R` orientado a ejecutar el código dentro un bucle que recorre los meses del año (_season_). De esta forma, se corrigen todos los eneros juntos, todos los febreros, etc. Una ventaja de la correción mensual, obviando la disminución de recursos computacionales, es que consiguies centrarte en la dependencia mensual de las variables, capturando así más variabilidad. Al utilizar todos los datos de un año, corres el riesgo de que los meses más cálidos y fríos, a primer orden, establezcan la variabilidad y la dependencia entre variables del estudio, ocultando la del resto de meses.

- `06_Calculate_AgroIndex_BC.R`: Este archivo calcula los índices agroclimáticos para cada uno de los modelos que habitan en __CordexDev_bias__, es decir, con las variables que han sido corregidas previamente por un método de correción de _bias_. De forma automática y utilizando los metadatos del índices agroclimáticos, carga las variables que son necesarias para calcular cada uno de los índices seleccionados. Como dichos índices utilizan la temperatura en grados centígrados ($ºC$) y la precipitación en $kg/m^2$, previo al cálculo, se cambia de unidades a las variables que no cumplan con estas especificaciones. El código está diseñado para crear una carpeta, **agroindex_...**, dentro de cada carpeta de modelo y almacenar en ella los índices calculados en formato .Rdata. Además, en la carpeta **plots_...** se guardan las representaciones gráficas en formato .png o .pdf de los índices calculados.


### Archivos auxiliares

- `00_Plot_Data.R`: Este archivo tiene como función representar los datos (.Rdata) asociados a cada modelo. El código está diseñado para examinar la carpeta __CordexDev__, seleccionar todos los modelos presentes en la misma y guardar en formato .png la representación gráfica de aquellos archivos .Rdata en cuyo nombre aparezca *pr_*, *tas_*, *tasmax_* y *tasmin_*. Por ese motivo es importante que los archivos de datos (.Rdata) contengan en sus nombres dichas cadenas de caracteres, indicando los datos de la variable que almacenan.

## Estructura de carpetas inicial

Para comenzar el proyecto desde el principio será necesario establecer la siguiente organización de carpetas y archivos:

```
.
├── ubuntu_env_meteo3.yml
├── win_env_climate4r3.yml
├── Scripts
│   ├── 00_Plot_Data.R
│   ├── 00_Calculate_Bias_CordexDev_Iberia01.R
│   ├── 01_GenerateMetadata.R
│   ├── 02_Generate_Model_Interpol.R
│   ├── 03_Calculate_AgroIndex.R
│   ├── 04_Calculate_Bias_of_AgroIndex.R
│   ├── 05_BiasCorrection_LocalData_all_year.R
│   ├── 05_BiasCorrection_LocalData_by_season.R
│   ├── 06_Calculate_AgroIndex_BC.R
│   └── Create_Figures/
|       ├── Fig1_AgroIndex_iberia01.R
|       ├── ...
|       └── Fig5_Correlacion_Variables_HIST.R
└── Datos
    ├── iberia01
    │   ├── pr_iberia01_1986-2005.Rdata
    │   ├── tas_iberia01_1986-2005.Rdata
    |   ├── tasmax_iberia01_1986-2005.Rdata
    │   └── tasmin_iberia01_1986-2005.Rdata
    |
    └── CordexDev
        └── Metadatos
            ├── CMIP5_Atlas_WarmingLevels.csv
            └── inventory.csv
    
```

<div style="background-color: #FFDAB9; color: black; padding: 10px;">
<strong>Importante:</strong> Es necesario que el nombre de los archivos de datos <em> .Rdata </em> contengan:
<ul>
  <li><em> pr_ </em> si se trata de un archivo de precipitación.</li>
  <li><em> tas_ </em> si se trata de un archivo de temperatura media del aire en la superficie.</li>
  <li><em> tasmax_ </em> si se trata de un archivo de temperatura máxima del aire en la superficie.</li>
  <li><em> tasmin_ </em> si se trata de un archivo de temperatura mínima del aire en la superficie.</li>
</ul>
</div>

## Estructura de carpetas final

Para comenzar el proyecto desde el principio será necesario establecer la siguiente organización de carpetas y archivos:

```
.
├── ubuntu_env_meteo3.yml
├── win_env_climate4r3.yml
├── Scripts
│   ├── 00_Plot_Data.R
│   ├── 00_Calculate_Bias_CordexDev_Iberia01.R
│   ├── 01_GenerateMetadata.R
│   ├── 02_Generate_Model_Interpol.R
│   ├── 03_Calculate_AgroIndex.R
│   ├── 04_Calculate_Bias_of_AgroIndex.R
│   ├── 05_BiasCorrection_LocalData_all_year.R
│   ├── 05_BiasCorrection_LocalData_by_season.R
│   ├── 06_Calculate_AgroIndex_BC.R
│   └── Create_Figures/
|       ├── Fig1_AgroIndex_iberia01.R
|       ├── ...
|       └── Fig5_Correlacion_Variables_HIST.R
└── Datos
    ├── iberia01
    │   ├── pr_iberia01_1986-2005.Rdata
    │   ├── tas_iberia01_1986-2005.Rdata
    |   ├── tasmax_iberia01_1986-2005.Rdata
    │   └── tasmin_iberia01_1986-2005.Rdata
    |
    ├── CordexDev
    │   ├── Metadatos
    |   |   ├── inventory.csv
    |   │   ├── list_metadata_models_DEV.txt
    |   │   └── df_metadata_models_DEV.Rdata
    |   ├── list_models_Dev.txt
    |   ├── model_1
    |   │   ├── pr_1_interpol_Dev.Rdata
    |   │   ├── pr_2_interpol_Dev.Rdata
    |   |   ├── tasmax_1_interpol_Dev.Rdata
    |   │   ├── tasmax_2_interpol_Dev.Rdata
    |   |   ├── ...
    |   │   ├── agroindex_model_1
    |   |   |   ├── HI.Rdata
    |   |   |   └── ...
    |   |   └── plots_model_1
    |   |       └── Figuras .png
    |   ├── model_2
    |   |   └── ...
    |   └── ....
    |
    ├── CordexDev_CV2_BC
    |   ├── list_models_bias.txt
    |   ├── model_1
    |   │   ├── pr-c_1986-2005_EQM(pr)_CV-Random.Rdata
    |   │   ├── pr-c_1986-2005_EQM(pr)_CV-WarmCold.Rdata
    |   │   ├── pr-c_1986-2005_PQM(pr)_CV-Random.Rdata
    |   │   ├── pr-c_1986-2005_PQM(pr)_CV-WarmCold.Rdata
    |   │   ├── pr-c_1986-2005_QDM(pr)_CV-Random.Rdata
    |   │   ├── pr-c_1986-2005_QDM(pr)_CV-WarmCold.Rdata
    |   │   ├── pr-c_1986-2005_MBCR(tas-tasmin-tasmax-pr)_CV-Random.Rdata
    |   │   ├── pr-c_1986-2005_MBCR(tas-tasmin-tasmax-pr)_CV-WarmCold.Rdata
    |   |   ├── tasmax-c_1986-2005_EQM(pr)_CV-Random.Rdata
    |   |   ├── ...
    |   │   ├── agroindex_model_1
    |   |   |   ├── HI.Rdata
    |   |   |   └── ...
    |   |   └── plots_model_1
    |   |       └── Figuras .png
    |   ├── model_2
    |   |   └── ...
    |   └── ....
```
