library(tidyverse)
library(magrittr)

# Reading the full dataset and save the RDS
# The lines are commented so because the files are not
# uploaded in GitHub
# Follow the official ICFES web page to get the full dataset

# LOAD COMPLETE ICFES DATASET (IT MAY TAKE A WHILE)
icfes_full <- read.csv("Data/Resultados_unicos_Saber_11_20260412.csv")

# Filtering for 2022
icfes_2022 <- icfes_full %>%
  mutate(ANIO = substr(PERIODO, 1, 4)) %>% 
  filter(ANIO == 2022) %>% 
  select(-ANIO)

# Removing main dataset to save memory
rm(icfes_full)

################################################################################
# NA AND BLANKS HANDLING
################################################################################
# Check dimensions
dim(icfes_2022) # 1 085 937 rows and 51 columns

# Change blanks for NA
icfes_2022 <- apply(icfes_2022, 2, function(x){replace(x, x == "",NA)}) %>% 
  as.data.frame()

# Check percentage of missing values
apply(icfes_2022, 2, function(x) 100*sum(is.na(x))/length(x))


# Cleaning process: Complete cases to not introduce additional assumptions of 
# data imputation
icfes_2022 %<>% 
  drop_na()

# Checking the percentages once again (sanity check)
apply(icfes_2022, 2, function(x) 100*sum(is.na(x))/length(x)) # Percentage of missing values

# Check dimensions of data without NA values
dim(icfes_2022) # 797401 rows and 51 columns

# Removing columns that offer no information for modeling
icfes_2022 %<>% 
  select(-c(ESTU_CONSECUTIVO, ESTU_TIPODOCUMENTO,
          COLE_COD_DANE_SEDE, 
          COLE_CODIGO_ICFES, 
          COLE_NOMBRE_SEDE, COLE_SEDE_PRINCIPAL, ESTU_COD_DEPTO_PRESENTACION, 
          ESTU_COD_MCPIO_PRESENTACION, ESTU_COD_RESIDE_DEPTO, ESTU_COD_RESIDE_MCPIO,
          ESTU_ESTADOINVESTIGACION,
          ESTU_PRIVADO_LIBERTAD, # The column only has the value ("N")
          
          # IN THE STUDY, WE CARE ABOUT THE MATH SCORE ONLY
          PUNT_INGLES, PUNT_SOCIALES_CIUDADANAS, 
          PUNT_C_NATURALES, PUNT_LECTURA_CRITICA, PUNT_GLOBAL),
         )


################################################################################
# ENCODING ISSUES
################################################################################
# COLE_CARACTER: TECNICO/ACADEMICO, TECNICO, ACADEMICO, NO APLICA
icfes_2022 %>% 
  select(COLE_CARACTER) %>% 
  group_by(COLE_CARACTER) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count))

# Remove commas
icfes_2022 %<>% 
  mutate(
    COLE_CARACTER_IMPUTED = case_when(
      COLE_CARACTER == "ACADÉMICO" ~ "ACADEMICO",
      COLE_CARACTER == "TÉCNICO" ~ "TECNICO",
      COLE_CARACTER == "TÉCNICO/ACADÉMICO" ~ "TECNICO/ACADEMICO",
      .default = COLE_CARACTER
    )
  ) %>% 
  select(-COLE_CARACTER)

# Sanity check
icfes_2022 %>% 
  select(COLE_CARACTER_IMPUTED) %>% 
  group_by(COLE_CARACTER_IMPUTED) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count))


# COLE_DEPTO_UBICACION: Name of the Colombian department where the school is 
# located at
icfes_2022 %>% 
  select(COLE_DEPTO_UBICACION) %>% 
  group_by(COLE_DEPTO_UBICACION) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count)) %>% 
  print(n = 35)

# Dealing with encoding issues
# BOGOTÁ -> BOGOTA
# NARIÑO -> NARINO
icfes_2022 %<>%
  mutate(
    COLE_DEPTO_UBICACION_IMPUTED = case_when(
      COLE_DEPTO_UBICACION == "BOGOTÁ" ~ "BOGOTA",
      COLE_DEPTO_UBICACION == "NARIÑO" ~ "NARINO",
      .default = COLE_DEPTO_UBICACION
    )
  ) %>% 
  select(-COLE_DEPTO_UBICACION)

# Sanity check
icfes_2022 %>% 
  select(COLE_DEPTO_UBICACION_IMPUTED) %>% 
  group_by(COLE_DEPTO_UBICACION_IMPUTED) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count)) %>% 
  print(n = 35)


# COLE_JORNADA
icfes_2022 %>% 
  select(COLE_JORNADA) %>% 
  group_by(COLE_JORNADA) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count))

# Solve the encoding
# MAÑANA -> MANANA
icfes_2022 %<>% 
  mutate(
    COLE_JORNADA_IMPUTED = if_else(
      COLE_JORNADA == "MAÑANA", "MANANA", COLE_JORNADA
    )
  ) %>% 
  select(-COLE_JORNADA)

# Sanity check
icfes_2022 %>% 
  select(COLE_JORNADA_IMPUTED) %>% 
  group_by(COLE_JORNADA_IMPUTED) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count))


# ESTU_DEPTO_PRESENTACION: Name of the department where the student took the exam. 
icfes_2022 %>% 
  select(ESTU_DEPTO_PRESENTACION) %>% 
  group_by(ESTU_DEPTO_PRESENTACION) %>% 
  summarise(count = n()) %>% 
  print(n = 35)

# Change the encoding as before
# BOGOTÁ -> BOGOTA
# NARIÑO -> NARINO
icfes_2022 %<>%
  mutate(
    ESTU_DEPTO_PRESENTACION_IMPUTED = case_when(
      ESTU_DEPTO_PRESENTACION == "BOGOTÁ" ~ "BOGOTA",
      ESTU_DEPTO_PRESENTACION == "NARIÑO" ~ "NARINO",
      .default = ESTU_DEPTO_PRESENTACION
    )
  ) %>% 
  select(-ESTU_DEPTO_PRESENTACION)

# Sanity check
icfes_2022 %>% 
  select(ESTU_DEPTO_PRESENTACION_IMPUTED) %>% 
  group_by(ESTU_DEPTO_PRESENTACION_IMPUTED) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count)) %>% 
  print(n = 35)


# ESTU_DEPTO_RESIDE: Department where the student lives. Here we have an 
# aditional level: EXTRANJERO to indicate if the student does not live in Colombia.
icfes_2022 %>% 
  select(ESTU_DEPTO_RESIDE) %>% 
  group_by(ESTU_DEPTO_RESIDE) %>% 
  summarise(count = n()) %>% 
  print(n = 35)

# The same treatment as the previous column
# BOGOTÁ -> BOGOTA
# NARIÑO -> NARINO
icfes_2022 %<>%
  mutate(
    ESTU_DEPTO_RESIDE_IMPUTED = case_when(
      ESTU_DEPTO_RESIDE == "BOGOTÁ" ~ "BOGOTA",
      ESTU_DEPTO_RESIDE == "NARIÑO" ~ "NARINO",
      .default = ESTU_DEPTO_RESIDE
    )
  ) %>% 
  select(-ESTU_DEPTO_RESIDE)

# Sanity check
icfes_2022 %>% 
  select(ESTU_DEPTO_RESIDE_IMPUTED) %>% 
  group_by(ESTU_DEPTO_RESIDE_IMPUTED) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count)) %>% 
  print(n = 35)


# ESTU_ESTUDIANTE: At least, in the 2022 data, it only has one value: ESTUDIANTE
# the column is dropped.
icfes_2022 %<>% 
  select(-ESTU_ESTUDIANTE)


# FAMI_CUARTOS_HOGAR
icfes_2022 %>% 
  select(FAMI_CUARTOSHOGAR) %>% 
  group_by(FAMI_CUARTOSHOGAR) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count))

# "Seis o mas" category is less than 2%, these rows are removed
icfes_2022 %<>% 
  filter(FAMI_CUARTOSHOGAR != "Seis o mas")

# ------------------------------------------------------------------------------
# ACADEMIC LEVEL BLOCK
#-------------------------------------------------------------------------------
# FAMI_EDUCACIONMADRE and FAMIEDUCACION_PADRE
icfes_2022 %>% 
  select(FAMI_EDUCACIONMADRE) %>% 
  group_by(FAMI_EDUCACIONMADRE) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count))

# THE VARIABLE MUST BE STANDARDIZED
# Primaria incompleta -> Primaria incompleta
# Educación profesional completa -> Profesional 
# Secundaria (Bachillerato) completa -> Bachiller 
# Postgrado -> Postgrado
# No sabe -> NA
# Primaria completa -> Primaria
# Técnica o tecnológica completa -> Tecnica/Tecnologica
# Educación profesional incompleta -> Bachiller
# Ninguno -> NA
# Secundaria (Bachillerato) incompleta -> Primaria
# Técnica o tecnológica incompleta -> Bachiller
# No Aplica -> NA


# IMPUTATION
# MOTHER
icfes_2022 %<>% 
  mutate(
    FAMI_EDUCACIONMADRE_IMPUTED = 
      case_when(
        FAMI_EDUCACIONMADRE %in% c("Primaria completa", "Secundaria (Bachillerato) incompleta") ~ "Primaria",
        FAMI_EDUCACIONMADRE %in% c("Secundaria (Bachillerato) completa", 
                                   "Educación profesional incompleta",
                                   "Técnica o tecnológica incompleta") ~ "Bachiller",
        FAMI_EDUCACIONMADRE == "Técnica o tecnológica completa" ~ "Tecnica/Tecnologica",
        FAMI_EDUCACIONMADRE == "Educación profesional completa" ~ "Profesional",
        FAMI_EDUCACIONMADRE %in% c("No sabe",
                                   "Ninguno",
                                   "No Aplica") ~ NA,
        .default = FAMI_EDUCACIONMADRE
      )
  ) %>% 
  select(-FAMI_EDUCACIONMADRE)

# FATHER
icfes_2022 %<>% 
  mutate(
    FAMI_EDUCACIONPADRE_IMPUTED = 
      case_when(
        FAMI_EDUCACIONPADRE %in% c("Primaria completa", "Secundaria (Bachillerato) incompleta") ~ "Primaria",
        FAMI_EDUCACIONPADRE %in% c("Secundaria (Bachillerato) completa", 
                                   "Educación profesional incompleta",
                                   "Técnica o tecnológica incompleta") ~ "Bachiller",
        FAMI_EDUCACIONPADRE == "Técnica o tecnológica completa" ~ "Tecnica/Tecnologica",
        FAMI_EDUCACIONPADRE == "Educación profesional completa" ~ "Profesional",
        FAMI_EDUCACIONPADRE %in% c("No sabe",
                                   "Ninguno",
                                   "No Aplica") ~ NA,
        .default = FAMI_EDUCACIONPADRE
      )
  ) %>% 
  select(-FAMI_EDUCACIONPADRE)

# CREATION OF THE HIGHEST ACADEMIC LEVEL OF EITHER PARENT
# We define a numerical scale for each category representing the natural order 
# of the academic levels 
academic_order <- data.frame(
  academic_level = c("Primaria incompleta", "Primaria", "Bachiller", 
                     "Tecnica/Tecnologica", "Profesional", "Postgrado"),
  academic_scale = 1:6
)

# Auxiliary function to map academic categories to the numerical scale
map_academic_level <- function(x) {
  academic_order$academic_scale[
    match(x, academic_order$academic_level)
  ]
}

# HIGHEST ACADEMIC LEVEL DEFINITION
icfes_2022 <- icfes_2022 %>%
  mutate(
    # ORDINAL ORDER OF EACH ACADEMIC LEVEL
    MOM_ACADEMIC_SCALE = map_academic_level(FAMI_EDUCACIONMADRE_IMPUTED),
    DAD_ACADEMIC_SCALE = map_academic_level(FAMI_EDUCACIONPADRE_IMPUTED),
    
    # ORDINAL ORDER OF THE HIGHEST ACADEMIC LEVEL
    MAX_ACADEMIC_SCALE = pmax(MOM_ACADEMIC_SCALE, DAD_ACADEMIC_SCALE, na.rm = TRUE),
    
    # pmax(..., na.rm = TRUE) returns -Inf when both parent values are missing.
    # We convert those cases back to NA.
    MAX_ACADEMIC_SCALE = if_else(
      is.infinite(MAX_ACADEMIC_SCALE),
      NA_real_,
      MAX_ACADEMIC_SCALE
    ),
    
    # DEFINITION OF THEHIGHEST ACADEMIC LEVEL COLUMN
    ACADEMIC_LEVEL =
      academic_order$academic_level[
        match(MAX_ACADEMIC_SCALE, academic_order$academic_scale)
      ],
  ) %>% 
  # DROP THE AUXILIARY COLUMNS USED TO DEFINE THE MAX ACADEMIC LEVEL
  select(-c(MOM_ACADEMIC_SCALE, DAD_ACADEMIC_SCALE, MAX_ACADEMIC_SCALE))

# Percentage of missingness
apply(icfes_2022, 2, function(x) 100*sum(is.na(x))/length(x))

# The percentage of missingness of the academic levels is:
# MOTHER: 4.47
# FATHER: 11.57
# MAX ACADEMIC LEVEL: 2.51

# THESE RESULTS ALSO MOTIVATES THE INCLUSION OF THE MAX ACADEMIC LEVEL ONLY:
# KEEPING BOTH COLUMNS AND COMPLETE CASE ANALYSIS WOULD IMPLY LESS DATA TO WORK
# WITH.

# NOTE: THESE NA VALUES ARE NOT REMOVED YET BECAUSE WE USE BOTH COLUMNS FOR
# THE SENSITIVITY ANALYSIS, WHICH IS THE MAIN JUSTIFICATION OF KEEPING ONLY
# THE HIGHEST ACADEMIC LEVEL.

# ------------------------------------------------------------------------------
# END OF ACADEMIC LEVEL FOCUSED BLOCK
#-------------------------------------------------------------------------------

# FAMI_ESTRATOVIVIENDA
icfes_2022 %>% 
  select(FAMI_ESTRATOVIVIENDA) %>% 
  group_by(FAMI_ESTRATOVIVIENDA) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count))

# "Sin estrato" -> NA
icfes_2022 %<>% 
  mutate(
    FAMI_ESTRATOVIVIENDA_IMPUTED = if_else(
      FAMI_ESTRATOVIVIENDA == "Sin Estrato", NA, FAMI_ESTRATOVIVIENDA
    )
  ) %>% 
  select(-FAMI_ESTRATOVIVIENDA) %>% 
  drop_na(FAMI_ESTRATOVIVIENDA_IMPUTED)
# After dropping NAs for "ESTRATO_VIVIENDA" -> 758447 rows
# Sanity check
icfes_2022 %>% 
  select(FAMI_ESTRATOVIVIENDA_IMPUTED) %>% 
  group_by(FAMI_ESTRATOVIVIENDA_IMPUTED) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count))


# FAMI_TIENEINTERNET
icfes_2022 %>% 
  select(FAMI_TIENEINTERNET) %>% 
  group_by(FAMI_TIENEINTERNET) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count))


# Percentage of missingness
apply(icfes_2022, 2, function(x) 100*sum(is.na(x))/length(x))

################################################################################
# MODIFYING DATA TYPES
################################################################################
# Changing character columns to numeric columns if possible
str(icfes_2022)

# ESTU_FECHANACIMIENTO -> date
# PUNT_INGLES, PUNT_MATEMATICAS, PUNT_SOCIALES_CIUDADANAS, PUNT_C_NATURALES, 
# PUNT_LECTURA_CRITICA, PUNT_GLOBAL
icfes_2022 %<>% 
  mutate(
    COLE_COD_DEPTO_UBICACION = as.numeric(COLE_COD_DEPTO_UBICACION),
    COLE_COD_MCPIO_UBICACION = as.numeric(COLE_COD_MCPIO_UBICACION),
    COLE_COD_DANE_ESTABLECIMIENTO = as.numeric(COLE_COD_DANE_ESTABLECIMIENTO),
    ESTU_FECHANACIMIENTO = as.Date(ESTU_FECHANACIMIENTO),
    PUNT_MATEMATICAS = as.numeric(PUNT_MATEMATICAS),
    FAMI_CUARTOSHOGAR = case_when(
      FAMI_CUARTOSHOGAR == "Uno" ~ 1,
      FAMI_CUARTOSHOGAR == "Dos" ~ 2,
      FAMI_CUARTOSHOGAR == "Tres" ~ 3,
      FAMI_CUARTOSHOGAR == "Cuatro" ~ 4,
      FAMI_CUARTOSHOGAR == "Cinco" ~ 5
    )
  )


################################################################################
# TRANSLATION FROM SPANISH TO ENGLISH
################################################################################
icfes_2022 %<>% 
  mutate(
    COLE_AREA_UBICACION = if_else(
      COLE_AREA_UBICACION == "URBANO", "Urban", "Rural"
    ),
    COLE_BILINGUE = case_when(
      COLE_BILINGUE == "S" ~ "Yes",
      COLE_BILINGUE == "N" ~ "No",
      .default = COLE_BILINGUE
    ),
    COLE_GENERO = case_when(
      COLE_GENERO == "MIXTO" ~ "Mixed",
      COLE_GENERO == "FEMENINO" ~ "Female",
      COLE_GENERO == "MASCULINO" ~ "Male",
      .default = COLE_GENERO
    ),
    COLE_NATURALEZA = case_when(
      COLE_NATURALEZA == "OFICIAL" ~ "Official",
      COLE_NATURALEZA == "NO OFICIAL" ~ "Unofficial",
      .default = COLE_NATURALEZA
    ),
    FAMI_TIENEAUTOMOVIL = case_when(
      FAMI_TIENEAUTOMOVIL == "Si" ~ "Yes",
      .default = FAMI_TIENEAUTOMOVIL
    ),
    FAMI_TIENECOMPUTADOR = case_when(
      FAMI_TIENECOMPUTADOR == "Si" ~ "Yes", 
      .default = FAMI_TIENECOMPUTADOR
    ),
    FAMI_TIENEINTERNET = case_when(
      FAMI_TIENEINTERNET == "Si" ~ "Yes",
      .default = FAMI_TIENEINTERNET
    ),
    FAMI_TIENELAVADORA = case_when(
      FAMI_TIENELAVADORA == "Si" ~ "Yes",
      .default = FAMI_TIENELAVADORA
    ),
    COLE_CARACTER_IMPUTED = case_when(
      COLE_CARACTER_IMPUTED == "TECNICO/ACADEMICO" ~ "Technical/Academic",
      COLE_CARACTER_IMPUTED == "TECNICO" ~ "Technical",
      COLE_CARACTER_IMPUTED == "ACADEMICO" ~ "Academic",
      COLE_CARACTER_IMPUTED == "NO APLICA" ~ "Does not apply",
      .default = COLE_CARACTER_IMPUTED
    ),
    COLE_JORNADA_IMPUTED = case_when(
      COLE_JORNADA_IMPUTED == "SABATINA" ~ "Saturdays",
      
      # "COMPLETA" AND "UNICA" MEAN THE SAME
      COLE_JORNADA_IMPUTED %in% c("COMPLETA", "UNICA") ~ "Complete",
      COLE_JORNADA_IMPUTED == "MANANA" ~ "Mornings",
      COLE_JORNADA_IMPUTED == "TARDE" ~ "Afternoons",
      COLE_JORNADA_IMPUTED == "NOCHE" ~ "Nights",
      .default = COLE_JORNADA_IMPUTED
    ),
    FAMI_EDUCACIONMADRE_IMPUTED = case_when(
      FAMI_EDUCACIONMADRE_IMPUTED == "Primaria incompleta" ~ "Primary incomplete",
      FAMI_EDUCACIONMADRE_IMPUTED == "Primaria" ~ "Primary",
      FAMI_EDUCACIONMADRE_IMPUTED == "Bachiller" ~ "High school",
      FAMI_EDUCACIONMADRE_IMPUTED == "Tecnica/Tecnologica" ~ "Technical",
      FAMI_EDUCACIONMADRE_IMPUTED == "Profesional" ~ "Graduate",
      FAMI_EDUCACIONMADRE_IMPUTED == "Postgrado" ~ "Postgraduate",
      .default = FAMI_EDUCACIONMADRE_IMPUTED
    ),
    FAMI_EDUCACIONPADRE_IMPUTED = case_when(
      FAMI_EDUCACIONPADRE_IMPUTED == "Primaria incompleta" ~ "Primary incomplete",
      FAMI_EDUCACIONPADRE_IMPUTED == "Primaria" ~ "Primary",
      FAMI_EDUCACIONPADRE_IMPUTED == "Bachiller" ~ "High school",
      FAMI_EDUCACIONPADRE_IMPUTED == "Tecnica/Tecnologica" ~ "Technical",
      FAMI_EDUCACIONPADRE_IMPUTED == "Profesional" ~ "Graduate",
      FAMI_EDUCACIONPADRE_IMPUTED == "Postgrado" ~ "Postgraduate",
      .default = FAMI_EDUCACIONPADRE_IMPUTED
    ),
    ACADEMIC_LEVEL = case_when(
      ACADEMIC_LEVEL == "Primaria incompleta" ~ "Primary incomplete",
      ACADEMIC_LEVEL == "Primaria" ~ "Primary",
      ACADEMIC_LEVEL == "Bachiller" ~ "High school",
      ACADEMIC_LEVEL == "Tecnica/Tecnologica" ~ "Technical",
      ACADEMIC_LEVEL == "Profesional" ~ "Graduate",
      ACADEMIC_LEVEL == "Postgrado" ~ "Postgraduate",
      .default = ACADEMIC_LEVEL
    ),
    
    # SOCIOECONOMIC STRATTUM IS "TRANSLATED" INTO NUMERICAL VALUES
    # For sanity reasons, it's kept as character and then it is transformed to numeric
    FAMI_ESTRATOVIVIENDA_IMPUTED = case_when(
      FAMI_ESTRATOVIVIENDA_IMPUTED == "Estrato 1" ~ "1",
      FAMI_ESTRATOVIVIENDA_IMPUTED == "Estrato 2" ~ "2",
      FAMI_ESTRATOVIVIENDA_IMPUTED == "Estrato 3" ~ "3",
      FAMI_ESTRATOVIVIENDA_IMPUTED == "Estrato 4" ~ "4",
      FAMI_ESTRATOVIVIENDA_IMPUTED == "Estrato 5" ~ "5",
      FAMI_ESTRATOVIVIENDA_IMPUTED == "Estrato 6" ~ "6",
      .default = FAMI_ESTRATOVIVIENDA_IMPUTED
    ),
    # Numerical value conversion
    FAMI_ESTRATOVIVIENDA_IMPUTED = as.numeric(FAMI_ESTRATOVIVIENDA_IMPUTED)
  )

str(icfes_2022)

################################################################################
# DOWNLOAD SPATIAL DATA
################################################################################
library(ColOpenData)
library(sf)

# Download the spatial dataframes
# Aggregation up to department level
# dpto <- download_geospatial(
#   spatial_level = "dpto",
#   simplified = TRUE,
#   include_geom = TRUE,
#   include_cnpv = TRUE
# )
# 
# # aggregation up to municipal level
# mpio <- download_geospatial(
#   spatial_level = "mpio",
#   simplified = TRUE,
#   include_geom = TRUE,
#   include_cnpv = TRUE
# )
# 
# 
# # changing the codes for numeric values
# dpto %<>%
#   mutate(codigo_departamento = as.numeric(codigo_departamento))
# 
# mpio %<>%
#   mutate(codigo_municipio = as.numeric(codigo_municipio))

# # FILTER THE SPATIAL DATAFRAMETO EXCLUDE ISLANDS
# imsel <- which(st_coordinates(st_centroid(mpio))[, 1]>(-80))
# mpio <- mpio[imsel, ]

# # AFTER DOWNLOADING THE DATA, WE SAVE THE OBJECTS IN RDS FORMAT
# saveRDS(dpto, "Data/col_dpto_geospatial.Rds")
# saveRDS(mpio, "Data/col_mpio_geospatial.Rds")


# LOAD THE OBJECTS
dpto <- readRDS("Data/col_dpto_geospatial.Rds")
mpio <- readRDS("Data/col_mpio_geospatial.Rds")


################################################################################
# IMPORTANT NOTE: BELEN DE BAJIRA MUNICIPALITY
################################################################################
# WE INSPECT THE MUNICIPALITY "BELEN DE BAJIRA" because further analysis showed
# that it does not appear in the spatial data.

# Check the municipality code within the icfes data
icfes_2022 %>%
  select(COLE_COD_MCPIO_UBICACION, COLE_MCPIO_UBICACION) %>% 
  filter(COLE_MCPIO_UBICACION == "BELÉN DE BAJIRÁ")

# The municipality ID is 27086

# Check existence of municipality in spatial data
mpio %>% 
  filter(codigo_municipio == 27086) %>% 
  # Select only a few columns to check the existence of observations
  select(codigo_municipio, municipio, version)
# EMPTY DATASET

# SUMMARY
# BELEN DE BAJIRA does not exist in the municipalities' dataset.
# From Wikipedia, the municipality exists since November 30 of 2022.
# https://es.wikipedia.org/wiki/Nuevo_Bel%C3%A9n_de_Bajir%C3%A1

# Check how many observations does the municipality have
icfes_2022 %>%
  select(COLE_COD_DEPTO_UBICACION, COLE_DEPTO_UBICACION_IMPUTED,
         COLE_COD_MCPIO_UBICACION, COLE_MCPIO_UBICACION) %>% 
  filter(COLE_MCPIO_UBICACION == "BELÉN DE BAJIRÁ") %>% 
  dim()

# 114 observations from the 758447 (approximately 0.01%)
# MANAGEMENT: Drop the observations
icfes_2022 %<>% filter(COLE_MCPIO_UBICACION != "BELÉN DE BAJIRÁ")

# SANITY CHECK
icfes_2022 %>%
  select(COLE_COD_DEPTO_UBICACION, COLE_DEPTO_UBICACION_IMPUTED,
         COLE_COD_MCPIO_UBICACION, COLE_MCPIO_UBICACION) %>% 
  filter(COLE_MCPIO_UBICACION == "BELÉN DE BAJIRÁ")

dim(icfes_2022)
# 758333 observations

################################################################################
# SAVE THE DATA
################################################################################
# Percentage of missingness
apply(icfes_2022, 2, function(x) 100*sum(is.na(x))/length(x))

# NA comes only from the academic level.
# These values are finally handled later after sensitivity analysis.

# Saving the object
saveRDS(icfes_2022, "Data/icfes_2022_cleaned.Rds")
