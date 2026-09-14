# ============================================================================
# 0. PREPARACION DEL ENTORNO
# ============================================================================
# Instalamos las paqueterias
# install.packages(c("dplyr", "ggplot2", "forcats", "corrplot", "scales"))

# Cargamos las librerias
library(dplyr)
library(ggplot2)
library(forcats)
library(corrplot)
library(scales)
library(openxlsx)

# Directprio de trabajo
setwd("~/Cursos-R/Ejercicios/Ejercicio-1")

# El archivo del INEGI usa codificacion "Latin1" (tiene acentos y enies en
# algunos campos de texto libre), por eso especificamos fileEncoding.
#El archivo es grande (~420,000 filas), la lectura puede tardar unos segundos.
enoe <- read.xlsx(
  " enoe_2025_4t_sample_50k.xlsx",
  stringsAsFactors = FALSE,
  na.strings       = c("", " ", "NA")
)

# ============================================================================
# 4. SELECCION DE VARIABLES DE INTERES PARA EL LABORATORIO
# ============================================================================
# La base tiene mas de 100 columnas, la mayoria son claves de diseno muestral
# que no usaremos. Nos quedamos con las variables relevantes para el mercado
# laboral y les damos nombres mas faciles de recordar.

mercado_laboral <- enoe %>%
  select(
    entidad   = cve_ent,   # entidad federativa (01 a 32)
    sexo      = sex,       # 1 = Hombre, 2 = Mujer
    edad      = eda,       # edad en anios cumplidos
    escolaridad = anios_esc, # anios de escolaridad aprobados
    niv_ins   = niv_ins,   # nivel de instruccion (1 a 5)
    clase1    = clase1,    # 1 = Poblacion Economicamente Activa (PEA)
    # 2 = Poblacion No Economicamente Activa (PNEA)
    clase2    = clase2,    # dentro de la PEA: 1 = Ocupada, 2 = Desocupada
    pos_ocu   = pos_ocu,   # tipo de empleo / posicion en la ocupacion
    rama      = rama,      # sector de actividad economica (7 categorias)
    ing7c     = ing7c,     # nivel de ingreso en 7 categorias (deciles agrupados)
    ingocup   = ingocup,   # ingreso mensual en pesos (poblacion ocupada)
    ing_x_hrs = ing_x_hrs, # ingreso por hora trabajada
    horas     = hrsocup,   # horas trabajadas a la semana
    ponderador = fac_tri   # factor de expansion trimestral
  )

# ============================================================================
# 5. LIMPIEZA Y RECODIFICACION DE VARIABLES CATEGORICAS
# ============================================================================
# En la ENOE, el codigo "0" casi siempre significa "no aplica" (por ejemplo,
# pos_ocu = 0 para alguien que no esta ocupado). Antes de analizar, debemos
# convertir esos codigos en NA y en factores con etiquetas legibles; de lo
# contrario R los tratara como numeros validos y las estadisticas saldran mal.

mercado_laboral <- mercado_laboral %>%
  mutate(
    # ---- Sexo ----
    sexo = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    
    
    # ---- Poblacion Economicamente Activa / No Activa ----
    clase1 = factor(clase1, levels = c(1, 2),
                    labels = c("PEA", "PNEA")),
    
    # ---- Condicion de ocupacion (solo tiene sentido dentro de la PEA) ----
    clase2 = factor(clase2, levels = c(1, 2, 3, 4),
                    labels = c("Ocupada", "Desocupada",
                               "Disponible", "No disponible")),
    
    # ---- Tipo de empleo / posicion en la ocupacion ----
    pos_ocu = na_if(pos_ocu, 0),
    pos_ocu = factor(pos_ocu, levels = c(1, 2, 3, 4, 5),
                     labels = c("Subordinado(a) y remunerado(a)",
                                "Empleador(a)",
                                "Trabajador(a) por cuenta propia",
                                "Trabajador(a) sin pago",
                                "No especificado")),
    
    # ---- Nivel de instruccion ----
    niv_ins = na_if(niv_ins, 0),
    niv_ins = factor(niv_ins, levels = c(1, 2, 3, 4, 5),
                     labels = c("Primaria incompleta",
                                "Primaria completa",
                                "Secundaria completa",
                                "Medio superior y superior",
                                "No especificado")),
    
    # ---- Sector de actividad economica (rama) ----
    rama = na_if(rama, 0),
    rama = factor(rama, levels = 1:7,
                  labels = c("Agropecuario",
                             "Industria extractiva y electricidad",
                             "Industria manufacturera",
                             "Construccion",
                             "Comercio",
                             "Restaurantes y alojamiento",
                             "Otros servicios")),
    
    # ---- Nivel de ingreso agrupado (7 categorias del INEGI) ----
    ing7c = na_if(ing7c, 0),
    ing7c = factor(ing7c, levels = 1:7,
                   labels = c("Hasta 1 s.m.", "Mas de 1 a 2 s.m.",
                              "Mas de 2 a 3 s.m.", "Mas de 3 a 5 s.m.",
                              "Mas de 5 s.m.", "No recibe ingresos",
                              "No especificado")),
    
    # ---- Variables numericas: 0 tambien significa "no aplica" en ingreso ----
    ingocup   = na_if(ingocup, 0),
    ing_x_hrs = na_if(ing_x_hrs, 0),
    horas     = na_if(horas, 0)
  )