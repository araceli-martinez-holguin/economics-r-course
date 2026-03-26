# ==============================================================================
#
#   PRÁCTICA 2 
#
#   ESCENARIO: Consejo Asesor del Secretario de Economía
#   CONTEXTO:  El T-MEC (USMCA) será renegociado en 2026. El equipo debe
#              identificar qué productos agrícolas de exportación proteger,
#              cuáles impulsar y cuáles presentan vulnerabilidades.
#
#   DATOS:     Principales cultivos de exportación 2003-2024 (SIAP)
#   LIBRERÍAS: readxl, dplyr, tidyr, ggplot2
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN INICIAL
# ------------------------------------------------------------------------------
#Clear the environment
rm(list=ls(all=TRUE))
gc()

# Instalar librerias
# install.packages(c("tidyr", "dplyr", "readxl", "ggplot2"))

# Cargar librerias
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)

# Working directory
setwd("~/Documents/Cursos-R/Laboratorio-2")

# Origen de la data
# https://nube.agricultura.gob.mx/cierre_agricola/

# Upload excel
data <- read_xlsx("cultivos.xlsx")

glimpse(data)
# Variables clave:
#   Anio         → año del registro
#   NomEdo       → nombre del estado
#   NomMpio      → nombre del municipio
#   Nomcultivo   → nombre del cultivo
#   Sembrada     → superficie sembrada (ha)
#   Cosechada    → superficie cosechada (ha)
#   Produccion   → volumen de producción (ton)
#   Rendimiento  → rendimiento (ton/ha)
#   PMR          → precio medio rural ($/ton)
#   Valor        → valor de la producción (miles de pesos)

# Cuáles cultivos considera la base
sort(unique(data$Nomcultivo), decreasing = F)

# ==============================================================================
# SECCIÓN 1 — DIAGNÓSTICO PRODUCTIVO
# BRIEF: "Secretaría necesita un diagnóstico base: ¿qué tan grande es
#         cada sector y cuál ha sido su trayectoria en dos décadas?"
# ==============================================================================

# ------------------------------------------------------------------------------
# PREGUNTA 1A
# ¿Cuál es el peso relativo de cada cultivo dentro del total exportable
# (por producción acumulada 2003-2024)?
# ------------------------------------------------------------------------------

colnames(data)

participacion <- data %>%
  group_by(Nomcultivo) %>%
  summarise(
    produccion_acumulada = sum(Volumenproduccion,  na.rm = TRUE),
    valor_acumulado      = sum(Valorproduccion,       na.rm = TRUE),
    superficie_acumulada = sum(Sembrada,    na.rm = TRUE)
  ) %>%
mutate(
    participacion_prod  = round(produccion_acumulada / sum(produccion_acumulada) * 100, 2),
    participacion_valor = round(valor_acumulado      / sum(valor_acumulado)      * 100, 2)
  ) %>%
  arrange(desc(participacion_valor))

View(participacion)

# Gráfica: participación por valor
participacion_valor <- ggplot(participacion,
       aes(x = reorder(Nomcultivo, participacion_valor),
           y = participacion_valor,
           fill = participacion_valor)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(participacion_valor, "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "#b7e4c7", high = "#1b4332") +
  scale_y_continuous(limits = c(0, max(participacion$participacion_valor) * 1.15)) +
  labs(
    title    = "Participación en el Valor Total de la Producción Exportable",
    subtitle = "México 2003-2024 | Acumulado",
    x        = NULL,
    y        = "Participación (%)"
  ) +
  theme_minimal(base_size = 12)

ggsave("participacion_valor.png", participacion_valor, dpi= 300, width = 12, height = 10)


# Gráfica: participación por volumen
participacion_prod <- ggplot(participacion,
                              aes(x = reorder(Nomcultivo, participacion_prod),
                                  y = participacion_prod,
                                  fill = participacion_prod)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(participacion_prod, "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "indianred1", high = "indianred4") +
  scale_y_continuous(limits = c(0, max(participacion$participacion_prod) * 1.15)) +
  labs(
    title    = "Participación en el Volumen Total de la Producción Exportable",
    subtitle = "México 2003-2024 | Acumulado",
    x        = NULL,
    y        = "Participación (%)"
  ) +
  theme_minimal(base_size = 12)

ggsave("participacion_prod.png", participacion_prod, dpi= 300, width = 12, height = 10)


### Obtener cuántos municipios producen al año por cultivo
# ------------------------------------------------------------------------------
# Municipios productores por año y por cultivo
# ¿Qué tan amplia es la base territorial de cada cultivo?
# ------------------------------------------------------------------------------

municipios_cultivo <- data %>%
  group_by(Anio, Nomcultivo) %>%
  summarise(
    num_municipios = n_distinct(Idmunicipio),
    .groups = "drop"
  ) %>%
  arrange(Nomcultivo, Anio)

# Gráfica (antes del wide, municipios_cultivo ya está en formato long)
municipios_plot <- ggplot(municipios_cultivo,
                          aes(x = Anio, y = num_municipios, color = Nomcultivo, group = Nomcultivo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(2003, 2024, by = 2)) +
  scale_color_manual(values = colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(14)) +
  labs(
    title    = "Municipios Productores por Cultivo y Año",
    subtitle = "México 2003–2024",
    x        = "Año",
    y        = "Número de municipios",
    color    = "Cultivo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.text     = element_text(size = 9)
  )

ggsave("municipios_por_cultivo.png", municipios_plot, dpi = 300, width = 14, height = 8)

# Wide
municipios_wide <- municipios_cultivo %>%
  pivot_wider(
    names_from  = Anio,
    values_from = num_municipios
  ) %>%
  arrange(Nomcultivo)

View(municipios_wide)
# ------------------------------------------------------------------------------
# PREGUNTA 1B
# ¿Qué cultivos han mostrado mayor crecimiento en producción entre
# el primer quinquenio (2003-2007) y el último (2020-2024)?
# Clasificar como: "Estrella", "Maduro" o "En declive"
# ------------------------------------------------------------------------------

crecimiento <- data %>%
  mutate(
    periodo = case_when(
      Anio <= 2007 ~ "inicio",
      Anio >= 2020 ~ "reciente",
      TRUE         ~ NA_character_
    )
  ) %>%
  filter(!is.na(periodo)) %>%
  group_by(Nomcultivo, periodo) %>%
  summarise(produccion_prom = mean(Valorproduccion, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = periodo, values_from = produccion_prom) %>%
  mutate(
    crecimiento_pct = round((reciente / inicio - 1) * 100, 1),
    clasificacion   = case_when(
      crecimiento_pct >= 50  ~ "Estrella 🌟",
      crecimiento_pct >= 0   ~ "Maduro 🌿",
      TRUE                   ~ "En declive ⚠️"
    )
  ) %>%
  arrange(desc(crecimiento_pct))

View(crecimiento)

# Gráfica
ggplot(crecimiento,
       aes(x = reorder(Nomcultivo, crecimiento_pct),
           y = crecimiento_pct,
           fill = clasificacion)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Estrella 🌟"    = "#2d6a4f",
    "Maduro 🌿"      = "#95d5b2",
    "En declive ⚠️"  = "#e63946"
  )) +
  labs(
    title    = "Crecimiento de la Producción por Cultivo",
    subtitle = "Comparación quinquenio 2003-2007 vs 2020-2024",
    x        = NULL,
    y        = "Crecimiento (%)",
    fill     = "Clasificación"
  ) +
  theme_minimal(base_size = 12)


# ==============================================================================
# SECCIÓN 2 — COMPETITIVIDAD Y EFICIENCIA
# BRIEF: "Para la mesa de negociación del T-MEC, necesitamos identificar
#         cuáles son nuestros cultivos más competitivos: los que producen
#         más valor con menor superficie."
# ==============================================================================

# ------------------------------------------------------------------------------
# PREGUNTA 2A
# ¿Cuál es el valor generado por hectárea cosechada en 2024 para cada cultivo?
# ¿Qué cultivos son los más rentables por unidad de superficie?
# ------------------------------------------------------------------------------

competitividad_2024 <- data %>%
  filter(Anio == 2024) %>%
  group_by(Nomcultivo) %>%
  summarise(
    produccion      = sum(Produccion,  na.rm = TRUE),
    valor_total     = sum(Valor,       na.rm = TRUE),
    superficie_cos  = sum(Cosechada,   na.rm = TRUE),
    rendimiento_med = mean(Rendimiento, na.rm = TRUE),
    pmr_promedio    = mean(PMR,         na.rm = TRUE)
  ) %>%
  mutate(
    valor_por_ha    = round(valor_total / superficie_cos, 0),
    indice_rentab   = round(valor_por_ha / mean(valor_por_ha) * 100, 1)
  ) %>%
  arrange(desc(valor_por_ha))

View(competitividad_2024)

# Gráfica: dispersión rendimiento vs valor por ha
ggplot(competitividad_2024,
       aes(x = rendimiento_med,
           y = valor_por_ha / 1000,
           size = produccion / 1e3,
           color = pmr_promedio,
           label = Nomcultivo)) +
  geom_point(alpha = 0.8) +
  geom_text(vjust = -1, size = 2.8, color = "gray20") +
  scale_color_gradient(low = "#ffe566", high = "#c1121f",
                       name = "Precio medio\nrural ($/ton)") +
  scale_size(name = "Producción\n(miles ton)", range = c(3, 12)) +
  labs(
    title    = "Competitividad de Cultivos de Exportación (2024)",
    subtitle = "Tamaño de burbuja = volumen producido | Color = precio de mercado",
    x        = "Rendimiento promedio (ton/ha)",
    y        = "Valor generado por hectárea (miles de pesos)"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# PREGUNTA 2B
# ¿Cuál ha sido la tendencia del precio medio rural (PMR) de los cultivos
# estrella entre 2010 y 2024? ¿Hay señales de pérdida de competitividad?
# ------------------------------------------------------------------------------

cultivos_estrella <- crecimiento %>%
  filter(clasificacion == "Estrella 🌟") %>%
  pull(Nomcultivo)

pmr_tendencia <- data %>%
  filter(Nomcultivo %in% cultivos_estrella, Anio >= 2010) %>%
  group_by(Nomcultivo, Anio) %>%
  summarise(pmr_nacional = mean(PMR, na.rm = TRUE), .groups = "drop")

ggplot(pmr_tendencia,
       aes(x = Anio, y = pmr_nacional, color = Nomcultivo, group = Nomcultivo)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ Nomcultivo, scales = "free_y", ncol = 2) +
  labs(
    title    = "Tendencia del Precio Medio Rural — Cultivos Estrella",
    subtitle = "2010-2024 | Pesos por tonelada",
    x        = "Año",
    y        = "PMR ($/ton)",
    color    = "Cultivo"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")


# ==============================================================================
# SECCIÓN 3 — CONCENTRACIÓN REGIONAL Y VULNERABILIDAD
# BRIEF: "Un riesgo en negociaciones comerciales es la concentración
#         geográfica. Si un cultivo depende de un solo estado, es más
#         vulnerable a shocks climáticos o sanciones comerciales."
# ==============================================================================

# ------------------------------------------------------------------------------
# PREGUNTA 3A
# ¿Qué tan concentrada está la producción de cada cultivo?
# Calcular el Índice de Concentración: % que aporta el top-3 de estados.
# ------------------------------------------------------------------------------

concentracion_estados <- data %>%
  filter(Anio >= 2020) %>%
  group_by(Nomcultivo, NomEdo) %>%
  summarise(produccion = sum(Produccion, na.rm = TRUE), .groups = "drop") %>%
  group_by(Nomcultivo) %>%
  mutate(
    total_cultivo  = sum(produccion),
    participacion  = produccion / total_cultivo * 100
  ) %>%
  arrange(Nomcultivo, desc(produccion)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# Índice: suma de participación del top-3 por cultivo
indice_concentracion <- concentracion_estados %>%
  filter(rank <= 3) %>%
  group_by(Nomcultivo) %>%
  summarise(concentracion_top3 = round(sum(participacion), 1)) %>%
  mutate(
    vulnerabilidad = case_when(
      concentracion_top3 >= 80 ~ "Alta ⚠️",
      concentracion_top3 >= 60 ~ "Media 🔶",
      TRUE                     ~ "Baja ✅"
    )
  ) %>%
  arrange(desc(concentracion_top3))

View(indice_concentracion)

# Gráfica
ggplot(indice_concentracion,
       aes(x = reorder(Nomcultivo, concentracion_top3),
           y = concentracion_top3,
           fill = vulnerabilidad)) +
  geom_col() +
  geom_hline(yintercept = 80, linetype = "dashed", color = "#e63946", linewidth = 0.8) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "#f4a261", linewidth = 0.8) +
  geom_text(aes(label = paste0(concentracion_top3, "%")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Alta ⚠️"  = "#e63946",
    "Media 🔶" = "#f4a261",
    "Baja ✅"  = "#2d6a4f"
  )) +
  scale_y_continuous(limits = c(0, 105)) +
  labs(
    title    = "Concentración Regional de la Producción por Cultivo",
    subtitle = "% aportado por los 3 principales estados | Promedio 2020-2024",
    x        = NULL,
    y        = "Concentración top-3 estados (%)",
    fill     = "Vulnerabilidad"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# PREGUNTA 3B
# ¿Cuál es el mapa productivo (heatmap) de los cultivos estrella por estado?
# ¿Qué estados son estratégicos para múltiples cultivos a la vez?
# ------------------------------------------------------------------------------

heatmap_data <- data %>%
  filter(Anio == 2024, Nomcultivo %in% cultivos_estrella) %>%
  group_by(NomEdo, Nomcultivo) %>%
  summarise(produccion = sum(Produccion, na.rm = TRUE), .groups = "drop") %>%
  group_by(Nomcultivo) %>%
  mutate(prod_norm = produccion / max(produccion, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(produccion > 0)

# Top 15 estados para no saturar el gráfico
top_estados_estrella <- heatmap_data %>%
  group_by(NomEdo) %>%
  summarise(total = sum(produccion)) %>%
  slice_max(total, n = 15) %>%
  pull(NomEdo)

heatmap_data %>%
  filter(NomEdo %in% top_estados_estrella) %>%
  ggplot(aes(x = Nomcultivo, y = NomEdo, fill = prod_norm)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(prod_norm > 0.1,
                               scales::comma(round(produccion / 1e3, 0)), "")),
            size = 2.5, color = "white") +
  scale_fill_gradient(low = "#d8f3dc", high = "#1b4332",
                      name = "Producción\n(normalizada)") +
  labs(
    title    = "Mapa de Calor: Producción de Cultivos Estrella por Estado (2024)",
    subtitle = "Valor normalizado respecto al estado líder por cultivo | Miles de ton",
    x        = "Cultivo",
    y        = "Estado"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))


# ==============================================================================
# SECCIÓN 4 — ANÁLISIS DE RIESGO PRODUCTIVO
# BRIEF: "Ante la renegociación del T-MEC, necesitamos identificar cultivos
#         con alta volatilidad en rendimiento: un cultivo inestable es difícil
#         de comprometer en cuotas de exportación."
# ==============================================================================

# ------------------------------------------------------------------------------
# PREGUNTA 4A
# ¿Cuál es el coeficiente de variación (CV) del rendimiento por cultivo
# en los últimos 10 años? ¿Qué cultivos son más estables para negociar?
# ------------------------------------------------------------------------------

volatilidad <- data %>%
  filter(Anio >= 2015) %>%
  group_by(Nomcultivo, Anio) %>%
  summarise(rend_nacional = mean(Rendimiento, na.rm = TRUE), .groups = "drop") %>%
  group_by(Nomcultivo) %>%
  summarise(
    rend_promedio = mean(rend_nacional,  na.rm = TRUE),
    rend_sd       = sd(rend_nacional,    na.rm = TRUE),
    rend_min      = min(rend_nacional,   na.rm = TRUE),
    rend_max      = max(rend_nacional,   na.rm = TRUE),
    cv_pct        = round(rend_sd / rend_promedio * 100, 1)
  ) %>%
  mutate(
    estabilidad = case_when(
      cv_pct < 10 ~ "Estable ✅",
      cv_pct < 20 ~ "Moderado 🔶",
      TRUE        ~ "Volátil ⚠️"
    )
  ) %>%
  arrange(cv_pct)

View(volatilidad)

# Gráfica: rango de variación por cultivo (lollipop con rango)
ggplot(volatilidad,
       aes(y = reorder(Nomcultivo, -cv_pct), color = estabilidad)) +
  geom_segment(aes(x = rend_min, xend = rend_max,
                   yend = reorder(Nomcultivo, -cv_pct)),
               linewidth = 1.2) +
  geom_point(aes(x = rend_promedio), size = 4) +
  geom_text(aes(x = rend_max, label = paste0("CV: ", cv_pct, "%")),
            hjust = -0.15, size = 3) +
  scale_color_manual(values = c(
    "Estable ✅"   = "#2d6a4f",
    "Moderado 🔶"  = "#f4a261",
    "Volátil ⚠️"  = "#e63946"
  )) +
  labs(
    title    = "Volatilidad del Rendimiento por Cultivo (2015-2024)",
    subtitle = "Segmento = rango mín-máx | Punto = promedio | CV = coeficiente de variación",
    x        = "Rendimiento (ton/ha)",
    y        = NULL,
    color    = "Estabilidad"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(5, 60, 5, 5))


# ------------------------------------------------------------------------------
# PREGUNTA 4B
# Para los cultivos volátiles: ¿la volatilidad en rendimiento va acompañada
# de volatilidad en precio? ¿Hay un efecto "doble golpe" para los productores?
# ------------------------------------------------------------------------------

cultivos_volatiles <- volatilidad %>%
  filter(estabilidad == "Volátil ⚠️") %>%
  pull(Nomcultivo)

doble_golpe <- data %>%
  filter(Nomcultivo %in% cultivos_volatiles, Anio >= 2015) %>%
  group_by(Nomcultivo, Anio) %>%
  summarise(
    rend  = mean(Rendimiento, na.rm = TRUE),
    pmr   = mean(PMR,         na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Nomcultivo) %>%
  mutate(
    rend_idx = rend / first(rend) * 100,
    pmr_idx  = pmr  / first(pmr)  * 100
  ) %>%
  pivot_longer(cols = c(rend_idx, pmr_idx),
               names_to  = "indicador",
               values_to = "indice") %>%
  mutate(indicador = recode(indicador,
                            "rend_idx" = "Rendimiento",
                            "pmr_idx"  = "Precio (PMR)"))

ggplot(doble_golpe, aes(x = Anio, y = indice,
                        color = indicador, group = indicador)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~ Nomcultivo, ncol = 2) +
  scale_color_manual(values = c("Rendimiento" = "#2d6a4f",
                                "Precio (PMR)" = "#c1121f")) +
  labs(
    title    = "Efecto 'Doble Golpe': Rendimiento vs Precio en Cultivos Volátiles",
    subtitle = "Índice base 100 = primer año disponible (2015)",
    x        = "Año",
    y        = "Índice (base 100)",
    color    = "Indicador"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")


# ==============================================================================
# SECCIÓN 5 — DOSSIER FINAL PARA EL SECRETARIO
# BRIEF: "El Secretario necesita una matriz de decisión con cada cultivo
#         clasificado en 4 cuadrantes estratégicos para la negociación del T-MEC"
#
#   CUADRANTE I   — PROTEGER:  alta participación + alta concentración regional
#   CUADRANTE II  — IMPULSAR:  alto crecimiento  + baja concentración
#   CUADRANTE III — MONITOREAR: volátiles con peso económico relevante
#   CUADRANTE IV  — DIVERSIFICAR: maduros con baja rentabilidad por ha
# ==============================================================================

# Construcción de la matriz estratégica
matriz_estrategica <- participacion %>%
  select(Nomcultivo, participacion_valor) %>%
  left_join(
    crecimiento %>% select(Nomcultivo, crecimiento_pct, clasificacion),
    by = "Nomcultivo"
  ) %>%
  left_join(
    indice_concentracion %>% select(Nomcultivo, concentracion_top3, vulnerabilidad),
    by = "Nomcultivo"
  ) %>%
  left_join(
    volatilidad %>% select(Nomcultivo, cv_pct, estabilidad),
    by = "Nomcultivo"
  ) %>%
  left_join(
    competitividad_2024 %>% select(Nomcultivo, valor_por_ha, rendimiento_med),
    by = "Nomcultivo"
  ) %>%
  mutate(
    recomendacion_tmec = case_when(
      participacion_valor >= 10 & concentracion_top3 >= 70          ~ "PROTEGER 🛡️",
      crecimiento_pct     >= 50 & concentracion_top3 <  70          ~ "IMPULSAR 🚀",
      cv_pct              >= 20 & participacion_valor >= 5          ~ "MONITOREAR 🔍",
      TRUE                                                           ~ "DIVERSIFICAR 🌐"
    )
  ) %>%
  arrange(recomendacion_tmec, desc(participacion_valor))

View(matriz_estrategica)

# Gráfica: Matriz estratégica (dispersión con cuadrantes)
media_crec  <- mean(matriz_estrategica$crecimiento_pct,  na.rm = TRUE)
media_valor <- mean(matriz_estrategica$participacion_valor, na.rm = TRUE)

ggplot(matriz_estrategica,
       aes(x = crecimiento_pct,
           y = participacion_valor,
           color = recomendacion_tmec,
           size  = valor_por_ha / 1000,
           label = Nomcultivo)) +
  # Cuadrantes
  annotate("rect", xmin = -Inf, xmax = media_crec,
           ymin = media_valor, ymax = Inf,
           fill = "#ffd166", alpha = 0.08) +
  annotate("rect", xmin = media_crec, xmax = Inf,
           ymin = media_valor, ymax = Inf,
           fill = "#06d6a0", alpha = 0.08) +
  annotate("rect", xmin = -Inf, xmax = media_crec,
           ymin = -Inf, ymax = media_valor,
           fill = "#ef476f", alpha = 0.08) +
  annotate("rect", xmin = media_crec, xmax = Inf,
           ymin = -Inf, ymax = media_valor,
           fill = "#118ab2", alpha = 0.08) +
  # Líneas de referencia
  geom_vline(xintercept = media_crec,  linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = media_valor, linetype = "dashed", color = "gray50") +
  # Etiquetas de cuadrante
  annotate("text", x = min(matriz_estrategica$crecimiento_pct, na.rm=TRUE),
           y = max(matriz_estrategica$participacion_valor, na.rm=TRUE),
           label = "PROTEGER", color = "#b5830a", fontface = "bold",
           hjust = 0, vjust = 1, size = 3.5) +
  annotate("text", x = max(matriz_estrategica$crecimiento_pct, na.rm=TRUE),
           y = max(matriz_estrategica$participacion_valor, na.rm=TRUE),
           label = "IMPULSAR", color = "#047857", fontface = "bold",
           hjust = 1, vjust = 1, size = 3.5) +
  # Puntos y etiquetas
  geom_point(alpha = 0.85) +
  geom_text(vjust = -0.9, size = 2.8, color = "gray20", fontface = "italic") +
  scale_color_manual(values = c(
    "PROTEGER 🛡️"     = "#f4a261",
    "IMPULSAR 🚀"      = "#2d6a4f",
    "MONITOREAR 🔍"   = "#e63946",
    "DIVERSIFICAR 🌐" = "#457b9d"
  )) +
  scale_size(range = c(3, 10), name = "Valor/ha\n(miles $)") +
  labs(
    title    = "Matriz Estratégica T-MEC 2026: Cultivos de Exportación Mexicanos",
    subtitle = "Eje X = crecimiento productivo (quinquenio) | Eje Y = peso en valor total exportable",
    x        = "Crecimiento producción 2003-2007 vs 2020-2024 (%)",
    y        = "Participación en valor total (%)",
    color    = "Recomendación T-MEC"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")


# Exportamos la matriz al Secretario
write_xlsx(
  matriz_estrategica %>%
    select(
      Cultivo            = Nomcultivo,
      `Part. Valor (%)`  = participacion_valor,
      `Crec. (%)`        = crecimiento_pct,
      `Clasif. Crec.`    = clasificacion,
      `Conc. Top3 (%)`   = concentracion_top3,
      `Vulnerabilidad`   = vulnerabilidad,
      `CV Rendimiento`   = cv_pct,
      `Estabilidad`      = estabilidad,
      `Valor/ha ($/ha)`  = valor_por_ha,
      `Recomendación`    = recomendacion_tmec
    ),
  "dossier_tmec_2026.xlsx",
  col_names = TRUE
)

message("✅ Dossier exportado: dossier_tmec_2026.xlsx")


# ==============================================================================
# RESUMEN DE FUNCIONES UTILIZADAS EN ESTA PRÁCTICA
#
#   dplyr:  filter, group_by, summarise, mutate, arrange, left_join,
#           slice_max, case_when, recode, n_distinct, lag, first, pull
#
#   tidyr:  pivot_wider, pivot_longer
#
#   ggplot2: geom_col, geom_point, geom_line, geom_tile, geom_segment,
#            geom_text, geom_hline, geom_vline, annotate, facet_wrap,
#            scale_fill_gradient, scale_color_manual, scale_size
# ==============================================================================