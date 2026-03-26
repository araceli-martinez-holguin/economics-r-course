# ==============================================================================
#
#   PRÁCTICA 2
#   Consejo Asesor del Secretario de Economía
#
#   CONTEXTO: El T-MEC (USMCA) será renegociado en 2026. El equipo debe
#             identificar qué productos agrícolas de exportación proteger,
#             cuáles impulsar y cuáles presentan vulnerabilidades.
#
#   DATOS:    Principales cultivos de exportación 2003–2024 (SIAP)
#             Fuente: https://nube.agricultura.gob.mx/cierre_agricola/
#
#   LIBRERÍAS: tidyr, dplyr, readxl, ggplot2, scales, RColorBrewer, writexl
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN INICIAL
# ------------------------------------------------------------------------------

rm(list = ls(all = TRUE))
gc()

install.packages(c("tidyr", "dplyr", "readxl", "ggplot2",
                   "scales", "RColorBrewer", "writexl",
                   "gganimate", "gifski"))

install.packages(c("gganimate", "gifski"))

install.packages("https://github.com/ricardo-bion/ggradar/archive/refs/heads/master.tar.gz",
repos = NULL,
type  = "source")


library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(writexl)
library(ggradar)
library(gganimate)
library(gifski)

# Directorio de Trabajo
setwd("~/Documents/Cursos-R/Laboratorio-2")

# Cargamos base de datos
data <- read_xlsx("cultivos.xlsx")

# Damos un vistazo a la base
glimpse(data)

# Columnas disponibles
colnames(data)
#   origen, Anio, Idestado, Nomestado, Idddr, Nomddr, Idcader, Nomcader,
#   Idmunicipio, Nommunicipio, Idciclo, Nomcicloproductivo, Idmodalidad,
#   Nommodalidad, Idunidadmedida, Nomunidad, Idcultivo, Nomcultivo,
#   Sembrada, Cosechada, Siniestrada, Volumenproduccion, Rendimiento,
#   Precio, Valorproduccion

# ¿Qué cultivos incluye la base?
sort(unique(data$Nomcultivo))

# ==============================================================================
# SECCIÓN 1 — DIAGNÓSTICO PRODUCTIVO
#
# Brief: "Antes de negociar necesitamos saber qué tan grande es cada sector
#         y cómo ha evolucionado en dos décadas. 
#         Sin ese diagnóstico base,vamos ciegos a la mesa."
# ==============================================================================


# ------------------------------------------------------------------------------
# 1A. Peso relativo de cada cultivo (2003–2024)
#
# ¿Cuál es la participación de cada cultivo en el total exportable?
# Funciones: group_by(), summarise(), mutate(), arrange(), sum(), round()
# ------------------------------------------------------------------------------

participacion <- data %>%
  # Paso 1: Agrupar por cultivo y sumar las variables de interés
  group_by(Nomcultivo) %>%
  summarise(
    produccion_acumulada = sum(Volumenproduccion, na.rm = TRUE),
    valor_acumulado      = sum(Valorproduccion,   na.rm = TRUE),
    superficie_acumulada = sum(Sembrada,           na.rm = TRUE)
  ) %>%
  # Paso 2: Calcular participación de cada cultivo en el total
  # Nota: mutate() opera sobre el dataframe ya resumido
  mutate(
    participacion_prod  = round(produccion_acumulada / sum(produccion_acumulada) * 100, 2),
    participacion_valor = round(valor_acumulado      / sum(valor_acumulado)      * 100, 2),
    participacion_sem   = round(superficie_acumulada      / sum(superficie_acumulada)  *100, 2)
  ) %>%
  arrange(desc(participacion_valor))

View(participacion)

# Gráfica: participación por valor
# Usamos reorder() para ordenar las barras de mayor a menor
p_part_valor <- ggplot(participacion,
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
    subtitle = "México 2003–2024 | Acumulado",
    x        = NULL,
    y        = "Participación (%)"
  ) +
  theme_minimal(base_size = 12)

p_part_valor
ggsave("participacion_valor.png", p_part_valor, dpi = 300, width = 12, height = 10)

# Gráfica: participación por volumen
p_part_prod <- ggplot(participacion,
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
    subtitle = "México 2003–2024 | Acumulado",
    x        = NULL,
    y        = "Participación (%)"
  ) +
  theme_minimal(base_size = 12)

p_part_prod
ggsave("participacion_prod.png", p_part_prod, dpi = 300, width = 12, height = 10)

# Gráfica: participación por superficie sembrada
p_part_sem <- ggplot(participacion,
                     aes(x = reorder(Nomcultivo, participacion_sem),
                         y = participacion_sem,
                         fill = participacion_sem)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(participacion_sem, "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "#1E90FF", high = "#104E8B") +
  scale_y_continuous(limits = c(0, max(participacion$participacion_sem) * 1.15)) +
  labs(
    title    = "Participación de la Superficie Sembrada en la Producción Exportable",
    subtitle = "México 2003–2024 | Acumulado",
    x        = NULL,
    y        = "Participación (%)"
  ) +
  theme_minimal(base_size = 12)

p_part_sem
ggsave("participacion_sem.png", p_part_sem, dpi = 300, width = 12, height = 10)


# Interpretación: ¿Coinciden los líderes en valor con los líderes en volumen?
# Cuando no coinciden, algunos cultivos tienen precios más altos por tonelada,
# lo que los hace estratégicamente más interesantes para la negociación.


# ------------------------------------------------------------------------------
# 1B. Cobertura territorial
#
# ¿En cuántos municipios y estados se produce cada cultivo cada año?
# Un cultivo presente en muchos municipios tiene base productiva diversificada.
# Funciones: n_distinct(), pivot_wider(), arrange()
# ------------------------------------------------------------------------------

n_cultivos <- n_distinct(data$Nomcultivo)

# --- Municipios ---
municipios_cultivo <- data %>%
  group_by(Anio, Nomcultivo) %>%
  summarise(
    num_municipios = n_distinct(Idmunicipio),  # IDs únicos, más robusto que nombres
    .groups = "drop"
  ) %>%
  arrange(Nomcultivo, Anio)

# Formato ancho: filas = cultivos, columnas = años
municipios_wide <- municipios_cultivo %>%
  pivot_wider(names_from = Anio, values_from = num_municipios) %>%
  arrange(Nomcultivo)

View(municipios_wide)

# Gráfica de lineas
p_municipios <- ggplot(municipios_cultivo,
                       aes(x = Anio, y = num_municipios,
                           color = Nomcultivo, group = Nomcultivo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(2003, 2024, by = 1)) +
  scale_color_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(n_cultivos)) +
  labs(
    title    = "Municipios Productores por Cultivo y Año",
    subtitle = "México 2003–2024",
    x = "", y = "Número de municipios", color = "Cultivo"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.text     = element_text(size = 9))

p_municipios
ggsave("municipios_por_cultivo.png", p_municipios, dpi = 300, width = 14, height = 8)

# Gráfica de barras traslapadas
municipios_cultivo <- municipios_cultivo %>%
  group_by(Anio) %>%
  mutate(pct = num_municipios / sum(num_municipios)) %>%
  arrange(Anio, pct) %>%
  ungroup() %>%
  mutate(Nomcultivo = reorder(Nomcultivo, pct))

pb_municipios <- ggplot(municipios_cultivo,
                        aes(x = Anio, y = pct, fill = Nomcultivo)) +
  geom_bar(stat = "identity", position = position_fill(reverse = FALSE)) +
  geom_text(aes(label = ifelse(pct >= 0.02, scales::percent(pct, accuracy = 1), "")),
            position = position_fill(vjust = 0.5),
            size = 2.5, color = "black") +
  scale_x_continuous(breaks = seq(2003, 2024, by = 1)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1.10),
                     expand = c(0, 0)) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(n_cultivos)) +
  labs(
    title    = "Municipios Productores por Cultivo y Año",
    subtitle = "México 2003–2024",
    x = "", y = "Porcentaje de municipios", fill = "Cultivo"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.text     = element_text(size = 9))

pb_municipios
ggsave("pb_municipio.png", pb_municipios, dpi = 300, width = 14, height = 8)


# --- 1. Prepare data ---
radar_data <- municipios_cultivo %>%
  group_by(Anio) %>%
  mutate(pct = num_municipios / sum(num_municipios)) %>%
  ungroup() %>%
  select(Anio, Nomcultivo, pct) %>%
  pivot_wider(names_from = Nomcultivo, values_from = pct, values_fill = 0) %>%
  arrange(Anio) %>%
  mutate(group = as.character(Anio)) %>%
  select(group, everything(), -Anio)

# --- 2. Colors per year ---
n_anios <- nrow(radar_data)
colores <- colorRampPalette(brewer.pal(9, "Set1"))(n_anios)

# --- 3. Build one radar per year and save frames ---
anios <- sort(unique(municipios_cultivo$Anio))

for (i in seq_along(anios)) {
  
  anio_i  <- anios[i]
  datos_i <- radar_data %>% filter(group == as.character(anio_i))
  
  p <- ggradar(
    datos_i,
    values.radar     = c("0%", "25%", "50%"),
    grid.min         = 0,
    grid.mid         = 0.25,
    grid.max         = 0.50,
    group.colours    = colores[i],
    group.line.width = 1.5,
    group.point.size = 4,
    axis.label.size  = 3,
    grid.label.size  = 4,
    legend.position  = "none",
    plot.title       = paste0("Municipios Productores por Cultivo\nMéxico — ", anio_i)
  ) +
    theme(
      plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(
    filename = sprintf("frame_%02d.png", i),
    plot     = p,
    width    = 10, height = 8, dpi = 150
  )
  
  message(sprintf("Frame %d/%d — %d", i, length(anios), anio_i))
}

# --- 4. Assemble GIF with gifski ---
frames <- sprintf("frame_%02d.png", seq_along(anios))

gifski(
  frames,
  gif_file = "radar_municipios.gif",
  width    = 1500,
  height   = 1200,
  delay    = 0.8
)

# --- 5. Clean up frames ---
file.remove(frames)

message("✓ GIF saved as radar_municipios.gif")

# --- Estados ---
# Usamos Idestado (numérico) en vez de Nomestado (texto)
# para evitar errores si hay variaciones ortográficas en los nombres
estados_cultivo <- data %>%
  group_by(Anio, Nomcultivo) %>%
  summarise(
    num_estados = n_distinct(Idestado),
    .groups = "drop"
  ) %>%
  arrange(Nomcultivo, Anio)

estados_wide <- estados_cultivo %>%
  pivot_wider(names_from = Anio, values_from = num_estados) %>%
  arrange(Nomcultivo)

View(estados_wide)

p_estados <- ggplot(estados_cultivo,
                    aes(x = Anio, y = num_estados,
                        color = Nomcultivo, group = Nomcultivo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(2003, 2024, by = 2)) +
  scale_color_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(n_cultivos)) +
  labs(
    title    = "Estados Productores por Cultivo y Año",
    subtitle = "México 2003–2024",
    x = "Año", y = "Número de estados", color = "Cultivo"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.text     = element_text(size = 9))

p_estados
ggsave("estados_por_cultivo.png", p_estados, dpi = 300, width = 14, height = 8)


# ------------------------------------------------------------------------------
# 1C. Perfil detallado por cultivo: loop con gráficas
#
# ¿Cómo se comporta cada estado dentro de cada cultivo en volumen,
# precio y rendimiento?
# Funciones: filter(), for, list(), ggsave(), dir.create()
# ------------------------------------------------------------------------------

dir.create("graficas_cultivos", showWarnings = FALSE)

tablas_cultivos <- list()  # Lista vacía que recibirá un df por cultivo

for (cultivo in unique(data$Nomcultivo)) {
  
  # Filtramos y resumimos solo para este cultivo
  df <- data %>%
    filter(Nomcultivo == cultivo) %>%
    group_by(Nomestado, Anio) %>%
    summarise(
      volumen     = round(sum(Volumenproduccion, na.rm = TRUE), 0),
      precio      = round(mean(Precio,           na.rm = TRUE), 2),
      rendimiento = round(mean(Rendimiento,      na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(Nomestado, Anio)
  
  tablas_cultivos[[cultivo]] <- df  # Guardamos en la lista con el nombre del cultivo
  
  # Preparamos nombre para archivo y paleta adaptada al número de estados
  nombre    <- gsub(" ", "_", cultivo)
  n_estados <- n_distinct(df$Nomestado)
  colores   <- colorRampPalette(brewer.pal(12, "Set3"))(n_estados)
  
  # --- Gráfica 1: Volumen ---
  p_vol <- ggplot(df, aes(x = Anio, y = volumen,
                          color = Nomestado, group = Nomestado)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    scale_x_continuous(breaks = seq(2003, 2024, by = 2)) +
    scale_color_manual(values = colores) +
    scale_y_continuous(labels = comma) +
    labs(title    = paste("Volumen de Producción —", cultivo),
         subtitle = "México 2003–2024 | Por estado",
         x = "Año", y = "Volumen (ton)", color = "Estado") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 8))
  
  # --- Gráfica 2: Precio ---
  p_precio <- ggplot(df, aes(x = Anio, y = precio,
                             color = Nomestado, group = Nomestado)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    scale_x_continuous(breaks = seq(2003, 2024, by = 2)) +
    scale_color_manual(values = colores) +
    scale_y_continuous(labels = comma) +
    labs(title    = paste("Precio Medio Rural —", cultivo),
         subtitle = "México 2003–2024 | Por estado",
         x = "Año", y = "Precio ($/ton)", color = "Estado") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 8))
  
  # --- Gráfica 3: Rendimiento ---
  p_rend <- ggplot(df, aes(x = Anio, y = rendimiento,
                           color = Nomestado, group = Nomestado)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    scale_x_continuous(breaks = seq(2003, 2024, by = 2)) +
    scale_color_manual(values = colores) +
    scale_y_continuous(labels = comma) +
    labs(title    = paste("Rendimiento —", cultivo),
         subtitle = "México 2003–2024 | Por estado",
         x = "Año", y = "Rendimiento (ton/ha)", color = "Estado") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 8))
  
  ggsave(paste0("graficas_cultivos/", nombre, "_volumen.png"),     p_vol,    dpi = 300, width = 14, height = 7)
  ggsave(paste0("graficas_cultivos/", nombre, "_precio.png"),      p_precio, dpi = 300, width = 14, height = 7)
  ggsave(paste0("graficas_cultivos/", nombre, "_rendimiento.png"), p_rend,   dpi = 300, width = 14, height = 7)
  
  cat("✓", cultivo, "\n")
}

# Así accedemos a un cultivo específico dentro de la lista
View(tablas_cultivos[["Aguacate"]])


# ------------------------------------------------------------------------------
# 1D. Crecimiento histórico y clasificación estratégica
#
# ¿Qué cultivos crecieron más entre 2003–2007 y 2020–2024?
#
# Escala de 5 niveles con lógica económica:
#   > 300%   → Boom exportador:     creció más de 4x. Casos excepcionales.
#   100–300% → Alto potencial:      creció 2x–4x. Expansión sostenida.
#   20–99%   → Crecimiento moderado: supera la inflación acumulada (~20%).
#   0–19%    → Estancado:           creció menos que la inflación; perdió valor real.
#   < 0%     → En declive:          cayó en valor absoluto.
#
# Nota: el umbral del 20% es clave — un cultivo que creció 5% en nominal
# perdió valor en términos reales. Llamarlo "Maduro" sería engañoso.
#
# Funciones: mutate(), case_when(), pivot_wider(), filter(), pull()
# ------------------------------------------------------------------------------

crecimiento <- data %>%
  # Paso 1: Etiquetar cada fila con su periodo
  mutate(
    periodo = case_when(
      Anio <= 2007 ~ "inicio",
      Anio >= 2020 ~ "reciente",
      TRUE         ~ NA_character_   # Los años intermedios no interesan
    )
  ) %>%
  filter(!is.na(periodo)) %>%
  # Paso 2: Promediar el valor por cultivo y periodo
  group_by(Nomcultivo, periodo) %>%
  summarise(valor_prom = mean(Valorproduccion, na.rm = TRUE), .groups = "drop") %>%
  # Paso 3: pivot_wider crea una columna por cada valor único de `periodo`
  pivot_wider(names_from = periodo, values_from = valor_prom) %>%
  # Paso 4: Calcular crecimiento y clasificar
  mutate(
    crecimiento_pct = round((reciente / inicio - 1) * 100, 1),
    clasificacion   = case_when(
      crecimiento_pct >  300 ~ "Boom exportador 🚀",
      crecimiento_pct >= 100 ~ "Alto potencial ⭐",
      crecimiento_pct >=  20 ~ "Crecimiento moderado 🌿",
      crecimiento_pct >=   0 ~ "Estancado ⚠️",
      TRUE                   ~ "En declive 🔴"
    )
  ) %>%
  arrange(desc(crecimiento_pct))

View(crecimiento)

ggplot(crecimiento,
       aes(x = reorder(Nomcultivo, crecimiento_pct),
           y = crecimiento_pct,
           fill = clasificacion)) +
  geom_col() +
  geom_text(aes(label = paste0(crecimiento_pct, "%")),
            hjust = -0.1, size = 3) +
  # Líneas de referencia que marcan los umbrales de clasificación
  geom_hline(yintercept =   0, linetype = "dashed", color = "gray40",  linewidth = 0.6) +
  geom_hline(yintercept =  20, linetype = "dotted", color = "#f4a261", linewidth = 0.6) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "#2d6a4f", linewidth = 0.6) +
  geom_hline(yintercept = 300, linetype = "dotted", color = "#1b4332", linewidth = 0.6) +
  coord_flip() +
  scale_y_continuous(limits = c(
    min(crecimiento$crecimiento_pct, na.rm = TRUE) * 1.1,
    max(crecimiento$crecimiento_pct, na.rm = TRUE) * 1.12
  )) +
  scale_fill_manual(values = c(
    "Boom exportador 🚀"      = "#1b4332",
    "Alto potencial ⭐"        = "#2d6a4f",
    "Crecimiento moderado 🌿" = "#95d5b2",
    "Estancado ⚠️"            = "#f4a261",
    "En declive 🔴"           = "#e63946"
  )) +
  labs(
    title    = "Crecimiento del Valor de Producción por Cultivo",
    subtitle = "Comparación quinquenio 2003–2007 vs 2020–2024\nLíneas de referencia: 0% | 20% (inflación acum.) | 100% | 300%",
    x = NULL, y = "Crecimiento (%)", fill = "Clasificación"
  ) +
  theme_minimal(base_size = 12)

# Los dos niveles superiores forman el grupo de cultivos de alto dinamismo
# pull() convierte una columna de un dataframe en un vector
cultivos_estrella <- crecimiento %>%
  filter(clasificacion %in% c("Boom exportador 🚀", "Alto potencial ⭐")) %>%
  pull(Nomcultivo)

cat("Cultivos de alto dinamismo identificados:\n")
cat(paste("→", cultivos_estrella, collapse = "\n"))


# ------------------------------------------------------------------------------
# 1E. Crecimiento interanual con lag()
#
# ¿En qué año tuvo cada cultivo su mayor salto de valor?
# lag() compara cada valor con el del año anterior dentro del mismo grupo.
# Funciones: lag(), slice_max(), slice_min()
# ------------------------------------------------------------------------------

crecimiento_anual <- data %>%
  group_by(Nomcultivo, Anio) %>%
  summarise(valor = sum(Valorproduccion, na.rm = TRUE), .groups = "drop") %>%
  arrange(Nomcultivo, Anio) %>%
  group_by(Nomcultivo) %>%
  mutate(
    valor_anterior = lag(valor),                                     # Valor del año previo
    cambio_pct     = round((valor / valor_anterior - 1) * 100, 1)   # Cambio %
  ) %>%
  filter(!is.na(cambio_pct))  # El primer año de cada cultivo no tiene lag

# ¿En qué año fue el mayor salto positivo por cultivo?
mejor_anio <- crecimiento_anual %>%
  slice_max(cambio_pct, n = 1) %>%
  select(Nomcultivo, Anio, cambio_pct)

View(mejor_anio)

# Crecimiento interanual de los cultivos de alto dinamismo
crecimiento_anual %>%
  filter(Nomcultivo %in% cultivos_estrella) %>%
  ggplot(aes(x = Anio, y = cambio_pct,
             color = Nomcultivo, group = Nomcultivo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(~Nomcultivo, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 4)) +
  labs(
    title    = "Crecimiento Interanual del Valor — Cultivos de Alto Dinamismo",
    subtitle = "Cambio porcentual respecto al año anterior",
    x = "Año", y = "Cambio (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


# ------------------------------------------------------------------------------
# 1F. Resumen eficiente con across()
#
# ¿Cómo calcular la misma estadística en múltiples columnas a la vez?
# across() aplica una o varias funciones a un conjunto de columnas en una línea.
# Funciones: across(), list(), .names
# ------------------------------------------------------------------------------

resumen_nacional <- data %>%
  group_by(Nomcultivo, Anio) %>%
  summarise(
    # across() aplica CADA función a CADA columna seleccionada
    # .names = "{.col}_{.fn}" nombra automáticamente: Volumenproduccion_media, etc.
    across(
      c(Volumenproduccion, Valorproduccion, Rendimiento, Precio),
      list(
        media = ~round(mean(.x, na.rm = TRUE), 2),
        total = ~round(sum(.x,  na.rm = TRUE), 0)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# Vista de las primeras filas del aguacate
resumen_nacional %>%
  filter(Nomcultivo == "Aguacate") %>%
  select(Nomcultivo, Anio, Volumenproduccion_total, Precio_media, Rendimiento_media)


# ------------------------------------------------------------------------------
# 1G. Detección de brechas con complete()
#
# ¿Hay estados que dejaron de producir un cultivo en algún período?
# complete() rellena las combinaciones faltantes con NA o cero.
# Funciones: complete(), fill, count(), rename()
# ------------------------------------------------------------------------------

data_completa <- data %>%
  group_by(Nomcultivo, Nomestado, Anio) %>%
  summarise(volumen = sum(Volumenproduccion, na.rm = TRUE), .groups = "drop") %>%
  # complete() genera TODAS las combinaciones posibles de las tres variables
  # y rellena el volumen con 0 donde no había registro
  complete(Nomcultivo, Nomestado, Anio,
           fill = list(volumen = 0))

# ¿Cuántos años-sin-producción tiene cada cultivo?
# Indica estados que entraron o salieron del mapa productivo
brechas <- data_completa %>%
  filter(volumen == 0) %>%
  count(Nomcultivo, sort = TRUE) %>%
  rename(registros_sin_produccion = n)

View(brechas)

# Interpretación: un cultivo con muchos registros_sin_produccion tiene
# base territorial inestable, lo que complica comprometer cuotas en el T-MEC.


# ==============================================================================
# SECCIÓN 2 — COMPETITIVIDAD Y EFICIENCIA
#
# Brief: "Para la mesa de negociación necesitamos saber cuáles son nuestros
#         cultivos más competitivos: los que generan más valor con menos
#         superficie. Esos son los que debemos proteger con aranceles."
# ==============================================================================


# ------------------------------------------------------------------------------
# 2A. Valor generado por hectárea (2024)
#
# ¿Cuál es el cultivo más rentable por unidad de superficie?
# Funciones: filter(), summarise(), mutate() con operaciones derivadas
# ------------------------------------------------------------------------------

competitividad_2024 <- data %>%
  filter(Anio == 2024) %>%
  group_by(Nomcultivo) %>%
  summarise(
    volumen         = sum(Volumenproduccion, na.rm = TRUE),
    valor_total     = sum(Valorproduccion,   na.rm = TRUE),
    superficie_cos  = sum(Cosechada,         na.rm = TRUE),
    rendimiento_med = mean(Rendimiento,      na.rm = TRUE),
    precio_promedio = mean(Precio,           na.rm = TRUE)
  ) %>%
  mutate(
    # Indicador central: ¿cuánto valor genera cada hectárea cosechada?
    valor_por_ha  = round(valor_total / superficie_cos, 0),
    # Índice relativo: 100 = promedio, >100 = más rentable que el promedio
    indice_rentab = round(valor_por_ha / mean(valor_por_ha) * 100, 1)
  ) %>%
  arrange(desc(valor_por_ha))

View(competitividad_2024 %>%
       select(Nomcultivo, valor_por_ha, indice_rentab, rendimiento_med, precio_promedio))

# Gráfica de dispersión: rendimiento vs valor por hectárea
# Tamaño de burbuja = volumen producido | Color = precio de mercado
# Cultivos en esquina superior derecha: alto rendimiento Y alto valor por ha
ggplot(competitividad_2024,
       aes(x = rendimiento_med,
           y = valor_por_ha / 1000,
           size  = volumen / 1e3,
           color = precio_promedio,
           label = Nomcultivo)) +
  geom_point(alpha = 0.8) +
  geom_text(vjust = -1, size = 2.8, color = "gray20") +
  scale_color_gradient(low = "#ffe566", high = "#c1121f",
                       name = "Precio\n($/ton)") +
  scale_size(name = "Producción\n(miles ton)", range = c(3, 12)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Competitividad de Cultivos de Exportación (2024)",
    subtitle = "Tamaño = volumen producido | Color = precio de mercado",
    x = "Rendimiento promedio (ton/ha)",
    y = "Valor generado por hectárea (miles de pesos)"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# 2B. Tendencia del precio de los cultivos de alto dinamismo (2010–2024)
#
# ¿Han mantenido su precio o muestran pérdida de competitividad?
# Un cultivo que crece en volumen pero cae en precio pierde poder de negociación.
# Funciones: %in%, facet_wrap(), scales = "free_y"
# ------------------------------------------------------------------------------

precio_tendencia <- data %>%
  filter(Nomcultivo %in% cultivos_estrella, Anio >= 2010) %>%
  group_by(Nomcultivo, Anio) %>%
  summarise(precio_nacional = mean(Precio, na.rm = TRUE), .groups = "drop")

ggplot(precio_tendencia,
       aes(x = Anio, y = precio_nacional,
           color = Nomcultivo, group = Nomcultivo)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  # facet_wrap divide la gráfica en paneles, uno por cultivo
  # scales = "free_y" permite eje Y propio por panel (unidades distintas)
  facet_wrap(~Nomcultivo, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  labs(
    title    = "Tendencia del Precio Medio Rural — Cultivos de Alto Dinamismo",
    subtitle = "2010–2024 | Pesos por tonelada",
    x = "Año", y = "Precio ($/ton)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


# ------------------------------------------------------------------------------
# 2C. Variabilidad del rendimiento con geom_ribbon()
#
# ¿Qué tan consistente es el rendimiento entre estados?
# Una banda ancha indica alta desigualdad productiva interna,
# lo cual complica comprometer cuotas uniformes en el T-MEC.
# Funciones: sd(), geom_ribbon()
# ------------------------------------------------------------------------------

data %>%
  group_by(Nomcultivo, Anio) %>%
  summarise(
    rend_mean = mean(Rendimiento, na.rm = TRUE),
    rend_sd   = sd(Rendimiento,   na.rm = TRUE),   # Desviación estándar entre estados
    .groups   = "drop"
  ) %>%
  filter(Nomcultivo %in% cultivos_estrella) %>%
  ggplot(aes(x = Anio, y = rend_mean,
             color = Nomcultivo, fill = Nomcultivo, group = Nomcultivo)) +
  # geom_ribbon dibuja una banda entre ymin y ymax
  geom_ribbon(aes(ymin = rend_mean - rend_sd,
                  ymax = rend_mean + rend_sd),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(2003, 2024, by = 2)) +
  facet_wrap(~Nomcultivo, scales = "free_y") +
  labs(
    title    = "Rendimiento Promedio Nacional y Dispersión entre Estados — Cultivos de Alto Dinamismo",
    subtitle = "Línea = promedio nacional | Banda = ±1 desviación estándar entre estados",
    x = "Año", y = "Rendimiento (ton/ha)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


# ==============================================================================
# SECCIÓN 3 — CONCENTRACIÓN REGIONAL Y VULNERABILIDAD
#
# Brief: "Si un cultivo depende de un solo estado y ese estado sufre una
#         sequía o una sanción regional, perdemos toda la capacidad de
#         exportación. Necesito saber cuáles cultivos son así de frágiles."
# ==============================================================================


# ------------------------------------------------------------------------------
# 3A. Índice de concentración regional (top-3 estados)
#
# ¿Qué % de la producción de cada cultivo está en los 3 estados líderes?
# Cuanto más alto, mayor la vulnerabilidad geográfica.
# Funciones: row_number(), filter(rank <= 3), case_when() con umbrales
# ------------------------------------------------------------------------------

# Paso 1: Producción por cultivo y estado (últimos 5 años)
concentracion_estados <- data %>%
  filter(Anio >= 2020) %>%
  group_by(Nomcultivo, Nomestado) %>%
  summarise(produccion = sum(Volumenproduccion, na.rm = TRUE), .groups = "drop") %>%
  group_by(Nomcultivo) %>%
  mutate(
    total_cultivo = sum(produccion),
    participacion = produccion / total_cultivo * 100
  ) %>%
  arrange(Nomcultivo, desc(produccion)) %>%
  mutate(rank = row_number()) %>%   # Rank dentro de cada cultivo
  ungroup()

# Paso 2: Sumar la participación de los 3 primeros estados por cultivo
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
  scale_y_continuous(limits = c(0, 108)) +
  labs(
    title    = "Concentración Regional de la Producción por Cultivo",
    subtitle = "% aportado por los 3 principales estados | Promedio 2020–2024",
    x = NULL, y = "Concentración top-3 estados (%)", fill = "Vulnerabilidad"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# 3B. Mapa de calor: cultivos de alto dinamismo por estado
#
# ¿Qué estados son estratégicos para múltiples cultivos a la vez?
# Un estado que lidera varios cultivos es un nodo crítico del sistema.
# Funciones: geom_tile(), slice_max(), normalización dentro de grupo
# ------------------------------------------------------------------------------

heatmap_data <- data %>%
  filter(Anio == 2024, Nomcultivo %in% cultivos_estrella) %>%
  group_by(Nomestado, Nomcultivo) %>%
  summarise(produccion = sum(Volumenproduccion, na.rm = TRUE), .groups = "drop") %>%
  group_by(Nomcultivo) %>%
  # Normalizamos respecto al estado líder de cada cultivo (= 1.0)
  mutate(prod_norm = produccion / max(produccion, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(produccion > 0)

# Top 15 estados con mayor producción total en estos cultivos
top_estados <- heatmap_data %>%
  group_by(Nomestado) %>%
  summarise(total = sum(produccion)) %>%
  slice_max(total, n = 15) %>%
  pull(Nomestado)

heatmap_data %>%
  filter(Nomestado %in% top_estados) %>%
  ggplot(aes(x = Nomcultivo, y = Nomestado, fill = prod_norm)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(prod_norm > 0.1,
                               comma(round(produccion / 1e3, 0)), "")),
            size = 2.5, color = "white") +
  scale_fill_gradient(low = "#d8f3dc", high = "#1b4332",
                      name = "Producción\n(normalizada)") +
  labs(
    title    = "Mapa de Calor: Cultivos de Alto Dinamismo por Estado (2024)",
    subtitle = "Color normalizado respecto al estado líder por cultivo | Cifras en miles de ton",
    x = "Cultivo", y = "Estado"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))


# ==============================================================================
# SECCIÓN 4 — RIESGO PRODUCTIVO Y VOLATILIDAD
#
# Brief: "Un cultivo volátil es imposible de comprometer en cuotas de
#         exportación. Si el rendimiento cae 40% en un año malo, incumplimos
#         el tratado. Necesito saber cuáles son estables y cuáles no."
# ==============================================================================


# ------------------------------------------------------------------------------
# 4A. Coeficiente de variación del rendimiento (2015–2024)
#
# CV = desviación estándar / media × 100
# Un CV bajo = estabilidad; uno alto = imprevisibilidad.
# Funciones: sd(), min(), max(), geom_segment(), doble group_by()
# ------------------------------------------------------------------------------

volatilidad <- data %>%
  filter(Anio >= 2015) %>%
  # Paso 1: Promedio nacional por año (sin diferencias entre estados)
  group_by(Nomcultivo, Anio) %>%
  summarise(rend_nacional = mean(Rendimiento, na.rm = TRUE), .groups = "drop") %>%
  # Paso 2: Estadísticas de la serie temporal por cultivo
  group_by(Nomcultivo) %>%
  summarise(
    rend_promedio = mean(rend_nacional, na.rm = TRUE),
    rend_sd       = sd(rend_nacional,   na.rm = TRUE),
    rend_min      = min(rend_nacional,  na.rm = TRUE),
    rend_max      = max(rend_nacional,  na.rm = TRUE),
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

# Lollipop con rango: segmento = mín a máx, punto = promedio
ggplot(volatilidad,
       aes(y = reorder(Nomcultivo, -cv_pct), color = estabilidad)) +
  geom_segment(aes(x = rend_min, xend = rend_max,
                   yend = reorder(Nomcultivo, -cv_pct)),
               linewidth = 1.2) +
  geom_point(aes(x = rend_promedio), size = 4) +
  geom_text(aes(x = rend_max, label = paste0("CV: ", cv_pct, "%")),
            hjust = -0.15, size = 3) +
  scale_color_manual(values = c(
    "Estable ✅"  = "#2d6a4f",
    "Moderado 🔶" = "#f4a261",
    "Volátil ⚠️"  = "#e63946"
  )) +
  labs(
    title    = "Volatilidad del Rendimiento por Cultivo (2015–2024)",
    subtitle = "Segmento = rango mín–máx | Punto = promedio | Etiqueta = coeficiente de variación",
    x = "Rendimiento (ton/ha)", y = NULL, color = "Estabilidad"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.margin = margin(5, 80, 5, 5))


# ------------------------------------------------------------------------------
# 4B. Efecto "doble golpe": rendimiento y precio simultáneos
#
# ¿Los cultivos volátiles también tienen precios inestables?
# Cuando rendimiento y precio caen juntos, el productor pierde doble:
# produce menos y recibe menos por cada tonelada.
# Funciones: first(), pivot_longer(), recode(), facet_wrap()
# ------------------------------------------------------------------------------

cultivos_volatiles <- volatilidad %>%
  filter(estabilidad == "Volátil ⚠️") %>%
  pull(Nomcultivo)

doble_golpe <- data %>%
  filter(Nomcultivo %in% cultivos_volatiles, Anio >= 2015) %>%
  group_by(Nomcultivo, Anio) %>%
  summarise(
    rend   = mean(Rendimiento, na.rm = TRUE),
    precio = mean(Precio,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Nomcultivo) %>%
  mutate(
    # Índice base 100 en 2015: permite comparar variables con unidades distintas
    # (ton/ha vs $/ton) en la misma escala
    rend_idx   = rend   / first(rend)   * 100,
    precio_idx = precio / first(precio) * 100
  ) %>%
  # pivot_longer lleva rend_idx y precio_idx a una sola columna "indice"
  pivot_longer(cols = c(rend_idx, precio_idx),
               names_to  = "indicador",
               values_to = "indice") %>%
  mutate(indicador = recode(indicador,
                            "rend_idx"   = "Rendimiento",
                            "precio_idx" = "Precio"))

ggplot(doble_golpe,
       aes(x = Anio, y = indice, color = indicador, group = indicador)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray60") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~Nomcultivo, ncol = 2) +
  scale_color_manual(values = c("Rendimiento" = "#2d6a4f",
                                "Precio"      = "#c1121f")) +
  labs(
    title    = "Efecto 'Doble Golpe': Rendimiento vs Precio en Cultivos Volátiles",
    subtitle = "Índice base 100 = 2015 | Cuando ambas líneas caen juntas, el productor pierde doble",
    x = "Año", y = "Índice (base 100 = 2015)", color = "Indicador"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")


# ==============================================================================
# SECCIÓN 5 — DOSSIER FINAL PARA EL SECRETARIO
#
# Brief: "Necesito una sola tabla que me diga, para cada cultivo, qué hacer:
#         ¿lo protegemos con aranceles? ¿lo impulsamos con apoyos?
#         ¿lo monitoreamos? Quiero llegar a la reunión con eso claro."
# ==============================================================================


# ------------------------------------------------------------------------------
# 5A. Construcción de la matriz estratégica
#
# Integramos todos los análisis anteriores con left_join() múltiple
# y generamos una recomendación de política por cultivo.
# Funciones: left_join() múltiple, select() con renombrado, write_xlsx()
# ------------------------------------------------------------------------------

matriz_estrategica <- participacion %>%
  select(Nomcultivo, participacion_valor) %>%
  # Unimos los resultados de cada sección anterior
  left_join(crecimiento          %>% select(Nomcultivo, crecimiento_pct, clasificacion),        by = "Nomcultivo") %>%
  left_join(indice_concentracion %>% select(Nomcultivo, concentracion_top3, vulnerabilidad),    by = "Nomcultivo") %>%
  left_join(volatilidad          %>% select(Nomcultivo, cv_pct, estabilidad),                   by = "Nomcultivo") %>%
  left_join(competitividad_2024  %>% select(Nomcultivo, valor_por_ha, rendimiento_med),         by = "Nomcultivo") %>%
  mutate(
    # Reglas de política T-MEC — las condiciones se evalúan en orden
    recomendacion_tmec = case_when(
      participacion_valor >= 10 & concentracion_top3 >= 70                                        ~ "PROTEGER 🛡️",    # Grande y geográficamente concentrado
      clasificacion %in% c("Boom exportador 🚀", "Alto potencial ⭐") & concentracion_top3 < 70  ~ "IMPULSAR 🚀",     # Dinámico y territorialmente diversificado
      cv_pct              >= 20 & participacion_valor >= 5                                        ~ "MONITOREAR 🔍",   # Volátil pero económicamente relevante
      TRUE                                                                                         ~ "DIVERSIFICAR 🌐" # El resto
    )
  ) %>%
  arrange(recomendacion_tmec, desc(participacion_valor))

View(matriz_estrategica)


# ------------------------------------------------------------------------------
# 5B. Gráfica de la matriz estratégica
# ------------------------------------------------------------------------------

media_crec  <- mean(matriz_estrategica$crecimiento_pct,    na.rm = TRUE)
media_valor <- mean(matriz_estrategica$participacion_valor, na.rm = TRUE)

ggplot(matriz_estrategica,
       aes(x = crecimiento_pct,
           y = participacion_valor,
           color = recomendacion_tmec,
           size  = valor_por_ha / 1000,
           label = Nomcultivo)) +
  # Fondos de cuadrante
  annotate("rect", xmin = -Inf,       xmax = media_crec, ymin = media_valor, ymax = Inf,  fill = "#ffd166", alpha = 0.08) +
  annotate("rect", xmin = media_crec, xmax = Inf,        ymin = media_valor, ymax = Inf,  fill = "#06d6a0", alpha = 0.08) +
  annotate("rect", xmin = -Inf,       xmax = media_crec, ymin = -Inf, ymax = media_valor, fill = "#ef476f", alpha = 0.08) +
  annotate("rect", xmin = media_crec, xmax = Inf,        ymin = -Inf, ymax = media_valor, fill = "#118ab2", alpha = 0.08) +
  # Líneas de referencia
  geom_vline(xintercept = media_crec,  linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = media_valor, linetype = "dashed", color = "gray50") +
  # Etiquetas de cuadrante
  annotate("text", x = min(matriz_estrategica$crecimiento_pct, na.rm = TRUE),
           y = max(matriz_estrategica$participacion_valor, na.rm = TRUE),
           label = "PROTEGER", color = "#b5830a", fontface = "bold", hjust = 0, vjust = 1, size = 3.5) +
  annotate("text", x = max(matriz_estrategica$crecimiento_pct, na.rm = TRUE),
           y = max(matriz_estrategica$participacion_valor, na.rm = TRUE),
           label = "IMPULSAR", color = "#047857", fontface = "bold", hjust = 1, vjust = 1, size = 3.5) +
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
    subtitle = "Eje X = crecimiento del valor productivo | Eje Y = peso en el valor total exportable\nTamaño de burbuja = valor generado por hectárea",
    x     = "Crecimiento del valor 2003–2007 vs 2020–2024 (%)",
    y     = "Participación en valor total (%)",
    color = "Recomendación T-MEC"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")


# ------------------------------------------------------------------------------
# 5C. Exportación del dossier a Excel
# ------------------------------------------------------------------------------

write_xlsx(
  matriz_estrategica %>%
    select(
      Cultivo             = Nomcultivo,
      `Part. Valor (%)`   = participacion_valor,
      `Crec. (%)`         = crecimiento_pct,
      `Clasif. Crec.`     = clasificacion,
      `Conc. Top3 (%)`    = concentracion_top3,
      `Vulnerabilidad`    = vulnerabilidad,
      `CV Rendimiento`    = cv_pct,
      `Estabilidad`       = estabilidad,
      `Valor/ha ($/ha)`   = valor_por_ha,
      `Recomendación`     = recomendacion_tmec
    ),
  "dossier_tmec_2026.xlsx"
)

message("✅ Dossier exportado: dossier_tmec_2026.xlsx")


# ==============================================================================
# EJERCICIOS
# ==============================================================================

# Trabaja los ejercicios en orden: cada uno introduce funciones nuevas
# o combina las de la sección anterior.

# ------------------------------------------------------------------------------
# EJERCICIO 1 — Guiado | filter + summarise + arrange
#
# ¿Cuáles son los 5 municipios con mayor valor de producción de un cultivo
# de tu elección en 2024? ¿En qué estados se ubican?
#
# Columnas útiles: Nommunicipio, Nomestado, Nomcultivo, Anio, Valorproduccion
# Pista: filtra primero por cultivo y año, luego agrupa por municipio,
#        suma el valor y ordena.
# ------------------------------------------------------------------------------

# Tu código aquí


# ------------------------------------------------------------------------------
# EJERCICIO 2 — Semi-guiado | lag() + slice_max() + slice_min()
#
# Para el cultivo con mayor participación en valor (Sección 1), calcula el
# crecimiento interanual del volumen entre 2003 y 2024.
#   - ¿En qué año fue el mayor salto positivo?
#   - ¿En qué año fue la mayor caída?
#
# Pista: usa lag() dentro de mutate() después de agrupar por cultivo.
#        Luego usa slice_max() y slice_min().
# ------------------------------------------------------------------------------

# Tu código aquí


# ------------------------------------------------------------------------------
# EJERCICIO 3 — Semi-guiado | pivot_longer() + facet_wrap()
#
# Para los cultivos de alto dinamismo, crea UNA sola gráfica con paneles
# separados que muestre la evolución del volumen Y el precio en el mismo
# panel usando líneas de diferente color.
#
# Pasos:
#   1. Resume volumen y precio por cultivo y año
#   2. Usa pivot_longer() para llevar ambas variables a una sola columna
#   3. Usa facet_wrap() con scales = "free_y" (unidades distintas)
# ------------------------------------------------------------------------------

# Tu código aquí


# ------------------------------------------------------------------------------
# EJERCICIO 4 — Abierto | pipeline completo
#
# Identifica los 3 estados más resilientes del sistema agroexportador.
# Define resiliencia como:
#   1. Producen al menos 5 cultivos distintos en 2024
#   2. Su valor total de producción aumentó entre 2010 y 2024
#   3. Su rendimiento promedio está por encima de la media nacional
#
# Justifica con al menos una gráfica.
#
# Columnas útiles: Nomestado, Nomcultivo, Anio, Valorproduccion, Rendimiento
# ------------------------------------------------------------------------------

# Tu código aquí


# ------------------------------------------------------------------------------
# EJERCICIO 5 — Reto | across() + loop + write_xlsx()
#
# Para cada cultivo de alto dinamismo, genera una hoja de Excel con su
# evolución anual de volumen, precio y rendimiento usando across() + loop.
#
# El archivo final debe tener una hoja por cultivo, nombrada con el nombre
# del cultivo.
#
# Pista: write_xlsx() acepta una lista nombrada donde cada elemento es un
#        dataframe → cada elemento se convierte en una hoja.
# ------------------------------------------------------------------------------

# Tu código aquí


# ==============================================================================
# RESUMEN DE FUNCIONES UTILIZADAS
#
#   dplyr:   filter, group_by, summarise, mutate, arrange, left_join,
#            select, slice_max, slice_min, case_when, recode, n_distinct,
#            lag, first, pull, across, rename, row_number, ungroup
#
#   tidyr:   pivot_wider, pivot_longer, complete
#
#   ggplot2: geom_col, geom_point, geom_line, geom_tile, geom_segment,
#            geom_text, geom_hline, geom_vline, geom_ribbon, annotate,
#            facet_wrap, coord_flip, scale_fill_gradient, scale_color_manual,
#            scale_size, scale_y_continuous, scale_x_continuous
#
#   writexl: write_xlsx
#   scales:  comma
#   RColorBrewer: brewer.pal, colorRampPalette
#
# ==============================================================================
