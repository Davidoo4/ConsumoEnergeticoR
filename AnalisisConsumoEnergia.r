# Paso 1: Configuración inicial
# Crear los vectores
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))
set.seed(123) # Para reproducibilidad
consumo <- c(rnorm(10, mean = 50, sd = 10), rnorm(10, mean = 80, sd = 15)) # Simula valores
consumo[5] <- NA  # Agregar un valor NA como ejemplo
consumo[15] <- NA # Agregar otro valor NA
costo_kwh <- c(rep(0.12, 10), rep(0.2, 10)) 

# Paso 2: Limpieza de datos
# Reemplazar los valores NA con la mediana del consumo diario por tipo de energía
library(dplyr)

# Crear un dataframe temporal para limpieza
df_temp <- data.frame(energia, consumo, costo_kwh)

# Calcular la mediana de consumo por tipo de energía y reemplazar los NA
df_temp <- df_temp %>%
  group_by(energia) %>%
  mutate(consumo = ifelse(is.na(consumo), median(consumo, na.rm = TRUE), consumo))

# Actualizar el vector consumo con los datos limpios
consumo <- df_temp$consumo

# Paso 3: Creación del dataframe
# Crear el dataframe con los datos limpios
df_consumo <- data.frame(
  energia = energia,
  consumo = consumo,
  costo_kwh = costo_kwh
)

# Paso 4: Cálculos
# Agregar columna costo_total
df_consumo <- df_consumo %>%
  mutate(costo_total = consumo * costo_kwh)

# Calcular el total de consumo y costo total por tipo de energía
totales <- df_consumo %>%
  group_by(energia) %>%
  summarise(
    total_consumo = sum(consumo),
    total_costo = sum(costo_total)
  )

# Calcular la media del consumo diario
medias <- df_consumo %>%
  group_by(energia) %>%
  summarise(media_consumo = mean(consumo))

# Agregar columna ganancia (aumento de precio del 10%)
df_consumo <- df_consumo %>%
  mutate(ganancia = costo_total * 1.1)

# Paso 5: Resumen
# Ordenar por costo_total en orden descendente
df_consumo_ordenado <- df_consumo %>%
  arrange(desc(costo_total))

# Calcular el total de consumo energético por tipo
total_energia <- df_consumo %>%
  group_by(energia) %>%
  summarise(total_consumo_energia = sum(consumo))

# Extraer las tres filas con mayor costo_total
top_3_costos <- df_consumo_ordenado %>%
  slice_head(n = 3)

# Crear la lista resumen_energia
resumen_energia <- list(
  datos_ordenados = df_consumo_ordenado,
  totales_energia = totales,
  medias_consumo = medias,
  top_3_costos = top_3_costos
)

# Mostrar la lista resumen_energia
print(resumen_energia)
