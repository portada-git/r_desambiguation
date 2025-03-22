library(parallel)
library(stringdist)
library(dplyr)
library(readr)

# Definir las ponderaciones para cada columna
ponderaciones <- c(
  ship_type = 1.5,
  ship_flag = 0.5,
  ship_name_combined = 3.0,  
  master_name_combined = 2.5, 
  ship_tons = 1.5
)

# Función para calcular similitud fonética
calcular_similitud_fonetica <- function(texto1, texto2) {
  # Usar múltiples algoritmos fonéticos
  soundex1 <- stringdist::phonetic(texto1, method = "soundex")
  soundex2 <- stringdist::phonetic(texto2, method = "soundex")
  
  # Para metaphone, usamos también "soundex"
  metaphone1 <- stringdist::phonetic(texto1, method = "soundex")
  metaphone2 <- stringdist::phonetic(texto2, method = "soundex")
  
  # Calcular similitudes para cada algoritmo
  sim_soundex <- ifelse(soundex1 == soundex2, 1, 0)
  sim_metaphone <- ifelse(metaphone1 == metaphone2, 1, 0)
  
  return((sim_soundex + sim_metaphone) / 2)
}

# Función para calcular similitud combinada (morfológica y fonética)
calcular_similitud_combinada <- function(texto1, texto2) {
  # Calcular similitud morfológica
  sim_morfologica <- 1 - stringdist(texto1, texto2, method = "jw")
  
  # Calcular similitud fonética
  sim_fonetica <- calcular_similitud_fonetica(texto1, texto2)
  
  # Retornar promedio
  return((sim_morfologica + sim_fonetica) / 2)
}

# Función para calcular similitud de nombres (nombre y apellido)
calcular_similitud_nombres <- function(nombre1, nombre2, apellido1, apellido2) {
  # Calcular similitud para nombres
  sim_nombres <- calcular_similitud_combinada(nombre1, nombre2)
  
  # Calcular similitud para apellidos
  sim_apellidos <- calcular_similitud_combinada(apellido1, apellido2)
  
  # Retornar promedio
  return((sim_nombres + sim_apellidos) / 2)
}

# Función modificada para calcular la similitud entre dos filas
similitud_filas <- function(fila1, fila2, ponderaciones) {
  similitud_total <- 0
  suma_ponderaciones <- 0
  
  # Procesar similitud de tipos de barco
  if (!is.na(fila1$ship_type) && !is.na(fila2$ship_type)) {
    similitud <- 1 - stringdist(fila1$ship_type, fila2$ship_type, method = "jw")
    similitud_total <- similitud_total + (similitud * ponderaciones["ship_type"])
    suma_ponderaciones <- suma_ponderaciones + ponderaciones["ship_type"]
  }
  
  # Procesar similitud de banderas
  if (!is.na(fila1$ship_flag) && !is.na(fila2$ship_flag)) {
    similitud <- 1 - stringdist(fila1$ship_flag, fila2$ship_flag, method = "jw")
    similitud_total <- similitud_total + (similitud * ponderaciones["ship_flag"])
    suma_ponderaciones <- suma_ponderaciones + ponderaciones["ship_flag"]
  }
  
  # Procesar similitud de tonelaje
  if (!is.na(fila1$ship_tons) && !is.na(fila2$ship_tons)) {
    # Convertir a numérico y calcular diferencia relativa
    tons1 <- as.numeric(fila1$ship_tons)
    tons2 <- as.numeric(fila2$ship_tons)
    if (!is.na(tons1) && !is.na(tons2) && (tons1 + tons2) > 0) {
      similitud <- 1 - abs(tons1 - tons2) / max(tons1, tons2)
      similitud_total <- similitud_total + (similitud * ponderaciones["ship_tons"])
      suma_ponderaciones <- suma_ponderaciones + ponderaciones["ship_tons"]
    }
  }
  
  # Procesar similitud combinada de nombres de barcos
  if (!is.na(fila1$ship_name) && !is.na(fila2$ship_name)) {
    similitud <- calcular_similitud_combinada(fila1$ship_name, fila2$ship_name)
    similitud_total <- similitud_total + (similitud * ponderaciones["ship_name_combined"])
    suma_ponderaciones <- suma_ponderaciones + ponderaciones["ship_name_combined"]
  }
  
  # Procesar similitud combinada de nombres de capitanes
  if (!is.na(fila1$master_name_p) && !is.na(fila2$master_name_p) &&
      !is.na(fila1$master_name_a) && !is.na(fila2$master_name_a)) {
    similitud <- calcular_similitud_nombres(
      fila1$master_name_p, fila2$master_name_p,
      fila1$master_name_a, fila2$master_name_a
    )
    similitud_total <- similitud_total + (similitud * ponderaciones["master_name_combined"])
    suma_ponderaciones <- suma_ponderaciones + ponderaciones["master_name_combined"]
  }
  
  # Calcular similitud final
  if (suma_ponderaciones > 0) {
    resultado <- (similitud_total / suma_ponderaciones) * 100
    return(ifelse(is.na(resultado), 0, resultado))
  } else {
    return(0)
  }
}

# Función para procesar un rango de filas
procesar_rango <- function(inicio, fin, df_muestra, ponderaciones, umbral) {
  filas_similares <- list()
  for (i in inicio:fin) {
    for (j in (i + 1):nrow(df_muestra)) {
      fila1 <- df_muestra[i, ]
      fila2 <- df_muestra[j, ]
      similitud <- similitud_filas(fila1, fila2, ponderaciones)
      if (similitud > umbral) {
        # Calcular representaciones fonéticas para mostrar en resultados
        phonetic_ship1 <- stringdist::phonetic(fila1$ship_name, method = "soundex")
        phonetic_ship2 <- stringdist::phonetic(fila2$ship_name, method = "soundex")
        
        filas_similares <- append(filas_similares, list(data.frame(
          row1 = i,
          row2 = j,
          similarity = similitud,
          master1 = fila1$master_name,
          master2 = fila2$master_name,
          master_p1 = fila1$master_name_p,
          master_p2 = fila2$master_name_p,
          master_a1 = fila1$master_name_a,
          master_a2 = fila2$master_name_a,
          ship1 = fila1$ship_name,
          ship2 = fila2$ship_name,
          phon1 = phonetic_ship1,
          phon2 = phonetic_ship2,
          type1 = fila1$ship_type,
          type2 = fila2$ship_type,
          flag1 = fila1$ship_flag,
          flag2 = fila2$ship_flag,
          tons1 = fila1$ship_tons,
          tons2 = fila2$ship_tons,
          id1 = fila1$ID,
          id2 = fila2$ID
        )))
      }
    }
  }
  return(filas_similares)
}

# Establecer umbral de similitud
umbral <- 90

# Verificar que el DataFrame existe
if (!exists("df_muestra")) {
  stop("El DataFrame df_muestra no está cargado")
}

# Calcular chunk_size ANTES de crear el cluster
num_cores <- detectCores() - 1  # Usar todos los núcleos menos uno
chunk_size <- ceiling(nrow(df_muestra) / num_cores)

# Configurar el clúster paralelo
cl <- makeCluster(num_cores)

# Cargar las bibliotecas necesarias en cada nodo del clúster
clusterEvalQ(cl, {
  library(stringdist)
  library(dplyr)
})

# Exportar las variables y funciones necesarias al clúster
clusterExport(cl, c("df_muestra", "similitud_filas", "ponderaciones", "umbral", 
                    "procesar_rango", "calcular_similitud_fonetica",
                    "calcular_similitud_combinada", "calcular_similitud_nombres",
                    "chunk_size"))

# Dividir el trabajo en chunks
ranges <- seq(1, nrow(df_muestra), by = chunk_size)

# Ejecutar el procesamiento en paralelo
resultados <- parLapply(cl, ranges, function(start) {
  end <- min(start + chunk_size - 1, nrow(df_muestra))
  procesar_rango(start, end, df_muestra, ponderaciones, umbral)
})

# Detener el clúster y limpiar
stopCluster(cl)
rm(cl)
gc()

# Combinar los resultados
filas_similares <- unlist(resultados, recursive = FALSE)

# Crear un nuevo DataFrame con las filas similares
df_similitudes <- bind_rows(filas_similares) |> 
  mutate(match = FALSE)

df_similitudes <- df_similitudes |> as_tibble() |> select(-match)

# Mostrar el DataFrame resultante
df_similitudes

# Guardar los resultados
write_excel_csv2(df_similitudes, "data/df_similitudes_BAR_barcos_v4.csv")
