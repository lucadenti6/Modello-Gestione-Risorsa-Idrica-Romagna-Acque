set.seed(123)
options(dplyr.summarise.inform = FALSE)
library(lubridate)
library(dplyr)
library(GA)
library(ggplot2)


data_frame <- read.csv("...percorso file... dataset_punto_1.csv", 
                 sep=";", stringsAsFactors = FALSE) %>%
  setNames(c("DATA", "Livello_invaso", "Volume_invaso", "Pioggia", "Apporto", "Prelievi")) %>%
  mutate(
    DATA = dmy(DATA),
    across(c(Volume_invaso, Apporto, Livello_invaso, Pioggia), 
           ~as.numeric(gsub(",", ".", .))),
    Mese = month(DATA),
    Anno = year(DATA)
  ) 



#eseguire i 3 pezzi di codice separatamente


########################################################### confronto modello e serie storiche ###########################################

data <- data_frame

# Vincoli
prelievi_minimi_l_s <- c(499, 569, 517, 486, 769, 974, 1374, 1325, 1015, 659, 532, 525)
data$prelievi_min_giorno <- prelievi_minimi_l_s[data$Mese] * 86.4  # Conversione l/s a m³/giorno

volume_min <- 6.7e6       # Volume minimo (7.5 milioni di m³)
volume_max <- 33e6        # Volume massimo (33 milioni di m³)
prelievo_max <- 190080    # Prelievo massimo giornaliero (m³)
volume_iniziale <- data$Volume_invaso[1] # Volume iniziale (m³)    
n_days <- nrow(data)

#Modellizzazione della domanda, valori medi mensili della serie storica
domanda_base_mensile <- c(
  90000, 90000, 100000, 110000, 120000, 130000, 
  150000, 150000, 130000, 110000, 100000, 95000
)

#Introduce perturbazione giornaliera alla domanda
data$Domanda <- sapply(data$Mese, function(m) {
  max(domanda_base_mensile[m] * (1 + rnorm(1, 0, 0.05)), 1000)  # Minimo 1000 m³/giorno
})

# Funzioni di simulazione 
simulate_reservoir <- function(x, apporto, volume_iniziale, volume_min, volume_max,
                               prelievi_min, prelievo_max, domanda, mesi, C_pen = 1e6) {
  n <- length(x)
  volume <- numeric(n)
  prelievi_effettivi <- numeric(n)
  penalty <- 0
  volume[1] <- volume_iniziale
  
  # Pesi stagionali
  peso_stagionale <- ifelse(mesi %in% c(11,12,1,2), 0.5,
                            ifelse(mesi %in% c(6,7,8), 2, 1))   # in estate i prelievi insufficienti sono più gravi
  
  for (t in 2:n) {
    #Aggiorno il volume con gli apporti (equazione di bilancio idrico 1/2)
    volume_disponibile <- volume[t-1] + apporto[t]
    
    #Controllo tracimazione: se supera il massimo → penalità (equazione di bilancio idrico 2A/2)
    if(volume_disponibile > volume_max) {
      penalty <- penalty + C_pen * (volume_disponibile - volume_max)^2
      volume_disponibile <- volume_max
    }
    
    #Se scende sotto il minimo → penalità, prelievo = 0 
    if(volume_disponibile <= volume_min) {
      prelievi_effettivi[t] <- 0
      penalty <- penalty + C_pen * (volume_min - volume_disponibile)^2
    } else {
      prelievi_effettivi[t] <- min(x[t],                              #quanto vorremmo prelevare
                                   volume_disponibile - volume_min,   #quanto posso permettermi senza svuotare troppo
                                   prelievo_max)                      #quanto è il massimo consentito
    }
    
    #Aggiorna volume (equazione di bilancio idrico 2B/2)
    volume[t] <- volume_disponibile - prelievi_effettivi[t]
  }
  
  # Calcolo fitness
  denominatore <- pmax(domanda, 10000)   #evita casi outlier in cui la domanda è vicina 0
  scarto_relativo <- (prelievi_effettivi - domanda) / denominatore
  errore_normalizzato <- sum((scarto_relativo^4) * peso_stagionale)   #Penalizza gli sbagli grandi soprattutto in estate
  
  #Una penalità di regolarità dei prelievi(con prelievi più stabili)
  regolarita <- 0.005 * sum(diff(prelievi_effettivi)^2)
  
  fit <- errore_normalizzato + penalty + regolarita
  
  return(list(fitness = -fit,   #negativo perchè la funzione ga massimizza
              volume = volume,
              prelievi_effettivi = prelievi_effettivi))
}

reservoir_fitness <- function(x, ...) {
  simulate_reservoir(x, ...)$fitness
}

#Ottimizzazione con Algoritmo Genetico 
#GA ottimizza una fitness che bilancia più aspetti:
#Soddisfazione della domanda
#Regolarità
#Vincoli idraulici
#Stagionalità

GA_result <- ga(
  type = "real-valued",
  fitness = reservoir_fitness,
  apporto = data$Apporto,
  volume_iniziale = volume_iniziale,
  volume_min = volume_min,
  volume_max = volume_max,
  prelievi_min = data$prelievi_min_giorno,
  prelievo_max = prelievo_max,
  domanda = data$Domanda,
  mesi = data$Mese,
  lower = data$prelievi_min_giorno,
  upper = rep(prelievo_max, n_days),
  popSize = 80,
  maxiter = 300,
  run = 100,
  parallel = TRUE,
  seed = 123
)

#Risultati finali
prelievi_ottimali <- GA_result@solution[1, ]

simulazione_finale <- simulate_reservoir(
  x = prelievi_ottimali,
  apporto = data$Apporto,
  volume_iniziale = volume_iniziale,
  volume_min = volume_min,
  volume_max = volume_max,
  prelievi_min = data$prelievi_min_giorno,
  prelievo_max = prelievo_max,
  domanda = data$Domanda,
  mesi = data$Mese
)


#Preparazione output 
df_risultati <- data.frame(
  Data = data$DATA,
  Mese = data$Mese,
  Domanda = data$Domanda,
  Prelievo_Effettivo = simulazione_finale$prelievi_effettivi,
  Volume = simulazione_finale$volume,
  Differenza = simulazione_finale$prelievi_effettivi - data$Domanda,
  prelievi_min = data$prelievi_min_giorno,
  Apporto = data$Apporto,
  Volume_invaso = data$Volume_invaso
)

#elimina la prima riga dei database dove il valore di prelievo è outlier, non è significativo
df_risultati <- df_risultati[2:n_days, ]
data <- data[2:n_days, ]

#controllo dei vincoli sui risultati
sum(df_risultati$Volume > 33000000)#0 
sum(df_risultati$Prelievo_Effettivo> 190080)#0
sum(df_risultati$Prelievo_Effettivo < data$prelievi_min_giorno )#0
sum(df_risultati$Volume < 6700000) #0

sum(df_risultati$Prelievo_Effettivo < data$Domanda)#889
sum(df_risultati$Prelievo_Effettivo > data$Domanda)# 2764




# Grafico 1: Domanda, area grigia e minimi tecnici
grafico1 <- ggplot(df_risultati, aes(x = Data)) +
  geom_ribbon(aes(ymin = prelievi_min, ymax = prelievo_max), fill = "gray90") +
  geom_line(aes(y = Domanda, color = "Domanda"), linewidth = 0.8) +
  geom_line(aes(y = Prelievo_Effettivo, color = "Prelievi ottimali"), linewidth = 0.6) +
  geom_line(aes(y = prelievi_min, color = "Prelievo minimo"), linetype = "dashed") +
  geom_line(aes(y = prelievo_max, color = "Prelievo massimo"), linetype = "dashed") +
  scale_color_manual(values = c("Domanda" = "red", "Prelievi ottimali" = "blue", "Prelievo minimo" = "green", "Prelievo massimo" = "green")) +
  labs(title = "Confronto prelievi ottimali e domanda", y = "m³/giorno", x = "Data", color = "Legenda") +
  theme_minimal()
 

# Grafico 2: Domanda, area grigia e minimi tecnici
grafico2 <- ggplot(df_risultati, aes(x = Data)) +
  geom_ribbon(aes(ymin = prelievi_min, ymax = Domanda), fill = "gray90") +
  geom_line(aes(y = Domanda, color = "Domanda"), linewidth = 0.8) +
  geom_line(aes(y = prelievi_min, color = "Minimi tecnici"), linetype = "dashed") +
  scale_color_manual(values = c("Domanda" = "red", "Minimi tecnici" = "green")) +
  labs(title = "Domanda e Minimi Tecnici", y = "m³/giorno", x = "Data", color = "Legenda") +
  theme_minimal()


# Grafico 3: Prelievi ottimali (blu)
grafico3 <- ggplot(df_risultati, aes(x = Data)) +
  geom_line(aes(y = Prelievo_Effettivo, color = "Prelievi ottimali"), linewidth = 0.6) +
  scale_color_manual(values = c("Prelievi ottimali" = "blue")) +
  labs(title = "Prelievi Ottimali", y = "m³/giorno", x = "Data", color = "Legenda") +
  theme_minimal()


# Grafico 4: Volume invaso ottimale
grafico4 <- ggplot(df_risultati, aes(x = Data)) +
  geom_ribbon(aes(ymin = volume_min, ymax = volume_max), fill = "gray90") +
  geom_line(aes(y = Volume_invaso, color = "Volume effettivo"), linewidth = 0.6) +
  geom_line(aes(y = Volume, color = "Volume ottimale"), linewidth = 0.8) +
  geom_line(aes(y = volume_min, color = "Minimo invaso"), linetype = "dashed") +
  geom_line(aes(y = volume_max, color = "Massimo invaso"), linetype = "dashed") +
  scale_color_manual(values = c("Volume ottimale" = "blue", "Volume effettivo" = "red", "Minimo invaso" = "green", "Massimo invaso" = "green")) +
  labs(title = "Confronto volumi ottimali ed effettivo", y = "m³", x = "Data", color = "Legenda") +
  theme_minimal()


# Combina i due grafici
grid.arrange(grafico1, grafico4, ncol = 1) # Affianca i grafici (ncol=2 per metterli uno a fianco l'altro)





##################################################### confronto modello e media giornaliera 10 anni 2015 - 2024 ############################

# Aggiungiamo il giorno dell'anno (1-366)
data <- data_frame %>%
  mutate(GiornoAnno = yday(DATA))

# Calcoliamo la media per ogni giorno dell'anno
data <- data %>%
  group_by(GiornoAnno) %>%
  summarise(
    Media_Livello = mean(Livello_invaso, na.rm = TRUE),
    Media_Volume = mean(Volume_invaso, na.rm = TRUE),
    Media_Pioggia = mean(Pioggia, na.rm = TRUE),
    Media_Apporto = mean(Apporto, na.rm = TRUE),
    Media_Prelievi = mean(Prelievi, na.rm = TRUE)
  )

data <- data %>%
  mutate(Data = as.Date(GiornoAnno - 1, origin = "2024-01-01")) %>%
  mutate(
    Giorno = day(Data),
    Mese = month(Data),
    Data_formato = format(Data, "%d/%m")
  )

data <- data %>%
  mutate(GiornoAnno = Data) %>%  # Sostituisce la prima colonna con la settima
  select(1:5)  # Mantiene solo le prime 6 colonne

data <- data %>%
  setNames(c("DATA", "Livello_invaso", "Volume_invaso", "Pioggia", "Apporto")) %>%
  mutate(
    across(c(Volume_invaso, Apporto, Livello_invaso, Pioggia), 
           ~as.numeric(gsub(",", ".", .))),
    Mese = month(DATA),
    Anno = year(DATA)
  ) 

# Vincoli
prelievi_minimi_l_s <- c(499, 569, 517, 486, 769, 974, 1374, 1325, 1015, 659, 532, 525)
data$prelievi_min_giorno <- prelievi_minimi_l_s[data$Mese] * 86.4  # Conversione l/s a m³/giorno

volume_min <- 6.7e6       # Volume minimo (7.5 milioni di m³)
volume_max <- 33e6        # Volume massimo (33 milioni di m³)
prelievo_max <- 190080    # Prelievo massimo giornaliero (m³)
volume_iniziale <- data$Volume_invaso[1] # Volume iniziale (m³)    
n_days <- nrow(data)

#Modellizzazione della domanda, valori medi mensili della serie storica
domanda_base_mensile <- c(
  90000, 90000, 100000, 110000, 120000, 130000, 
  150000, 150000, 130000, 110000, 100000, 95000
)

#Introduce perturbazione giornaliera alla domanda
data$Domanda <- sapply(data$Mese, function(m) {
  max(domanda_base_mensile[m] * (1 + rnorm(1, 0, 0.05)), 1000)  # Minimo 1000 m³/giorno
})

# Funzioni di simulazione 
simulate_reservoir <- function(x, apporto, volume_iniziale, volume_min, volume_max,
                               prelievi_min, prelievo_max, domanda, mesi, C_pen = 1e6) {
  n <- length(x)
  volume <- numeric(n)
  prelievi_effettivi <- numeric(n)
  penalty <- 0
  volume[1] <- volume_iniziale
  
  # Pesi stagionali
  peso_stagionale <- ifelse(mesi %in% c(11,12,1,2), 0.5,
                            ifelse(mesi %in% c(6,7,8), 2, 1))   # in estate i prelievi insufficienti sono più gravi
  
  for (t in 2:n) {
    #Aggiorno il volume con gli apporti (equazione di bilancio idrico 1/2)
    volume_disponibile <- volume[t-1] + apporto[t]
    
    #Controllo tracimazione: se supera il massimo → penalità (equazione di bilancio idrico 2A/2)
    if(volume_disponibile > volume_max) {
      penalty <- penalty + C_pen * (volume_disponibile - volume_max)^2
      volume_disponibile <- volume_max
    }
    
    #Se scende sotto il minimo → penalità, prelievo = 0 
    if(volume_disponibile <= volume_min) {
      prelievi_effettivi[t] <- 0
      penalty <- penalty + C_pen * (volume_min - volume_disponibile)^2
    } else {
      prelievi_effettivi[t] <- min(x[t],                              #quanto vorremmo prelevare
                                   volume_disponibile - volume_min,   #quanto posso permettermi senza svuotare troppo
                                   prelievo_max)                      #quanto è il massimo consentito
    }
    
    #Aggiorna volume (equazione di bilancio idrico 2B/2)
    volume[t] <- volume_disponibile - prelievi_effettivi[t]
  }
  
  # Calcolo fitness
  denominatore <- pmax(domanda, 10000)   #evita casi outlier in cui la domanda è vicina 0
  scarto_relativo <- (prelievi_effettivi - domanda) / denominatore
  errore_normalizzato <- sum((scarto_relativo^4) * peso_stagionale)   #Penalizza gli sbagli grandi soprattutto in estate
  
  #Una penalità di regolarità dei prelievi(con prelievi più stabili)
  regolarita <- 0.005 * sum(diff(prelievi_effettivi)^2)
  
  fit <- errore_normalizzato + penalty + regolarita
  
  return(list(fitness = -fit,   #negativo perchè la funzione ga massimizza
              volume = volume,
              prelievi_effettivi = prelievi_effettivi))
}

reservoir_fitness <- function(x, ...) {
  simulate_reservoir(x, ...)$fitness
}

#Ottimizzazione con Algoritmo Genetico 
#GA ottimizza una fitness che bilancia più aspetti:
#Soddisfazione della domanda
#Regolarità
#Vincoli idraulici
#Stagionalità

GA_result <- ga(
  type = "real-valued",
  fitness = reservoir_fitness,
  apporto = data$Apporto,
  volume_iniziale = volume_iniziale,
  volume_min = volume_min,
  volume_max = volume_max,
  prelievi_min = data$prelievi_min_giorno,
  prelievo_max = prelievo_max,
  domanda = data$Domanda,
  mesi = data$Mese,
  lower = data$prelievi_min_giorno,
  upper = rep(prelievo_max, n_days),
  popSize = 80,
  maxiter = 300,
  run = 100,
  parallel = TRUE,
  seed = 123
)

#Risultati finali
prelievi_ottimali <- GA_result@solution[1, ]

simulazione_finale <- simulate_reservoir(
  x = prelievi_ottimali,
  apporto = data$Apporto,
  volume_iniziale = volume_iniziale,
  volume_min = volume_min,
  volume_max = volume_max,
  prelievi_min = data$prelievi_min_giorno,
  prelievo_max = prelievo_max,
  domanda = data$Domanda,
  mesi = data$Mese
)


#Preparazione output 
df_risultati <- data.frame(
  Data = data$DATA,
  Mese = data$Mese,
  Domanda = data$Domanda,
  Prelievo_Effettivo = simulazione_finale$prelievi_effettivi,
  Volume = simulazione_finale$volume,
  Differenza = simulazione_finale$prelievi_effettivi - data$Domanda,
  prelievi_min = data$prelievi_min_giorno,
  Apporto = data$Apporto,
  Volume_invaso = data$Volume_invaso
)

#elimina la prima riga dei database dove il valore di prelievo è outlier, non è significativo
df_risultati <- df_risultati[2:n_days, ]
data <- data[2:n_days, ]

#controllo dei vincoli sui risultati
sum(df_risultati$Volume > 33000000)#0 
sum(df_risultati$Prelievo_Effettivo> 190080)#0
sum(df_risultati$Prelievo_Effettivo < data$prelievi_min_giorno )#0
sum(df_risultati$Volume < 6700000) #0

sum(df_risultati$Prelievo_Effettivo < data$Domanda)#889
sum(df_risultati$Prelievo_Effettivo > data$Domanda)# 2764




# Grafico 1: Domanda, area grigia e minimi tecnici
grafico1 <- ggplot(df_risultati, aes(x = Data)) +
  geom_ribbon(aes(ymin = prelievi_min, ymax = prelievo_max), fill = "gray90") +
  geom_line(aes(y = Domanda, color = "Domanda"), linewidth = 0.8) +
  geom_line(aes(y = Prelievo_Effettivo, color = "Prelievi ottimali"), linewidth = 0.6) +
  geom_line(aes(y = prelievi_min, color = "Prelievo minimo"), linetype = "dashed") +
  geom_line(aes(y = prelievo_max, color = "Prelievo massimo"), linetype = "dashed") +
  scale_color_manual(values = c("Domanda" = "red", "Prelievi ottimali" = "blue", "Prelievo minimo" = "green", "Prelievo massimo" = "green")) +
  labs(title = "Confronto prelievi ottimali e domanda media giornaliera 10 anni", y = "m³/giorno", x = "Data", color = "Legenda") +
  theme_minimal()


# Grafico 2: Domanda, area grigia e minimi tecnici
grafico2 <- ggplot(df_risultati, aes(x = Data)) +
  geom_ribbon(aes(ymin = prelievi_min, ymax = Domanda), fill = "gray90") +
  geom_line(aes(y = Domanda, color = "Domanda"), linewidth = 0.8) +
  geom_line(aes(y = prelievi_min, color = "Minimi tecnici"), linetype = "dashed") +
  scale_color_manual(values = c("Domanda" = "red", "Minimi tecnici" = "green")) +
  labs(title = "Domanda e Minimi Tecnici", y = "m³/giorno", x = "Data", color = "Legenda") +
  theme_minimal()


# Grafico 3: Prelievi ottimali (blu)
grafico3 <- ggplot(df_risultati, aes(x = Data)) +
  geom_line(aes(y = Prelievo_Effettivo, color = "Prelievi ottimali"), linewidth = 0.6) +
  scale_color_manual(values = c("Prelievi ottimali" = "blue")) +
  labs(title = "Prelievi Ottimali", y = "m³/giorno", x = "Data", color = "Legenda") +
  theme_minimal()


# Grafico 4: Volume invaso ottimale
grafico4 <- ggplot(df_risultati, aes(x = Data)) +
  geom_ribbon(aes(ymin = volume_min, ymax = volume_max), fill = "gray90") +
  geom_line(aes(y = Volume_invaso, color = "Volume effettivo"), linewidth = 0.6) +
  geom_line(aes(y = Volume, color = "Volume ottimale"), linewidth = 0.8) +
  geom_line(aes(y = volume_min, color = "Minimo invaso"), linetype = "dashed") +
  geom_line(aes(y = volume_max, color = "Massimo invaso"), linetype = "dashed") +
  scale_color_manual(values = c("Volume ottimale" = "blue", "Volume effettivo" = "red", "Minimo invaso" = "green", "Massimo invaso" = "green")) +
  labs(title = "Confronto volumi ottimali e volumi medi giornalieri 10 anni", y = "m³", x = "Data", color = "Legenda") +
  theme_minimal()


# Combina i due grafici
grid.arrange(grafico1, grafico4, ncol = 1) # Affianca i grafici (ncol=2 per metterli uno a fianco l'altro)






####################################################### scenario RCP 4.5 e aumento delle temperature ##################################

data <- data_frame %>%
  mutate(
    Apporto = 0.88*Apporto   #stimiamo una diminuzione dell'apporto di acqua del 12%
    
  ) 



# Vincoli, stimiamo un aumento dei fabbisogni minimi di acqua del 15%
prelievi_minimi_l_s <- 1.15*c(499, 569, 517, 486, 769, 974, 1374, 1325, 1015, 659, 532, 525)
data$prelievi_min_giorno <- prelievi_minimi_l_s[data$Mese] * 86.4  # Conversione l/s a m³/giorno

volume_min <- 6.7e6       # Volume minimo (7.5 milioni di m³)
volume_max <- 33e6        # Volume massimo (33 milioni di m³)
prelievo_max <- 190080    # Prelievo massimo giornaliero (m³)
volume_iniziale <- data$Volume_invaso[1] # Volume iniziale (m³)    
n_days <- nrow(data)

#Modellizzazione della domanda, stimiamo un aumento della domanda di acqua del 15%
domanda_base_mensile <- 1.15*c(
  90000, 90000, 100000, 110000, 120000, 130000, 
  150000, 150000, 130000, 110000, 100000, 95000
)

#Introduce perturbazione giornaliera alla domanda
data$Domanda <- sapply(data$Mese, function(m) {
  max(domanda_base_mensile[m] * (1 + rnorm(1, 0, 0.05)), 1000)  # Minimo 1000 m³/giorno
})

# Funzioni di simulazione 
simulate_reservoir <- function(x, apporto, volume_iniziale, volume_min, volume_max,
                               prelievi_min, prelievo_max, domanda, mesi, C_pen = 1e6) {
  n <- length(x)
  volume <- numeric(n)
  prelievi_effettivi <- numeric(n)
  penalty <- 0
  volume[1] <- volume_iniziale
  
  # Pesi stagionali
  peso_stagionale <- ifelse(mesi %in% c(11,12,1,2), 0.5,
                            ifelse(mesi %in% c(6,7,8), 2, 1))   # in estate i prelievi insufficienti sono più gravi
  
  for (t in 2:n) {
    #Aggiorno il volume con gli apporti (equazione di bilancio idrico 1/2)
    volume_disponibile <- volume[t-1] + apporto[t]
    
    #Controllo tracimazione: se supera il massimo → penalità (equazione di bilancio idrico 2A/2)
    if(volume_disponibile > volume_max) {
      penalty <- penalty + C_pen * (volume_disponibile - volume_max)^2
      volume_disponibile <- volume_max
    }
    
    #Se scende sotto il minimo → penalità, prelievo = 0 
    if(volume_disponibile <= volume_min) {
      prelievi_effettivi[t] <- 0
      penalty <- penalty + C_pen * (volume_min - volume_disponibile)^2
    } else {
      prelievi_effettivi[t] <- min(x[t],                              #quanto vorremmo prelevare
                                   volume_disponibile - volume_min,   #quanto posso permettermi senza svuotare troppo
                                   prelievo_max)                      #quanto è il massimo consentito
    }
    
    #Aggiorna volume (equazione di bilancio idrico 2B/2)
    volume[t] <- volume_disponibile - prelievi_effettivi[t]
  }
  
  # Calcolo fitness
  denominatore <- pmax(domanda, 10000)   #evita casi outlier in cui la domanda è vicina 0
  scarto_relativo <- (prelievi_effettivi - domanda) / denominatore
  errore_normalizzato <- sum((scarto_relativo^4) * peso_stagionale)   #Penalizza gli sbagli grandi soprattutto in estate
  
  #Una penalità di regolarità dei prelievi(con prelievi più stabili)
  regolarita <- 0.005 * sum(diff(prelievi_effettivi)^2)
  
  fit <- errore_normalizzato + penalty + regolarita
  
  return(list(fitness = -fit,   #negativo perchè la funzione ga massimizza
              volume = volume,
              prelievi_effettivi = prelievi_effettivi))
}

reservoir_fitness <- function(x, ...) {
  simulate_reservoir(x, ...)$fitness
}

#Ottimizzazione con Algoritmo Genetico 
#GA ottimizza una fitness che bilancia più aspetti:
#Soddisfazione della domanda
#Regolarità
#Vincoli idraulici
#Stagionalità

GA_result <- ga(
  type = "real-valued",
  fitness = reservoir_fitness,
  apporto = data$Apporto,
  volume_iniziale = volume_iniziale,
  volume_min = volume_min,
  volume_max = volume_max,
  prelievi_min = data$prelievi_min_giorno,
  prelievo_max = prelievo_max,
  domanda = data$Domanda,
  mesi = data$Mese,
  lower = data$prelievi_min_giorno,
  upper = rep(prelievo_max, n_days),
  popSize = 80,
  maxiter = 300,
  run = 100,
  parallel = TRUE,
  seed = 123
)

#Risultati finali
prelievi_ottimali <- GA_result@solution[1, ]

simulazione_finale <- simulate_reservoir(
  x = prelievi_ottimali,
  apporto = data$Apporto,
  volume_iniziale = volume_iniziale,
  volume_min = volume_min,
  volume_max = volume_max,
  prelievi_min = data$prelievi_min_giorno,
  prelievo_max = prelievo_max,
  domanda = data$Domanda,
  mesi = data$Mese
)


#Preparazione output 
df_risultati <- data.frame(
  Data = data$DATA,
  Mese = data$Mese,
  Domanda = data$Domanda,
  Prelievo_Effettivo = simulazione_finale$prelievi_effettivi,
  Volume = simulazione_finale$volume,
  Differenza = simulazione_finale$prelievi_effettivi - data$Domanda,
  prelievi_min = data$prelievi_min_giorno,
  Apporto = data$Apporto,
  Volume_invaso_storico = data_frame$Volume_invaso
)

#elimina la prima riga dei database dove il valore di prelievo è outlier, non è significativo
df_risultati <- df_risultati[2:n_days, ]
data <- data[2:n_days, ]

#controllo dei vincoli sui risultati
sum(df_risultati$Volume > 33000000)#0 
sum(df_risultati$Prelievo_Effettivo> 190080)#0
sum(df_risultati$Prelievo_Effettivo < data$prelievi_min_giorno )#0
sum(df_risultati$Volume < 6700000) #0

sum(df_risultati$Prelievo_Effettivo < data$Domanda)#889
sum(df_risultati$Prelievo_Effettivo > data$Domanda)# 2764




# Grafico 1: Domanda, area grigia e minimi tecnici
grafico1 <- ggplot(df_risultati, aes(x = Data)) +
  geom_ribbon(aes(ymin = prelievi_min, ymax = prelievo_max), fill = "gray90") +
  geom_line(aes(y = Domanda, color = "Domanda"), linewidth = 0.8) +
  geom_line(aes(y = Prelievo_Effettivo, color = "Prelievi ottimali"), linewidth = 0.6) +
  geom_line(aes(y = prelievi_min, color = "Prelievo minimo"), linetype = "dashed") +
  geom_line(aes(y = prelievo_max, color = "Prelievo massimo"), linetype = "dashed") +
  scale_color_manual(values = c("Domanda" = "red", "Prelievi ottimali" = "blue", "Prelievo minimo" = "green", "Prelievo massimo" = "green")) +
  labs(title = "Confronto prelievi ottimali e domanda - scenario RCP 4.5", y = "m³/giorno", x = "Data", color = "Legenda") +
  theme_minimal()


# Grafico 2: Domanda, area grigia e minimi tecnici
grafico2 <- ggplot(df_risultati, aes(x = Data)) +
  geom_ribbon(aes(ymin = prelievi_min, ymax = Domanda), fill = "gray90") +
  geom_line(aes(y = Domanda, color = "Domanda"), linewidth = 0.8) +
  geom_line(aes(y = prelievi_min, color = "Minimi tecnici"), linetype = "dashed") +
  scale_color_manual(values = c("Domanda" = "red", "Minimi tecnici" = "green")) +
  labs(title = "Domanda e Minimi Tecnici", y = "m³/giorno", x = "Data", color = "Legenda") +
  theme_minimal()


# Grafico 3: Prelievi ottimali (blu)
grafico3 <- ggplot(df_risultati, aes(x = Data)) +
  geom_line(aes(y = Prelievo_Effettivo, color = "Prelievi ottimali"), linewidth = 0.6) +
  scale_color_manual(values = c("Prelievi ottimali" = "blue")) +
  labs(title = "Prelievi Ottimali", y = "m³/giorno", x = "Data", color = "Legenda") +
  theme_minimal()


# Grafico 4: Volume invaso ottimale
grafico4 <- ggplot(df_risultati, aes(x = Data)) +
  geom_ribbon(aes(ymin = volume_min, ymax = volume_max), fill = "gray90") +
  geom_line(aes(y = Volume_invaso_storico, color = "Volume 2015 - 2024"), linewidth = 0.6) +
  geom_line(aes(y = Volume, color = "Volume ottimale"), linewidth = 0.8) +
  geom_line(aes(y = volume_min, color = "Minimo invaso"), linetype = "dashed") +
  geom_line(aes(y = volume_max, color = "Massimo invaso"), linetype = "dashed") +
  scale_color_manual(values = c("Volume ottimale" = "blue", "Volume 2015 - 2024" = "red", "Minimo invaso" = "green", "Massimo invaso" = "green")) +
  labs(title = "Confronto volumi ottimali e volumi storici 2015 - 2024 - scenario RCP 4.5", y = "m³", x = "Data", color = "Legenda") +
  theme_minimal()


# Combina i due grafici
grid.arrange(grafico1, grafico4, ncol = 1) # Affianca i grafici (ncol=2 per metterli uno a fianco l'altro)


