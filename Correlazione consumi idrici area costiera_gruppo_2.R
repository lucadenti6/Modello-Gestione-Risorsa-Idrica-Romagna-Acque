library(tidyverse)
library(corrplot)



data <- read.csv2("...percorso file... dataset_punto_2.csv", 
                  header = TRUE, sep = ";")

colnames(data)[3] <- "Provincia"


data <- data %>%
  mutate(
    Mese_Anno = as.Date(paste(Anno, Mese, "01", sep = "-")),
    Giorni_mese = days_in_month(Mese_Anno),
    Consumi_mensili = Consumi_totali * 86400 * Giorni_mese , # da l/s a litri/giorno
      Stagione = case_when(
      Mese %in% 6:8 ~ "Estate",
      Mese %in% c(12, 1, 2) ~ "Inverno",
      TRUE ~ "Mezza_stagione"),
    Consumo_procapite_tot = Consumi_totali / (Residenti + Turisti),
    Impatto_turistico = Turisti / (Residenti + Turisti))


data_Rimini <- data%>%
  filter(Provincia=="Rimini")

#grafico della serie temporale dei consumi idrici
grafico_serie_consumi_idrici <- ggplot(data_Rimini, aes(x = Mese_Anno, y = Consumi_mensili)) +
  geom_line(linewidth = 1, color = "blue") +
  labs(
  title = "Consumi idrici - Rimini",
    x = "Mese",
    y = "Consumi totali (L/giorno)") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grafico_serie_consumi_idrici


#grafico della serie temporale dell'impatto turistico
grafico_impatto_turistico <- ggplot(data_Rimini %>% filter(Provincia == "Rimini"),
       aes(x = Mese_Anno, y = Impatto_turistico * 100)) +
  geom_line(size = 1, color = "tomato") +
  labs(
    title = "Impatto turistico - Rimini",
    x = "Mese", y = "Impatto turistico (%)"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grafico_impatto_turistico



#boxplot dei consumi totali per stagione
boxplot_consumi_stagione <- ggplot(data_Rimini, aes(x = Stagione, y = Consumi_mensili))+
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Consumi totali per stagione",
    x = "Stagione",
    y = "Consumi totali (litri/giorno)"
  ) +
  theme_minimal()

boxplot_consumi_stagione



#boxplot dell'impatto turistico
boxplot_impatto_turistico <- ggplot(data, aes(x = Stagione, y = Impatto_turistico * 100))+
  geom_boxplot(fill = "tomato") +
  labs(
    title = "Impatto turistico per stagione - Rimini",
    x = "Stagione",
    y = "Impatto turistico (%)"
  ) +
  theme_minimal()

boxplot_impatto_turistico






#correlazioni consumi, turisti, temperatura
cor_matrix <- data %>%
  select(Consumi_mensili, Turisti, Temperatura)%>%
  cor()
corrplot(cor_matrix, method="number", type="upper")


cor_matrix <- corrplot(cor_matrix, 
         method = "number", 
         type = "upper",
         diag = FALSE,
         number.digits = 2,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.col = "black")


cor_matrix


#grafico consumo, turisti, temperatura, regressione
grafico_regressione <- ggplot(data_Rimini, aes(x = Turisti, y = Consumi_mensili, color = Temperatura)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +  # Aggiunge la retta di regressione
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "Consumo idrico - Turisti - Temperatura",
    x = "Numero di Turisti",
    y = "Consumo Totale (litri/giorno)",
    color = "Temperatura (°C)"
  ) +
  theme_minimal()

grafico_regressione


data$Stagione <- factor(data$Stagione, levels = c("Estate", "Inverno", "Mezza_stagione"))   #baseline estate




consumo_medio_residenti <- data_Rimini %>%
  filter(Mese %in% c(11, 12, 1, 2, 3)) %>%
  summarise(
    Consumo_medio_residenti = mean(Consumi_mensili, na.rm = TRUE)
  ) %>%
  pull(Consumo_medio_residenti)


data_Rimini <- data_Rimini %>%
  mutate(
    Consumo_turisti = ifelse(
      Provincia == "Rimini" & Mese %in% 4:9,
      Consumi_mensili - consumo_medio_residenti,
      0))


#modello regressione lineare calcolata a seconda della stagionalità
mod1 <- lm(Consumi_mensili ~ Turisti*Stagione + Residenti,
           data = data_Rimini)
summary(mod1)







