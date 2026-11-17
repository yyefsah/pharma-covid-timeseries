###########################################################
# PROJET COVID - SECTEUR PHARMACEUTIQUE (NAF 21.10)
# Production industrielle et chiffre d'affaires
###########################################################

###########################################################
# 1. PACKAGES
###########################################################

library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(tseries)
library(moments)
library(knitr)
library(kableExtra)

###########################################################
# 2. IMPORTATION DES DONNEES
###########################################################

# Vérifier le dossier de travail
getwd()

# Voir les fichiers présents
list.files()

# IMPORTATION PRODUCTION
prod_raw <- read.csv(
  "valeurs_mensuelles.csv",
  sep = ";",
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# IMPORTATION CARACTERISTIQUES
carac_prod <- read.csv(
  "caractéristiques.csv",
  sep = ";",
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# IMPORTATION CHIFFRE D'AFFAIRES
ca_raw <- read.csv(
  "valeurs_mensuellesca.csv",
  sep = ";",
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

###########################################################
# 3. EXTRACTION DES SERIES
###########################################################

# Production brute 21.10
prod_brut <- prod_raw %>%
  filter(idBank == 10767835)

# Production CVS-CJO 21.10
prod_cvs <- prod_raw %>%
  filter(idBank == 10767837)

# Contrefactuel : Préparations pharmaceutiques 21.20
prod_2120 <- prod_raw %>%
  filter(idBank == 10767842)

# Industrie pharmaceutique totale
prod_21 <- prod_raw %>%
  filter(idBank == 10767832)

###########################################################
# 4. FONCTION DE CONVERSION EN FORMAT LONG
###########################################################

convert_series <- function(df){
  
  df %>%
    pivot_longer(
      cols = matches("^\\d{4}-\\d{2}$"),
      names_to = "date",
      values_to = "indice"
    ) %>%
    mutate(
      date = ym(date),
      indice = as.numeric(indice)
    ) %>%
    filter(!is.na(indice))
}

###########################################################
# 5. APPLICATION DE LA FONCTION
###########################################################

prod_brut_long <- convert_series(prod_brut)

prod_cvs_long <- convert_series(prod_cvs)

prod_2120_long <- convert_series(prod_2120)

prod_21_long <- convert_series(prod_21)

###########################################################
# 6. PREPARATION DU CHIFFRE D'AFFAIRES
###########################################################

str(ca_raw)
names(ca_raw)

# Adapter les noms de colonnes si nécessaire

# Export brut INSEE : colonne 1 = période (lignes d'en-tête ignorées), colonne 2 = valeur
ca <- ca_raw %>%
  mutate(
    date = suppressWarnings(ym(Libellé))
  ) %>%
  filter(!is.na(date)) %>%
  transmute(
    date,
    indice = as.numeric(.data[[names(ca_raw)[2]]])
  )

###########################################################
# 7. ANALYSE DESCRIPTIVE
###########################################################

# PRODUCTION BRUTE

ggplot(prod_brut_long,
       aes(date, indice)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Production brute 21.10",
    x = "",
    y = "Indice"
  )

###########################################################
# PRODUCTION CVS-CJO
###########################################################

ggplot(prod_cvs_long,
       aes(date, indice)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Production CVS-CJO 21.10"
  )

###########################################################
# LOGARITHME
###########################################################

prod_cvs_long <- prod_cvs_long %>%
  mutate(
    log_indice = log(indice)
  )

ggplot(prod_cvs_long,
       aes(date, log_indice)) +
  geom_line()

###########################################################
# TAUX DE CROISSANCE
###########################################################

prod_cvs_long <- prod_cvs_long %>%
  arrange(date) %>%
  mutate(
    croissance =
      100 * (indice/lag(indice)-1)
  )

ggplot(prod_cvs_long,
       aes(date, croissance)) +
  geom_line() +
  geom_hline(yintercept = 0)

###########################################################
# MOYENNE MOBILE 12 MOIS
###########################################################

prod_cvs_long <- prod_cvs_long %>%
  mutate(
    mm12 =
      rollmean(
        indice,
        12,
        fill = NA,
        align = "right"
      )
  )

ggplot(prod_cvs_long) +
  geom_line(
    aes(date, indice)
  ) +
  geom_line(
    aes(date, mm12),
    linewidth = 1
  )

###########################################################
# 8. COMPARAISON DES SERIES (CONTREFACTUELS)
###########################################################

comparaison <- bind_rows(
  
  prod_cvs_long %>%
    mutate(serie = "21.10"),
  
  prod_2120_long %>%
    mutate(serie = "21.20"),
  
  prod_21_long %>%
    mutate(serie = "21")
)

ggplot(
  comparaison,
  aes(date, indice, color = serie)
) +
  geom_line() +
  theme_minimal()

###########################################################
# 9. VARIABLE COVID
###########################################################

prod_cvs_long <- prod_cvs_long %>%
  mutate(
    covid =
      ifelse(
        date >= as.Date("2020-03-01"),
        1,
        0
      ),
    
    trend = 1:n()
  )

###########################################################
# 10. STATISTIQUES DESCRIPTIVES
###########################################################

stats <- data.frame(
  
  Moyenne = mean(prod_cvs_long$indice),
  
  Mediane = median(prod_cvs_long$indice),
  
  SD = sd(prod_cvs_long$indice),
  
  Skewness = skewness(prod_cvs_long$indice),
  
  Kurtosis = kurtosis(prod_cvs_long$indice)
)

stats

###########################################################
# 11. SERIE TEMPORELLE
###########################################################

ts_prod <- ts(
  prod_cvs_long$indice,
  start = c(
    year(min(prod_cvs_long$date)),
    month(min(prod_cvs_long$date))
  ),
  frequency = 12
)

###########################################################
# 12. DECOMPOSITION STL
###########################################################

decomp <- stl(
  ts_prod,
  s.window = "periodic"
)

plot(decomp)

###########################################################
# 13. TEST DE STATIONNARITE
###########################################################

adf.test(ts_prod)

###########################################################
# 14. MODELE 1 : REGRESSION COVID
###########################################################

modele_lm <- lm(
  indice ~ trend + covid,
  data = prod_cvs_long
)

summary(modele_lm)

###########################################################
# 15. MODELE 2 : ARIMA
###########################################################

train <- prod_cvs_long %>%
  filter(date < "2020-03-01")

ts_train <- ts(
  train$indice,
  start = c(
    year(min(train$date)),
    month(min(train$date))
  ),
  frequency = 12
)

fit_arima <- auto.arima(ts_train)

summary(fit_arima)

###########################################################
# PREVISION CONTREFACTUELLE
###########################################################

h <- nrow(
  prod_cvs_long %>%
    filter(date >= "2020-03-01")
)

fc <- forecast(
  fit_arima,
  h = h
)

autoplot(fc)

###########################################################
# IMPACT COVID
###########################################################

observed <- prod_cvs_long %>%
  filter(date >= "2020-03-01")

impact <- observed$indice -
  as.numeric(fc$mean)

mean(impact)

###########################################################
# 16. MODELE 3 : ARIMAX
###########################################################

fit_arimax <- auto.arima(
  ts_prod,
  xreg = prod_cvs_long$covid
)

summary(fit_arimax)

###########################################################
# 17. COMPARAISON DES MODELES
###########################################################

rmse_lm <- sqrt(
  mean(
    residuals(modele_lm)^2
  )
)

rmse_arima <- sqrt(
  mean(
    residuals(fit_arima)^2
  )
)

rmse_arimax <- sqrt(
  mean(
    residuals(fit_arimax)^2
  )
)

comparaison_modeles <- data.frame(
  
  Modele = c(
    "Régression",
    "ARIMA",
    "ARIMAX"
  ),
  
  AIC = c(
    AIC(modele_lm),
    AIC(fit_arima),
    AIC(fit_arimax)
  ),
  
  BIC = c(
    BIC(modele_lm),
    BIC(fit_arima),
    BIC(fit_arimax)
  ),
  
  RMSE = c(
    rmse_lm,
    rmse_arima,
    rmse_arimax
  )
)

comparaison_modeles

