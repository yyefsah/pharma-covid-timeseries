############################################################
# PARTIE CHIFFRE D'AFFAIRES
############################################################

############################################################
# 1. LOGARITHME ET TAUX DE CROISSANCE
############################################################

ca <- ca %>%
  arrange(date) %>%
  mutate(
    log_indice = log(indice),
    croissance = 100 * (indice / lag(indice) - 1)
  )

############################################################
# 2. LES 3 GRAPHIQUES
############################################################

# Série brute

ggplot(
  ca,
  aes(date, indice)
) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Chiffre d'affaires pharmaceutique",
    x = "",
    y = "Indice"
  )

# Série logarithmique

ggplot(
  ca,
  aes(date, log_indice)
) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Log du chiffre d'affaires"
  )

# Taux de croissance

ggplot(
  ca,
  aes(date, croissance)
) +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    title = "Croissance mensuelle du CA (%)"
  )

############################################################
# 3. MOYENNE MOBILE
############################################################

ca <- ca %>%
  mutate(
    mm12 = zoo::rollmean(
      indice,
      k = 12,
      fill = NA,
      align = "right"
    )
  )

ggplot(ca) +
  geom_line(
    aes(date, indice)
  ) +
  geom_line(
    aes(date, mm12),
    linewidth = 1
  ) +
  theme_minimal() +
  labs(
    title = "CA et moyenne mobile 12 mois"
  )

############################################################
# 4. VARIABLE COVID
############################################################

ca <- ca %>%
  mutate(
    covid = ifelse(
      date >= as.Date("2020-03-01"),
      1,
      0
    ),
    trend = 1:n()
  )

############################################################
# 5. STATISTIQUES DESCRIPTIVES
############################################################

stats_ca <- data.frame(
  Moyenne = mean(ca$indice),
  Mediane = median(ca$indice),
  Ecart_Type = sd(ca$indice),
  Minimum = min(ca$indice),
  Maximum = max(ca$indice),
  Skewness = moments::skewness(ca$indice),
  Kurtosis = moments::kurtosis(ca$indice)
)

stats_ca

############################################################
# 6. SERIE TEMPORELLE
############################################################

ts_ca <- ts(
  ca$indice,
  frequency = 12,
  start = c(
    year(min(ca$date)),
    month(min(ca$date))
  )
)

############################################################
# 7. DECOMPOSITION STL
############################################################

stl_ca <- stl(
  ts_ca,
  s.window = "periodic"
)

plot(stl_ca)

############################################################
# 8. TEST ADF
############################################################

adf.test(ts_ca)

############################################################
# 9. MODELE 1 : REGRESSION COVID
############################################################

modele_lm_ca <- lm(
  indice ~ trend + covid,
  data = ca
)

summary(modele_lm_ca)

############################################################
# 10. MODELE 2 : ARIMA
############################################################

train_ca <- ca %>%
  filter(
    date < as.Date("2020-03-01")
  )

ts_train_ca <- ts(
  train_ca$indice,
  frequency = 12,
  start = c(
    year(min(train_ca$date)),
    month(min(train_ca$date))
  )
)

fit_arima_ca <- auto.arima(
  ts_train_ca
)

summary(fit_arima_ca)

############################################################
# 11. PREVISION CONTREFACTUELLE
############################################################

h_ca <- nrow(
  ca %>%
    filter(
      date >= as.Date("2020-03-01")
    )
)

fc_ca <- forecast(
  fit_arima_ca,
  h = h_ca
)

autoplot(fc_ca)

############################################################
# 12. IMPACT DU COVID SUR LE CA
############################################################

observed_ca <- ca %>%
  filter(
    date >= as.Date("2020-03-01")
  )

impact_ca <- observed_ca$indice -
  as.numeric(fc_ca$mean)

summary(impact_ca)

mean(impact_ca)

############################################################
# 13. MODELE 3 : ARIMAX
############################################################

fit_arimax_ca <- auto.arima(
  ts_ca,
  xreg = ca$covid
)

summary(fit_arimax_ca)

############################################################
# 14. COMPARAISON DES MODELES
############################################################

rmse_lm_ca <- sqrt(
  mean(residuals(modele_lm_ca)^2)
)

rmse_arima_ca <- sqrt(
  mean(residuals(fit_arima_ca)^2)
)

rmse_arimax_ca <- sqrt(
  mean(residuals(fit_arimax_ca)^2)
)

comparaison_modeles_ca <- data.frame(
  
  Modele = c(
    "Régression Covid",
    "ARIMA",
    "ARIMAX"
  ),
  
  AIC = c(
    AIC(modele_lm_ca),
    AIC(fit_arima_ca),
    AIC(fit_arimax_ca)
  ),
  
  BIC = c(
    BIC(modele_lm_ca),
    BIC(fit_arima_ca),
    BIC(fit_arimax_ca)
  ),
  
  RMSE = c(
    rmse_lm_ca,
    rmse_arima_ca,
    rmse_arimax_ca
  )
)

comparaison_modeles_ca



############################################################
# 15. COMPARAISON PRODUCTION VS CA EN BASE 100
############################################################

prod_plot <- prod_cvs_long %>%
  filter(date >= min(ca$date)) %>%
  mutate(
    indice100 = indice / first(indice) * 100
  )

ca_plot <- ca %>%
  mutate(
    indice100 = indice / first(indice) * 100
  )

comparaison_base100 <- bind_rows(
  
  prod_plot %>%
    select(date, indice100) %>%
    mutate(serie = "Production"),
  
  ca_plot %>%
    select(date, indice100) %>%
    mutate(serie = "Chiffre d'affaires")
  
)

ggplot(
  comparaison_base100,
  aes(date, indice100, color = serie)
) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Evolution comparée Production vs CA (Base 100)",
    x = "",
    y = "Base 100",
    color = "Série"
  )