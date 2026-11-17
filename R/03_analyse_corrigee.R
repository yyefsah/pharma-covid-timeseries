###########################################################
# ANALYSE COMPLEMENTAIRE : tester la robustesse des résultats du rapport
# - date(s) de rupture ESTIMÉE(S) (Bai-Perron) au lieu d'une date imposée
# - écart contrefactuel par sous-période, avec intervalle de prévision
# - tests "placebo" : même méthode appliquée à de fausses dates de crise
# - comparaison de modèles HORS ÉCHANTILLON (même période, même métrique)
# - régression avec erreurs-types robustes (Newey-West)
# Autonome : lit les 3 CSV INSEE placés dans le dossier de travail.
###########################################################
suppressPackageStartupMessages({ library(tidyverse); library(lubridate); library(forecast); library(strucchange); library(sandwich); library(lmtest) })
set.seed(42)
dir.create("outputs", showWarnings = FALSE); dir.create("figures", showWarnings = FALSE)
COVID <- as.Date("2020-03-01")

# ---------------------------------------------------------- 1. Données (mêmes séries que 01_production.R / 02_chiffre_affaires.R)
prod_raw <- read.csv("valeurs_mensuelles.csv", sep = ";", check.names = FALSE, fileEncoding = "UTF-8")
ca_raw   <- read.csv("valeurs_mensuellesca.csv", sep = ";", check.names = FALSE, fileEncoding = "UTF-8")
prod <- prod_raw %>% filter(idBank == 10767837) %>%                      # production 21.10 CVS-CJO
  pivot_longer(matches("^\\d{4}-\\d{2}$"), names_to = "date", values_to = "indice") %>%
  transmute(date = ym(date), indice = as.numeric(indice)) %>% filter(!is.na(indice)) %>% arrange(date)
ca <- ca_raw %>% mutate(date = suppressWarnings(ym(Libellé))) %>% filter(!is.na(date)) %>%
  transmute(date, indice = as.numeric(.data[[names(ca_raw)[2]]])) %>% arrange(date)
series <- list(`Production 21.10 (CVS-CJO)` = prod, `Chiffre d'affaires 21 (CVS-CJO)` = ca)
to_ts <- function(d) ts(d$indice, start = c(year(min(d$date)), month(min(d$date))), frequency = 12)

# ---------------------------------------------------------- 2. Dates de rupture estimées (Bai-Perron sur log, tendance par segment)
brk <- lapply(names(series), function(nm) {
  d <- series[[nm]] %>% mutate(ly = log(indice), t = row_number())
  bp <- breakpoints(ly ~ t, data = d, h = 0.12, breaks = 5)
  k <- which.min(summary(bp)$RSS["BIC", ]) - 1
  if (k == 0) return(data.frame(serie = nm, n_ruptures = 0, date = NA, ic_bas = NA, ic_haut = NA))
  bpk <- breakpoints(bp, breaks = k); ci <- confint(bp, breaks = k)$confint
  data.frame(serie = nm, n_ruptures = k, date = d$date[bpk$breakpoints], ic_bas = d$date[pmax(ci[, 1], 1)], ic_haut = d$date[pmin(ci[, 3], nrow(d))])
})
brk <- bind_rows(brk); print(brk); write.csv(brk, "outputs/ruptures_estimees.csv", row.names = FALSE)

# ---------------------------------------------------------- 3. Contrefactuel ARIMA : écart par sous-période + couverture de l'intervalle de prévision
cf <- function(d, origin = COVID, level = 95) {
  tr <- d %>% filter(date < origin); post <- d %>% filter(date >= origin)
  fit <- auto.arima(to_ts(tr)); fc <- forecast(fit, h = nrow(post), level = level)
  post %>% mutate(prevu = as.numeric(fc$mean), bas = as.numeric(fc$lower[, 1]), haut = as.numeric(fc$upper[, 1]),
                  ecart = indice - prevu, ecart_pct = 100 * (indice / prevu - 1), hors_ic = indice < bas | indice > haut)
}
cfs <- lapply(series, cf); names(cfs) <- names(series)
per <- bind_rows(lapply(names(cfs), function(nm) cfs[[nm]] %>%
  mutate(periode = case_when(date < as.Date("2022-01-01") ~ "mars 2020 - 2021", date < as.Date("2024-01-01") ~ "2022 - 2023", TRUE ~ "2024 et après")) %>%
  group_by(periode) %>% summarise(n_mois = n(), ecart_moyen = mean(ecart), ecart_moyen_pct = mean(ecart_pct), part_hors_ic95 = mean(hors_ic), .groups = "drop") %>% mutate(serie = nm)))
glob <- bind_rows(lapply(names(cfs), function(nm) data.frame(serie = nm, periode = "ensemble", n_mois = nrow(cfs[[nm]]), ecart_moyen = mean(cfs[[nm]]$ecart),
                 ecart_moyen_pct = mean(cfs[[nm]]$ecart_pct), part_hors_ic95 = mean(cfs[[nm]]$hors_ic))))
cf_tab <- bind_rows(glob, per) %>% select(serie, everything()); print(as.data.frame(cf_tab), digits = 3); write.csv(cf_tab, "outputs/ecart_contrefactuel_par_periode.csv", row.names = FALSE)

# ---------------------------------------------------------- 4. Placebos : même méthode, fausses dates de crise (mars de chaque année avec fenêtre complète avant le Covid)
placebo <- function(d, nm, years) {
  h <- nrow(d %>% filter(date >= COVID))
  bind_rows(lapply(years, function(y) {
    o <- as.Date(sprintf("%d-03-01", y)); tr <- d %>% filter(date < o); post <- d %>% filter(date >= o) %>% slice_head(n = h)
    fit <- auto.arima(to_ts(tr)); fc <- forecast(fit, h = nrow(post))
    data.frame(serie = nm, annee = y, ecart_moyen = mean(post$indice - as.numeric(fc$mean)), ecart_moyen_pct = mean(100 * (post$indice / as.numeric(fc$mean) - 1)))
  }))
}
pl <- bind_rows(placebo(prod, names(series)[1], 2002:2013), placebo(ca, names(series)[2], 2003:2013))
real <- cf_tab %>% filter(periode == "ensemble") %>% select(serie, reel_pct = ecart_moyen_pct, reel = ecart_moyen)
pl_sum <- pl %>% group_by(serie) %>% summarise(n_placebos = n(), placebo_moy_pct = mean(ecart_moyen_pct), placebo_sd_pct = sd(ecart_moyen_pct),
                                               placebo_min_pct = min(ecart_moyen_pct), placebo_max_pct = max(ecart_moyen_pct), .groups = "drop") %>% left_join(real, by = "serie") %>%
  left_join(pl %>% left_join(real, by = "serie") %>% group_by(serie) %>% summarise(rang_reel = sum(ecart_moyen_pct >= first(reel_pct)) + 1, .groups = "drop"), by = "serie")
print(as.data.frame(pl_sum), digits = 3); write.csv(pl, "outputs/placebos.csv", row.names = FALSE); write.csv(pl_sum, "outputs/placebos_resume.csv", row.names = FALSE)

# ---------------------------------------------------------- 5. Comparaison de modèles HORS ÉCHANTILLON (avant 2020, horizon 1-12 mois, origine glissante)
oos <- function(d, nm) {
  d <- d %>% filter(date < COVID); y <- to_ts(d); n <- length(y)
  origins <- seq(from = which(d$date == as.Date("2008-01-01")), to = n - 12, by = 3)
  sc <- mean(abs(diff(y)))       # échelle MASE : erreur moyenne de la prévision naïve en échantillon
  res <- lapply(origins, function(o) {
    tr <- window(y, end = time(y)[o]); te <- y[(o + 1):(o + 12)]
    f <- list(`ARIMA (auto)` = forecast(auto.arima(tr), h = 12)$mean, `ETS (auto)` = forecast(ets(tr), h = 12)$mean,
              `Tendance linéaire` = forecast(tslm(tr ~ trend), h = 12)$mean, `Naïf avec dérive` = rwf(tr, h = 12, drift = TRUE)$mean)
    bind_rows(lapply(names(f), function(k) data.frame(modele = k, origine = o, e = te - as.numeric(f[[k]]), h = 1:12)))
  })
  bind_rows(res) %>% group_by(modele) %>% summarise(RMSE = sqrt(mean(e^2)), MASE = mean(abs(e)) / sc, n_prevision = n(), .groups = "drop") %>% mutate(serie = nm) %>% arrange(RMSE)
}
oos_tab <- bind_rows(oos(prod, names(series)[1]), oos(ca, names(series)[2])); print(as.data.frame(oos_tab), digits = 3)
write.csv(oos_tab, "outputs/comparaison_hors_echantillon.csv", row.names = FALSE)

# ---------------------------------------------------------- 6. Régression : erreurs-types robustes (Newey-West, 12 retards) ; coefficient Covid de l'ARIMAX avec son intervalle
reg <- bind_rows(lapply(names(series), function(nm) {
  d <- series[[nm]] %>% mutate(trend = row_number(), covid = as.numeric(date >= COVID)); m <- lm(indice ~ trend + covid, data = d)
  V <- NeweyWest(m, lag = 12, prewhite = FALSE); se <- sqrt(diag(V)); b <- coef(m)
  ax <- auto.arima(to_ts(d), xreg = d$covid); bx <- coef(ax)["xreg"]; sx <- sqrt(diag(vcov(ax)))["xreg"]
  data.frame(serie = nm, coef_covid_ols = b["covid"], se_ols = summary(m)$coefficients["covid", 2], se_newey_west = se["covid"],
             ic95_bas_nw = b["covid"] - 1.96 * se["covid"], ic95_haut_nw = b["covid"] + 1.96 * se["covid"], coef_covid_arimax = bx, se_arimax = sx,
             ic95_bas_arimax = bx - 1.96 * sx, ic95_haut_arimax = bx + 1.96 * sx)
}))
print(as.data.frame(reg), digits = 3); write.csv(reg, "outputs/regression_robuste.csv", row.names = FALSE)

# ---------------------------------------------------------- 7. Figures
theme_set(theme_minimal(base_size = 11))
bk <- brk %>% filter(!is.na(date)) %>% mutate(serie = factor(serie, levels = names(series)))
dd <- bind_rows(lapply(names(series), function(nm) series[[nm]] %>% mutate(serie = nm))) %>% mutate(serie = factor(serie, levels = names(series)))
p1 <- ggplot(dd, aes(date, indice)) + geom_line(colour = "grey25", linewidth = 0.5) +
  geom_rect(data = bk, aes(xmin = ic_bas, xmax = ic_haut, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "#2a78d6", alpha = 0.18) +
  geom_vline(data = bk, aes(xintercept = date), colour = "#2a78d6", linewidth = 0.8) + geom_vline(xintercept = COVID, colour = "#eb6834", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~serie, ncol = 1, scales = "free_y") + labs(title = "Ruptures estimées (bleu, avec intervalle) et date du rapport (orange)", x = NULL, y = "Indice (base 2021 = 100)")
ggsave("figures/fig12_ruptures_estimees.png", p1, width = 9, height = 6.5, dpi = 150, bg = "white")
cd <- bind_rows(lapply(names(cfs), function(nm) cfs[[nm]] %>% mutate(serie = nm))) %>% mutate(serie = factor(serie, levels = names(series)))
p2 <- ggplot(cd, aes(date, ecart)) + geom_hline(yintercept = 0, colour = "grey60") +
  geom_ribbon(aes(ymin = bas - prevu, ymax = haut - prevu), fill = "#2a78d6", alpha = 0.18) + geom_line(colour = "#0b0b0b") +
  facet_wrap(~serie, ncol = 1, scales = "free_y") + labs(title = "Écart observé - prévision ARIMA sans Covid, avec intervalle de prévision à 95 % (bleu)", x = NULL, y = "Points d'indice")
ggsave("figures/fig13_ecart_contrefactuel.png", p2, width = 9, height = 6.5, dpi = 150, bg = "white")
rl <- real %>% mutate(serie = factor(serie, levels = names(series))); plf <- pl %>% mutate(serie = factor(serie, levels = names(series)))
p3 <- ggplot(plf, aes(ecart_moyen_pct)) + geom_dotplot(binwidth = 1, fill = "grey65", colour = NA, dotsize = 0.9) +
  geom_vline(data = rl, aes(xintercept = reel_pct), colour = "#eb6834", linewidth = 1) + facet_wrap(~serie, ncol = 1) + scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "Écart moyen en % de la prévision : fausses dates (gris), mars 2020 (orange)", x = "Écart moyen observé / prévu (%)")
ggsave("figures/fig14_placebos.png", p3, width = 9, height = 5.5, dpi = 150, bg = "white")
cat("Analyse complémentaire terminée.\n")
