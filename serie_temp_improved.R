# ==============================================================================
# Analyse de série temporelle - Trafic Internet (en bits)
# ==============================================================================
# Ce script :
#   1. Charge et nettoie les données de trafic Internet
#   2. Construit un objet ts() pour l'analyse de série temporelle
#   3. Trace la série avec base R et avec dygraphs (interactif)
# ==============================================================================

# ---- 1. Librairies ----------------------------------------------------------
library(readr)
library(lubridate)
library(dygraphs)
library(xts)
library(dplyr)     # NB : masque xts::first/last et stats::filter/lag (sans impact ici)
library(forecast)  # decompose/stl utilities + Arima + accuracy()

# ---- 2. Chargement des données ----------------------------------------------
# Utiliser un chemin relatif (ou here::here()) plutôt qu'un chemin en dur
# rend le script portable et reproductible sur GitHub / une autre machine.
data_path <- "internet-traffic-data-in-bits-fr.csv"

data <- read_csv(
  data_path,
  skip = 1,
  col_names = c("Date", "Traffic"),
  col_types = cols(
    Date    = col_character(),
    Traffic = col_double()
  )
)

# Aperçu rapide
head(data)
tail(data)
summary(data)

# ---- 3. Nettoyage des dates ---------------------------------------------
# On garde la chaîne brute de côté pour pouvoir diagnostiquer les échecs de
# parsing (au lieu de se retrouver avec des NA silencieux plus loin).
raw_date <- data$Date
data$Date <- ymd_hms(raw_date, tz = "GMT")

n_parse_failed <- sum(is.na(data$Date))
if (n_parse_failed > 0) {
  warning(n_parse_failed, " date(s) n'ont pas pu être converties et vont être exclues.")
  cat("Exemples de chaînes non parsées :\n")
  print(head(unique(raw_date[is.na(data$Date)]), 10))

  # On retire les lignes dont la date n'a pas pu être interprétée : les
  # garder ferait planter le tri, les diffs de temps et le passage en ts().
  data <- data[!is.na(data$Date), ]
}

# Trier par date pour être sûr que la série est bien ordonnée
data <- data[order(data$Date), ]

# ---- 4. Analyse de la qualité des données --------------------------------
# Étape indispensable avant toute modélisation sur des données réelles.

cat("=== Qualité des données ===\n")

# a) Valeurs manquantes
n_missing <- sum(is.na(data$Traffic))
cat("Valeurs manquantes (Traffic) :", n_missing, "\n")
cat("Dates non parsées (exclues) :", n_parse_failed, "\n")

# b) Timestamps dupliqués
n_dup_dates <- sum(duplicated(data$Date))
cat("Timestamps dupliqués :", n_dup_dates, "\n")
if (n_dup_dates > 0) {
  # On garde la première occurrence en cas de doublon
  data <- data[!duplicated(data$Date), ]
}

# c) Intervalles de temps irréguliers
diffs <- diff(data$Date)
units(diffs) <- "mins"                    # fige l'unité pour un table() lisible
diff_table <- table(round(as.numeric(diffs), 2))
cat("Intervalles entre observations, en minutes (les plus fréquents) :\n")
print(head(sort(diff_table, decreasing = TRUE), 5))

most_common_interval <- as.numeric(names(sort(diff_table, decreasing = TRUE))[1])
n_irregular <- sum(round(as.numeric(diffs), 2) != most_common_interval, na.rm = TRUE)
cat("Nombre d'intervalles irréguliers :", n_irregular,
    "sur", length(diffs), "\n")

# d) Statistiques de base
cat("\n=== Statistiques descriptives ===\n")
print(summary(data$Traffic))
cat("Écart-type :", sd(data$Traffic, na.rm = TRUE), "\n")

# ---- 5. Construction de l'objet série temporelle -----------------------
# Fréquence : ici on suppose des données toutes les 5 minutes -> 24*12 = 288
# points par jour. Adapter selon la granularité réelle des données.
Traffic.ts <- ts(data$Traffic, start = 1, frequency = 24 * 12)

# ---- 6. Visualisation statique (base R) ---------------------------------
plot(
  data$Date, data$Traffic,
  type = "l",
  xaxt = "n",
  xlab = "",
  ylab = "Trafic Internet (bits)",
  col  = "royalblue3",
  main = "Trafic Internet au cours du temps"
)
axis.POSIXct(
  1,
  at     = seq(data$Date[1], tail(data$Date, 1), by = "weeks"),
  format = "%d/%m/%y",
  las    = 2
)

# ---- 7. Visualisation interactive (dygraphs) -----------------------------
Traffic.xts <- xts(data$Traffic, order.by = data$Date)

dygraph(Traffic.xts, main = "Trafic Internet (interactif)") |>
  dyRangeSelector() |>
  dyAxis("y", label = "Trafic (bits)")

# ---- 8. Décomposition de la série -----------------------------------------
# On sépare Observé = Tendance + Saisonnalité + Résidu.
# stl() est préférable à decompose() : plus robuste (loess), gère mieux les
# séries bruitées et permet une saisonnalité qui évolue dans le temps.
Traffic.stl <- stl(Traffic.ts, s.window = "periodic")
plot(Traffic.stl, main = "Décomposition STL du trafic Internet")

# Comparaison rapide avec decompose() (méthode classique, additive)
Traffic.decomp <- decompose(Traffic.ts)
plot(Traffic.decomp)

# -> À discuter : la composante saisonnière (period = 24*12, soit un cycle
# journalier) capture-t-elle bien les habitudes d'usage (creux nocturne,
# pics en journée) ? Pour une saisonnalité hebdomadaire, il faudrait une
# fréquence de 24*12*7 ou une décomposition multi-saisonnière (msts + mstl()).

# ---- 9. Autocorrélation (ACF / PACF) ---------------------------------------
# Vérifie la dépendance temporelle entre observations successives :
# indispensable avant de choisir un modèle ARIMA.
par(mfrow = c(1, 2))
acf(Traffic.ts, lag.max = 24 * 12 * 2, main = "ACF - Trafic Internet")
pacf(Traffic.ts, lag.max = 24 * 12 * 2, main = "PACF - Trafic Internet")
par(mfrow = c(1, 1))

# -> Une décroissance lente de l'ACF suggère une tendance/non-stationnarité ;
# des pics périodiques (tous les 288 lags) confirment la saisonnalité
# journalière déjà visible dans la décomposition.

# ---- 10. Prévision (baseline -> ARIMA/SARIMA) ------------------------------
# Découpage train/test temporel (PAS de split aléatoire pour une série
# temporelle : on prédit le futur à partir du passé).
h <- 24 * 12          # horizon de test = 1 jour
n <- length(Traffic.ts)

train.ts <- window(Traffic.ts, end = time(Traffic.ts)[n - h])
test.ts  <- window(Traffic.ts, start = time(Traffic.ts)[n - h + 1])

# a) Modèle de référence (baseline) : naïf saisonnier
#    (prévoit "même valeur qu'à la même heure hier")
fit_baseline <- snaive(train.ts, h = h)

# b) ARIMA/SARIMA automatique
fit_arima <- auto.arima(train.ts, seasonal = TRUE, stepwise = TRUE)
summary(fit_arima)
fc_arima <- forecast(fit_arima, h = h)

# c) Visualisation des prévisions face aux vraies valeurs
autoplot(fc_arima) +
  autolayer(test.ts, series = "Valeurs réelles") +
  ggtitle("Prévision ARIMA vs valeurs réelles") +
  xlab("Temps") + ylab("Trafic (bits)")

# d) Évaluation : MAE, RMSE, MAPE (sur l'ensemble de test)
acc_baseline <- accuracy(fit_baseline, test.ts)
acc_arima    <- accuracy(fc_arima, test.ts)

cat("\n=== Évaluation des modèles (ensemble de test) ===\n")
cat("-- Naïf saisonnier --\n")
print(acc_baseline["Test set", c("MAE", "RMSE", "MAPE")])
cat("-- ARIMA/SARIMA --\n")
print(acc_arima["Test set", c("MAE", "RMSE", "MAPE")])

# -> Le modèle ARIMA n'est intéressant que s'il fait mieux que la baseline
# naïve saisonnière sur ces trois métriques.