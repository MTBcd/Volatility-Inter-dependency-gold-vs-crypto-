

library(vars)
library(tseries)
library(forecast)
library(rugarch)
library(ARDL)
library(utils)
library(urca)
library(tsDyn)


best_garch_volatility <- function(data, max_lag = 2) {
  arima_fit <- auto.arima(data, trace = TRUE, allowdrift = TRUE)
  ar_lag <- arima_fit$arma[1]
  ma_lag <- arima_fit$arma[2]
  
  garch_models <- c('gjrGARCH', 'iGARCH', 'eGARCH')
  distributions <- c('ged', 'norm')#, 'std')
  
  best_aic <- Inf
  best_model_fit <- NULL
  best_model_name <- ""
  best_dist_name <- ""
  
  for (model in garch_models) {
    for (dist in distributions) {
      for (p in 1:max_lag) {
        for (q in 1:max_lag) {
          spec <- ugarchspec(variance.model = list(model = model, garchOrder = c(p, q)),
                             mean.model = list(armaOrder = c(ar_lag, ma_lag)),
                             distribution.model = dist)
          
          model_fit <- try(ugarchfit(spec = spec, data = data, solver = "hybrid"), silent = TRUE)
          
          if (class(model_fit) != "try-error") {
            aic_value <- infocriteria(model_fit)[1]
            if (aic_value < best_aic) {
              best_aic <- aic_value
              best_model_fit <- model_fit
              best_model_name <- model
              best_dist_name <- dist
            }
          }
        }
      }
    }
  }
  
  if (is.null(best_model_fit)) {
    stop("Aucun modèle GARCH n'a pu être ajusté avec succès.")
  }
  
  volatilite_estimee <- sigma(best_model_fit)
  return(list(volatility = volatilite_estimee, best_submodel = best_model_name, best_distribution = best_dist_name, best_aic = best_aic))

}

vol_Gold_serie <- best_garch_volatility(Gold_serie)$volatility
vol_health_serie <- best_garch_volatility(health_serie)$volatility
vol_construction_serie <- best_garch_volatility(construction_serie)$volatility
vol_finance_serie <- best_garch_volatility(finance_serie)$volatility
vol_consumer_goods_serie <- best_garch_volatility(consumer_goods_serie)$volatility
vol_technology_serie <- best_garch_volatility(technology_serie)$volatility
vol_real_estate_serie <- best_garch_volatility(real_estate_serie)$volatility
vol_energy_serie <- best_garch_volatility(energy_serie)$volatility
vol_metals_serie <- best_garch_volatility(metals_serie)$volatility
vol_food_serie <- best_garch_volatility(food_serie)$volatility
vol_energy_commodities_serie <- best_garch_volatility(energy_commodities_serie)$volatility
vol_emerging_markets_index_serie <- best_garch_volatility(emerging_markets_index_serie)$volatility
vol_europe_index_serie <- best_garch_volatility(europe_index_serie)$volatility
vol_us_index_serie <- best_garch_volatility(us_index_serie)$volatility
vol_asia_index_serie <- best_garch_volatility(asia_index_serie)$volatility
vol_emerging_markets_currencies_serie <- best_garch_volatility(emerging_markets_currencies_serie)$volatility
vol_europe_currencies_serie <- best_garch_volatility(europe_currencies_serie)$volatility
vol_us_currencies_serie <- best_garch_volatility(us_currencies_serie)$volatility
vol_asia_currencies_serie <- best_garch_volatility(asia_currencies_serie)$volatility
vol_crypto_serie <- best_garch_volatility(crypto_serie)$volatility
vol_european_corporate_bonds_serie <- best_garch_volatility(european_corporate_bonds_serie)$volatility
vol_us_government_bonds_serie <- best_garch_volatility(us_government_bonds_serie)$volatility
vol_us_corporate_bonds_serie <- best_garch_volatility(us_corporate_bonds_serie)$volatility
vol_emerging_markets_government_bonds_serie <- best_garch_volatility(emerging_markets_government_bonds_serie)$volatility
vol_emerging_markets_corporate_bonds_serie <- best_garch_volatility(emerging_markets_corporate_bonds_serie)$volatility


data_vol <- data.frame(
  vol_Gold_serie
  ,vol_health_serie
  ,vol_construction_serie
  ,vol_finance_serie
  ,vol_consumer_goods_serie
  ,vol_technology_serie
  ,vol_real_estate_serie
  ,vol_energy_serie
  ,vol_metals_serie
  ,vol_food_serie
  ,vol_energy_commodities_serie
  ,vol_emerging_markets_index_serie
  ,vol_europe_index_serie
  ,vol_us_index_serie
  ,vol_asia_index_serie
  ,vol_emerging_markets_currencies_serie
  ,vol_europe_currencies_serie
  ,vol_us_currencies_serie
  ,vol_asia_currencies_serie
  ,vol_crypto_serie
  ,vol_european_corporate_bonds_serie
  ,vol_us_government_bonds_serie
  ,vol_us_corporate_bonds_serie
  ,vol_emerging_markets_government_bonds_serie
  ,vol_emerging_markets_corporate_bonds_serie
)


stationarize_df <- function(df) {
  df_stationarized <- df
  for(column in names(df)) {
    if(adf.test(df[[column]], alternative = "stationary")$p.value > 0.05) {
      # Apply differencing
      diff_series <- diff(df[[column]], differences = 1)
      # Replace the original series with the differenced series
      df_stationarized[[column]] <- c(NA, diff_series)  # Adding NA for the first element lost in differencing
    }
  }
  df_stationarized <- xts(df_stationarized, order.by = as.Date(row.names(df)))
  colnames(df_stationarized) <- paste0(colnames(df_stationarized), "_std")
  return(as.ts(na.omit(df_stationarized)))  # Removing rows with NA values
}

stationarized_data_vol <- stationarize_df(data_vol)

stationarized_data_ts <- as.ts(stationarized_data_vol)


model_gld_ <- auto_ardl( vol_Gold_serie_std ~ 
                         vol_health_serie_std +
                         vol_construction_serie_std +
                         vol_finance_serie_std +
                         vol_consumer_goods_serie_std +
                         vol_technology_serie_std +
                         vol_real_estate_serie_std +
                         vol_energy_serie_std +
                         vol_metals_serie_std +
                         vol_food_serie_std +
                         vol_energy_commodities_serie_std +
                         vol_emerging_markets_index_serie_std +
                         vol_europe_index_serie_std +
                         vol_us_index_serie_std +
                         vol_asia_index_serie_std +
                         vol_emerging_markets_currencies_serie_std +
                         vol_europe_currencies_serie_std +
                         vol_us_currencies_serie_std +
                         vol_asia_currencies_serie_std +
                         vol_crypto_serie_std +
                         vol_european_corporate_bonds_serie_std +
                         vol_us_government_bonds_serie_std +
                         vol_us_corporate_bonds_serie_std +
                         vol_emerging_markets_government_bonds_serie_std +
                         vol_emerging_markets_corporate_bonds_serie_std,
                       data = stationarized_data_ts,
                       max_order = rep(10, 25), 
                       selection = "AIC")

summary(model_gld_$best_model)

model_btc_ <- auto_ardl( vol_crypto_serie_std ~ 
                           vol_health_serie_std +
                           vol_construction_serie_std +
                           vol_finance_serie_std +
                           vol_consumer_goods_serie_std +
                           vol_technology_serie_std +
                           vol_real_estate_serie_std +
                           vol_energy_serie_std +
                           vol_metals_serie_std +
                           vol_food_serie_std +
                           vol_energy_commodities_serie_std +
                           vol_emerging_markets_index_serie_std +
                           vol_europe_index_serie_std +
                           vol_us_index_serie_std +
                           vol_asia_index_serie_std +
                           vol_emerging_markets_currencies_serie_std +
                           vol_europe_currencies_serie_std +
                           vol_us_currencies_serie_std +
                           vol_asia_currencies_serie_std +
                           vol_Gold_serie_std +
                           vol_european_corporate_bonds_serie_std +
                           vol_us_government_bonds_serie_std +
                           vol_us_corporate_bonds_serie_std +
                           vol_emerging_markets_government_bonds_serie_std +
                           vol_emerging_markets_corporate_bonds_serie_std,
                         data = stationarized_data_ts,
                         max_order = rep(10, 25), 
                         selection = "AIC")

summary(model_btc_$best_model)



summary(uecm(model_gld_$best_model))


summary(uecm(model_btc_$best_model))


# Convert ARDL to VAR model
var_model <- VAR(stationarized_data_ts, p = 12, type = "const")
summary(var_model)

# Compute and plot IRFs
irf_results <- irf(var_model, impulse = "vol_crypto_serie_std", response = "vol_Gold_serie_std", n.ahead = 20)
plot(irf_results)

#variance decomposition 
fevd_results <- fevd(var_model, n.ahead = 10)
plot(fevd_results)

variables <- colnames(stationarized_data_ts)
# Store results in a list
granger_results <- list()
for (variable in variables) {
  granger_results[[variable]] <- causality(var_model, cause = variable)
}
# Print the results
lapply(granger_results, print)


stationarized_data_df <-as.data.frame(stationarized_data_ts)

data_vol_gld_std_xts <- xts(stationarized_data_df$vol_Gold_serie_std, order.by = as.Date(index(stationarized_data_df)))

data_vol_crypto_std_xts <- xts(stationarized_data_df$vol_crypto_serie_std, order.by = as.Date(index(stationarized_data_ts)))

dates <- seq(from = min(as.Date(index(data_vol_gld_std_xts))), 
             to = max(as.Date(index(data_vol_crypto_std_xts))), 
             by = "day")

# Interpolate the data for both gold and bitcoin volatilities
volatility_gold_interp <- approx(as.Date(index(data_vol_gld_std_xts)), data_vol_gld_std_xts, xout = dates)$y
volatility_btc_interp <- approx(as.Date(index(data_vol_crypto_std_xts)), data_vol_crypto_std_xts, xout = dates)$y

# Combine dates with the interpolated data
t1 <- cbind(dates, volatility_gold_interp)
t2 <- cbind(dates, volatility_btc_interp)

# Set a higher number of simulations for robust testing
nrands <- 100

# Perform wavelet coherence analysis
wtc_results <- wtc(t1, t2, nrands = nrands)

# Plot the results with enhanced visualization
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.3)
plot(wtc_results, plot.phase = TRUE, 
     lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, 
     ylab = "Frequency (Period)", xlab = "Time", 
     plot.cb = TRUE, main = "Wavelet Coherence: Gold vs Bitcoin Volatility",
     col = heat.colors(10)) # You can adjust the color palette as needed




