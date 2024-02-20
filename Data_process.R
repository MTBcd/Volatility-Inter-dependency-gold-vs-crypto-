
# Required Packages
library(quantmod)
library(dplyr)
library(tidyr)
library(skimr)
library(zoo)
library(imputeTS)

# Preprocessing Function
preprocess_data <- function(assets_data) {
  return(as.xts(assets_data))
}

# Apply Preprocessing
processed_data <- lapply(merged_data, preprocess_data)

# Merge Data
all_data_df <- do.call(merge, c(processed_data, all = TRUE))

clean_data <- all_data_df %>% na.omit()

skim(clean_data)


#impute_kalman <- function(df) {
#  df_imputed <- df
#  for(i in 1:ncol(df)) {
#    df_imputed[, i] <- na_kalman(df[, i])
#  }
#  return(df_imputed)
#}


# Min-Max Normalization Function (Vectorized Approach)
min_max_normalisation_df <- function(df) {
  if (is.data.frame(df) && ncol(df) > 0) {
    # Save the original row names (index)
    original_row_names <- row.names(df)
    
    # Apply Min-Max Normalization
    normalized_df <- as.data.frame(lapply(df, function(x) {
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    }))
    
    # Restore the original row names
    row.names(normalized_df) <- original_row_names
    
    return(normalized_df)
  } else {
    stop("Input is not a valid data frame or is empty")
  }
}


norm_data <- min_max_normalisation_df(as.data.frame(clean_data))



# Data Subsets
Gold <- xts(norm_data[,c("GC.F.Close")], order.by = as.Date(rownames(norm_data)))
# Health Sector
health_sector <- xts(norm_data[,c("AZN.Close", "JNJ.Close", "BIIB.Close", "ALNY.Close", "CI.Close")], order.by = as.Date(rownames(norm_data)))
# Construction Sector
construction_sector <- xts(norm_data[,c("CAT.Close", "VMC.Close", "MLM.Close", "PWR.Close", "DHI.Close")], order.by = as.Date(rownames(norm_data)))
# Finance Sector
finance_sector <- xts(norm_data[,c("JPM.Close", "BAC.Close", "WFC.Close", "C.Close", "GS.Close")], order.by = as.Date(rownames(norm_data)))
# Consumer Goods Sector
consumer_goods_sector <- xts(norm_data[,c("PG.Close", "KO.Close", "PEP.Close", "NKE.Close", "EL.Close")], order.by = as.Date(rownames(norm_data)))
# Technology Sector
technology_sector <- xts(norm_data[,c("AAPL.Close", "MSFT.Close", "GOOGL.Close", "AMZN.Close", "META.Close")], order.by = as.Date(rownames(norm_data)))
# Real Estate Sector
real_estate_sector <- xts(norm_data[,c("AMT.Close", "PLD.Close", "CCI.Close", "EQIX.Close", "DLR.Close")], order.by = as.Date(rownames(norm_data)))
# Energy Sector
energy_sector <- xts(norm_data[,c("XOM.Close", "CVX.Close", "COP.Close", "EOG.Close", "SLB.Close")], order.by = as.Date(rownames(norm_data)))
# Commodities - Metals
metals_commodities <- xts(norm_data[,c("SI.F.Close", "HG.F.Close", "ALI.F.Close")], order.by = as.Date(rownames(norm_data)))
# Commodities - Food
food_commodities <- xts(norm_data[,c("ZC.F.Close", "ZW.F.Close", "ZS.F.Close", "KC.F.Close", "SB.F.Close")], order.by = as.Date(rownames(norm_data)))
# Commodities - Energy
energy_commodities <- xts(norm_data[,c("NG.F.Close", "CL.F.Close", "BZ.F.Close")], order.by = as.Date(rownames(norm_data)))
# INDEX - Emerging Markets
emerging_markets_index <- xts(norm_data[,c("VWO.Close", "IEMG.Close", "EEM.Close")], order.by = as.Date(rownames(norm_data)))
# INDEX - Europe
europe_index <- xts(norm_data[,c("STOXX.Close", "N100.Close", "GDAXI.Close")], order.by = as.Date(rownames(norm_data)))
# INDEX - United States
us_index <- xts(norm_data[,c("GSPC.Close", "DJI.Close", "IXIC.Close")], order.by = as.Date(rownames(norm_data)))
# INDEX - Asia
asia_index <- xts(norm_data[,c("N225.Close", "X000001.SS.Close", "HSI.Close")], order.by = as.Date(rownames(norm_data)))
# Currencies - Emerging Markets
emerging_markets_currencies <- xts(norm_data[,c("BRL.X.Close", "RUB.X.Close", "INR.X.Close")], order.by = as.Date(rownames(norm_data)))
# Currencies - Europe
europe_currencies <- xts(norm_data[,c("EURUSD.X.Close", "EURGBP.X.Close", "EURCHF.X.Close")], order.by = as.Date(rownames(norm_data)))
# Currencies - United States
us_currencies <- xts(norm_data[,c("USDCHF.X.Close", "USDJPY.X.Close", "GBPUSD.X.Close")], order.by = as.Date(rownames(norm_data)))
# Currencies - Asia
asia_currencies <- xts(norm_data[,c("SGDJPY.X.Close", "EURJPY.X.Close", "AUDJPY.X.Close")], order.by = as.Date(rownames(norm_data)))
# Crypto
crypto <- xts(norm_data[,c("LTC.USD.Close", "BTC.USD.Close")], order.by = as.Date(rownames(norm_data)))
# European Corporate Bonds
european_corporate_bonds <- xts(norm_data[,c("PICB.Close", "XHY1.DE.Close")], order.by = as.Date(rownames(norm_data)))
# US Government Bonds
us_government_bonds <- xts(norm_data[,c("IGOV.Close", "BWX.Close")], order.by = as.Date(rownames(norm_data)))
# US Corporate Bonds
us_corporate_bonds <- xts(norm_data[,c("SJNK.Close", "FALN.Close")], order.by = as.Date(rownames(norm_data)))
# Emerging Markets Government Bonds
emerging_markets_government_bonds <- xts(norm_data[,c("VWOB.Close", "EMB.Close")], order.by = as.Date(rownames(norm_data)))
# Emerging Markets Corporate Bonds
emerging_markets_corporate_bonds <- xts(norm_data[,c("HYXU.Close", "SHYG.Close")], order.by = as.Date(rownames(norm_data)))



# ACP Function with Date Index Preservation
realiser_acp <- function(df, labels = NULL) {
  if (!inherits(df, "xts")) {
    stop("Input dataframe is not an xts object.")
  }
  
  # Standardisation des données
  df_std <- as.data.frame(lapply(df, function(x) (x - mean(x)) / sd(x)))
  
  # Calcul de la matrice de covariance et ACP
  cov_matrix <- cov(df_std)
  eigen_values_vectors <- eigen(cov_matrix)
  
  # Calcul des scores pondérés
  scores <- as.matrix(df_std) %*% eigen_values_vectors$vectors
  variance_expliquee <- eigen_values_vectors$values / sum(eigen_values_vectors$values)
  scores_ponderees <- scores %*% diag(variance_expliquee)
  
  # Agglomération des scores pondérés
  serie_temporelle_agglomeree <- rowSums(scores_ponderees)
  
  # Création d'un objet xts avec l'index temporel original
  output_xts <- xts(serie_temporelle_agglomeree, order.by = index(df))
  
  # Affichage graphique si les labels sont fournis
  if (!is.null(labels) && length(labels) == length(variance_expliquee)) {
    barplot(variance_expliquee, main = "Contribution des composantes", xlab = "Composantes", ylab = "Part de variance expliquée")
    text(x = 1:length(variance_expliquee), y = variance_expliquee, labels = labels, pos = 3, cex = 0.8)
  }
  
  # Retourne les résultats
  list(valeurs_propres = eigen_values_vectors$values, vecteurs_propres = eigen_values_vectors$vectors, serie_agglomeree_xts = output_xts)
}


# Gold
acp_result_Gold <- realiser_acp(Gold)
Gold_serie <- acp_result_Gold$serie_agglomeree_xts

# Health Sector
acp_result_health <- realiser_acp(health_sector)
health_serie <- acp_result_health$serie_agglomeree_xts

# Construction Sector
acp_result_construction <- realiser_acp(construction_sector)
construction_serie <- acp_result_construction$serie_agglomeree_xts

# Finance Sector
acp_result_finance <- realiser_acp(finance_sector)
finance_serie <- acp_result_finance$serie_agglomeree_xts

# Consumer Goods Sector
acp_result_consumer_goods <- realiser_acp(consumer_goods_sector)
consumer_goods_serie <- acp_result_consumer_goods$serie_agglomeree_xts

# Technology Sector
acp_result_technology <- realiser_acp(technology_sector)
technology_serie <- acp_result_technology$serie_agglomeree_xts

# Real Estate Sector
acp_result_real_estate <- realiser_acp(real_estate_sector)
real_estate_serie <- acp_result_real_estate$serie_agglomeree_xts

# Energy Sector
acp_result_energy <- realiser_acp(energy_sector)
energy_serie <- acp_result_energy$serie_agglomeree_xts

# Commodities - Metals
acp_result_metals <- realiser_acp(metals_commodities)
metals_serie <- acp_result_metals$serie_agglomeree_xts

# Commodities - Food
acp_result_food <- realiser_acp(food_commodities)
food_serie <- acp_result_food$serie_agglomeree_xts

# Commodities - Energy
acp_result_energy_commodities <- realiser_acp(energy_commodities)
energy_commodities_serie <- acp_result_energy_commodities$serie_agglomeree_xts

# INDEX - Emerging Markets
acp_result_emerging_markets_index <- realiser_acp(emerging_markets_index)
emerging_markets_index_serie <- acp_result_emerging_markets_index$serie_agglomeree_xts

# INDEX - Europe
acp_result_europe_index <- realiser_acp(europe_index)
europe_index_serie <- acp_result_europe_index$serie_agglomeree_xts

# INDEX - United States
acp_result_us_index <- realiser_acp(us_index)
us_index_serie <- acp_result_us_index$serie_agglomeree_xts

# INDEX - Asia
acp_result_asia_index <- realiser_acp(asia_index)
asia_index_serie <- acp_result_asia_index$serie_agglomeree_xts

# Currencies - Emerging Markets
acp_result_emerging_markets_currencies <- realiser_acp(emerging_markets_currencies)
emerging_markets_currencies_serie <- acp_result_emerging_markets_currencies$serie_agglomeree_xts

# Currencies - Europe
acp_result_europe_currencies <- realiser_acp(europe_currencies)
europe_currencies_serie <- acp_result_europe_currencies$serie_agglomeree_xts

# Currencies - United States
acp_result_us_currencies <- realiser_acp(us_currencies)
us_currencies_serie <- acp_result_us_currencies$serie_agglomeree_xts

# Currencies - Asia
acp_result_asia_currencies <- realiser_acp(asia_currencies)
asia_currencies_serie <- acp_result_asia_currencies$serie_agglomeree_xts

# Crypto
acp_result_crypto <- realiser_acp(crypto)
crypto_serie <- acp_result_crypto$serie_agglomeree_xts

# European Corporate Bonds
acp_result_european_corporate_bonds <- realiser_acp(european_corporate_bonds)
european_corporate_bonds_serie <- acp_result_european_corporate_bonds$serie_agglomeree_xts

# US Government Bonds
acp_result_us_government_bonds <- realiser_acp(us_government_bonds)
us_government_bonds_serie <- acp_result_us_government_bonds$serie_agglomeree_xts

# US Corporate Bonds
acp_result_us_corporate_bonds <- realiser_acp(us_corporate_bonds)
us_corporate_bonds_serie <- acp_result_us_corporate_bonds$serie_agglomeree_xts

# Emerging Markets Government Bonds
acp_result_emerging_markets_government_bonds <- realiser_acp(emerging_markets_government_bonds)
emerging_markets_government_bonds_serie <- acp_result_emerging_markets_government_bonds$serie_agglomeree_xts

# Emerging Markets Corporate Bonds
acp_result_emerging_markets_corporate_bonds <- realiser_acp(emerging_markets_corporate_bonds)
emerging_markets_corporate_bonds_serie <- acp_result_emerging_markets_corporate_bonds$serie_agglomeree_xts