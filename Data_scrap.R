
# Installation et chargement de quantmod
if (!requireNamespace("quantmod", quietly = TRUE)) {
  install.packages("quantmod")
}
library(quantmod)

# Définir la période de récupération des données
start_date <- as.Date("2014-12-31")
end_date <- as.Date("2023-12-31")


symbols_list <- c(
  "AZN", "JNJ", "BIIB", "ALNY", "CI",  # Health Sector
  "CAT", "VMC", "MLM", "PWR", "DHI",  # Construction Sector
  "JPM", "BAC", "WFC", "C", "GS",  # Finance Sector
  "PG", "KO", "PEP", "NKE", "EL",  # Consumer Goods Sector
  "AAPL", "MSFT", "GOOGL", "AMZN", "META",  # Technology Sector
  "AMT", "PLD", "CCI", "EQIX", "DLR",  # Real Estate Sector
  "XOM", "CVX", "COP", "EOG", "SLB",  # Energy Sector
  "GC=F", "SI=F", "HG=F", "ALI=F",  # Metals (Commodities)
  "ZC=F", "ZW=F", "ZS=F", "KC=F", "SB=F",  # Food (Commodities)
  "NG=F", "CL=F", "BZ=F", # Energy (Commodities)
  "VWO", "IEMG", "EEM",  # INDEX - Emerging Markets
  "^STOXX", "^N100", "^GDAXI",  # INDEX - Europe
  "^GSPC", "^DJI", "^IXIC",  # INDEX - United States
  "^N225", "000001.SS", "^HSI",  # INDEX - Asia
  "BRL=X", "RUB=X", "INR=X",  # Emerging Markets Currencies
  "EURUSD=X", "EURGBP=X", "EURCHF=X",  # Europe Currencies
  "USDCHF=X", "USDJPY=X", "GBPUSD=X",  # United States Currencies
  "SGDJPY=X", "EURJPY=X", "AUDJPY=X",  # Asia Currencies
  "LTC-USD", "BTC-USD",  # Crypto
  "PICB", "XHY1.DE",  # European Corporate Bonds
  "IGOV", "BWX",  # US Government Bonds
  "SJNK", "FALN",  # US Corporate Bonds
  "VWOB", "EMB",  # Emerging Markets Government Bonds
  "HYXU", "SHYG"  # Emerging Markets Corporate Bonds
)


# Fonction pour récupérer les données d'un symbole via Google Finance
get_asset_data_google <- function(symbol) {
  asset_data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  return(Cl(asset_data))  # Récupérer uniquement la colonne de prix de clôture
}

# Récupération des données pour chaque symbole
assets_data_google <- lapply(symbols_list, get_asset_data_google)

# Fusionner toutes les séries temporelles avec le même index temporel
merged_data <- do.call(merge, assets_data_google)

# Afficher les données récupérées
print(merged_data)
library(skimr)
skim(merged_data)



