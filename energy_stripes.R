# Dependencies ------------------------------------------------------------
  
  library(RCurl)
  library(jsonlite)
  library(lubridate)
  library(ggplot2)

# Downloading data from energidataservice ---------------------------------

  url <- "https://api.energidataservice.dk/dataset/" # API access point

  # Downloading settlement data (since 2005) --------------------------------

  request <- paste0(url, "ProductionConsumptionSettlement")
  limit <- fromJSON(getURL(request))$total # no. of available observations

  # Fetching observations
  dat <- fromJSON(getURL(paste0(request, "?limit=", limit))) # (getting app. 95Mb, 5 min.)
  data <- dat$records
  data <- data[-1] # no need for this
  data$HourDK <- as.POSIXct(data$HourDK, format="%Y-%m-%dT%H:%M:%S")
  data_val <- aggregate(. ~ HourDK, data[c(1,3:26)], FUN=sum, na.action = na.pass)
  
  remove(data, limit, request, dat) # clearning up
  
  # Downloading data at 5 mins intervals ------------------------------------
  
  request <- paste0(url, "ElectricityProdex5MinRealtime")
  limit <- fromJSON(getURL(request))$total # no. of available observations
  
  # Fetching observations
  dat <- fromJSON(getURL(paste0(request, "?limit=", limit))) # (getting app. 95Mb, 5 min.)
  data <- dat$records
  data <- data[-1] # no need for this
  data$Minutes5DK <- as.POSIXct(data$Minutes5DK, format="%Y-%m-%dT%H:%M:%S")
  data_5mins <- aggregate(. ~ Minutes5DK, data_5mins[c(1,3:13)], FUN=sum, na.action = na.pass)
  
  remove(data, limit, request, dat) # clearning up

  # Downloading data non-validated (but real time) -----------------------
  
  request <- paste0(url, "ElectricityBalanceNonv") # Production and consumption settlement
  limit <- fromJSON(getURL(request))$total # no. of available observations
  
  # Fetching observations
  dat <- fromJSON(getURL(paste0(request, "?limit=", limit))) # (getting app. 95Mb, 5 min.)
  data <- dat$records
  data <- data[-1] # no need for this
  data$HourDK <- as.POSIXct(data$HourDK, format="%Y-%m-%dT%H:%M:%S")
  data_nonval <- aggregate(. ~ HourDK, data[c(1,3:16)], FUN=sum, na.action = na.pass)
  
  remove(data, limit, request, dat) # clearning up
  
# Aggregating and ordering (nothing here) ---------------------------------

# Rearranging (nothing here) ----------------------------------------------

# Graphs ... --------------------------------------------------------------

  # Yearly production from wind and solar PV in Denmark by date -------------
  
  A <- data_nonval
  
  A$wind_total <- rowSums(A[c(9, 11:12)])
  A$date <- as.Date.character(A$HourDK)
  A$year <- year(A$HourDK)
  A$wind_cummulative <- ave(A$wind_total, A$year, FUN=cumsum)/10^6
  A$y_hour <- (yday(A$HourDK) - 1) * 24 + hour(A$HourDK)
  
  A <- A[A$year>2019,]
  A$year <- as.character(A$year)
  A$Hour_labels <- A$HourDK
  year(A$Hour_labels) <- 2022
  
  plot <- ggplot(A, aes(x=Hour_labels, y=wind_cummulative, group=year, col=year)) +
    geom_line(linewidth=2) +
    scale_x_datetime(name="Date", date_labels = "%b %d") +
    labs(title="Yearly power production from wind and solarPV in Denmark",
         subtitle="18.6 TWh produced per Nov. 18",
         caption="Source: Energinet") +
    ylab("Cummulative production, TWh") +
    xlab("Dato") +
    theme_test() +
    theme(text = element_text(size = 20))
  
  remove(A)
  
  # Energy_stripes --------------------------------------------------------

  A <- data_val
  A$Wind_total <- rowSums(A[c(6:9)])
  
  
