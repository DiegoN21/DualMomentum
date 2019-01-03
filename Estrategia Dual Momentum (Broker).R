# Librerias necesarias para el calculo
library(quantmod)
library(lubridate)
library(TTR)
library(dplyr)
library(knitr)

# Definimos las variables
activos    <- c("IVV", "VEU", "BND", "BIL")
nombres    <- c("US.Equity", "Int.Equity", "Total.Bond", "T.BILL")          
frecuencia <- "monthly" 
historico  <- 2 # Buscamos descargar toda la data historica disponible
look.Back  <- 12  # Períodos historicos a considerar

# Descargamos el historico de precios
eDate <- Sys.Date()             
sDate <- eDate - years(historico)   

portfolioPrices <- NULL
i <- 1
for (Ticker in activos){
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(Ticker, 
                                                             from = sDate, 
                                                             to = eDate, 
                                                             periodicity = frecuencia, 
                                                             auto.assign=FALSE)[,6])
  print(paste("Hemos descargado el Ticket: ", Ticker, " (", nombres[i], ")", sep = ""))
  i <- i + 1
}

portfolioPrices <- na.omit(portfolioPrices)
colnames(portfolioPrices) <- nombres
portfolioPrices <- as.data.frame(portfolioPrices)

# Creamos la estrategia momentum
momentum.Data <- portfolioPrices %>% 
  mutate(US.ReturnLB = ROC(US.Equity, n = look.Back, type = "continuous"),
         INT.ReturnLB = ROC(Int.Equity, n = look.Back, type = "continuous"),
         BOND.ReturnLB = ROC(Total.Bond, n = look.Back, type = "continuous"),
         TB.ReturnLB = ROC(T.BILL, n = look.Back, type = "continuous"))

momentum.Data <- momentum.Data[, -c(1:4)] # Reordenamos las columnas
momentum.Data <- na.omit(momentum.Data)
row.names(momentum.Data) <- NULL

# Creamos la estrategia
data.Actual <- momentum.Data[nrow(momentum.Data), ]

# Donde Invertimos?
if(data.Actual[1] > data.Actual[2]){
  if(data.Actual[1] > data.Actual[4]){
    decision <- paste("Invertimos en Equity Americano. El ticket es: ", activos[1], sep = "")
  }else{
    decision <- paste("Invertimos en Bonos Americanos. El ticket es: ", activos[3], sep = "")
  }
}else{
  if(data.Actual[2] > data.Actual[4]){
    decision <- paste("Invertimos en Equity Internacional. El ticket es: ", activos[2], sep = "")
  }else{
    decision <- paste("Invertimos en Bonos Americanos. El ticket es: ", activos[3], sep = "")
  }
}

cat("\f")
decision
