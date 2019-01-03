# Librerias necesarias para el calculo
library(quantmod)
library(lubridate)
library(TTR)
library(dplyr)
library(ggplot2)
library(reshape)
library(gridExtra)
library(PerformanceAnalytics)
library(xts)
library(kableExtra)

# Definimos las variables
activos    <- c("IVV", "VEU", "BND", "BIL")
nombres    <- c("US.Equity", "Int.Equity", "Total.Bond", "T.BILL")          
frecuencia <- "monthly" 
historico  <- 100  # Buscamos descargar toda la data historica disponible
look.Back  <- 12   # Períodos historicos a considerar

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
  mutate(DATES = as.Date(row.names(portfolioPrices), format = "%Y-%m-%d"),
         US.Return = ROC(US.Equity, n = 1, type = "continuous"),
         INT.Return = ROC(Int.Equity, n = 1, type = "continuous"),
         BOND.Return = ROC(Total.Bond, n = 1, type = "continuous"),
         US.ReturnLB = ROC(US.Equity, n = look.Back, type = "continuous"),
         INT.ReturnLB = ROC(Int.Equity, n = look.Back, type = "continuous"),
         BOND.ReturnLB = ROC(Total.Bond, n = look.Back, type = "continuous"),
         TB.ReturnLB = ROC(T.BILL, n = look.Back, type = "continuous"))

momentum.Data <- momentum.Data[, -c(1:4)] # Reordenamos las columnas
momentum.Data <- na.omit(momentum.Data)
row.names(momentum.Data) <- NULL

# Creamos la estrategia
momentum.Data$Asset  <- NA
momentum.Data$col    <- NA
momentum.Data$Return <- 0

for(i in 1:nrow(momentum.Data)){
  if(momentum.Data$US.ReturnLB[i] > momentum.Data$INT.ReturnLB[i]){
    if(momentum.Data$US.ReturnLB[i] > momentum.Data$TB.ReturnLB[i]){
      momentum.Data$Asset[i]  <- "US"
      momentum.Data$col[i]    <- "green"
      momentum.Data$Return[i] <- momentum.Data$US.Return[i + 1]
    }else{
      momentum.Data$Asset[i]  <- "BOND"
      momentum.Data$col[i]    <- "blue"
      momentum.Data$Return[i] <- momentum.Data$BOND.Return[i + 1]
    }
  }else{
    if(momentum.Data$INT.ReturnLB[i] > momentum.Data$TB.ReturnLB[i]){
      momentum.Data$Asset[i]  <- "INT"
      momentum.Data$col[i]    <- "red"
      momentum.Data$Return[i] <- momentum.Data$INT.Return[i + 1]
    }else{
      momentum.Data$Asset[i]  <- "BOND"
      momentum.Data$col[i]    <- "blue"
      momentum.Data$Return[i] <- momentum.Data$BOND.Return[i + 1]
    }
  }
}

momentum.Data$Return    <- lag(momentum.Data$Return, n = 1)
momentum.Data$Return[1] <- 0

# Calculamos los retornos de la estrategia
momentum.Data$Acum.Ret <- cumprod(1 + momentum.Data$Return) - 1
momentum.Data$drawdown <- (momentum.Data$Acum.Ret + 1) / cummax(momentum.Data$Acum.Ret + 1) - 1

# Contabilizamos el tiempo que estamos en cada activo
US   <- 0
INT  <- 0
BOND <- 0
for(i in 1:nrow(momentum.Data)){
  if(momentum.Data$Asset[i] == "US"){
    US <- US + 1
  }else if(momentum.Data$Asset[i] == "INT"){
    INT <- INT + 1
  }else{
    BOND <- BOND + 1
  }
}

position <- t(round(data.frame(US, INT, BOND)/sum(data.frame(US, INT, BOND)), digits = 4))*100
colnames(position) <- "Weight(%)"
position

# Graficamos
backtest.Retorno <- data.frame(momentum.Data$DATES,
                               momentum.Data$Acum.Ret,
                               momentum.Data$drawdown,
                               momentum.Data$col)
colnames(backtest.Retorno) <- c("Dates", "Returns", "Drawdown", "Col")

ggplot(data = backtest.Retorno) + 
  geom_line(aes(x = Dates, y = Returns, colour = Col, group = 1), size = 1.2) +
  scale_colour_identity() +
  scale_y_continuous(labels = scales::percent) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.caption = element_text(hjust = 0),
        plot.subtitle = element_text(face = "italic",size = 9),
        plot.title = element_text(face = "bold", size = 14)) +
  labs(title = "Dual Momentum Strategy",
       subtitle = "Rebalancing between US-Equity (GREEN), Int-Equity (RED) and US-Bonds (BLUE) according to their relative and absolute momentum.",
       caption = paste("By: Carlos Jimenez.\nSource of historical prices: Yahoo Finance\nStatistics (time % invested in each asset type): ", position[1], "% in US-Equity, ", position[2], "% in Int-Equity and ", position[3], "% in US-Bonds", sep = ""),
       y = "Acum Return (%)", 
       x = "Date")

ggplot(data = backtest.Retorno) + 
  geom_line(aes(x = Dates, y = Drawdown), colour = "blue", size = 0.9) +
  scale_y_continuous(labels = scales::percent) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.caption = element_text(hjust = 0),
        plot.subtitle = element_text(face = "italic",size = 9),
        plot.title = element_text(face = "bold", size = 14)) +
  labs(title = "Dual Momentum Strategy Drawdown",
       subtitle = paste("Max Drawdown was: ", round(min(backtest.Retorno$Drawdown), digits = 4)*100, "% and the Average Drawdown was: ", round(mean(backtest.Retorno$Drawdown), digits = 4)*100, "%.", sep = ""),
       caption = "By: Carlos Jimenez.\nSource of historical prices: Yahoo Finance",
       y = "Drawdown", 
       x = "Date")

# Creacion del calendario de retornos
calendarReturns <- data.frame(momentum.Data$Return)
row.names(calendarReturns) <- momentum.Data$DATES
colnames(calendarReturns) <- "Returns"

Performance <- table.AnnualizedReturns(calendarReturns,
                                       scale = 12,
                                       Rf = 0.00/12)

calendarReturns       <- as.xts(calendarReturns)
tablaRetornos         <- table.CalendarReturns(calendarReturns)
tablaRetornos$Periods <- row.names(tablaRetornos)

tablaRetornos <- tablaRetornos[, c(14,1,2,3,4,5,6,7,8,9,10,11,12,13)]

tablaRetornos[is.na(tablaRetornos)] <- ""

colnames(tablaRetornos) <- c("PERIODS",
                             "JAN",
                             "FEB",
                             "MAR",
                             "APR",
                             "MAY",
                             "JUN",
                             "JUL",
                             "AGO",
                             "SEP",
                             "OCT",
                             "NOV",
                             "DEC",
                             "TotalReturn")

tablaRetornos %>%
  mutate(
    JAN = cell_spec(JAN, color = ifelse(JAN < 0, "red", "blue"), bold = T),
    FEB = cell_spec(FEB, color = ifelse(FEB < 0, "red", "blue"), bold = T),
    MAR = cell_spec(MAR, color = ifelse(MAR < 0, "red", "blue"), bold = T),
    APR = cell_spec(APR, color = ifelse(APR < 0, "red", "blue"), bold = T),
    MAY = cell_spec(MAY, color = ifelse(MAY < 0, "red", "blue"), bold = T),
    JUN = cell_spec(JUN, color = ifelse(JUN < 0, "red", "blue"), bold = T),
    JUL = cell_spec(JUL, color = ifelse(JUL < 0, "red", "blue"), bold = T),
    AGO = cell_spec(AGO, color = ifelse(AGO < 0, "red", "blue"), bold = T),
    SEP = cell_spec(SEP, color = ifelse(SEP < 0, "red", "blue"), bold = T),
    OCT = cell_spec(OCT, color = ifelse(OCT < 0, "red", "blue"), bold = T),
    NOV = cell_spec(NOV, color = ifelse(NOV < 0, "red", "blue"), bold = T),
    DEC = cell_spec(DEC, color = ifelse(DEC < 0, "red", "blue"), bold = T),
    TotalReturn = cell_spec(TotalReturn, color = ifelse(TotalReturn < 0, "red", "blue"), bold = T)
  ) %>%
  select(everything()) %>%
  kable(escape = F, align = "c") %>%
  kable_styling("striped", full_width = F)  %>%
  footnote(general = "",
           number = c("All the numbers are in %",
                      paste("CAGR Since Inception is: ", round(Performance[1,1]*100,3), "%"),
                      paste("Date of the Report: ", Sys.Date(), sep = "")))
