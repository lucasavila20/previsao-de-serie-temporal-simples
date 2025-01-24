# Carregar pacotes necessários
library(forecast)
library(tseries)
library(dplyr)
# Dataset original
# https://www.kaggle.com/datasets/uciml/electric-power-consumption-data-set 

dados <- read.table("household_power_consumption.txt", sep = ";",
                    header = TRUE) %>%
  tibble()
?as.Date
dados$Date <- as.Date(dados$Date, format = "%d/%m/%Y")
dados$Global_active_power <- as.numeric(dados$Global_active_power)
dados$Global_reactive_power <- as.numeric(dados$Global_reactive_power)
dados$Voltage <- as.numeric(dados$Voltage)
dados$Global_intensity <- as.numeric(dados$Global_intensity)
dados$Sub_metering_1 <- as.numeric(dados$Sub_metering_1)
dados$Sub_metering_2 <- as.numeric(dados$Sub_metering_2)
dados$Sub_metering_3 <- as.numeric(dados$Sub_metering_3)

for(i in 3:9){
  dados[is.na(dados[,i]), i] <- mean(unlist(dados[, i]), na.rm = TRUE)
}

dados <- dados %>%
  group_by(Date) %>%
  reframe(gap = sum(Global_active_power)*1000/60,
          volt = sum(Voltage),
          sm1 = sum(Sub_metering_1),
          sm2 = sum(Sub_metering_2),
          sm3 = sum(Sub_metering_3))

dados$fds <- c(0,1,1, rep(c(0,0,0,0,0,1,1), times = 205), 0,0,0,0)
dados$temp <- (dados$sm3-mean(dados$sm3))*3/sd(dados$sm3) + 20
range(dados$temp)
write.csv(dados, "consumo.csv", row.names = FALSE)

######### Modelagem


dados %>%
  mutate(mesano = paste(month(Date), "-", year(Date), sep = ""))


dados2 <- dados %>%
  mutate(mesano = paste(month(Date), "-", year(Date), sep = "")) %>%
  group_by(mesano) %>%
  reframe(gap = mean(gap),
          temp = mean(temp))


dados2

dados2 <- dados %>%
  mutate(mesano = paste("01/", month(Date), "/", year(Date), sep = "")) %>%
  mutate(mesano = as.Date(mesano, 
                          format = "%d/%m/%Y")) %>%
  group_by(mesano) %>%
  reframe(gap = sum(gap),
          temp = mean(temp))

dados2

acf(dados2$gap, lag.max = 48)
pacf(dados2$gap, lag.max = 48)

  # Carregar os dados22 e criar divisão treino/teste
# Supondo que os dados2 já estão em um data frame chamado `dados2` com colunas "Date" e "gap"

# Divisão dos dados2 em treino e teste
treinamento <- dados2 %>%
  filter(mesano >= as.Date("2006-12-01") & mesano <= as.Date("2009-11-25"))

teste <- dados2 %>%
  filter(mesano >= as.Date("2009-11-26") & mesano <= as.Date("2010-11-26"))

# Criar séries temporais para treino e teste
serie_treino <- ts(treinamento$gap, 
                   start = c(2006, 12), 
                   end = c(2009,11),
                   frequency = 12)

serie_teste <- ts(teste$gap, 
                  start = c(2009, 12), 
                  end = c(2010, 11),
                  frequency = 12)

# Teste de estacionariedade
adf_test <- tseries::adf.test(serie_treino)
print(adf_test)

# Identificação do modelo com auto.arima
auto_model <- auto.arima(serie_treino)
print(summary(auto_model))

# Ajuste do modelo SARIMA com os parâmetros encontrados
sarima_model <- Arima(
  serie_treino, 
  order = c(0, 0, 1), 
  seasonal = list(order = c(1, 1, 0), period = 12)
)
print(summary(sarima_model))

# Previsão para o período de teste
forecast_values <- forecast(sarima_model, h = length(serie_teste))

# Gráfico comparando os dados2 reais e previstos
plot(
  serie_teste, 
  col = "black", lwd = 2, 
  main = "Comparação: dados2 Reais vs Previsões",
  ylab = "Gap", xlab = "Tempo"
)
lines(forecast_values$mean, col = "red", lwd = 2) # Previsões em vermelho
legend(
  "topright", 
  legend = c("dados2 reais", "Previsões"), 
  col = c("black", "red"), 
  lty = 1, lwd = 2
)



# Gráfico comparando os dados2 reais e previstos
plot(
  c(serie_treino, serie_teste), 
  col = "black", lwd = 2, 
  main = "Comparação: dados2 Reais vs Previsões",
  ylab = "Gap", xlab = "Tempo", type = "l"
)

lines(y = forecast_values$mean,
      x = (length(c(serie_treino, serie_teste)) - 11):length(c(serie_treino, serie_teste)),
      col = "red", lwd = 2) # Previsões em vermelho
legend(
  "topright", 
  legend = c("dados2 reais", "Previsões"), 
  col = c("black", "red"), 
  lty = 1, lwd = 2
)

plot(y = dados2$gap, x = 1:nrow(dados2), type = "l")


df <- data.frame(Data = dados2$mesano,
                 valorreal = dados2$gap,
                 previsao = c(rep("", times = length(serie_treino)), forecast_values$mean),
                 inf = c(rep("", times = length(serie_treino)), forecast_values$lower[,2]),
                 sup = c(rep("", times = length(serie_treino)), forecast_values$upper[,2]))
df

write.table(df, "serietemporal2.txt", dec = ",", row.names = FALSE)


plot(forecast_values)
forecast_values$lower
df$inferior <- forecast_values$lower[,2]