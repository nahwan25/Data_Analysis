library(tseries)
library(forecast)
library(starma)
library(tseries)
library(MASS)
library(Metrics)
library(TSA)


# Input data
library(readxl)
Data_AAPL <- read_excel("C:/Users/ThinkPad/Downloads/Tubes ADW/Data Tubes AAPL 2022-2024.xlsx")
View(Data_AAPL)
View(Data_AAPL)
View(Data_AAPL)
dim(Data_AAPL)         
str(Data_AAPL)

# Visualize missing values in the dataset
sapply(Data_AAPL, function(x) sum(is.na(x)))
library(visdat) 
vis_miss(Data_AAPL)
summary(Data_AAPL)

# Mengonversi kolom numerik dengan koma desimal tetap terjaga
Data_AAPL$Terakhir <- as.numeric(gsub(",", ".", Data_AAPL$Terakhir))
Data_AAPL$Pembukaan <- as.numeric(gsub(",", ".", Data_AAPL$Pembukaan))
Data_AAPL$Tertinggi <- as.numeric(gsub(",", ".", Data_AAPL$Tertinggi))
Data_AAPL$Terendah <- as.numeric(gsub(",", ".", Data_AAPL$Terendah))
#Menghilangkan simbol %
Data_AAPL$`Perubahan%` <- as.numeric(gsub("%", "", gsub(",", ".", Data_AAPL$`Perubahan%`)))
# Menghapus karakter 'M' dan mengonversi ke numerik (dikalikan 1 juta)
Data_AAPL$Vol. <- as.numeric(gsub(",", ".", gsub("M", "", Data_AAPL$Vol.))) * 1e6

# Deskripsi statistika
summary(Data_AAPL)

# Summing the values of a specific column (assuming it's called "Close")
AAPL = ts(Data_AAPL[,2])
AAPL

# 1 Plot data deret waktu
ts.plot(AAPL, main = "TS: AAPL")

# 2. Plot Korelogram ACF dan PACF
library(forecast)
par(mfrow=c(2,1))
Acf(AAPL, lag.max=24)
Pacf(AAPL, lag.max=24)

# 3. Pengujian stasioner dengan uji ADf(Mean) dan BxCox (Varians)
# Uji ADF(Mean)
adf.test(AAPL)
# Uji Box-Cox(Varians)
u1 <- boxcox(lm(AAPL ~ 1))  # Mencari nilai lambda terbaik
(lambda1 <- u1$x[which.max(u1$y)])
# Transformasi data menggunakan nilai lambda
data_var = matrix(0, nrow = 713, ncol = 1)
data_var = (AAPL^lambda1 - 1) / lambda1
plot(data_var)

# 4. Tranformasi data
# 4.1 Diference 1
AAPL.diff1=diff(data_var, difference = 1)

# 4.2 Plot data deret waktu dif 1
ts.plot(AAPL.diff1, main="TS: AAPL Differensi orde 1")

# 4.3 Plot Korelogram ACF dan PACF dif 1
library(forecast)
par(mfrow=c(2,1))
Acf(AAPL.diff1, lag.max=24)
Pacf(AAPL.diff1, lag.max=24)

# 4.4 Pengujian stasioner dengan uji ADf(Mean) dif 1
adf.test(AAPL.diff1)

# Menerapkan analisis EACF
eacf_result <- eacf(AAPL.diff1)

# Menampilkan hasil EACF
print(eacf_result)

# 4. Tranformasi data
# 4.1 Diference 2
AAPL.diff2=diff(data_var, difference = 2)

# 4.2 Plot data deret waktu dif 2
ts.plot(AAPL.diff2, main="TS: AAPL Differensi orde 2")

# 4.3 Plot Korelogram ACF dan PACF dif 1
library(forecast)
par(mfrow=c(2,1))
Acf(AAPL.diff2, lag.max=24)
Pacf(AAPL.diff2, lag.max=24)

# 4.4 Pengujian stasioner dengan uji ADf(Mean) dif 1
adf.test(AAPL.diff2)

# Membuat model ARIMA terlebih dahulu
Close.ARIMA.1.1.1 <- arima(data_var, order=c(1,1,1))
Close.ARIMA.0.1.1 <- arima(data_var, order=c(0,1,1))
Close.ARIMA.1.1.0 <- arima(data_var, order=c(1,1,0))
Close.ARIMA.2.1.2 <- arima(data_var, order=c(2,1,2))
Close.ARIMA.0.1.2 <- arima(data_var, order=c(0,1,2))
Close.ARIMA.2.1.0 <- arima(data_var, order=c(2,1,0))

# Parameter
Close.ARIMA.1.1.1 
Close.ARIMA.0.1.1 
Close.ARIMA.1.1.0 
Close.ARIMA.2.1.2 
Close.ARIMA.0.1.2 
Close.ARIMA.2.1.0 

# 5. Mencari nilai AIC terkecil
# Molina: AIC
Close.ARIMA.1.1.1$aic

Close.ARIMA.0.1.1$aic

Close.ARIMA.1.1.0$aic

Close.ARIMA.2.1.2$aic

Close.ARIMA.0.1.2$aic

Close.ARIMA.2.1.0$aic

Close.ARIMA = c("ARIMA(1,1,1)", "ARIMA(0,1,1)", "ARIMA(1,1,0)", "ARIMA(2,1,2)", "ARIMA(0,1,2)", "ARIMA(2,1,0)")
AIC = c(Close.ARIMA.1.1.1$aic, Close.ARIMA.0.1.1$aic, Close.ARIMA.1.1.0$aic, Close.ARIMA.2.1.2$aic, Close.ARIMA.0.1.2$aic, Close.ARIMA.2.1.0$aic)
Close.AIC = cbind(Close.ARIMA, AIC)
Close.AIC

# 6. Uji Diagnostik
# 6.1 Plot Residual
Res.Close.Arima012 = residuals(Close.ARIMA.0.1.2)
plot(Res.Close.Arima012, ylab = "Residual", type = 'o')
abline(h = 0, col = 'red')

# 6.2 Plot Residual Terstandarkan
library(TSA)
Res.Std.Close.ARIMA012 = rstandard(Close.ARIMA.0.1.2)
plot(Res.Std.Close.ARIMA012, ylab = "Residual Terstandarkan", type = "o")
abline(h=0, col = "red")

# 6.3 Plot Kuantil Kenormalan
qqnorm(Res.Close.Arima012)
qqline(Res.Close.Arima012)

# 6.4 ACF Residual
acf(Res.Close.Arima012)

# 6.5 LJung Box
tsdiag(Close.ARIMA.0.1.2, gof.lag = 25)

# Residual dari model ARIMA(0,1,2)
Res.Close.ARIMA012 <- residuals(Close.ARIMA.0.1.2)
shapiro.test(Res.Close.ARIMA012)

# 7. Prediksi Model ARIMA(0,1,2)
forecast = predict(Close.ARIMA.0.1.2, 12)
forecast$pred

# Nilai prediksi di skala Box-Cox
boxcox_pred <- forecast$pred

# Mengembalikan prediksi ke skala asli
if (lambda1 != 0) {
  original_pred <- (boxcox_pred * lambda1 + 1)^(1 / lambda1)
} else {
  original_pred <- exp(boxcox_pred)
}

# Hasil prediksi di skala asli
print(original_pred)

# Mengambil parameter model ARIMA(0,1,2)
params <- coef(Close.ARIMA.0.1.2)  # Parameter model ARIMA
se <- sqrt(diag(vcov(Close.ARIMA.0.1.2)))  # Standar error parameter
aic <- AIC(Close.ARIMA.0.1.2)  # AIC
sigma2 <- Close.ARIMA.0.1.2$sigma2  # Varians residual
loglik <- logLik(Close.ARIMA.0.1.2)  # Log-likelihood

# Menghitung nilai fitted
fitted_values <- fitted(Close.ARIMA.0.1.2)
plot(fitted_values)

# Memisahkan data aktual untuk validasi
train_length <- length(AAPL) - 12
train_data <- AAPL[1:train_length]
test_data <- AAPL[(train_length + 1):length(AAPL)]

# Forecast dengan model ARIMA(0,1,2)
forecast_test <- predict(Close.ARIMA.0.1.2, n.ahead = 12)

# Mengembalikan prediksi ke skala asli
if (lambda1 != 0) {
  original_forecast <- (forecast_test$pred * lambda1 + 1)^(1 / lambda1)
} else {
  original_forecast <- exp(forecast_test$pred)
}

# Input data
library(readxl)
Data_Historis_AAPL_Baru <- read_excel("C:/Users/ThinkPad/Downloads/Tubes ADW/Data Historis AAPL Baru.xlsx")
View(Data_Historis_AAPL_Baru)

# Input data
library(readxl)
Data_Historis_AAPL_Baru <- read_excel("C:/Users/ThinkPad/Downloads/Tubes ADW/Data Historis AAPL Baru.xlsx")
View(Data_Historis_AAPL_Baru)

# Mengonversi kolom numerik dengan koma desimal tetap terjaga
Data_Historis_AAPL_Baru$Terakhir <- as.numeric(gsub(",", ".", Data_Historis_AAPL_Baru$Terakhir))
Data_Historis_AAPL_Baru$Pembukaan <- as.numeric(gsub(",", ".", Data_Historis_AAPL_Baru$Pembukaan))
Data_Historis_AAPL_Baru$Tertinggi <- as.numeric(gsub(",", ".", Data_Historis_AAPL_Baru$Tertinggi))
Data_Historis_AAPL_Baru$Terendah <- as.numeric(gsub(",", ".", Data_Historis_AAPL_Baru$Terendah))

# Menghilangkan simbol % dan mengonversi ke numerik
Data_Historis_AAPL_Baru$`Perubahan%` <- as.numeric(gsub("%", "", gsub(",", ".", Data_Historis_AAPL_Baru$`Perubahan%`)))

# Menghapus karakter 'M' dan mengonversi ke numerik (dikalikan 1 juta)
Data_Historis_AAPL_Baru$Vol. <- as.numeric(gsub(",", ".", gsub("M", "", Data_Historis_AAPL_Baru$Vol.))) * 1e6

# Mengambil kolom kedua (misalnya, "Close") dan mengonversinya menjadi time series
AAPL2 = ts(Data_Historis_AAPL_Baru[,2])

# Menentukan panjang data pelatihan (train) dan pengujian (test)
train_length <- length(AAPL2) - 12  # Data pelatihan adalah semua data kecuali 12 terakhir
train_data <- AAPL2[1:train_length]  # Data pelatihan
test_data <- AAPL2[(train_length + 1):length(AAPL2)]  # 12 data terakhir untuk pengujian

# Melihat hasilnya
train_data
test_data

# 1. Transformasi data uji menggunakan transformasi Box-Cox
if (any(test_data <= 0)) {
  stop("Data uji mengandung nilai non-positif; transformasi Box-Cox tidak dapat diterapkan.")
}
test_data_transformed <- (test_data^lambda1 - 1) / lambda1

# 2. Transformasi data pelatihan menggunakan transformasi Box-Cox
if (any(train_data <= 0)) {
  stop("Data pelatihan mengandung nilai non-positif; transformasi Box-Cox tidak dapat diterapkan.")
}
train_data_transformed <- (train_data^lambda1 - 1) / lambda1

# 3. Pembangunan model ARIMA pada data pelatihan yang ditransformasi
# Tentukan parameter ARIMA yang sesuai; misalnya, ARIMA(0,1,2)
model <- Arima(train_data_transformed, order = c(0, 1, 2))

# 4. Prediksi menggunakan model ARIMA
forecast_transformed <- forecast(model, h = length(test_data_transformed))

# 5. Uji Shapiro-Wilk pada residual prediksi
residuals <- test_data_transformed - forecast_transformed$mean
shapiro_test <- shapiro.test(residuals)

# Tampilkan hasil uji Shapiro-Wilk
cat("Shapiro-Wilk Test p-value:", shapiro_test$p.value, "\n")


