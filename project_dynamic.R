library(readxl)
project_dyn <- read_excel("C:/Users/moahm/OneDrive/Desktop/project_dyn/project_dyn.xlsx", 
                          col_types = c("date", "numeric", "numeric", 
                                        "numeric"))
library(forecast)
library(tseries)
#transform data to tseries
project_dy<- ts(project_dyn)
#test if data is stationary
adf.test(project_dy[,2])
p_values <- 0:10
d_values <- 0:1
q_values <- 0:10

# Initialize variables to store the best model and its AIC
best_model <- NULL
best_aic <- Inf

# Loop through all possible combinations of p, d, and q values
for (p in p_values) {
  for (d in d_values) {
    for (q in q_values) {
      # Fit an ARIMA model with the current p, d, and q values
      model <- arima(project_dy[,2], order = c(p, d, q))
      if (!any(is.na(diag(model$var.coef)))) {
        # Calculate the AIC
        aic <- AIC(model)
        # Check if the current model has a lower AIC than the best model so far
        if (aic < best_aic) {
          best_aic <- aic
          best_model <- model
        }
      }
    }
  }
}
best_model
p
d
q
model_price2<- arima(project_dy[,2],order = c(3,1,3))
model_price2
risiduals<-checkresiduals(model_price2)
Box.test(model_price2$residuals,type = "Ljung-Box", lag= 10 )
fit3<-forecast(model_price2,  h=6)
fit3
plot(fit3)
#make arima model with auto generated order (1,0,1)
model_price<- auto.arima(project_dy[,2],seasonal = TRUE, xreg= as.matrix(project_dy[,3:4]))
model_price

#forecast the future values of the external regressors that will be used to forecast the price
daysdestroyed_<- forecast(project_dy[,3], h = 6)
costpertransaction_<-forecast(project_dy[,4], h=6)
#combine the forecast of the external regressors to 1 matrix
newxreg= as.matrix(cbind(daysdestroyed_$mean,costpertransaction_$mean))
#forecast the data
fit2<-forecast(model_price, xreg=newxreg, h=6)
fit2
#check for lag and distribution
residual<-checkresiduals(model_price)
#test for the auto-correlation of the residuals
Box.test(model_price$residuals,type = "Ljung-Box", lag= 20 )



