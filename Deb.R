library(readr)
weather_data <- read_csv("weatherHistory.csv")
View(weather_data)
str(weather_data)
table(weather_data$Summary)
unique(weather_data$CloudCver)


table(weather_data$PrecipType)

weather_data[(weather_data$`Precip Type` == "null"),]$`Precip Type` <- "other"

weather_data <- weather_data[,c(-1,-2,-10,-12)]  
colnames(weather_data) <- c("PrecipType", "Temperature", "ApparentTemperature", "Humidity", "WindSpeed", "WindBearing", "Visibility", "Pressure")

weather_data$PrecipType <- as.factor(weather_data$PrecipType)

#=====================================================================

weather_model_int <- lm(Visibility ~ (. - ApparentTemperature)^2, data = weather_data)

coef(weather_model_int)

summary(weather_model_int)

summary(weather_model_int)$r.squared


plot(fitted(weather_model_int), resid(weather_model_int), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

#=========================================================================

sum(cooks.distance(weather_model_int) > 4/nrow(weather_data))

weather_model_int_low_influence <- lm(Visibility ~ (. - ApparentTemperature)^2, data = weather_data, 
                                  subset = cooks.distance(weather_model_int) <= 4/nrow(weather_data))

summary(weather_model_int_low_influence)

summary(weather_model_int_low_influence)$r.squared

plot(fitted(weather_model_int_low_influence), resid(weather_model_int_low_influence), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

#====================================================================

weather_model_int_bic <- step(weather_model_int, k=log(nrow(weather_data)), trace = 0)

summary(weather_model_int_bic)$r.squared

plot(fitted(weather_model_int_bic), resid(weather_model_int_bic), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

sum(cooks.distance(weather_model_int_bic) > 4/nrow(weather_data))

summary(weather_model_int_bic)$r.squared
#===========================================================================

weather_model_log_int <- lm(log(Visibility) ~ (. - ApparentTemperature)^2, data = weather_data,
                            subset = is.finite(log(weather_data$Visibility)))

weather_model_log_int_1 <- lm(log(Visibility) ~ (. - ApparentTemperature)^2, data = weather_data)

any(is.na(log(weather_data$Visibility)))
any(is.nan(log(weather_data$Visibility)))

any(is.infinite(log(weather_data$Visibility)))

any(is.finite(log(weather_data$Visibility)))


plot(fitted(weather_model_log_int), resid(weather_model_log_int), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

summary(weather_model_log_int)$r.squared


#==============================================================================

weather_model_poly <- lm(Visibility ~ (. - ApparentTemperature) + 
                           I(Temperature^2) +
                           I(Humidity^2) +
                           I(WindSpeed^2) +
                           I(WindBearing^2) +
                           I(Pressure^2) , data = weather_data)

summary(weather_model_poly)$r.squared


weather_model_poly_bic <- step(weather_model_poly, k=log(nrow(weather_data)), trace = 0)



summary(weather_model_poly_bic)$r.squared

plot(fitted(weather_model_log_int), resid(weather_model_log_int), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

#=================================================================


weather_model_poly_log <- lm(log(Visibility) ~ (. - ApparentTemperature) + 
                           I(Temperature^2) +
                           I(Humidity^2) +
                           I(WindSpeed^2) +
                           I(WindBearing^2) +
                           I(Pressure^2) , data = weather_data,
                           subset = is.finite(log(weather_data$Visibility)))

summary(weather_model_poly_log)$r.squared

weather_model_poly_log_bic <- step(weather_model_poly_log, k=log(nrow(weather_data)), trace = 0)

summary(weather_model_poly_log_bic)$r.squared
#======================================================================

weather_model_add <- lm(Visibility ~ . - ApparentTemperature, data = weather_data)

summary(weather_model_add)$r.squared

weather_model_add_small <- lm(Visibility ~ Temperature + Humidity + WindSpeed + WindBearing + Pressure, data = weather_data)


weather_model_add_small_cooks <- lm(Visibility ~ Temperature + Humidity + WindSpeed + WindBearing + Pressure, data = weather_data, subset = cooks.distance(weather_model_add_small) <= 4/nrow(weather_data))

summary(weather_model_add_small)$r.squared

summary(weather_model_add_small_cooks)$r.squared

weather_model_add <- lm(Visibility ~ . - ApparentTemperature, data = weather_data)

summary(weather_model_add)$r.squared

weather_model <- lm(Visibility ~ 1, data = weather_data)

weather_model_add_select <- step(weather_model, scope = Visibility ~ Temperature + Humidity + WindSpeed + WindBearing + Pressure, k = log(nrow(weather_data)), trace = 0, direction = "forward")

summary(weather_model_add_select)

weather_model_add_small <- lm(Visibility ~ Temperature + Humidity, data = weather_data)

summary(weather_model_add_small)

#============================================================================

weather_model_poly_degree2 <- lm(Visibility ~ 
                           I(Temperature^2) +
                           I(Humidity^2) +
                           I(WindSpeed^2) +
                           I(WindBearing^2) +
                           I(Pressure^2) , data = weather_data)
