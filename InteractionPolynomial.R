---
title: "Project Report - Data Juggernaut"
author: "Debajit Mitra; Bipin Chandra Karnati; Varun Sharma"
date: '8/3/2020'
output:
  html_document: 
    highlight: haddock
    theme: spacelab
    toc: yes
urlcolor: cyan
editor_options: 
  chunk_output_type: console
---

***

```{r, message=FALSE}
library(readr)
weather_data <- read_csv("weatherHistory.csv")

table(weather_data$PrecipType)

weather_data[(weather_data$`Precip Type` == "null"),]$`Precip Type` <- "other"

weather_data <- weather_data[,c(-1,-10,-12)]  
colnames(weather_data) <- c("Summary" , "PrecipType", "Temperature", "ApparentTemperature", "Humidity", "WindSpeed", "WindBearing", "Visibility", "Pressure")

weather_data$PrecipType <- as.factor(weather_data$PrecipType)


```



## Below is our analysis with interaction model having all the predictors :

```{r}
# Interaction Model
weather_model_int = lm(Visibility ~ .^2, data = weather_data)

rsquared_int = summary(weather_model_int)$r.squared

plot(fitted(weather_model_int), resid(weather_model_int), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)
```

- The R-Squared value for the interaction model is $`r round(rsquared_int*100,2)`$, but based on the Fitted vs Residual plot we can clearly see that the model assumptions has been violated.
- We are also seeing some influencial observations affecting the regression, so we decided to remove them and fit the model again expecting the significance of regression will improve.
- We are removing $`r sum(cooks.distance(weather_model_int) > 4/nrow(weather_data))`$, influencial observations from our fitted model.

```{r}
# Interaction Model after removing Influencial Observation
weather_model_int_low_influence = lm(Visibility ~ .^2, data = weather_data, 
                            subset = cooks.distance(weather_model_int) <= 4/nrow(weather_data))

rsquared__low_influence = summary(weather_model_int_low_influence)$r.squared

plot(fitted(weather_model_int_low_influence), resid(weather_model_int_low_influence), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)
```

- The R-Squared of the model after removing ifluencial observations is calculated to $`r round(rsquared_int*100,2)`$, which is a improved value than the previois model.
- But the Fitted vs Residual plot is still showing violations in assumptions. 
- With the expectation to improve the model, we are performing `Backword BIC` on the previous interaction model to see if it can better explain the variablity of our response.

```{r}
weather_model_int_bic = step(weather_model_int, k=log(nrow(weather_data)), trace = 0)

rsquared_int_bic = summary(weather_model_int_bic)$r.squared

```

- The R-Squared after performing the BIC is calculated to $`r round(rsquared_int_bic*100,2)`$. 
- And as we can see that the value is not improving much with the previous attempts on interaction models, we are trying on a log transformation of the Response

## Below is our analysis with log transformation and interaction with all predictors.

```{r}
any(is.infinite(log(weather_data$Visibility)))
```

- We found that there are some observations having `0` value in Visibility, which is failing the log transformation with *NAN* error. And we have decided to subset the observations only non-zero values in Visibility.

```{r}
weather_model_log_int = lm(log(Visibility) ~ .^2, data = weather_data,
                            subset = is.finite(log(weather_data$Visibility)))

plot(fitted(weather_model_log_int), resid(weather_model_log_int), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

rsquared_log_interaction = summary(weather_model_log_int)$r.squared
```

- We see a much improved value of R-Squared calculated to $`r round(rsquared_log_interaction*100,2)`$, but Fitted vs Residual plot showing violation with `Equal Variance` assumption of our linear model. 
- We are removing influencial observations from our data and fitting the model again to acheive a better plot.

```{r}
rows_removed = sum((as.numeric(weather_data$Visibility) == 0))
weather_data = weather_data[(as.numeric(weather_data$Visibility) != 0),]
```
r
- As `0` value in Visibility is not feasible for our Response, we have decided to remove $`r rows_removed`$ observation having `0` value for the Visibility colum.

```{r}
library(lmtest)
weather_model_log_int_low_influence = lm(log(Visibility) ~ .^2, data = weather_data,
                            subset = cooks.distance(weather_model_log_int) <= 4/nrow(weather_data))

plot(fitted(weather_model_log_int_low_influence), resid(weather_model_log_int_low_influence), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

rsquared_log_interaction_low_influence = summary(weather_model_log_int_low_influence)$r.squared

bptest(weather_model_log_int_low_influence)$p.value
```

- The Fitted vs Residual plot improved but still the Equal Varience and Linearity of the model is a suspect here. Also from a very low value of the bptest we can infer that the Normality assumptions has also been violated. 
- Thus although we have a high R-Squared value of $`r round(rsquared_log_interaction_low_influence*100,2)`$, the model is not useful.

## Below is our analysis with polynomial transformation.

```{r}
weather_model_poly = lm(Visibility ~ . + 
                           I(Temperature^2) +
                           I(Humidity^2) +
                           I(WindSpeed^2) +
                           I(WindBearing^2) +
                           I(Pressure^2) , data = weather_data)

rsquared_poly = summary(weather_model_poly)$r.squared


plot(fitted(weather_model_poly), resid(weather_model_poly), col = "grey", pch = 20,
xlab = "Fitted", ylab = "Residuals", main = "Data from Model 1")
abline(h = 0, col = "darkorange", lwd = 2)

```

- The R-Squared value, `r round(rsquared_poly*100,2)` did'nt improved much from our previous model and the Fitted vs Residual plot also showing violations of assumptions.
