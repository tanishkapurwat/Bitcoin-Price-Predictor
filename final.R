library(tidyverse)
library(lubridate)
library(caret)
library(ggplot2)
library(car)
library(segmented)
library(strucchange)

data <- read.csv("merged_fix_to_hour.csv")
summary(data)

head(data)

#dealing with missing values
missing_summary <- colSums(is.na(data))
missing_summary[missing_summary > 0] 
data <- data %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

#normalisation (min-max scaling) of numeric coloumns
num_cols <- data %>% select(where(is.numeric)) %>% names()
data[num_cols] <- lapply(data[num_cols], function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))

#time stamps into features
data$Datetime <- ymd_hms(data$Datetime)  # correct column name
data$hour <- hour(data$Datetime)
data$day_of_week <- wday(data$Datetime, label = TRUE)
data$weekend <- ifelse(data$day_of_week %in% c("Sat", "Sun"), 1, 0)
data <- data %>% select(-Datetime)

glimpse(data)

#drop coloumns with too many or all NaN values
volume_cols <- names(data)[str_detect(names(data), "Volume")]
useless_volumes <- volume_cols[sapply(data[volume_cols], function(col) all(is.na(col) | col == 0))]
data <- data %>% select(-all_of(useless_volumes))
data <- data %>% filter(!is.na(hour))

glimpse(data)

#check correlation

num_data <- data %>% select(where(is.numeric))
cor_matrix <- cor(num_data, use = "complete.obs")
cor_target <- cor_matrix[, "BTC_USDT_1h_close"]

print(sort(cor_target, decreasing = TRUE)[2:11])

cor_target <- cor(data, use = "complete.obs")[, "BTC_USDT_1h_close"]
top_corr_vars <- sort(cor_target, decreasing = TRUE)[2:11]
corr_df <- data.frame(
  Variable = names(top_corr_vars),
  Correlation = as.numeric(top_corr_vars)
)

ggplot(corr_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Positive Correlations with BTC_USDT_1h_close",
       x = "Variables", y = "Correlation") +
  theme_minimal()

#highly correlated predictors are removed to avoid multicollinearity
data <- data %>% select(-c(BTC_USDT_1h_open, BTC_USDT_1h_high, BTC_USDT_1h_low))

#looking at aliased variables
alias(vif_model)

data_vif <- data %>% select(-day_of_week)
vif_model_clean <- lm(BTC_USDT_1h_close ~ ., data = data_vif)
vif_values <- vif(vif_model_clean)
vif_sorted <- sort(vif_values, decreasing = TRUE)
print(vif_sorted)

drop_vif_vars <- c(
  "S.P_Open..GSPC", "S.P_High..GSPC", "S.P_Low..GSPC",
  "NASDAQ_High..IXIC", "NASDAQ_Low..IXIC", "NASDAQ_Open..IXIC",
  "Dow_High..DJI", "Dow_Low..DJI", "Dow_Open..DJI",
  "DAX_High..GDAXI", "DAX_Low..GDAXI", "DAX_Open..GDAXI",
  "EURO_High..STOXX50E", "EURO_Low..STOXX50E", "EURO_Open..STOXX50E",
  "FTSE_High..FTSE", "FTSE_Low..FTSE", "FTSE_Open..FTSE",
  "CAC_High..FCHI", "CAC_Low..FCHI", "CAC_Open..FCHI",
  "IBOVESPA_High..BVSP", "IBOVESPA_Low..BVSP", "IBOVESPA_Open..BVSP",
  "SOL_USDT_1h_open", "SOL_USDT_1h_high", "SOL_USDT_1h_low",
  "ETH_USDT_1h_open", "ETH_USDT_1h_high", "ETH_USDT_1h_low",
  "DOGE_USDT_1h_open", "DOGE_USDT_1h_high", "DOGE_USDT_1h_low",
  "BNB_USDT_1h_open", "BNB_USDT_1h_high", "BNB_USDT_1h_low",
  "XRP_USDT_1h_open", "XRP_USDT_1h_high", "XRP_USDT_1h_low"
)

data_clean <- data_vif %>% select(-any_of(drop_vif_vars))

vif_model_final <- lm(BTC_USDT_1h_close ~ ., data = data_clean)
vif_final <- vif(vif_model_final)
print(sort(vif_final, decreasing = TRUE))

drop_more_vif_vars <- c(
  "S.P_High..GSPTSE", "S.P_Low..GSPTSE", "S.P_Open..GSPTSE",
  "gold_High.GC.F", "gold_Open.GC.F",
  "silver_High.SI.F", "silver_Open.SI.F", "silver_Low.SI.F",
  "Russell_High..RUT", "Russell_Low..RUT", "Russell_Open..RUT",
  "crude_High.CL.F", "crude_Low.CL.F", "crude_Open.CL.F",
  "IPC_High..MXX", "IPC_Low..MXX", "IPC_Open..MXX",
  "soybeans_High.ZS.F", "soybeans_Low.ZS.F", "soybeans_Open.ZS.F",
  "corn_High.ZC.F", "corn_Low.ZC.F", "corn_Open.ZC.F",
  "cattle_High.LE.F", "cattle_Low.LE.F", "cattle_Open.LE.F"
)

data_vif_final <- data_clean %>% select(-any_of(drop_more_vif_vars))
vif_model_final2 <- lm(BTC_USDT_1h_close ~ ., data = data_vif_final)
vif_final2 <- vif(vif_model_final2)
print(sort(vif_final2, decreasing = TRUE))

drop_final_vars <- c(
  "gold_Low.GC.F", "gold_Close.GC.F",
  "S.P_Close..GSPC", "NASDAQ_Close..IXIC",
  "VIX_Close..VIX", "VIX_Low..VIX", "VIX_High..VIX", "VIX_Open..VIX",
  "Dow_Close..DJI", "EURO_Close..STOXX50E", "DAX_Close..GDAXI",
  "wheat_High.ZW.F", "wheat_Low.ZW.F", "wheat_Open.ZW.F"
)

data_vif_final3 <- data_vif_final %>% select(-any_of(drop_final_vars))

vif_model_final3 <- lm(BTC_USDT_1h_close ~ ., data = data_vif_final3)
vif_final3 <- vif(vif_model_final3)
print(sort(vif_final3, decreasing = TRUE))

predictors <- c(
  "funding_rate", "google_trends_buy_crypto", "S.P_Volume..GSPC", 
  "Dow_Volume..DJI", "IBOVESPA_Close..BVSP", "IPC_Close..MXX", 
  "google_trends_bitcoin", "cattle_Close.LE.F", "wheat_Close.ZW.F", 
  "silver_Volume.SI.F", "crude_Close.CL.F", "gold_Volume.GC.F", 
  "fear_gread_index", "ETH_USDT_1h_volume", "BTC_USDT_1h_volume", 
  "SOL_USDT_1h_volume", "S.P_Volume..GSPTSE", "DOGE_USDT_1h_volume", 
  "XRP_USDT_1h_volume", "BNB_USDT_1h_volume", "corn_Volume.ZC.F", 
  "soybeans_Volume.ZS.F", "NASDAQ_Volume..IXIC", "IPC_Volume..MXX",
  "weekend", "wheat_Volume.ZW.F", "crude_Volume.CL.F", "hour",
  "cattle_Volume.LE.F", "open_interest"
)

formula <- as.formula(paste("BTC_USDT_1h_close ~", paste(predictors, collapse = "+")))
model <- lm(formula, data = data)
summary(model)

# Residual Plot
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# Cross-validation
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(formula, data = data, method = "lm", trControl = train_control)

print(cv_model)

#piecewise linear model

lin_mod <- lm(BTC_USDT_1h_close ~ funding_rate, data = data)
seg_mod1 <- segmented(lin_mod, seg.Z = ~funding_rate, psi = median(data$funding_rate))
summary(seg_mod1)

seg_mod2 <- segmented(lin_mod, seg.Z = ~funding_rate,
                      psi = c(quantile(data$funding_rate, 0.33),
                              quantile(data$funding_rate, 0.66)))
summary(seg_mod2)

AIC(seg_mod1, seg_mod2)
BIC(seg_mod1, seg_mod2)

  # Estimated breakpoints
anova(seg_mod1, seg_mod2)

plot(data$funding_rate, data$BTC_USDT_1h_close,
     col = "gray", pch = 20, main = "Segmented Model (2 breakpoints)")
plot(seg_mod2, add = TRUE, col = "blue", lwd = 2)

base_model <- lm(BTC_USDT_1h_close ~ funding_rate, data = data)
seg_model <- segmented(base_model, seg.Z = ~funding_rate, npsi = 2)
summary(seg_model)

bp <- seg_model$psi
print("Estimated Breakpoints:")
print(bp)



predictors <- c(
  "funding_rate", "google_trends_buy_crypto", "S.P_Volume..GSPC", 
  "Dow_Volume..DJI", "IBOVESPA_Close..BVSP", "IPC_Close..MXX", 
  "google_trends_bitcoin", "cattle_Close.LE.F", "wheat_Close.ZW.F", 
  "silver_Volume.SI.F", "crude_Close.CL.F", "gold_Volume.GC.F", 
  "fear_gread_index", "ETH_USDT_1h_volume", "BTC_USDT_1h_volume", 
  "SOL_USDT_1h_volume", "S.P_Volume..GSPTSE", "DOGE_USDT_1h_volume", 
  "XRP_USDT_1h_volume", "BNB_USDT_1h_volume", "corn_Volume.ZC.F", 
  "soybeans_Volume.ZS.F", "NASDAQ_Volume..IXIC", "IPC_Volume..MXX",
  "weekend", "wheat_Volume.ZW.F", "crude_Volume.CL.F", "hour",
  "cattle_Volume.LE.F", "open_interest"
)
formula <- as.formula(paste("BTC_USDT_1h_close ~", paste(predictors, collapse = " + ")))

# Extend to all predictors
full_model <- lm(formula, data = data)
segmented_model <- segmented(full_model, seg.Z = ~ funding_rate, npsi = 2)
summary(segmented_model)

fitted_vals <- fitted(segmented_model)
residuals_vals <- resid(segmented_model)

# Residuals vs. Fitted Plot
plot(fitted_vals, residuals_vals,
     pch = 20, col = "darkgray",
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values (Segmented Model)")
abline(h = 0, col = "blue", lwd = 2)

#Removing outliers using Cook's distance
cooks_d <- cooks.distance(segmented_model)

n <- nrow(data)
threshold <- 4 / n

influential_obs <- which(cooks_d > threshold)

data_cleaned <- data[-influential_obs, ] #Remove influential points 

full_model_clean <- lm(as.formula(
  paste("BTC_USDT_1h_close ~", paste(predictors, collapse = " + "))
), data = data_cleaned)

segmented_model_clean <- segmented(
  full_model_clean,
  seg.Z = ~funding_rate,
  npsi = 2
)

summary(segmented_model_clean)

# Plot removed points in red and rest in grey
plot(data$funding_rate, data$BTC_USDT_1h_close, 
     col = ifelse(1:nrow(data) %in% influential_obs, "red", "gray"),
     pch = 20, main = "Influential points (Cook's Distance)",
     xlab = "Funding Rate", ylab = "BTC Close")

legend("topright", legend = c("Removed", "Kept"), 
       col = c("red", "gray"), pch = 20)

fitted_vals <- fitted(segmented_model_clean)
residuals_vals <- residuals(segmented_model_clean)

plot(
  fitted_vals, residuals_vals,
  col = "gray", pch = 20,
  main = "Residuals vs. Fitted Values (Cleaned Segmented Model)",
  xlab = "Fitted Values",
  ylab = "Residuals"
)
abline(h = 0, col = "blue", lwd = 2)

#Integrate three interaction terms
interaction_formula <- as.formula(
  paste(
    "BTC_USDT_1h_close ~",
    paste(predictors, collapse = " + "),
    "+ funding_rate:fear_gread_index",
    "+ funding_rate:weekend",
    "+ google_trends_buy_crypto:fear_gread_index"
  )
)

interaction_model <- lm(interaction_formula, data = data_cleaned)

segmented_model_interaction <- segmented(
  interaction_model,
  seg.Z = ~funding_rate,
  npsi = 2
)

summary(segmented_model_interaction)

fitted_vals_int <- fitted(segmented_model_interaction)
residuals_vals_int <- residuals(segmented_model_interaction)

plot(
  fitted_vals_int, residuals_vals_int,
  col = "gray", pch = 20,
  main = "Residuals vs. Fitted Values (Segmented Model with Interactions)",
  xlab = "Fitted Values",
  ylab = "Residuals"
)
abline(h = 0, col = "blue", lwd = 2)

#adding a non linear term

nonlinear_formula <- as.formula(
  paste(
    "BTC_USDT_1h_close ~",
    paste(predictors, collapse = " + "),
    "+ funding_rate:fear_gread_index",
    "+ funding_rate:weekend",
    "+ google_trends_buy_crypto:fear_gread_index",
    "+ I(funding_rate^2)"  #non-linear term added based on predictor used for break points
  )
)

nonlinear_model <- lm(nonlinear_formula, data = data_cleaned)

segmented_model_nonlinear <- segmented(
  nonlinear_model,
  seg.Z = ~funding_rate,
  npsi = 2
)

summary(segmented_model_nonlinear)

fitted_vals_nl <- fitted(segmented_model_nonlinear)
residuals_vals_nl <- residuals(segmented_model_nonlinear)

plot(
  fitted_vals_nl, residuals_vals_nl,
  col = "gray", pch = 20,
  main = "Residuals vs. Fitted Values (With funding_rate^2)",
  xlab = "Fitted Values",
  ylab = "Residuals"
)
abline(h = 0, col = "blue", lwd = 2)

#CV - 10 FOLD

cv_control <- trainControl(method = "cv", number = 10)

cv_model_nonlinear <- train(
  nonlinear_formula,
  data = data_cleaned,
  method = "lm",
  trControl = cv_control
)

print(cv_model_nonlinear)

data_plot <- data.frame(
  funding_rate = data_cleaned$funding_rate,
  predicted_close = fitted(segmented_model_nonlinear)
)

data_plot <- data_plot[order(data_plot$funding_rate), ]

breakpoints <- segmented_model_nonlinear$psi[,"Est."]

ggplot(data_plot, aes(x = funding_rate, y = predicted_close)) +
  geom_point(alpha = 0.3, color = "gray") +  # scatter of predicted points
  geom_line(color = "blue", size = 1) +      # fitted curve
  geom_vline(xintercept = breakpoints, linetype = "dashed", color = "red") +  # breakpoints
  labs(
    title = "Funding_rate vs. Predicted BTC Price",
    x = "Funding Rate",
    y = "Predicted BTC_USDT_1h_close"
  ) +
  theme_minimal()

actual <- data_cleaned$BTC_USDT_1h_close
predicted <- fitted(segmented_model_nonlinear)

plot(
  actual, predicted,
  col = "gray", pch = 20,
  xlab = "Actual BTC_USDT_1h_close",
  ylab = "Predicted BTC_USDT_1h_close",
  main = "Actual vs Predicted BTC Price"
)

abline(a = 0, b = 1, col = "blue", lwd = 2)

actual_smoothed <- lowess(actual, f = 0.05)
predicted_smoothed <- lowess(predicted, f = 0.05)

plot(
  actual_smoothed$x, actual_smoothed$y,
  type = "l", col = rgb(0, 0, 1, 0.6), lwd = 3, lty = 1,
  xlab = "Index", ylab = "BTC_USDT_1h_close",
  main = "Actual vs Predicted BTC Price (Smoothed)",
  ylim = range(c(actual, predicted)), xaxt = "n"
)
axis(1)  

lines(
  predicted_smoothed$x, predicted_smoothed$y,
  col = rgb(1, 0, 0, 0.6), lwd = 3, lty = 2
)

grid()
  
  legend(
    "topleft",
    legend = c("Actual (Blue)", "Predicted (Red)"),
    col = c(rgb(0, 0, 1, 0.6), rgb(1, 0, 0, 0.6)),
    lty = c(1, 2), lwd = 3, bty = "n"
  )

standardized_data <- data_cleaned
standardized_data[predictors] <- scale(data_cleaned[predictors])

standardized_model <- lm(nonlinear_formula, data = standardized_data)

coefs <- summary(standardized_model)$coefficients

clean_coefs <- coefs[!grepl("Intercept|U\\d+|psi", rownames(coefs)), , drop = FALSE]

coef_abs <- abs(clean_coefs[, "Estimate"])

top5 <- sort(coef_abs, decreasing = TRUE)[1:5]

# Shows the top 5 predictors
print(top5)

