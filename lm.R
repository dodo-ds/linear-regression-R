library(tidyverse)
library(car)
library(Metrics)
blocket_cars <- readxl::read_xlsx("cars.xlsx", na = "N/A") |> select(-id, -url, -advertiser, -region, -municipality)
# We have 13 predictors and 1 response variable
head(blocket_cars)
# Select relevant columns
# Having 67 unique colors as a feature may not be effective for predicting prices,
# this can lead to overfitting and reduce model interpretability. Logically color does not determine price.
# Same with model, we have 10 unique models which is not too much but still using this as dummy variables will create 9 new columns.
# We are focusing on Hybrids making fuel type irrelevant.
# Gearbox is irrelevant as we only have 1 type of gearbox.
# To predict the price we are now left with 6 predictors:
# type, drive, hp, engine_size, mileage, regdate
blocket_cars |> count(color)
blocket_cars |> count(model)
blocket_cars |> count(fuel)
blocket_cars |> count(type)

cars_fltrd <- blocket_cars |>
    select(type, drive, regdate, hp, engine_size, mileage, price) |>
    filter(!type == "Coupé")
cars_fltrd |> count(type)
dim(cars_fltrd)
str(cars_fltrd)
summary(cars_fltrd)
# check for missing values
colSums(is.na(cars_fltrd))

# find duplicates
cars_fltrd[duplicated(cars_fltrd), ]
# drop duplicates
cars_fltrd <- cars_fltrd[!duplicated(cars_fltrd), ]
# check for duplicates again
cars_fltrd[duplicated(cars_fltrd), ]

# Remove outlier identified earlier
cars_fltrd <- cars_fltrd |>
    filter(mileage < 60000)

# factorize (dummy variable encode) categorical variables and sqrt-transform numeric variables except age
# and calculate age from regdate and remove regdate
max_year <- max(cars_fltrd$regdate) + 1
dataset_sqrt <- cars_fltrd |>
    mutate(
        type = factor(type),
        drive = factor(drive),
        hp = sqrt(hp),
        engine_size = sqrt(engine_size),
        age = max_year - regdate,
        mileage = sqrt(mileage + 1),
        price = log10(price)
    ) |>
    select(-regdate)

chosen_dataset <- dataset_sqrt

numeric_cols <- chosen_dataset |> select(where(is.numeric))
numeric_cols |>
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    facet_wrap(~variable, scales = "free") +
    theme_minimal() +
    labs(title = "Histograms of All Columns", x = "Value", y = "Frequency")


# Let's run a VIF test to check for multicollinearity.
# VIF values greater than 5 for engine size and hp indicate multicollinearity.
vif_model <- lm(price ~ ., data = chosen_dataset)
print(car::vif(vif_model))

# Removing engine_size reduces vif values across the board.
# Dropping Engine Size reduces the number of predictors to 5.
vif_model_without_engine_size <- lm(price ~ . - engine_size, data = chosen_dataset)
print(car::vif(vif_model_without_engine_size))

chosen_dataset <- chosen_dataset |> select(-engine_size)


# Lets test 3 models with different formulas
# 1. A model with all predictors
# 2. A model with all predictors and a interaction term
# 3. A model with all predictors and a polynomial term

# split the data into train, validate and test sets
set.seed(42)
spec <- c(train = .6, validate = .2, test = .2)
g <- sample(cut(
    seq_len(nrow(chosen_dataset)),
    nrow(chosen_dataset) * cumsum(c(0, spec)),
    labels = names(spec)
))

res <- split(chosen_dataset, g)
train_set <- res$train
validate_set <- res$validate
test_set <- res$test

cat("Training set price distribution:\n")
print(summary(train_set$price))
cat("\nValidation set price distribution:\n")
print(summary(validate_set$price))
cat("\nTest set price distribution:\n")
print(summary(test_set$price))

par(mfrow = c(2, 2))

formula_1 <- price ~ .
model_1 <- lm(formula_1, data = train_set)
summary(model_1)
plot(model_1, main = "Formula 1")


formula_2 <- price ~ . + age:mileage
model_2 <- lm(formula_2, data = train_set)
summary(model_2)
plot(model_2, main = "Formula 2")

formula_3 <- price ~ +drive + type + hp + age + poly(mileage, 3)
model_3 <- lm(formula_3, data = train_set)
summary(model_3)
plot(model_3, main = "Formula 3")

# Evaluate all three models on the validation set and compare metrics
val_pred_m1 <- predict(model_1, newdata = validate_set)
val_pred_m2 <- predict(model_2, newdata = validate_set)
val_pred_m3 <- predict(model_3, newdata = validate_set)

val_actuals <- 10^validate_set$price
val_pred_m1_orig <- 10^val_pred_m1
val_pred_m2_orig <- 10^val_pred_m2
val_pred_m3_orig <- 10^val_pred_m3

results <- tibble(
    Model = c("Model 1", "Model 2", "Model 3"),
    RMSE_val_data = c(
        rmse(val_actuals, val_pred_m1_orig),
        rmse(val_actuals, val_pred_m2_orig),
        rmse(val_actuals, val_pred_m3_orig)
    ),
    Adj_R_squared = c(
        summary(model_1)$adj.r.squared,
        summary(model_2)$adj.r.squared,
        summary(model_3)$adj.r.squared
    ),
    BIC = c(
        BIC(model_1),
        BIC(model_2),
        BIC(model_3)
    )
)
results
# Model 3 has the lowest RMSE, highest adjusted R², and lowest BIC.
test_pred <- predict(model_3, newdata = test_set)
cat("Root Mean Squared Error (RMSE):", rmse(10^test_pred, 10^test_set$price), "kr")

# Plot predicted vs actual prices
ggplot(tibble(Predicted = test_pred, Actual = test_set$price), aes(x = Actual, y = Predicted)) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = "Predicted vs Actual Prices", x = "Actual Price", y = "Predicted Price")

# Read in cars_ci_pi.csv and apply transformations to relevant variables
cars_ci_pi <- readr::read_csv("cars_ci_pi.csv") |> select(type, drive, hp, mileage, regdate, price)

new_cars <- cars_ci_pi |>
    mutate(
        type = factor(type, levels = levels(chosen_dataset$type)),
        drive = factor(drive, levels = levels(chosen_dataset$drive)),
        hp = sqrt(hp),
        mileage = sqrt(mileage + 1),
        age = max_year - regdate
    ) |>
    select(-regdate)
new_cars

# Predict prices for new car data
pred_prices_log <- predict(model_3, newdata = new_cars)
pred_prices_ci <- predict(model_3, newdata = new_cars, interval = "confidence", level = 0.95)
pred_prices_pi <- predict(model_3, newdata = new_cars, interval = "prediction", level = 0.95)

cars_ci_pi |>
    mutate(
        predicted_price = round(10^pred_prices_log),
        diff = abs(price - predicted_price),
        age = max_year - regdate
    ) |>
    select(type, drive, hp, mileage, age, price, predicted_price, diff) |>
    View()

ci_df <- as_tibble(pred_prices_ci) |>
    mutate(
        predicted_price = 10^fit,
        lower_ci = 10^lwr,
        upper_ci = 10^upr
    ) |>
    bind_cols(new_cars |> select(type, price))

pi_df <- as_tibble(pred_prices_pi) |>
    mutate(
        predicted_price = 10^fit,
        lower_ci = 10^lwr,
        upper_ci = 10^upr
    ) |>
    bind_cols(new_cars |> select(type, price))

# Plot predicted price with confidence intervals
ggplot(ci_df, aes(x = type)) +
    geom_point(aes(y = predicted_price, color = "Predicted Price"), size = 2) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "darkorange") +
    geom_point(aes(y = price, color = "Actual Price"), shape = 4, size = 2) +
    scale_color_manual(
        name = "Legend",
        values = c("Predicted Price" = "blue", "Actual Price" = "red")
    ) +
    labs(
        title = "Predicted Prices with 95% Confidence Intervals",
        x = "Type",
        y = "Predicted Price (kr)"
    )
ggplot(pi_df, aes(x = type)) +
    geom_point(aes(y = predicted_price, color = "Predicted Price"), size = 2) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "darkorange") +
    geom_point(aes(y = price, color = "Actual Price"), shape = 4, size = 2) +
    scale_color_manual(
        name = "Legend",
        values = c("Predicted Price" = "blue", "Actual Price" = "red")
    ) +
    labs(
        title = "Predicted Prices with 95% Prediction Intervals",
        x = "Type",
        y = "Predicted Price (kr)"
    )
# Predicted prices with confidence intervals are narrower than prediction intervals because they only account for the variability of the estimated mean response
10^pred_prices_ci
# Prediction intervals are wider than confidence intervals beacause they account for the variability of the individual predictions
10^pred_prices_pi
