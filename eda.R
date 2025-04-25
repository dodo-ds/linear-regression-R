library(corrplot)
library(tidyverse)
library(GGally)
blocket_cars <- readxl::read_xlsx("cars.xlsx", na = "N/A") |> select(-id, -url, -advertiser, -region, -municipality)
# We have 13 predictors and 1 response variable
head(blocket_cars)
colSums(is.na(blocket_cars))
numeric_cols <- blocket_cars |> select(where(is.numeric))
categorical_cols <- blocket_cars |> select(where(is.character))
# Remove Coupe because it only occurs once
blocket_cars |> count(type)
blocket_cars <- blocket_cars |>
    filter(!type == "Coupé") |>
    filter(mileage < 60000) |>
    mutate(age = 2026 - regdate)


## Plot the data to see what we have
# Number of Cars by Model and Type
blocket_cars |>
    count(model, type) |>
    ggplot(aes(x = reorder(model, n), y = n, fill = type)) +
    geom_bar(stat = "identity", color = "black", alpha = 0.7) +
    coord_flip() +
    labs(title = "Number of Cars by Model and Type", x = "Model", y = "Number of Cars", fill = "Type")
blocket_cars |>
    count(type) |>
    ggplot(aes(x = reorder(type, n), y = n, fill = type)) +
    geom_bar(stat = "identity", color = "black", alpha = 0.7) +
    labs(title = "Number of Cars Type", x = "Type", y = "Number of Cars", fill = "Type")

# TOP 5 colors
blocket_cars |>
    count(color) |>
    arrange(desc(n)) |>
    slice(1:5) |>
    ggplot(aes(x = reorder(color, n), y = n)) +
    geom_col(fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Most Popular Colors", x = "Color", y = "Number of Cars")


# average price per type
blocket_cars |>
    group_by(type) |>
    summarise(avg_price = median(price, na.rm = TRUE), n = n()) |>
    ggplot(aes(x = reorder(type, avg_price), y = avg_price, fill = n)) +
    geom_col(color = "black", alpha = 0.7) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Average Price per Type", x = "Type", y = "Average Price (kr)", fill = "Count")

# Calculate average age for the whole dataset
avg_age_whole_dataset <- blocket_cars |>
    mutate(age = 2026 - regdate) |>
    summarise(avg_age = round(mean(age, na.rm = TRUE), 2))
blocket_cars |>
    mutate(age = 2026 - regdate) |>
    group_by(type) |>
    summarise(avg_age = mean(age, na.rm = TRUE), n = n()) |>
    ggplot(aes(x = reorder(type, avg_age), y = avg_age, fill = n)) +
    geom_col(color = "black", alpha = 0.7) +
    geom_text(aes(label = round(avg_age, 1)), hjust = -0.1, size = 6) +
    coord_flip() +
    theme_classic() +
    labs(title = "Average Age per Type", x = "Model", y = "Average Age (years)", fill = "Count", caption = paste("Average age for the whole dataset:", avg_age_whole_dataset, "years"))

# Plot price vs all other numeric columns
# As age increases, price descreases which is expected.
# As mileage increases, price decreases which is expected.
# As engine size increases, price increases which is expected.
# As hp increases, price increases which is expected.
blocket_cars |>
    pivot_longer(cols = all_of(c("mileage", "hp", "engine_size", "age")), names_to = "variable", values_to = "value") |>
    ggplot(aes(x = value, y = price)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) +
    facet_wrap(~variable, scales = "free_x") +
    labs(
        title = "Price vs Numeric Predictors",
        y = "Price"
    )

# Plot price vs type
blocket_cars |>
    ggplot(aes(x = type, y = price, fill = type)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "yellow") +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
    scale_y_continuous(labels = scales::comma) +
    labs(
        title = "Price vs Type with 95% Confidence Intervals",
        x = "Type",
        y = "Price"
    )

lm_fit <- lm(price ~ hp + engine_size + mileage + age + type + drive, data = blocket_cars)

# Get coefficients and confidence intervals
coef_df <- broom::tidy(lm_fit, conf.int = TRUE) |>
    arrange(desc(abs(estimate)))
coef_df

# Plot confidence intervals for each coefficient
coef_df |>
    ggplot(aes(x = term, y = estimate)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    facet_wrap(~term, scales = "free_y") +
    scale_y_continuous(labels = scales::comma) +
    labs(
        title = "Confidence Intervals for Model Coefficients",
        x = "Predictor",
        y = "Estimate (with 95% CI)"
    ) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Price has positive correlation with all predictors except mileage which has negative correlation.
# We see VERY strong multicollinearity between engine_size and hp.
# And strong negative correlation between regdate and mileage.
# We need to investigate this further.
cor_matrix <- cor(numeric_cols, use = "complete.obs")
cor_matrix <- cor_matrix[order(-abs(cor_matrix[, "price"])), order(-abs(cor_matrix["price", ]))]
# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")

# As expected, price is higher for 4wd cars than 2wd cars.
blocket_cars |>
    ggplot(aes(x = drive, y = price, fill = drive)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "yellow") +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
    scale_y_continuous(labels = scales::comma) +
    labs(
        title = "Price vs Model with 95% Confidence Intervals",
        x = "Drive",
        y = "Price"
    )
# Plot price vs model
blocket_cars |>
    group_by(model) |>
    mutate(mean_price = mean(price, na.rm = TRUE)) |>
    ungroup() |>
    ggplot(aes(x = reorder(model, mean_price), y = price, fill = model)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2, color = "black", fill = "yellow") +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
    scale_y_continuous(labels = scales::comma) +
    labs(
        title = "Price vs Model with 95% Confidence Intervals",
        x = "Model",
        y = "Price"
    )

# Create boxplots for all numeric columns
# Mileage and regdate have outliers
numeric_cols |>
    mutate(age = 2026 - regdate) |>
    select(-regdate) |>
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
    ggplot(aes(x = variable, y = value)) +
    geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~variable, scales = "free")


# Create histograms for all columns
# Engine Size: We have 4 peaks. One around 1500cc, one around 1750cc, another around 2000cc and last one around 2500cc
# There are gaps between these peaks. 1500 to 1750cc, 1750 to 2000cc, 2000cc to 2500cc
# Highest peak is around 1750cc
# Maybe we can convert engine size to categorical values like small if < 1500cc, medium if between 1500cc and 2000cc, large if more than 2000cc
# HP: Gaps here also but not clear as engine size, it is not normally distributed like engine size and is right skewed. Most cars fall between 100 and 150hp
# Mileage: Right skewed but continuous, we have a serious outlier with 60000 miles under its belt. We need to investigate this further.
#   type      drive             hp engine_size mileage regdate  price
#   <chr>     <chr>          <dbl>       <dbl>   <dbl>   <dbl>  <dbl>
# 1 Halvkombi Tvåhjulsdriven   124        1798   62120    2021 229900
# Ad is removed https://www.blocket.se/annons/skane/toyota_corolla_hybrid_1_8_5_d_style_spi/1002542114
# Assuming this is a mistake by the seller. We can remove this row from our dataset.
# Price: Looks like a "normal" distribution but is right skewed. most cars are centered around 250000 kr.
# RegDate: Left skewed which is normal as most cars are newer. We have outliers here also. Investigation is needed.
numeric_cols |>
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    facet_wrap(~variable, scales = "free") +
    theme_minimal() +
    labs(title = "Histograms of All Columns", x = "Value", y = "Frequency")
