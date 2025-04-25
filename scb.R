library(pxweb)
library(tidyverse)
pop_query_list <-
    list(
        "Region" = c("00"),
        "Kon" = c("1", "2"),
        "ContentsCode" = c("000000VK"),
        "Tid" = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")
    )
pop_data <-
    pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/FolkmangdDistrikt",
        query = pop_query_list
    )
pop_df <- as.data.frame(pop_data, column.name.type = "text", variable.value.type = "text")
by_year <- pop_df |>
    group_by(year) |>
    summarize(
        population = sum(Number),
    ) |>
    ungroup() |>
    arrange(year) |>
    mutate(
        percent_increase = (population / lag(population) - 1) * 100
    )
by_year |>
    ggplot(aes(x = year, y = population, group = 1)) +
    geom_line(color = "red") +
    geom_point(color = "red") +
    geom_text(aes(label = ifelse(!is.na(percent_increase), sprintf("+%.2f%%", percent_increase), "")),
        vjust = -1, size = 3
    ) +
    labs(
        title = "Population by Year in Sweden",
        x = "Year",
        y = "Population",
        caption = "Source: Statistics Sweden (SCB)"
    )
reg_cars_query <-
    list(
        "Region" = c("00"),
        "Agarkategori" = c("000"),
        "ContentsCode" = c("TK1001AB"),
        "Tid" = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")
    )

reg_cars_data <-
    pxweb_get(
        url = "https://api.scb.se/OV0104/v1/doris/en/ssd/TK/TK1001/TK1001A/PersBilarA",
        query = reg_cars_query
    )

reg_cars_df <- as.data.frame(reg_cars_data, column.name.type = "text", variable.value.type = "text")

reg_cars_by_year <- reg_cars_df |>
    group_by(year) |>
    summarize(
        cars = sum(Number)
    ) |>
    ungroup() |>
    arrange(year) |>
    mutate(
        percent_increase = (cars / lag(cars) - 1) * 100
    )

reg_cars_by_year |>
    ggplot(aes(x = year, y = cars, group = 1)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    geom_text(aes(label = ifelse(!is.na(percent_increase), sprintf("+%.2f%%", percent_increase), "")),
        vjust = -1, size = 3
    ) +
    labs(
        title = "Registered Passenger Cars by Year in Sweden",
        x = "Year",
        y = "Number of Cars",
        caption = "Source: Statistics Sweden (SCB)"
    )
