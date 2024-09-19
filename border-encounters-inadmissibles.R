#! /usr/bin/env R

# Import necessary libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)

# Define date range for filtering
start_date <- as.Date("2019-10-01")
end_date <- as.Date("2024-08-01")

# Vectorized function to convert a Date object from fiscal to calendar date
fiscal_to_calendar_date <- function(date, fiscal_start_month = 10) {
  fiscal_year <- year(date)
  fiscal_month <- month(date)
  calendar_year <- ifelse(
    fiscal_month >= fiscal_start_month,
    fiscal_year - 1,
    fiscal_year
  )
  as.Date(paste(calendar_year, fiscal_month, "01", sep = "-"))
}

# Function to process nationwide encounter data
process_nationwide_data <- function(file_path, start_date, end_date) {
  data <- read_csv(file_path) %>%
    mutate(Date = fiscal_to_calendar_date(
      as.Date(paste0("01-", `Month (abbv)`, "-", `Fiscal Year`),
        format = "%d-%b-%Y"
      )
    )) %>%
    filter(Date >= start_date & Date < end_date)

  # Expelled Data
  illegal_expelled <- data %>%
    filter(
      `Encounter Type` == "Expulsions"
    ) %>%
    group_by(Date) %>%
    summarize(Illegals_Expelled = sum(`Encounter Count`)) %>%
    pivot_longer(
      cols = Illegals_Expelled,
      names_to = "Encounter_Type",
      values_to = "Count"
    )

  # Other Regions && Inadmissibles Data
  other_inadmissibles <- data %>%
    filter(
      `Encounter Type` == "Inadmissibles" &
        `Land Border Region` == "Other"
    ) %>%
    group_by(Date) %>%
    summarize(Other_Inadmissibles = sum(`Encounter Count`)) %>%
    pivot_longer(
      cols = Other_Inadmissibles,
      names_to = "Encounter_Type",
      values_to = "Count"
    )

  # Southern Apprehensions Data
  southern_apprenhensions <- data %>%
    filter(
      `Land Border Region` == "Southwest Land Border" &
        `Encounter Type` == "Apprehensions"
    ) %>%
    group_by(Date) %>%
    summarize(Southern_Apprenhensions = sum(`Encounter Count`)) %>%
    pivot_longer(
      cols = Southern_Apprenhensions,
      names_to = "Encounter_Type",
      values_to = "Count"
    )

  # CHNV && Inadmissibles
  chnv_inadmissibles <- data %>%
    filter(
      `Citizenship` %in% c("CUBA", "HAITI", "NICARAGUA", "VENEZUELA"),
      `Encounter Type` == "Inadmissibles"
    ) %>%
    group_by(Date) %>%
    summarize(CHNV_Inadmissibles = sum(`Encounter Count`)) %>%
    pivot_longer(
      cols = CHNV_Inadmissibles,
      names_to = "Encounter_Type",
      values_to = "Count"
    )

  bind_rows(southern_apprenhensions, other_inadmissibles, illegal_expelled, chnv_inadmissibles)
}

# Source: CBP, FY 2021-2024 (https://www.cbp.gov/document/stats/nationwide-encounters)
nationwide_data_long <- process_nationwide_data(
  "nationwide-encounters-fy21-fy24-aug-aor.csv",
  start_date = start_date,
  end_date = end_date
)
# Source: CBP, FY 2020-2022 (https://www.cbp.gov/sites/default/files/assets/documents/2022-Oct/nationwide-encounters-fy20-fy22.csv)
nationwide_data_2020_2022_long <- process_nationwide_data(
  "nationwide-encounters-fy20-fy22.csv",
  start_date = start_date,
  end_date = as.Date("2020-10-01")
)

# Combine only border-related data for the stacked bars
combined_data <- bind_rows(
  nationwide_data_long,
  nationwide_data_2020_2022_long
)

# Create the plot
ggplot() +
  geom_col(
    data = combined_data,
    aes(
      x = Date,
      y = Count,
      fill = factor(Encounter_Type,
        levels = c(
          "Illegals_Expelled",
          "Southern_Apprenhensions",
          "Other_Inadmissibles",
          "CHNV_Inadmissibles"
        )
      )
    ),
  ) +
  labs(
    caption = "Sources: \nU.S. Customs and Border Protection: Nationwide Encounters (2020-2022)\nU.S. Customs and Border Protection: Nationwide Encounters (2021-2024)\n",
    x = "Date",
    y = "Count",
    fill = "",
    color = "",
  ) +
  scale_y_continuous(breaks = seq(0, 400000, by = 50000), labels = scales::comma) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "1 month") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

ggsave("border_encounters_inadmissibles.png", height = 8, width = 16, dpi = 300)
