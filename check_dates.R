# Check publish_date data
library(tidyverse)

csv_files <- list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE)
news_data <- map_dfr(csv_files, ~ read.csv(.x, stringsAsFactors = FALSE))

# Check raw publish_date values
cat("Sample of raw publish_date values:\n")
print(head(unique(news_data$publish_date), 20))

# Check after date conversion
news_data$publish_date_converted <- as.Date(news_data$publish_date)
cat("\nAfter as.Date() conversion:\n")
print(head(unique(news_data$publish_date_converted), 20))

# Check for different date formats
cat("\nChecking for different date patterns:\n")
date_patterns <- news_data %>%
  mutate(
    has_time = str_detect(publish_date, "\\d{2}:\\d{2}"),
    has_year = str_detect(publish_date, "20\\d{2}"),
    format_type = case_when(
      str_detect(publish_date, "^\\d{4}-\\d{2}-\\d{2}$") ~ "YYYY-MM-DD",
      str_detect(publish_date, "^\\d{4}-\\d{2}-\\d{2}T") ~ "ISO datetime",
      str_detect(publish_date, "^\\w{3}, \\d{2} \\w{3} 20\\d{2}") ~ "RFC format",
      TRUE ~ "Other"
    )
  ) %>%
  count(format_type, sort = TRUE)

print(date_patterns)

# Try different parsing approaches
cat("\nTrying different date parsing:\n")
sample_dates <- head(unique(news_data$publish_date), 5)
for(date_str in sample_dates) {
  cat("Original:", date_str, "\n")
  cat("  as.Date():", as.character(as.Date(date_str)), "\n")
  cat("  lubridate::ymd():", as.character(lubridate::ymd(date_str)), "\n")
  cat("  lubridate::mdy():", as.character(lubridate::mdy(date_str)), "\n")
  cat("\n")
}