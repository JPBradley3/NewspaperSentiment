library(tidyverse)

# Read data
sentiment_data <- read.csv("article_sentiments.csv")

# Check what dates look like
cat("Sample publish_date values:\n")
print(head(unique(sentiment_data$publish_date), 10))

# Check if any have 2025
cat("\nDates with 2025:\n")
dates_2025 <- sentiment_data$publish_date[str_detect(sentiment_data$publish_date, "2025")]
print(head(dates_2025, 5))

# Try different extraction
sentiment_data$date_clean <- case_when(
  str_detect(sentiment_data$publish_date, "2025-\\d{2}-\\d{2}") ~ as.Date(str_extract(sentiment_data$publish_date, "2025-\\d{2}-\\d{2}")),
  str_detect(sentiment_data$publish_date, "Aug 2025") ~ as.Date("2025-08-04"),
  str_detect(sentiment_data$publish_date, "August.*2025") ~ as.Date("2025-08-04"),
  TRUE ~ as.Date(NA)
)

cat("\nCleaned dates:\n")
print(table(sentiment_data$date_clean, useNA = "always"))