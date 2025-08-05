# Debug timeline data
library(tidyverse)

# Check what dates we actually have
csv_files <- list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE)
news_data <- map_dfr(csv_files, ~ read.csv(.x, stringsAsFactors = FALSE))

cat("Raw publish_date samples:\n")
print(head(unique(news_data$publish_date), 10))

# Apply date cleaning
news_data <- news_data %>%
  mutate(
    publish_date_clean = case_when(
      is.na(publish_date) | publish_date == "" ~ as.Date(NA),
      str_detect(publish_date, "^\\d{4}-\\d{2}-\\d{2}$") ~ as.Date(publish_date),
      str_detect(publish_date, "^\\d{4}-\\d{2}-\\d{2}T") ~ as.Date(str_extract(publish_date, "^\\d{4}-\\d{2}-\\d{2}")),
      str_detect(publish_date, "^\\w{3}, \\d{2} \\w{3} 20\\d{2}") ~ as.Date(publish_date, format = "%a, %d %b %Y"),
      TRUE ~ tryCatch(as.Date(publish_date), error = function(e) as.Date(NA))
    )
  )

cat("\nCleaned dates:\n")
print(table(news_data$publish_date_clean, useNA = "always"))

cat("\nDate range:\n")
valid_dates <- news_data$publish_date_clean[!is.na(news_data$publish_date_clean)]
if(length(valid_dates) > 0) {
  print(range(valid_dates))
} else {
  cat("No valid dates found!\n")
}

# Check sentiment data
if(file.exists("article_sentiments.csv")) {
  sentiment_data <- read.csv("article_sentiments.csv")
  cat("\nSentiment data has", nrow(sentiment_data), "rows\n")
  
  # Join and check
  timeline_data <- sentiment_data %>%
    left_join(news_data %>% select(url, publish_date_clean), by = "url") %>%
    filter(!is.na(publish_date_clean))
  
  cat("Timeline data after join:", nrow(timeline_data), "rows\n")
  
  if(nrow(timeline_data) > 0) {
    daily_summary <- timeline_data %>%
      group_by(publish_date_clean, source) %>%
      summarise(avg_sentiment = mean(avg_sentiment), articles = n(), .groups = "drop")
    
    print(daily_summary)
  }
}