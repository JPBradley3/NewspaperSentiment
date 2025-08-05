# Enhanced Timeline Visualization for Limited Date Range Data
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

# Read the data
csv_files <- list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE)
news_data <- map_dfr(csv_files, ~ read.csv(.x, stringsAsFactors = FALSE)) %>%
  mutate(publish_date = as.Date(publish_date)) %>%
  filter(!is.na(publish_date))

# Check date range
date_range <- range(news_data$publish_date, na.rm = TRUE)
cat("Date range:", as.character(date_range[1]), "to", as.character(date_range[2]), "\n")
cat("Total unique dates:", n_distinct(news_data$publish_date), "\n")

# Read sentiment data
if (file.exists("article_sentiments.csv")) {
  sentiment_scores <- read.csv("article_sentiments.csv", stringsAsFactors = FALSE)
  
  # Join with news data
  timeline_data <- sentiment_scores %>%
    left_join(news_data %>% select(url, publish_date, scraped_at), by = "url") %>%
    filter(!is.na(publish_date))
  
  # Since we have limited date range, create hourly timeline using scraped_at
  if ("scraped_at" %in% colnames(news_data)) {
    hourly_data <- timeline_data %>%
      left_join(news_data %>% select(url, scraped_at), by = "url") %>%
      mutate(
        scraped_datetime = as.POSIXct(scraped_at, format = "%Y-%m-%dT%H:%M:%S"),
        hour = floor_date(scraped_datetime, "hour")
      ) %>%
      filter(!is.na(hour))
    
    if (nrow(hourly_data) > 0) {
      # Hourly sentiment analysis
      hourly_sentiment <- hourly_data %>%
        group_by(hour, source) %>%
        summarise(
          avg_sentiment = mean(avg_sentiment),
          articles = n(),
          .groups = "drop"
        ) %>%
        arrange(hour)
      
      # Create enhanced hourly timeline
      p1 <- ggplot(hourly_sentiment, aes(x = hour, y = avg_sentiment)) +
        geom_line(aes(color = source), size = 1.2, alpha = 0.8) +
        geom_point(aes(color = source, size = articles), alpha = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
        scale_x_datetime(
          date_labels = "%H:%M",
          date_breaks = "2 hours",
          expand = expansion(mult = 0.02)
        ) +
        scale_size_continuous(
          range = c(2, 6),
          name = "Articles",
          breaks = c(1, 5, 10, 20),
          labels = c("1", "5", "10", "20+")
        ) +
        scale_color_viridis_d(name = "News Source") +
        labs(
          title = "Sentiment Timeline by Hour of Scraping",
          subtitle = "Shows when articles were collected and their sentiment",
          x = "Time",
          y = "Average Sentiment Score",
          caption = "Point size indicates number of articles scraped at that hour"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray60")
        )
      
      print(p1)
      ggsave("hourly_sentiment_timeline.png", p1, width = 12, height = 8, dpi = 300)
    }
  }
  
  # Create article sequence timeline (order of processing)
  sequence_timeline <- timeline_data %>%
    arrange(source, title) %>%
    group_by(source) %>%
    mutate(
      article_sequence = row_number(),
      # 5-article rolling average
      sentiment_smooth = zoo::rollmean(avg_sentiment, k = min(5, n()), fill = NA, align = "center")
    ) %>%
    ungroup()
  
  # Enhanced sequence plot
  p2 <- ggplot(sequence_timeline, aes(x = article_sequence)) +
    geom_point(aes(y = avg_sentiment, color = source), alpha = 0.4, size = 1.5) +
    geom_line(aes(y = sentiment_smooth, color = source), size = 1.2, na.rm = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    facet_wrap(~source, scales = "free_x") +
    scale_color_viridis_d(name = "News Source") +
    labs(
      title = "Sequential Article Sentiment Analysis",
      subtitle = "Articles ordered by processing sequence with rolling average trend",
      x = "Article Processing Order",
      y = "Sentiment Score",
      caption = "Thick line shows rolling average trend"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray60")
    )
  
  print(p2)
  ggsave("sequential_sentiment_timeline.png", p2, width = 12, height = 8, dpi = 300)
  
  # Create sentiment distribution over time (even with limited dates)
  daily_summary <- timeline_data %>%
    group_by(publish_date, source) %>%
    summarise(
      avg_sentiment = mean(avg_sentiment),
      median_sentiment = median(avg_sentiment),
      articles = n(),
      sentiment_range = max(avg_sentiment) - min(avg_sentiment),
      .groups = "drop"
    )
  
  # Enhanced daily summary plot
  p3 <- ggplot(daily_summary, aes(x = publish_date)) +
    geom_col(aes(y = avg_sentiment, fill = source), 
             position = "dodge", alpha = 0.8, width = 0.7) +
    geom_point(aes(y = median_sentiment, color = source, size = articles), 
               position = position_dodge(width = 0.7)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_fill_viridis_d(name = "Source (Avg)") +
    scale_color_viridis_d(name = "Source (Median)") +
    scale_size_continuous(range = c(2, 5), name = "Articles") +
    labs(
      title = "Daily Sentiment Summary by Source",
      subtitle = "Bars show average sentiment, points show median with article count",
      x = "Date",
      y = "Sentiment Score",
      caption = "Point size indicates number of articles"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray60")
    )
  
  print(p3)
  ggsave("daily_sentiment_summary.png", p3, width = 12, height = 6, dpi = 300)
  
  # Create sentiment volatility analysis
  volatility_data <- timeline_data %>%
    group_by(source) %>%
    arrange(title) %>%
    mutate(
      sentiment_change = avg_sentiment - lag(avg_sentiment),
      cumulative_sentiment = cumsum(avg_sentiment),
      article_order = row_number()
    ) %>%
    filter(!is.na(sentiment_change))
  
  if (nrow(volatility_data) > 0) {
    p4 <- ggplot(volatility_data, aes(x = article_order)) +
      geom_line(aes(y = cumulative_sentiment, color = source), size = 1.2) +
      geom_point(aes(y = cumulative_sentiment, size = abs(sentiment_change), color = source), alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      facet_wrap(~source, scales = "free") +
      scale_color_viridis_d(name = "News Source") +
      scale_size_continuous(range = c(1, 4), name = "Sentiment\nChange") +
      labs(
        title = "Cumulative Sentiment Progression",
        subtitle = "Running total of sentiment with volatility indicators",
        x = "Article Order",
        y = "Cumulative Sentiment Score",
        caption = "Point size shows magnitude of sentiment change between articles"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray60")
      )
    
    print(p4)
    ggsave("sentiment_volatility.png", p4, width = 12, height = 8, dpi = 300)
  }
  
  # Summary statistics
  cat("\n=== TIMELINE ANALYSIS SUMMARY ===\n")
  cat("Total articles analyzed:", nrow(timeline_data), "\n")
  cat("Date range:", as.character(min(timeline_data$publish_date)), "to", as.character(max(timeline_data$publish_date)), "\n")
  cat("Sources:", paste(unique(timeline_data$source), collapse = ", "), "\n")
  
  source_summary <- timeline_data %>%
    group_by(source) %>%
    summarise(
      articles = n(),
      avg_sentiment = round(mean(avg_sentiment), 3),
      sentiment_sd = round(sd(avg_sentiment), 3),
      .groups = "drop"
    )
  
  print(source_summary)
  
  cat("\nVisualization files created:\n")
  cat("- hourly_sentiment_timeline.png: Hourly sentiment patterns\n")
  cat("- sequential_sentiment_timeline.png: Article processing order trends\n")
  cat("- daily_sentiment_summary.png: Daily summary with median comparison\n")
  cat("- sentiment_volatility.png: Cumulative sentiment progression\n")
  
} else {
  cat("Error: article_sentiments.csv not found. Please run sentiment analysis first.\n")
}