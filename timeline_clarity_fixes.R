# Timeline Visualization Clarity Fixes
# Addresses common issues that make time-based visualizations hard to read

library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

# Function to create clear, readable timeline visualizations
create_clear_timeline <- function(data, date_col, value_col, group_col = NULL, 
                                 title = "Timeline", subtitle = NULL) {
  
  # Ensure proper date formatting
  data[[date_col]] <- as.Date(data[[date_col]])
  
  # Calculate date range and determine appropriate breaks
  date_range <- range(data[[date_col]], na.rm = TRUE)
  date_diff <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))
  
  # Smart date formatting based on range
  if (date_diff <= 7) {
    date_breaks <- "1 day"
    date_labels <- "%m/%d"
  } else if (date_diff <= 30) {
    date_breaks <- "3 days"
    date_labels <- "%m/%d"
  } else if (date_diff <= 90) {
    date_breaks <- "1 week"
    date_labels <- "%m/%d"
  } else if (date_diff <= 365) {
    date_breaks <- "2 weeks"
    date_labels <- "%m/%d"
  } else {
    date_breaks <- "1 month"
    date_labels <- "%b %Y"
  }
  
  # Base plot
  p <- ggplot(data, aes_string(x = date_col, y = value_col))
  
  # Add grouping if specified
  if (!is.null(group_col)) {
    p <- p + 
      geom_line(aes_string(color = group_col), size = 1.2, alpha = 0.8) +
      geom_point(aes_string(color = group_col), size = 2.5, alpha = 0.7) +
      scale_color_viridis_d(name = str_to_title(str_replace_all(group_col, "_", " ")))
  } else {
    p <- p + 
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "steelblue", size = 2.5, alpha = 0.7)
  }
  
  # Add reference line and formatting
  p <- p +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5, color = "gray50") +
    scale_x_date(
      date_labels = date_labels,
      date_breaks = date_breaks,
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Date",
      y = str_to_title(str_replace_all(value_col, "_", " ")),
      caption = paste("Data from", format(date_range[1], "%B %d, %Y"), 
                     "to", format(date_range[2], "%B %d, %Y"))
    ) +
    theme_minimal() +
    theme(
      # Clear, readable text
      text = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, color = "gray60", margin = margin(b = 15)),
      
      # Rotated x-axis labels for clarity
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      
      # Clean grid
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", size = 0.5),
      panel.grid.major.y = element_line(color = "gray90", size = 0.5),
      
      # Legend positioning
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      
      # Caption styling
      plot.caption = element_text(size = 9, color = "gray60", hjust = 0)
    )
  
  return(p)
}

# Read and prepare data
if (file.exists("article_sentiments.csv")) {
  sentiment_data <- read.csv("article_sentiments.csv", stringsAsFactors = FALSE)
  
  # Read news data for dates
  csv_files <- list.files(pattern = "seattle_news_.*\\.csv", full.names = TRUE)
  news_data <- map_dfr(csv_files, ~ read.csv(.x, stringsAsFactors = FALSE)) %>%
    mutate(publish_date = as.Date(publish_date)) %>%
    select(url, publish_date, source, scraped_at)
  
  # Combine data
  timeline_data <- sentiment_data %>%
    left_join(news_data, by = "url") %>%
    filter(!is.na(publish_date))
  
  # 1. CLEAR DAILY SENTIMENT TIMELINE
  daily_sentiment <- timeline_data %>%
    group_by(publish_date, source) %>%
    summarise(
      avg_sentiment = mean(avg_sentiment),
      articles = n(),
      .groups = "drop"
    )
  
  if (nrow(daily_sentiment) > 0) {
    p1 <- create_clear_timeline(
      daily_sentiment, 
      "publish_date", 
      "avg_sentiment", 
      "source",
      title = "Daily News Sentiment by Source",
      subtitle = "Clear timeline showing sentiment trends over time"
    ) +
    # Add article count as point size
    geom_point(aes(color = source, size = articles), alpha = 0.7) +
    scale_size_continuous(
      range = c(2, 6), 
      name = "Articles",
      breaks = c(1, 5, 10, 20),
      labels = c("1", "5", "10", "20+")
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 3)),
      size = guide_legend(override.aes = list(alpha = 1))
    )
    
    print(p1)
    ggsave("clear_daily_timeline.png", p1, width = 14, height = 8, dpi = 300)
  }
  
  # 2. FACETED TIMELINE FOR CLARITY
  if (n_distinct(daily_sentiment$source) > 1) {
    p2 <- ggplot(daily_sentiment, aes(x = publish_date, y = avg_sentiment)) +
      geom_line(color = "steelblue", size = 1.5) +
      geom_point(aes(size = articles), color = "steelblue", alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      facet_wrap(~source, scales = "free_y", ncol = 2) +
      scale_x_date(
        date_labels = "%m/%d",
        date_breaks = "2 days"
      ) +
      scale_size_continuous(
        range = c(2, 5),
        name = "Articles"
      ) +
      labs(
        title = "Individual Source Sentiment Trends",
        subtitle = "Separate panels for each news source show clearer individual patterns",
        x = "Date",
        y = "Average Sentiment Score",
        caption = "Point size indicates number of articles published that day"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 11),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray60"),
        strip.text = element_text(size = 12, face = "bold", color = "white", 
                                 margin = margin(5, 5, 5, 5)),
        strip.background = element_rect(fill = "steelblue", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      )
    
    print(p2)
    ggsave("faceted_timeline.png", p2, width = 14, height = 10, dpi = 300)
  }
  
  # 3. ENHANCED COMPARISON TIMELINE
  if (n_distinct(daily_sentiment$source) > 1) {
    # Calculate relative sentiment (compared to each source's average)
    relative_sentiment <- daily_sentiment %>%
      group_by(source) %>%
      mutate(
        source_avg = mean(avg_sentiment),
        relative_sentiment = avg_sentiment - source_avg
      ) %>%
      ungroup()
    
    p3 <- ggplot(relative_sentiment, aes(x = publish_date, y = relative_sentiment, color = source)) +
      geom_line(size = 1.3, alpha = 0.8) +
      geom_point(aes(size = articles), alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "solid", alpha = 0.7, color = "gray40") +
      scale_color_viridis_d(name = "News Source") +
      scale_size_continuous(range = c(2, 5), name = "Articles") +
      scale_x_date(
        date_labels = "%m/%d",
        date_breaks = "1 day"
      ) +
      labs(
        title = "Relative Sentiment Comparison",
        subtitle = "Shows how each source's sentiment compares to their own average",
        x = "Date",
        y = "Sentiment Relative to Source Average",
        caption = "Zero line represents each source's typical sentiment level"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray60"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
    
    print(p3)
    ggsave("relative_sentiment_timeline.png", p3, width = 14, height = 8, dpi = 300)
  }
  
  # 4. SUMMARY STATISTICS TABLE
  summary_stats <- timeline_data %>%
    group_by(source) %>%
    summarise(
      articles = n(),
      avg_sentiment = round(mean(avg_sentiment), 3),
      median_sentiment = round(median(avg_sentiment), 3),
      sentiment_sd = round(sd(avg_sentiment), 3),
      min_sentiment = round(min(avg_sentiment), 3),
      max_sentiment = round(max(avg_sentiment), 3),
      date_range = paste(min(publish_date), "to", max(publish_date)),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_sentiment))
  
  cat("\n=== TIMELINE CLARITY ANALYSIS ===\n")
  cat("Issues addressed:\n")
  cat("✓ Clear date formatting based on data range\n")
  cat("✓ Appropriate axis labels and breaks\n")
  cat("✓ Reference lines for context\n")
  cat("✓ Readable text sizes and colors\n")
  cat("✓ Faceted views for multiple series\n")
  cat("✓ Relative comparisons to show patterns\n\n")
  
  print(summary_stats)
  
  cat("\nClear timeline visualizations created:\n")
  cat("- clear_daily_timeline.png: Enhanced daily sentiment with clear formatting\n")
  cat("- faceted_timeline.png: Individual source panels for clarity\n")
  cat("- relative_sentiment_timeline.png: Relative comparison timeline\n")
  
} else {
  cat("Error: article_sentiments.csv not found.\n")
  cat("Please run sentiment analysis first to generate timeline data.\n")
}