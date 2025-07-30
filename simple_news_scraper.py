import requests
import feedparser
import pandas as pd
from datetime import datetime

def scrape_news_rss(keyword="mayor", sources=None):
    """Simple RSS-based news scraper"""
    if sources is None:
        sources = [
            ("https://feeds.npr.org/1001/rss.xml", "NPR"),
            ("https://rss.cnn.com/rss/edition.rss", "CNN"),
            ("https://feeds.bbci.co.uk/news/rss.xml", "BBC"),
            ("https://www.seattletimes.com/feed/", "Seattle Times"),
        ]
    
    all_articles = []
    
    for rss_url, source_name in sources:
        print(f"Scraping {source_name}...")
        try:
            feed = feedparser.parse(rss_url)
            print(f"  Found {len(feed.entries)} entries")
            if len(feed.entries) == 0:
                print(f"  Warning: No entries found for {source_name}")
            
            for entry in feed.entries:
                title = entry.get('title', '')
                summary = entry.get('summary', entry.get('description', ''))
                
                # Check if keyword appears in title or summary
                if keyword.lower() in (title + ' ' + summary).lower():
                    all_articles.append({
                        'url': entry.get('link', ''),
                        'source': source_name,
                        'title': title,
                        'content': summary,
                        'publish_date': entry.get('published', ''),
                        'scraped_at': datetime.now().isoformat(),
                        'keyword': keyword
                    })
                    print(f"  Found: {title[:50]}...")
                    
        except Exception as e:
            print(f"  Error scraping {source_name}: {e}")
            import traceback
            traceback.print_exc()
    
    return all_articles

if __name__ == "__main__":
    # First, let's see what articles are available
    print("=== Checking available articles ===")
    sources = [
        ("https://feeds.npr.org/1001/rss.xml", "NPR"),
        ("https://rss.cnn.com/rss/edition.rss", "CNN"),
        ("https://feeds.bbci.co.uk/news/rss.xml", "BBC"),
    ]
    
    for rss_url, source_name in sources:
        try:
            feed = feedparser.parse(rss_url)
            print(f"\n{source_name} - Sample titles:")
            for i, entry in enumerate(feed.entries[:3]):
                print(f"  {i+1}. {entry.get('title', 'No title')[:80]}...")
        except Exception as e:
            print(f"Error with {source_name}: {e}")
    
    print("\n=== Scraping for keywords ===")
    # Try multiple keywords
    keywords = ["mayor", "election", "politics", "city", "government"]
    
    all_found = []
    for keyword in keywords:
        print(f"\nTrying keyword: '{keyword}'")
        articles = scrape_news_rss(keyword=keyword)
        all_found.extend(articles)
    
    if all_found:
        df = pd.DataFrame(all_found)
        df.to_csv("news_articles.csv", index=False)
        print(f"\nSaved {len(all_found)} articles to news_articles.csv")
        
        # Show summary
        print("\nSummary by source:")
        for source in df['source'].unique():
            count = len(df[df['source'] == source])
            print(f"  {source}: {count} articles")
    else:
        print("No articles found with any keywords.")