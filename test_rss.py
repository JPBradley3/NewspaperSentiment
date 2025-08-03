#!/usr/bin/env python3
"""Quick RSS feed tester"""

import feedparser
import requests

def test_rss_feeds():
    feeds = [
        ("https://www.kuow.org/feeds/news.rss", "KUOW"),
        ("https://www.seattletimes.com/seattle-news/feed/", "Seattle Times RSS"),
        ("https://www.kiro7.com/arcio/rss/category/news/local/?outputType=xml", "KIRO 7"),
        ("https://rss.king5.com/", "KING 5"),
        ("https://komonews.com/rss.xml", "KOMO News"),
    ]
    
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
        'Accept': 'application/rss+xml, application/xml, text/xml'
    }
    
    for url, name in feeds:
        try:
            print(f"\nTesting {name}: {url}")
            response = requests.get(url, headers=headers, timeout=10)
            print(f"  Status: {response.status_code}")
            
            if response.status_code == 200:
                feed = feedparser.parse(response.content)
                print(f"  Entries: {len(feed.entries)}")
                if feed.entries:
                    print(f"  Sample title: {feed.entries[0].get('title', 'No title')[:60]}...")
            else:
                print(f"  Error: HTTP {response.status_code}")
                
        except Exception as e:
            print(f"  Exception: {e}")

if __name__ == "__main__":
    test_rss_feeds()