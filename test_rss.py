#!/usr/bin/env python3
"""Quick RSS feed tester"""

import feedparser
import requests

def test_rss_feeds():
    feeds = [
        ("https://www.kuow.org/rss.xml", "KUOW (News)"),
        ("http://www.kuow.org/feeds/podcasts/morning-edition/podcasts/rss.xml", "KUOW (Morning Edition)"),
        ("https://www.kuow.org/feeds/podcasts/all-things-considered/podcasts/rss.xml", "KUOW (All Things Considered)"),
        ("http://www.kuow.org/feeds/podcasts/weekend-edition-saturday/podcasts/rss.xml", "KUOW (Weekend Edition Sat)"),
        ("http://www.kuow.org/feeds/podcasts/weekendedition-sunday/podcasts/rss.xml", "KUOW (Weekend Edition Sun)"),
        ("https://www.seattletimes.com/seattle-news/feed/", "Seattle Times RSS"),
        ("https://www.kiro7.com/arc/outboundfeeds/rss/category/news/local/?outputType=xml", "KIRO 7"),
        ("https://www.king5.com/feeds/syndication/rss/news/local", "KING 5"),
        ("https://komonews.com/news/local.rss", "KOMO News"),
        ("https://www.thestranger.com/rss.xml", "The Stranger"),
        ("https://www.capitolhillseattle.com/feed/", "Capitol Hill Seattle"),
    ]
    
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
        'Accept': 'application/rss+xml, application/xml, text/xml'
    }
    
    for url, name in feeds:
        try:
            print(f"\nTesting {name}: {url}")
            # Increased timeout for slow servers like KING 5
            response = requests.get(url, headers=headers, timeout=20)
            print(f"  Status: {response.status_code}")
            
            if response.status_code == 200:
                feed = feedparser.parse(response.content)
                print(f"  Entries: {len(feed.entries)}")
                if feed.entries:
                    print(f"  Sample title: {feed.entries[0].get('title', 'No title')[:60]}...")
            elif response.status_code == 202:
                print(f"  Warning: HTTP {response.status_code} (Accepted). The server may be rate-limiting.")
            else:
                print(f"  Error: HTTP {response.status_code}")
                
        except Exception as e:
            print(f"  Exception: {e}")

if __name__ == "__main__":
    test_rss_feeds()