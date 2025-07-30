import requests
from bs4 import BeautifulSoup
import feedparser
import pandas as pd
import time
from urllib.parse import urljoin, urlparse

# Enable more verbose debugging
DEBUG = True

def debug_print(message):
    if DEBUG:
        print(f"DEBUG: {message}")

def collect_all(keyword="mayor", max_pages=1):
    all_results = []
    print("Scraping Seattle Times ...")
    st_results = scrape_seattle_times(query=keyword, max_pages=max_pages)
    debug_print(f"Seattle Times returned {len(st_results)} articles")
    all_results.extend(st_results)
    
    print("Scraping RSS feeds ...")
    for rss_url, source_name in RSS_FEEDS:
        rss_results = scrape_rss(rss_url, source_name, keyword)
        debug_print(f"{source_name} RSS returned {len(rss_results)} articles")
        all_results.extend(rss_results)
    
    print("Scraping The Stranger ...")
    stranger_results = scrape_stranger(query=keyword)
    debug_print(f"The Stranger returned {len(stranger_results)} articles")
    all_results.extend(stranger_results)
    
    print("Scraping Capitol Hill Seattle Blog ...")
    chs_results = scrape_chs(query=keyword, max_pages=max_pages)
    debug_print(f"CHS returned {len(chs_results)} articles")
    all_results.extend(chs_results)
    
    return all_results

def extract_article_bs4(url, title_selector, content_selector, date_selector=None):
    try:
        headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
        }
        resp = requests.get(url, timeout=10, headers=headers)
        debug_print(f"Status code for {url}: {resp.status_code}")
        
        if resp.status_code != 200:
            return None, None, None
            
        soup = BeautifulSoup(resp.text, "html.parser")
        
        # Try multiple title selectors
        title = None
        title_selectors = [title_selector, 'h1', '.headline', '.entry-title', 'title']
        for selector in title_selectors:
            title_elem = soup.select_one(selector)
            if title_elem:
                title = title_elem.get_text().strip()
                debug_print(f"Found title with selector '{selector}': {title[:50]}...")
                break
        
        # Try multiple content selectors
        content = ""
        content_selectors = [content_selector, 'article p', '.article-body p', '.entry-content p', 'p']
        for selector in content_selectors:
            paragraphs = soup.select(selector)
            if paragraphs:
                content = ' '.join([p.get_text().strip() for p in paragraphs])
                debug_print(f"Found content with selector '{selector}': {len(content)} characters")
                break
        
        publish_date = None
        if date_selector:
            date_elem = soup.select_one(date_selector)
            if date_elem:
                publish_date = date_elem.get('content') or date_elem.get_text().strip()
        
        return title, content, publish_date
    except Exception as e:
        debug_print(f"Failed to parse {url} : {e}")
        return None, None, None

# Keywords for relevance filtering
RELEVANCE_KEYWORDS = [
    "mayor", "city council", "seattle politics", "election", "candidate",
    "municipal", "governance", "policy", "budget", "housing", "homeless",
    "transportation", "zoning", "development", "public safety", "police",
    "fire department", "parks", "infrastructure", "taxes", "referendum"
]

def check_relevance(title, content, keywords=RELEVANCE_KEYWORDS):
    """Check if article is relevant based on keyword matches"""
    text = (title + " " + content).lower()
    matches = [kw for kw in keywords if kw.lower() in text]
    return len(matches) >= 1, matches

def get_wayback_url(original_url):
    """Get archived version of URL from Internet Archive"""
    try:
        wayback_api = f"http://archive.org/wayback/available?url={original_url}"
        resp = requests.get(wayback_api, timeout=10)
        data = resp.json()
        
        if data.get('archived_snapshots', {}).get('closest', {}).get('available'):
            archived_url = data['archived_snapshots']['closest']['url']
            debug_print(f"Found archived version: {archived_url}")
            return archived_url
    except Exception as e:
        debug_print(f"Error getting wayback URL: {e}")
    return None

def scrape_seattle_times(query="mayor", max_pages=1):
    results = []
    try:
        # Try the main Seattle news section and look for recent articles
        base_urls = [
            "https://www.seattletimes.com/seattle-news/",
            "https://www.seattletimes.com/politics/"
        ]
        
        headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
        }
        
        for base_url in base_urls:
            debug_print(f"Scraping Seattle Times section: {base_url}")
            resp = requests.get(base_url, headers=headers, timeout=10)
            soup = BeautifulSoup(resp.text, "html.parser")
            
            # Look for article links with more flexible patterns
            article_links = []
            all_links = soup.find_all('a', href=True)
            debug_print(f"Total links found on page: {len(all_links)}")
            
            for a in all_links:
                href = a['href']
                # Convert relative URLs to absolute
                if href.startswith('/'):
                    href = "https://www.seattletimes.com" + href
                
                # Look for article patterns - be more flexible
                if 'seattletimes.com' in href and any(x in href for x in ['/2024/', '/2023/', '/2022/', 'seattle-news', 'politics']):
                    article_links.append(href)
                    debug_print(f"Added article link: {href}")
            
            debug_print(f"Found {len(article_links)} potential article links")
            
            # Remove duplicates and limit
            article_links = list(set(article_links))[:3]
            
            for article_url in article_links:
                debug_print(f"Processing article: {article_url}")
                
                # Try archived version first
                wayback_url = get_wayback_url(article_url)
                url_to_scrape = wayback_url if wayback_url else article_url
                
                title, content, date = extract_article_bs4(
                    url_to_scrape,
                    'h1',
                    'p',
                    'meta[property="article:published_time"]'
                )
                
                # Check relevance using keyword list
                if title and content:
                    is_relevant, matched_keywords = check_relevance(title, content)
                    if is_relevant:
                        results.append({
                            "url": article_url,  # Keep original URL for reference
                            "source": "Seattle Times" + (" (Archived)" if wayback_url else ""),
                            "title": title,
                            "content": content[:1000] + "..." if len(content) > 1000 else content,
                            "publish_date": date,
                            "matched_keywords": matched_keywords
                        })
                time.sleep(1)
            
            time.sleep(2)
            
    except Exception as e:
        debug_print(f"Error scraping Seattle Times: {e}")
    
    debug_print(f"Seattle Times returned {len(results)} articles")
    return results

def scrape_rss(rss_url, source_name, keyword):
    results = []
    try:
        debug_print(f"Parsing RSS feed: {rss_url}")
        feed = feedparser.parse(rss_url)
        debug_print(f"RSS feed has {len(feed.entries)} entries")
        
        for entry in feed.entries:
            title = entry.get('title', '')
            summary = entry.get('summary', '')
            snippet = title + ' ' + summary
            
            if keyword.lower() in snippet.lower():
                debug_print(f"Found matching entry: {title[:50]}...")
                url = entry.link
                
                # For RSS, we might just use the summary if full content isn't available
                results.append({
                    "url": url,
                    "source": source_name,
                    "title": title,
                    "content": summary or title,
                    "publish_date": entry.get('published')
                })
    except Exception as e:
        debug_print(f"Error scraping RSS {source_name}: {e}")
    
    return results

# Updated RSS feeds (some might not work)
RSS_FEEDS = [
    ("https://www.kuow.org/rss.xml", "KUOW"),
    ("https://www.seattletimes.com/rss/", "Seattle Times RSS"),
]

def scrape_stranger(query="mayor"):
    results = []
    try:
        search_url = f"https://www.thestranger.com/search?q={query}"
        debug_print(f"Searching The Stranger: {search_url}")
        
        headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
        }
        resp = requests.get(search_url, headers=headers, timeout=10)
        soup = BeautifulSoup(resp.text, "html.parser")
        
        # Look for article links
        links = soup.find_all('a', href=True)
        debug_print(f"Found {len(links)} total links on Stranger search page")
        
        article_links = []
        for a in links:
            href = a['href']
            if '/2024/' in href or '/2023/' in href:  # Look for recent articles
                if not href.startswith('http'):
                    href = urljoin('https://www.thestranger.com', href)
                article_links.append(href)
        
        debug_print(f"Found {len(article_links)} potential article links")
        
        for article_url in article_links[:3]:  # Limit to avoid overloading
            title, content, date = extract_article_bs4(
                article_url, 'h1', 'p')
            if title and content and len(content) > 100:
                is_relevant, matched_keywords = check_relevance(title, content)
                if is_relevant:
                    results.append({
                        "url": article_url,
                        "source": "The Stranger",
                        "title": title,
                        "content": content[:1000] + "..." if len(content) > 1000 else content,
                        "publish_date": date,
                        "matched_keywords": matched_keywords
                    })
            time.sleep(1)
    except Exception as e:
        debug_print(f"Error scraping The Stranger: {e}")
    
    return results

def scrape_chs(query="mayor", max_pages=1):
    results = []
    try:
        # Try the search URL format
        search_url = f"https://www.capitolhillseattle.com/?s={query}"
        debug_print(f"Searching CHS: {search_url}")
        
        headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
        }
        resp = requests.get(search_url, headers=headers, timeout=10)
        soup = BeautifulSoup(resp.text, "html.parser")
        
        # Look for article links
        links = soup.find_all('a', href=True)
        article_links = []
        
        for a in links:
            href = a['href']
            if 'capitolhillseattle.com' in href and ('/2024/' in href or '/2023/' in href):
                article_links.append(href)
        
        debug_print(f"Found {len(article_links)} potential CHS article links")
        
        for article_url in article_links[:3]:  # Limit to avoid overloading
            title, content, date = extract_article_bs4(
                article_url, 'h1', 'p')
            if title and content and len(content) > 100:
                is_relevant, matched_keywords = check_relevance(title, content)
                if is_relevant:
                    results.append({
                        "url": article_url,
                        "source": "CHS",
                        "title": title,
                        "content": content[:1000] + "..." if len(content) > 1000 else content,
                        "publish_date": date,
                        "matched_keywords": matched_keywords
                    })
            time.sleep(1)
    except Exception as e:
        debug_print(f"Error scraping CHS: {e}")
    
    return results

if __name__ == "__main__":
    # Test with debug enabled
    all_results = collect_all(keyword="mayor", max_pages=1)
    
    if all_results:
        df = pd.DataFrame(all_results)
        df.to_csv("seattle_mayoral_news.csv", index=False)
        print(f"Saved {len(df)} articles to seattle_mayoral_news.csv")
        
        # Print summary
        print("\nSummary:")
        for source in df['source'].unique():
            count = len(df[df['source'] == source])
            print(f"  {source}: {count} articles")
    else:
        print("No articles found. Check the debug output above for issues.")