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

def scrape_seattle_times(query="mayor", max_pages=1):
    results = []
    for page in range(1, max_pages+1):
        try:
            url = f"https://www.seattletimes.com/search/?q={query}&page={page}"
            debug_print(f"Searching Seattle Times: {url}")
            
            headers = {
                'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
            }
            resp = requests.get(url, headers=headers, timeout=10)
            soup = BeautifulSoup(resp.text, "html.parser")
            
            # Look for article links - try multiple patterns
            article_links = []
            
            # Look for actual article links (not section pages)
            for a in soup.find_all('a', href=True):
                href = a['href']
                # Look for URLs that contain year patterns and specific article indicators
                if ('/2024/' in href or '/2023/' in href) and any(pattern in href for pattern in ['/seattle-news/', '/politics/', '/business/', '/opinion/']):
                    if href.startswith('/'):
                        href = "https://www.seattletimes.com" + href
                    # Exclude section pages (they typically end with just the section name)
                    if not href.endswith('/seattle-news/') and not href.endswith('/politics/') and not href.endswith('/business/') and not href.endswith('/opinion/'):
                        article_links.append(href)
            
            debug_print(f"Found {len(article_links)} potential article links")
            
            # Remove duplicates
            article_links = list(set(article_links))
            
            for article_url in article_links[:5]:  # Limit to first 5 to avoid overloading
                debug_print(f"Processing article: {article_url}")
                title, content, date = extract_article_bs4(
                    article_url,
                    'h1',
                    'p',
                    'meta[property="article:published_time"]'
                )
                if title and content and len(content) > 100:  # Ensure substantial content
                    results.append({
                        "url": article_url,
                        "source": "Seattle Times",
                        "title": title,
                        "content": content[:1000] + "..." if len(content) > 1000 else content,
                        "publish_date": date
                    })
                time.sleep(1)  # Be polite
                
        except Exception as e:
            debug_print(f"Error scraping Seattle Times page {page}: {e}")
        
        time.sleep(2)  # Be polite between pages
    
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
                results.append({
                    "url": article_url,
                    "source": "The Stranger",
                    "title": title,
                    "content": content[:1000] + "..." if len(content) > 1000 else content,
                    "publish_date": date
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
                results.append({
                    "url": article_url,
                    "source": "CHS",
                    "title": title,
                    "content": content[:1000] + "..." if len(content) > 1000 else content,
                    "publish_date": date
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