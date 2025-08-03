import requests
from bs4 import BeautifulSoup
import feedparser
import pandas as pd
import time
from urllib.parse import urljoin, urlparse
from dataclasses import dataclass
from typing import List, Dict, Optional
import json
import logging
from functools import wraps
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
from datetime import datetime
import re
from abc import ABC, abstractmethod

# Configuration Management
@dataclass
class ScrapingConfig:
    max_pages: int = 1
    max_articles_per_source: int = 10
    request_timeout: int = 10
    delay_between_requests: float = 1.0
    delay_between_sources: float = 2.0
    min_content_length: int = 100
    max_content_length: int = 1000
    use_wayback_fallback: bool = True
    max_wayback_attempts: int = 2

def load_config(config_file: str = "scraping_config.json") -> ScrapingConfig:
    try:
        with open(config_file, 'r') as f:
            config_data = json.load(f)
        return ScrapingConfig(**config_data)
    except FileNotFoundError:
        return ScrapingConfig()  # Use defaults

# Enhanced Logging Setup
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('scraper.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

# Error Handling Decorator
def retry_on_failure(max_retries=3, backoff_factor=1):
    """Decorator for retrying failed operations"""
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            for attempt in range(max_retries):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if attempt == max_retries - 1:
                        logger.error(f"Failed after {max_retries} attempts: {e}")
                        raise
                    logger.warning(f"Attempt {attempt + 1} failed: {e}. Retrying...")
                    time.sleep(backoff_factor * (2 ** attempt))
            return None
        return wrapper
    return decorator

def create_session(config: ScrapingConfig) -> requests.Session:
    """Create a robust requests session"""
    session = requests.Session()
    
    retry_strategy = Retry(
        total=2,
        backoff_factor=2,
        status_forcelist=[429, 500, 502, 503, 504],
    )
    
    adapter = HTTPAdapter(max_retries=retry_strategy)
    session.mount("http://", adapter)
    session.mount("https://", adapter)
    
    session.headers.update({
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
        'Accept-Language': 'en-US,en;q=0.5',
        'Accept-Encoding': 'gzip, deflate',
        'Connection': 'keep-alive',
        'Upgrade-Insecure-Requests': '1'
    })
    
    return session

# Enhanced Keywords with Weighting
RELEVANCE_KEYWORDS = {
    "high": [
        "mayor", "mayoral", "city council", "councilmember", "councilwoman", "councilman",
        "seattle politics", "election", "candidate", "municipal", "governance"
    ],
    "medium": [
        "budget", "housing", "homeless", "homelessness", "transportation", "zoning", 
        "public safety", "police", "SPD", "infrastructure", "taxes"
    ],
    "low": [
        "policy", "development", "parks", "fire department", "referendum",
        "ordinance", "legislation", "ballot measure"
    ]
}

def check_relevance_weighted(title: str, content: str) -> tuple[bool, List[str], float]:
    """Enhanced relevance checking with weighted scoring"""
    text = (title + " " + content).lower()
    
    score = 0
    matches = []
    weights = {"high": 3, "medium": 2, "low": 1}
    
    for weight_level, keywords in RELEVANCE_KEYWORDS.items():
        for keyword in keywords:
            # Use word boundaries to avoid partial matches
            pattern = r'\b' + re.escape(keyword.lower()) + r'\b'
            if re.search(pattern, text):
                matches.append(keyword)
                score += weights[weight_level]
    
    # Title matches get bonus points
    title_lower = title.lower()
    for weight_level, keywords in RELEVANCE_KEYWORDS.items():
        for keyword in keywords:
            if keyword.lower() in title_lower:
                matches.append(f"{keyword} (title)")
                score += weights[weight_level] * 0.5  # 50% bonus for title matches
    
    # Extremely permissive threshold to catch all articles
    is_relevant = True  # Accept all articles for now
    # Original: score >= 0.5 or len(matches) > 0 or any(keyword in text for keyword in ['seattle', 'city', 'local', 'news'])
    return is_relevant, matches, score

# Data Validation Classes
class ArticleValidator:
    @staticmethod
    def is_valid_url(url: str) -> bool:
        try:
            result = urlparse(url)
            return all([result.scheme, result.netloc])
        except:
            return False
    
    @staticmethod
    def clean_text(text: str) -> str:
        if not text:
            return ""
        # Remove extra whitespace, normalize
        text = re.sub(r'\s+', ' ', text.strip())
        # Remove common artifacts
        text = re.sub(r'Advertisement\s*', '', text, flags=re.IGNORECASE)
        return text
    
    @staticmethod
    def normalize_date(date_str: str) -> Optional[str]:
        if not date_str:
            return None
        
        # Add date parsing logic for common formats
        date_patterns = [
            r'\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}',  # ISO format
            r'\d{4}-\d{2}-\d{2}',  # Simple date
        ]
        
        for pattern in date_patterns:
            if re.search(pattern, date_str):
                return date_str
        
        return date_str  # Return as-is if no pattern matches

def process_article_data(raw_data: Dict) -> Optional[Dict]:
    """Clean and validate article data"""
    validator = ArticleValidator()
    
    # Validate required fields
    if not all(key in raw_data for key in ['url', 'title', 'content']):
        logger.warning("Missing required fields in article data")
        return None
    
    if not validator.is_valid_url(raw_data['url']):
        logger.warning(f"Invalid URL: {raw_data['url']}")
        return None
    
    processed = {
        'url': raw_data['url'],
        'source': raw_data.get('source', 'Unknown'),
        'title': validator.clean_text(raw_data['title']),
        'content': validator.clean_text(raw_data['content']),
        'publish_date': validator.normalize_date(raw_data.get('publish_date')),
        'matched_keywords': raw_data.get('matched_keywords', []),
        'relevance_score': raw_data.get('relevance_score', 0),
        'scraped_at': datetime.now().isoformat(),
        'content_length': len(raw_data.get('content', ''))
    }
    
    return processed

# Base Scraper Class
class BaseScraper(ABC):
    def __init__(self, config: ScrapingConfig, session: requests.Session):
        self.config = config
        self.session = session
        self.logger = logging.getLogger(self.__class__.__name__)
    
    @abstractmethod
    def scrape(self, query: str) -> List[Dict]:
        pass
    
    def extract_article_content(self, url: str, selectors: Dict[str, List[str]]) -> Optional[Dict]:
        """Generic article extraction method with Wayback fallback"""
        try:
            response = self.session.get(url, timeout=self.config.request_timeout)
            response.raise_for_status()
            
            soup = BeautifulSoup(response.text, "html.parser")
            
            # Extract using multiple selector fallbacks
            title = self._extract_with_fallback(soup, selectors.get('title', []))
            content = self._extract_with_fallback(soup, selectors.get('content', []))
            date = self._extract_with_fallback(soup, selectors.get('date', []))
            
            self.logger.debug(f"Extracted from {url}: title={len(title)} chars, content={len(content)} chars")
            
            if not title and not content:
                self.logger.warning(f"No content extracted from {url}")
                return None
            
            return {
                'title': title or 'No title',
                'content': content or title,
                'publish_date': date
            }
            
        except Exception as e:
            self.logger.warning(f"Failed to extract from {url}: {e}. Trying Wayback Machine...")
            return self._try_wayback_extraction(url, selectors)
    
    def _try_wayback_extraction(self, url: str, selectors: Dict[str, List[str]]) -> Optional[Dict]:
        """Try extracting from Internet Archive"""
        wayback_url = get_wayback_url(url)
        if wayback_url:
            try:
                response = self.session.get(wayback_url, timeout=self.config.request_timeout)
                soup = BeautifulSoup(response.text, "html.parser")
                
                title = self._extract_with_fallback(soup, selectors.get('title', []))
                content = self._extract_with_fallback(soup, selectors.get('content', []))
                date = self._extract_with_fallback(soup, selectors.get('date', []))
                
                if title or content:
                    self.logger.info(f"Successfully extracted from Wayback: {wayback_url}")
                    return {
                        'title': title or 'No title',
                        'content': content or title,
                        'publish_date': date
                    }
            except Exception as e:
                self.logger.error(f"Wayback extraction failed: {e}")
        return None
    
    def _extract_with_fallback(self, soup: BeautifulSoup, selectors: List[str]) -> str:
        """Try multiple selectors until one works"""
        for selector in selectors:
            try:
                elements = soup.select(selector)
                if elements:
                    if selector.startswith('meta'):
                        return elements[0].get('content', '')
                    else:
                        return ' '.join([elem.get_text().strip() for elem in elements])
            except:
                continue
        return ""
    
    def _is_relevant(self, article_data: Dict) -> bool:
        """Check if article is relevant using weighted scoring"""
        is_relevant, matches, score = check_relevance_weighted(
            article_data.get('title', ''), 
            article_data.get('content', '')
        )
        article_data['matched_keywords'] = matches
        article_data['relevance_score'] = score
        
        self.logger.debug(f"Relevance check: score={score}, matches={matches}, relevant={is_relevant}")
        return is_relevant

# Specific Scraper Implementations
class SeattleTimesScraper(BaseScraper):
    def scrape(self, query: str) -> List[Dict]:
        results = []
        base_urls = [
            "https://www.seattletimes.com/seattle-news/",
            "https://www.seattletimes.com/politics/"
        ]
        
        selectors = {
            'title': ['h1', '.headline', '.entry-title'],
            'content': ['article p', '.article-body p', 'p'],
            'date': ['meta[property="article:published_time"]', 'time']
        }
        
        for base_url in base_urls:
            try:
                article_urls = self._find_article_urls(base_url)
                
                for url in article_urls[:self.config.max_articles_per_source]:
                    article_data = self.extract_article_content(url, selectors)
                    if article_data and self._is_relevant(article_data):
                        article_data['url'] = url
                        article_data['source'] = 'Seattle Times'
                        results.append(article_data)
                    
                    time.sleep(self.config.delay_between_requests)
                    
            except Exception as e:
                self.logger.error(f"Error scraping {base_url}: {e}")
        
        return results
    
    def _find_article_urls(self, base_url: str) -> List[str]:
        try:
            # Add extra headers for Seattle Times
            headers = {
                'Referer': 'https://www.google.com/',
                'Cache-Control': 'no-cache'
            }
            response = self.session.get(base_url, timeout=self.config.request_timeout, headers=headers)
            
            if response.status_code == 202:
                self.logger.info(f"Site may be rate limiting (202). Trying Wayback Machine...")
                return self._find_wayback_urls(base_url)
            elif response.status_code != 200:
                self.logger.warning(f"Got status {response.status_code} from {base_url}. Trying Wayback...")
                return self._find_wayback_urls(base_url)
            
            soup = BeautifulSoup(response.text, "html.parser")
            
            article_links = set()  # Use set to avoid duplicates
            all_links = soup.find_all('a', href=True)
            self.logger.info(f"Found {len(all_links)} total links on {base_url}")
            
            for a in all_links:
                href = a['href']
                if href.startswith('/'):
                    href = "https://www.seattletimes.com" + href
                
                # More flexible matching - look for article patterns from past year
                if ('seattletimes.com' in href and 
                    ('seattle-news' in href or 'politics' in href or 'local' in href) and
                    any(year in href for year in ['/2025/', '/2024/']) and
                    not any(skip in href for skip in ['/feed/', '/rss', '/tag/', '/category/'])):
                    article_links.add(href)
            
            self.logger.info(f"Found {len(article_links)} potential article links")
            return list(article_links)
        
        except Exception as e:
            self.logger.error(f"Error finding article URLs from {base_url}: {e}")
            return []
    
    def _find_wayback_urls(self, base_url: str) -> List[str]:
        """Find article URLs from Wayback Machine"""
        wayback_url = get_wayback_url(base_url)
        if wayback_url:
            try:
                response = self.session.get(wayback_url, timeout=self.config.request_timeout)
                soup = BeautifulSoup(response.text, "html.parser")
                
                article_links = set()
                all_links = soup.find_all('a', href=True)
                
                for a in all_links:
                    href = a['href']
                    if href.startswith('/'):
                        href = "https://www.seattletimes.com" + href
                    
                    # More flexible Seattle Times filtering
                    if ('seattletimes.com' in href and 
                        len(href.split('/')) > 4 and
                        not any(skip in href for skip in ['/feed/', '/rss', '/tag/', '/category/', '/author/', '/games-', '/horoscopes', '/obituaries', '/jobs', '/classifieds'])):
                        article_links.add(href)
                
                self.logger.info(f"Found {len(article_links)} URLs from Wayback Machine")
                return list(article_links)
            except Exception as e:
                self.logger.error(f"Wayback URL finding failed: {e}")
        return []

class StrangerScraper(BaseScraper):
    def scrape(self, query: str) -> List[Dict]:
        results = []
        try:
            # Instead of search, crawl main news and election sections
            section_urls = [
                "https://www.thestranger.com/news/",
                "https://www.thestranger.com/stranger-election-control-board/"
            ]
            article_links = set()
            for section_url in section_urls:
                article_links.update(self._find_all_stranger_articles(section_url))
                time.sleep(self.config.delay_between_requests)
            selectors = {
                'title': ['h1', '.headline', '.entry-title'],
                'content': ['article p', '.article-body p', 'p'],
                'date': ['time', '.date']
            }
            for url in list(article_links)[:self.config.max_articles_per_source]:
                article_data = self.extract_article_content(url, selectors)
                if (article_data and 
                    len(article_data.get('content', '')) > self.config.min_content_length and 
                    self._is_relevant(article_data)):
                    article_data['url'] = url
                    article_data['source'] = 'The Stranger'
                    results.append(article_data)
                time.sleep(self.config.delay_between_requests)
        except Exception as e:
            self.logger.error(f"Error scraping The Stranger: {e}")
        return results

    def _find_all_stranger_articles(self, section_url: str) -> set:
        """Find all article URLs from a Stranger section page (not just search)"""
        article_links = set()
        try:
            response = self.session.get(section_url, timeout=self.config.request_timeout)
            soup = BeautifulSoup(response.text, "html.parser")
            links = soup.find_all('a', href=True)
            for a in links:
                href = a['href']
                # Accept all Stranger articles from 2024/2025 with enough path segments
                if any(year in href for year in ['/2025/', '/2024/']):
                    if not href.startswith('http'):
                        href = urljoin('https://www.thestranger.com', href)
                    if 'thestranger.com' in href and len(href.split('/')) > 4:
                        article_links.add(href)
        except Exception as e:
            self.logger.error(f"Error finding Stranger articles from section: {e}")
        self.logger.info(f"Found {len(article_links)} Stranger article links from {section_url}")
        return article_links

class CHSScraper(BaseScraper):
    def scrape(self, query: str) -> List[Dict]:
        results = []
        try:
            search_url = f"https://www.capitolhillseattle.com/?s={query}"
            article_links = self._find_chs_articles(search_url)
            selectors = {
                'title': ['h1', '.entry-title', '.headline'],
                'content': ['.entry-content p', 'article p', 'p'],
                'date': ['time', '.date', '.published']
            }
            for url in article_links[:self.config.max_articles_per_source]:
                article_data = self.extract_article_content(url, selectors)
                if (article_data and 
                    len(article_data.get('content', '')) > self.config.min_content_length and 
                    self._is_relevant(article_data)):
                    article_data['url'] = url
                    article_data['source'] = 'Capitol Hill Seattle'
                    results.append(article_data)
                time.sleep(self.config.delay_between_requests)
        except Exception as e:
            self.logger.error(f"Error scraping CHS: {e}")
        return results

    def _find_chs_articles(self, search_url: str) -> List[str]:
        article_links = set()
        try:
            response = self.session.get(search_url, timeout=self.config.request_timeout)
            soup = BeautifulSoup(response.text, "html.parser")
            links = soup.find_all('a', href=True)
            self.logger.info(f"Found {len(links)} links on CHS search page")
            for a in links:
                href = a['href']
                if ('capitolhillseattle.com' in href and 
                    any(year in href for year in ['/2025/', '/2024/']) and 
                    len(href.split('/')) > 4 and
                    'share=' not in href and
                    '#' not in href):
                    article_links.add(href)
            if not article_links:
                self.logger.info("No CHS articles found, trying Wayback Machine...")
                wayback_url = get_wayback_url(search_url)
                if wayback_url:
                    try:
                        response = self.session.get(wayback_url, timeout=self.config.request_timeout)
                        wayback_soup = BeautifulSoup(response.text, "html.parser")
                        wayback_links = wayback_soup.find_all('a', href=True)
                        for a in wayback_links:
                            href = a['href']
                            if ('capitolhillseattle.com' in href and 
                                any(year in href for year in ['/2025/', '/2024/']) and 
                                len(href.split('/')) > 4 and
                                'share=' not in href and
                                '#' not in href):
                                article_links.add(href)
                        self.logger.info(f"Found {len(article_links)} CHS articles from Wayback")
                    except Exception as e:
                        self.logger.error(f"Wayback CHS search failed: {e}")
        except Exception as e:
            self.logger.error(f"Error finding CHS articles: {e}")
        self.logger.info(f"Found {len(article_links)} CHS article links")
        return list(article_links)

class WebScrapingScraper(BaseScraper):
    """Scraper for news sites that don't have working RSS feeds"""
    def __init__(self, config: ScrapingConfig, session: requests.Session):
        super().__init__(config, session)
        self.news_sites = [
            {
                'name': 'KIRO 7',
                'url': 'https://www.kiro7.com/news/local/',
                'selectors': {
                    'title': ['h1', '.headline', '.story-headline', 'h2', '.title'],
                    'content': ['.story-body p', 'article p', '.content p', 'p'],
                    'date': ['time', '.date', '.story-date', '.published']
                }
            },
            {
                'name': 'KOMO News',
                'url': 'https://komonews.com/news/local',
                'selectors': {
                    'title': ['h1', '.headline', '.story-title', 'h2', '.title'],
                    'content': ['.story-content p', 'article p', '.content p', 'p'],
                    'date': ['time', '.date', '.story-date', '.published']
                }
            },
            {
                'name': 'KUOW',
                'url': 'https://www.kuow.org/news',
                'selectors': {
                    'title': ['h1', '.headline', '.story-title', 'h2', '.title'],
                    'content': ['.story-text p', 'article p', '.content p', 'p'],
                    'date': ['time', '.date', '.story-date', '.published']
                }
            },
            {
                'name': 'Seattle Times (Web)',
                'url': 'https://www.seattletimes.com/seattle-news/',
                'selectors': {
                    'title': ['h1', '.headline', '.entry-title', 'h2', '.title'],
                    'content': ['article p', '.article-body p', '.content p', 'p'],
                    'date': ['time', '.date', '.published', 'meta[property="article:published_time"]']
                }
            }
        ]

    def scrape(self, query: str = None) -> List[Dict]:
        """Scrape articles from news sites directly"""
        results = []
        seen_urls = set()
        
        for site in self.news_sites:
            try:
                self.logger.info(f"Scraping {site['name']}...")
                articles = self._scrape_site(site)
                
                self.logger.info(f"Extracted {len(articles)} articles from {site['name']}")
                
                for article in articles:
                    if article['url'] not in seen_urls and self._is_relevant(article):
                        article['source'] = site['name']
                        results.append(article)
                        seen_urls.add(article['url'])
                        
                        if len([r for r in results if r['source'] == site['name']]) >= self.config.max_articles_per_source:
                            break
                            
                time.sleep(self.config.delay_between_requests)
                
            except Exception as e:
                self.logger.error(f"Error scraping {site['name']}: {e}")
        
        return results
    
    def _scrape_site(self, site: dict) -> List[Dict]:
        """Scrape a single news site"""
        articles = []
        
        try:
            # Get the main page
            response = self.session.get(site['url'], timeout=self.config.request_timeout)
            soup = BeautifulSoup(response.text, 'html.parser')
            
            # Find article links - use broader selectors
            links = soup.find_all('a', href=True)
            article_urls = set()
            
            for link in links[:50]:  # Check more links
                href = link.get('href')
                if href:
                    if not href.startswith('http'):
                        href = urljoin(site['url'], href)
                    
                    # More flexible filtering - look for news articles
                    if (site['name'].lower().replace(' ', '') in href.lower() or 
                        'news' in href.lower() or 'local' in href.lower()) and \
                       len(href.split('/')) > 4 and \
                       not any(skip in href.lower() for skip in ['video', 'photo', 'gallery', 'rss', 'feed']):
                        article_urls.add(href)
            
            self.logger.info(f"Found {len(article_urls)} potential article URLs for {site['name']}")
            
            # Extract content from each article
            for url in list(article_urls)[:self.config.max_articles_per_source]:
                article_data = self.extract_article_content(url, site['selectors'])
                if article_data:
                    article_data['url'] = url
                    articles.append(article_data)
                    self.logger.debug(f"Successfully extracted: {article_data['title'][:50]}...")
                else:
                    self.logger.debug(f"Failed to extract content from: {url}")
                    
                time.sleep(self.config.delay_between_requests)
                
        except Exception as e:
            self.logger.error(f"Error scraping {site['name']}: {e}")
        
        return articles

class RSSFeedScraper(BaseScraper):
    def __init__(self, config: ScrapingConfig, session: requests.Session):
        super().__init__(config, session)
        self.rss_feeds = [
            ("https://www.seattletimes.com/seattle-news/feed/", "Seattle Times RSS"),
            ("https://www.king5.com/feeds/syndication/rss/news/local", "KING 5"),
        ]

    def scrape(self, query: str = None) -> List[Dict]:
        """Scan all articles from all RSS feeds, not just those matching a query."""
        results = []
        seen_urls = set()
        for rss_url, source_name in self.rss_feeds:
            try:
                headers = {
                    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
                    'Accept': 'application/rss+xml, application/xml, text/xml'
                }
                try:
                    response = self.session.get(rss_url, headers=headers, timeout=self.config.request_timeout)
                    if response.status_code == 200:
                        feed = feedparser.parse(response.content)
                    else:
                        self.logger.debug(f"RSS {source_name} returned status {response.status_code}")
                        feed = feedparser.parse(rss_url)
                except Exception as e:
                    self.logger.debug(f"RSS {source_name} request failed: {e}")
                    feed = feedparser.parse(rss_url)
                if len(feed.entries) > 0:
                    self.logger.info(f"RSS {source_name}: {len(feed.entries)} entries")
                else:
                    self.logger.debug(f"RSS {source_name}: {len(feed.entries)} entries")
                entries = feed.entries if feed.entries else []

                # If feed is empty, try limited Wayback Machine snapshots
                if not entries and self.config.use_wayback_fallback:
                    self.logger.info(f"No entries in {source_name} RSS, trying Wayback Machine...")
                    wayback_urls = get_recent_wayback_urls(rss_url, limit=self.config.max_wayback_attempts)
                    for wayback_url in wayback_urls:
                        try:
                            response = self.session.get(wayback_url, headers=headers, timeout=self.config.request_timeout)
                            wayback_feed = feedparser.parse(response.content)
                            if len(wayback_feed.entries) > 0:
                                self.logger.info(f"RSS {source_name} (Wayback): {len(wayback_feed.entries)} entries")
                            else:
                                self.logger.debug(f"RSS {source_name} (Wayback): {len(wayback_feed.entries)} entries")
                            entries.extend(wayback_feed.entries)
                            if entries:  # Stop after first successful wayback fetch
                                break
                        except Exception as e:
                            self.logger.debug(f"Wayback RSS fetch failed for {wayback_url}: {e}")

                for entry in entries:
                    title = entry.get('title', '')
                    summary = entry.get('summary', '')
                    if not summary and 'content' in entry:
                        summary = entry['content'][0].get('value', '')
                    publish_date = entry.get('published', '')
                    url = entry.get('link', '')

                    # Remove strict date filtering to get more articles
                    # if publish_date and not any(year in publish_date for year in ['2024', '2025']):
                    #     continue
                    if url in seen_urls:
                        continue
                    article_data = {
                        'title': title,
                        'content': summary or title,
                        'publish_date': publish_date
                    }
                    if self._is_relevant(article_data):
                        article_data.update({
                            'url': url,
                            'source': source_name
                        })
                        results.append(article_data)
                        seen_urls.add(url)
            except Exception as e:
                self.logger.error(f"Error scraping RSS {source_name}: {e}")

        return results

# Enhanced Wayback Machine Integration
def get_wayback_url(original_url: str, timestamp: str = None) -> Optional[str]:
    """Get archived version of URL from Internet Archive"""
    try:
        if timestamp:
            wayback_api = f"http://archive.org/wayback/available?url={original_url}&timestamp={timestamp}"
        else:
            wayback_api = f"http://archive.org/wayback/available?url={original_url}"
        
        resp = requests.get(wayback_api, timeout=10)
        data = resp.json()
        
        if data.get('archived_snapshots', {}).get('closest', {}).get('available'):
            archived_url = data['archived_snapshots']['closest']['url']
            logger.debug(f"Found archived version: {archived_url}")
            return archived_url
    except Exception as e:
        logger.debug(f"Error getting wayback URL for {original_url}: {e}")
    return None

def get_recent_wayback_urls(base_url: str, limit: int = 5) -> List[str]:
    """Get recent archived URLs from a base URL"""
    try:
        # Get URLs from the past year
        timestamps = ['20250130', '20250101', '20241201', '20241101', '20241001', '20240901', '20240801', '20240701', '20240601', '20240501', '20240401', '20240301']
        urls = []
        
        for timestamp in timestamps[:limit]:
            wayback_url = get_wayback_url(base_url, timestamp)
            if wayback_url:
                urls.append(wayback_url)
        
        return urls
    except Exception as e:
        logger.warning(f"Error getting recent wayback URLs: {e}")
        return []

# Alternative simple scraper for Seattle Times
class SimpleSeattleTimesScraper(BaseScraper):
    def scrape(self, query: str) -> List[Dict]:
        results = []
        try:
            # Try direct RSS approach first
            rss_url = "https://www.seattletimes.com/seattle-news/feed/"
            feed = feedparser.parse(rss_url)
            
            for entry in feed.entries:
                publish_date = entry.get('published', '')
                
                # Remove strict date filtering
                # if publish_date and not any(year in publish_date for year in ['2024', '2025']):
                #     continue
                
                article_data = {
                    'title': entry.get('title', ''),
                    'content': entry.get('summary', entry.get('description', '')),
                    'publish_date': publish_date,
                    'url': entry.get('link', ''),
                    'source': 'Seattle Times (RSS)'
                }
                
                if self._is_relevant(article_data):
                    results.append(article_data)
                    if len(results) >= self.config.max_articles_per_source:
                        break
            
            # Always try Wayback Machine for more articles
            self.logger.info("Trying Wayback Machine for more Seattle Times articles...")
            wayback_urls = get_recent_wayback_urls("https://www.seattletimes.com/seattle-news/", limit=5)
            
            for wayback_url in wayback_urls:
                try:
                    response = self.session.get(wayback_url, timeout=self.config.request_timeout)
                    soup = BeautifulSoup(response.text, "html.parser")
                    
                    # Look for article links in archived page - more permissive
                    links = soup.find_all('a', href=True)
                    for a in links[:50]:  # Check more links
                        href = a['href']
                        # Fix malformed Wayback URLs
                        if href.startswith('/web/'):
                            href = 'http://web.archive.org' + href
                        elif not href.startswith('http') and 'seattletimes.com' in href:
                            href = 'https://' + href.lstrip('/')
                        
                        if 'seattletimes.com' in href and len(href.split('/')) > 4:
                            # Try to extract this article
                            selectors = {
                                'title': ['h1', '.headline', '.entry-title'],
                                'content': ['article p', '.article-body p', 'p'],
                                'date': ['time', '.date']
                            }
                            article_data = self.extract_article_content(href, selectors)
                            if article_data and self._is_relevant(article_data):
                                article_data['url'] = href
                                article_data['source'] = 'Seattle Times (Simple)'
                                results.append(article_data)
                                if len(results) >= self.config.max_articles_per_source:
                                    break
                    if len(results) >= self.config.max_articles_per_source:
                        break
                except Exception as e:
                    self.logger.warning(f"Wayback extraction failed for {wayback_url}: {e}")
                    
        except Exception as e:
            self.logger.error(f"Error with simple Seattle Times scraper: {e}")
        
        return results

# Debug function to test individual scrapers
def test_scraper_debug():
    """Test function to debug scraper issues"""
    config = load_config()
    session = create_session(config)
    
    # Test Seattle Times scraper
    st_scraper = SeattleTimesScraper(config, session)
    print("Testing Seattle Times scraper...")
    
    # Test URL finding
    test_url = "https://www.seattletimes.com/seattle-news/"
    urls = st_scraper._find_article_urls(test_url)
    print(f"Found {len(urls)} URLs from {test_url}")
    if urls:
        print(f"Sample URLs: {urls[:3]}")
        
        # Test content extraction
        test_article = urls[0] if urls else None
        if test_article:
            selectors = {
                'title': ['h1', '.headline', '.entry-title'],
                'content': ['article p', '.article-body p', 'p'],
                'date': ['meta[property="article:published_time"]', 'time']
            }
            content = st_scraper.extract_article_content(test_article, selectors)
            if content:
                print(f"Sample content: title='{content['title'][:50]}...', content_len={len(content['content'])}")
                
                # Test relevance
                is_relevant = st_scraper._is_relevant(content)
                print(f"Is relevant: {is_relevant}")
    
    # Test simple Seattle Times scraper
    print("\nTesting Simple Seattle Times scraper...")
    simple_st = SimpleSeattleTimesScraper(config, session)
    simple_results = simple_st.scrape("mayor")
    print(f"Simple scraper found {len(simple_results)} articles")
    
    # Test RSS scraper
    print("\nTesting RSS scraper...")
    rss_scraper = RSSFeedScraper(config, session)
    rss_results = rss_scraper.scrape("mayor")
    print(f"RSS scraper found {len(rss_results)} articles")
    if rss_results:
        sample = rss_results[0]
        print(f"Sample RSS: {sample['title'][:50]}...")

# Main Execution Function
def main():
    config = load_config()
    session = create_session(config)
    
    # Initialize scrapers - prioritize working scrapers, minimize Seattle Times duplicates
    scrapers = [
        StrangerScraper(config, session),
        CHSScraper(config, session),
        RSSFeedScraper(config, session),
        WebScrapingScraper(config, session),
        SimpleSeattleTimesScraper(config, session)  # Only one Seattle Times scraper
    ]
    
    all_results = []
    
    for scraper in scrapers:
        try:
            logger.info(f"Running {scraper.__class__.__name__}...")
            results = scraper.scrape("mayor")
            
            # Process and validate results
            processed_results = []
            for result in results:
                processed = process_article_data(result)
                if processed:
                    processed_results.append(processed)
            
            all_results.extend(processed_results)
            logger.info(f"{scraper.__class__.__name__} returned {len(processed_results)} articles")
            
            time.sleep(config.delay_between_sources)
            
        except Exception as e:
            logger.error(f"Error with {scraper.__class__.__name__}: {e}")
    
    # Save results with metadata
    if all_results:
        df = pd.DataFrame(all_results)
        
        # Remove duplicates based on URL and title
        initial_count = len(df)
        df = df.drop_duplicates(subset=['url'], keep='first')
        df = df.drop_duplicates(subset=['title'], keep='first')
        logger.info(f"Removed {initial_count - len(df)} duplicates")
        
        # Sort by relevance score
        df = df.sort_values('relevance_score', ascending=False)
        
        # Add summary statistics
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"seattle_news_{timestamp}.csv"
        
        df.to_csv(filename, index=False)
        logger.info(f"Saved {len(df)} articles to {filename}")
        
        # Print detailed summary
        print(f"\nScraping completed. Found {len(df)} relevant articles:")
        source_counts = df['source'].value_counts()
        for source, count in source_counts.items():
            print(f"  {source}: {count} articles")
        
        print(f"\nTop articles by relevance score:")
        top_articles = df.head(5)
        for idx, row in top_articles.iterrows():
            print(f"  {row['relevance_score']:.1f} - {row['title'][:60]}... ({row['source']})")
            
    else:
        logger.warning("No articles found")

if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1 and sys.argv[1] == "--debug":
        test_scraper_debug()
    else:
        main()