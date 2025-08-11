import pandas as pd
import glob
from datetime import datetime
import os

def merge_all_csv_files():
    """Merge all seattle_news_*.csv files into a master database"""
    
    # Find all CSV files matching the pattern
    csv_files = glob.glob("seattle_news_*.csv")
    
    if not csv_files:
        print("No seattle_news_*.csv files found")
        return
    
    print(f"Found {len(csv_files)} CSV files to merge")
    
    all_data = []
    
    for file in csv_files:
        try:
            df = pd.read_csv(file)
            # Add source file info
            df['source_file'] = file
            all_data.append(df)
            print(f"Loaded {len(df)} articles from {file}")
        except Exception as e:
            print(f"Error loading {file}: {e}")
    
    if not all_data:
        print("No data loaded")
        return
    
    # Combine all dataframes
    master_df = pd.concat(all_data, ignore_index=True)
    print(f"Combined total: {len(master_df)} articles")
    
    # Remove duplicates based on URL
    initial_count = len(master_df)
    master_df = master_df.drop_duplicates(subset=['url'], keep='first')
    print(f"After deduplication: {len(master_df)} articles ({initial_count - len(master_df)} duplicates removed)")
    
    # Sort by scraped_at timestamp if available, otherwise by source_file
    if 'scraped_at' in master_df.columns:
        master_df = master_df.sort_values('scraped_at')
    else:
        master_df = master_df.sort_values('source_file')
    
    # Save master database
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_file = f"seattle_news_master_{timestamp}.csv"
    master_df.to_csv(output_file, index=False)
    
    print(f"Master database saved to {output_file}")
    
    # Print summary statistics
    print("\nSummary by source:")
    if 'source' in master_df.columns:
        source_counts = master_df['source'].value_counts()
        for source, count in source_counts.items():
            print(f"  {source}: {count} articles")
    
    # Print date range if available
    if 'scraped_at' in master_df.columns:
        dates = pd.to_datetime(master_df['scraped_at'], errors='coerce')
        valid_dates = dates.dropna()
        if len(valid_dates) > 0:
            print(f"\nDate range: {valid_dates.min()} to {valid_dates.max()}")

if __name__ == "__main__":
    merge_all_csv_files()