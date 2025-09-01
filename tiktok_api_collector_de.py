# tiktok_data_collector.py
import os
import requests
import logging
import json
import pandas as pd
from datetime import datetime, timedelta
from dotenv import load_dotenv, find_dotenv
import argparse
import subprocess
import keyring

# --- Logging Configuration ---
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)

load_dotenv()

# --- Configuration ---
# Prefer environment var, then keyring (set by refresh script). If neither present, collector will raise.
ACCESS_TOKEN = os.environ.get('TIKTOK_ACCESS_TOKEN')
if not ACCESS_TOKEN:
    # Use the project .env path as the keyring service name (same as the refresh script uses)
    env_path = os.path.abspath(find_dotenv() or '.env')
    try:
        ACCESS_TOKEN = keyring.get_password(env_path, 'TIKTOK_ACCESS_TOKEN')
    except Exception:
        ACCESS_TOKEN = None
if not ACCESS_TOKEN:
    raise ValueError("TIKTOK_ACCESS_TOKEN not found in environment or keyring. Run refresh_tiktok_token.py to obtain one.")

def get_tiktok_video_time_series(username, total_days=1095):
    """
    Fetches the performance of past videos for a given TikTok user to create a time-series dataset.

    Args:
        username (str): The TikTok username to query.
        total_days (int): How many days back to search for videos.

    Returns:
        pd.DataFrame: A DataFrame with 'Date' and 'Views' for the user's videos.
    """
    all_videos = []
    end_date = datetime.now()
    days_searched = 0
    # Work with a local token variable so we can update it on refresh without
    # creating an unbound local error for the global ACCESS_TOKEN.
    token = ACCESS_TOKEN
    
    logging.info(f"--- Starting data collection for TikTok user: {username} ---")

    while days_searched < total_days:
        start_date = end_date - timedelta(days=30)
        start_date_str = start_date.strftime('%Y%m%d')
        end_date_str = end_date.strftime('%Y%m%d')
        
        logging.info(f"--> Searching from {start_date_str} to {end_date_str}...")

        cursor = None
        has_more = True
        
        # Loop to handle pagination within the 30-day chunk
        while has_more:
            try:
                url = 'https://open.tiktokapis.com/v2/research/video/query/?fields=id,create_time,view_count,like_count,comment_count,share_count'
                headers = {'Authorization': f'Bearer {token}', 'Content-Type': 'application/json'}
                
                query_conditions = [{"field_name": "username", "operation": "IN", "field_values": [username]}]
                
                payload = {
                    "query": {"and": query_conditions},
                    "start_date": start_date_str,
                    "end_date": end_date_str,
                    "max_count": 100,
                    "cursor": cursor
                }

                response = requests.post(url, headers=headers, data=json.dumps(payload))
                # If token invalid, try a one-time refresh and retry
                if response.status_code == 401:
                    logging.info('Received 401 from TikTok API, attempting to refresh token and retry once')
                    # Call refresh script using conda python (adjust path if needed)
                    try:
                        subprocess.run(['/opt/anaconda3/bin/python', 'refresh_tiktok_token.py'], check=True)
                        # reload token from keyring or env
                        token = os.environ.get('TIKTOK_ACCESS_TOKEN') or keyring.get_password(os.path.abspath(find_dotenv() or '.env'), 'TIKTOK_ACCESS_TOKEN')
                        headers['Authorization'] = f'Bearer {token}'
                        response = requests.post(url, headers=headers, data=json.dumps(payload))
                    except subprocess.CalledProcessError:
                        logging.error('Token refresh script failed')
                        response.raise_for_status()
                response.raise_for_status()
                data = response.json()

                if data.get("data") and data["data"].get("videos"):
                    videos = data["data"]["videos"]
                    for video in videos:
                        all_videos.append({
                            'Date': pd.to_datetime(video['create_time'], unit='s').date(),
                            'Views': video['view_count'],
                            'Likes': video['like_count']
                        })
                    
                    cursor = data["data"].get("cursor")
                    has_more = data["data"].get("has_more", False)
                else:
                    has_more = False

            except requests.exceptions.HTTPError as http_err:
                logging.error(f"HTTP error occurred: {http_err}. Response: {response.text}")
                has_more = False # Stop on error for this chunk
            except Exception as e:
                logging.error(f"An unexpected error occurred: {e}", exc_info=True)
                has_more = False

        end_date = start_date
        days_searched += 30

    if not all_videos:
        logging.warning(f"No videos found for user {username} in the last {total_days} days.")
        return pd.DataFrame()

    df = pd.DataFrame(all_videos)
    df = df.drop_duplicates()
    df = df.sort_values(by='Date').reset_index(drop=True)
    return df

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Fetch TikTok video data for a user.")
    parser.add_argument("--username", required=True, help="TikTok username to query.")
    parser.add_argument("--days", type=int, default=1095, help="How many days back to search for videos. Default is 1095 (3 years).")
    args = parser.parse_args()

    time_series_df = get_tiktok_video_time_series(args.username, total_days=args.days)
    if not time_series_df.empty:
        output_filename = f"{args.username}_tiktok_timeseries_data.csv"
        time_series_df.to_csv(output_filename, index=False)
        logging.info(f"Successfully collected {len(time_series_df)} videos from the last {args.days} days and saved data to {output_filename}")
