# tiktok_data_collector.py
import os
import requests
import time
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
# We'll fetch an access token on-demand using client_credentials when needed.
# Prefer TIKTOK_ACCESS_TOKEN env var as an override; otherwise use client credentials.
ACCESS_TOKEN = os.environ.get('TIKTOK_ACCESS_TOKEN')
TIKTOK_CLIENT_KEY = os.getenv('TIKTOK_CLIENT_KEY') or os.getenv('TIKTOK_CLIENT_ID')
TIKTOK_CLIENT_SECRET = os.getenv('TIKTOK_CLIENT_SECRET')
TIKTOK_TOKEN_ENDPOINT = os.getenv('TIKTOK_TOKEN_ENDPOINT', 'https://open.tiktokapis.com/v2/oauth/token/')


def get_access_token():
    """Return an access token. Prefer environment override, else request one via client_credentials."""
    # If a token is explicitly set in the environment, use it (no caching)
    if os.environ.get('TIKTOK_ACCESS_TOKEN'):
        return os.environ.get('TIKTOK_ACCESS_TOKEN')

    # In-memory cache to avoid requesting a token on every call.
    # Cached values live only for the process lifetime.
    global _cached_token, _cached_token_expiry
    try:
        _cached_token
    except NameError:
        _cached_token = None
        _cached_token_expiry = 0

    now = int(time.time())
    if _cached_token and _cached_token_expiry and now < _cached_token_expiry:
        return _cached_token

    if not (TIKTOK_CLIENT_KEY and TIKTOK_CLIENT_SECRET):
        return None

    payload = {
        'grant_type': 'client_credentials',
        'client_key': TIKTOK_CLIENT_KEY,
        'client_secret': TIKTOK_CLIENT_SECRET,
    }
    try:
        resp = requests.post(TIKTOK_TOKEN_ENDPOINT, data=payload, timeout=15)
        logging.debug('Token endpoint response code: %s, body: %s', resp.status_code, resp.text)
        resp.raise_for_status()
        data = resp.json()
        token = data.get('access_token') or data.get('accessToken')
        expires_in = data.get('expires_in') or data.get('expiresIn') or 3600
        # Cache token with a small safety margin
        _cached_token = token
        _cached_token_expiry = now + int(expires_in) - 60
        return token
    except Exception:
        logging.exception('Failed to obtain access token from TikTok')
        return None

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
    # Work with a local token variable so we can update it on refresh
    token = ACCESS_TOKEN or get_access_token()
    
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
                # If token invalid, try obtaining a fresh one via client_credentials and retry once
                if response.status_code == 401:
                    logging.info('Received 401 from TikTok API, attempting to obtain new token and retry once')
                    token = get_access_token()
                    if token:
                        headers['Authorization'] = f'Bearer {token}'
                        response = requests.post(url, headers=headers, data=json.dumps(payload))
                    else:
                        logging.error('Could not obtain a new access token; aborting')
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
        out_dir = os.getenv('APP_DATA_DIR', '.')
        os.makedirs(out_dir, exist_ok=True)
        output_filename = f"{args.username}_tiktok_timeseries_data.csv"
        output_path = os.path.join(out_dir, output_filename)
        try:
            time_series_df.to_csv(output_path, index=False)
            logging.info(f"Successfully collected {len(time_series_df)} videos from the last {args.days} days and saved data to {output_path}")
            print(f"OUTPUT_PATH:{output_path}")
        except Exception as e:
            logging.exception('Failed to write TikTok output CSV')
            print(f"ERROR_WRITING_TIKTOK_OUTPUT:{e}")
