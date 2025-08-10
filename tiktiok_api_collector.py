#!/usr/bin/env python
# coding: utf-8

# tiktok_data_collector.py
import config 
import requests
import json
import pandas as pd
from datetime import datetime, timedelta
import argparse

# --- Configuration ---
ACCESS_TOKEN = config.TIKTOK_ACCESS_TOKEN

def get_tiktok_video_time_series(username, total_days=365):
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
    
    print(f"--- Starting data collection for TikTok user: {username} ---")

    while days_searched < total_days:
        start_date = end_date - timedelta(days=30)
        start_date_str = start_date.strftime('%Y%m%d')
        end_date_str = end_date.strftime('%Y%m%d')
        
        print(f"--> Searching from {start_date_str} to {end_date_str}...")

        cursor = None
        has_more = True
        
        # Loop to handle pagination within the 30-day chunk
        while has_more:
            try:
                url = 'https://open.tiktokapis.com/v2/research/video/query/?fields=id,create_time,view_count,like_count,comment_count,share_count'
                headers = {'Authorization': f'Bearer {ACCESS_TOKEN}', 'Content-Type': 'application/json'}
                
                query_conditions = [{"field_name": "username", "operation": "IN", "field_values": [username]}]
                
                payload = {
                    "query": {"and": query_conditions},
                    "start_date": start_date_str,
                    "end_date": end_date_str,
                    "max_count": 100,
                    "cursor": cursor
                }

                response = requests.post(url, headers=headers, data=json.dumps(payload))
                response.raise_for_status()
                data = response.json()

                if data.get("data") and data["data"].get("videos"):
                    videos = data["data"]["videos"]
                    for video in videos:
                        all_videos.append({
                            'Date': pd.to_datetime(video['create_time'], unit='s').date(),
                            'Views': video['view_count']
                        })
                    
                    cursor = data["data"].get("cursor")
                    has_more = data["data"].get("has_more", False)
                else:
                    has_more = False

            except requests.exceptions.HTTPError as http_err:
                print(f"🛑 HTTP error occurred: {http_err}. Response: {response.text}")
                has_more = False # Stop on error for this chunk
            except Exception as e:
                print(f"An unexpected error occurred: {e}")
                has_more = False

        end_date = start_date
        days_searched += 30

    if not all_videos:
        print(f"No videos found for user {username} in the last {total_days} days.")
        return pd.DataFrame()

    df = pd.DataFrame(all_videos)
    # The API can sometimes return duplicates across chunks, so we drop them
    df = df.drop_duplicates()
    df = df.sort_values(by='Date').reset_index(drop=True)
    return df

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Fetch TikTok video data for a user.")
    parser.add_argument("--username", required=True, help="TikTok username to query.")
    args = parser.parse_args()

    if ACCESS_TOKEN == "Placeholder":
        print("⚠️ Please replace 'Placeholder' with your actual access token in the script.")
    else:
        time_series_df = get_tiktok_video_time_series(args.username)
        if not time_series_df.empty:
            time_series_df.to_csv(f"{args.username}_timeseries_data.csv", index=False)
            print(f"\n✅ Successfully collected {len(time_series_df)} videos and saved data to timeseries_data.csv")



