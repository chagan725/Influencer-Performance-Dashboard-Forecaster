import os
import logging
import pandas as pd
import argparse
from dotenv import load_dotenv
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError

# --- Logging Configuration ---
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
load_dotenv()

# --- Configuration ---
# Defer API key validation and client creation to runtime so importing this module
# doesn't fail in environments where the key is intentionally absent.
API_KEY = os.environ.get('YOUTUBE_API_KEY')
YOUTUBE_API_SERVICE_NAME = 'youtube'
YOUTUBE_API_VERSION = 'v3'


def get_video_time_series_for_channel(handle, max_results=5000):
    """
    Fetches the performance of past videos for a given channel handle to create a time-series dataset.

    Args:
        handle (str): The YouTube channel handle (e.g., 'MrBeast').
        max_results (int): The maximum number of recent videos to fetch.

    Returns:
        pd.DataFrame: A DataFrame with 'Date' and 'Views' for the channel's videos.
    """
    try:
        # Validate API key and create client at runtime
        if not API_KEY:
            logging.error("YOUTUBE_API_KEY environment variable not set. Cannot fetch data.")
            return pd.DataFrame()
        youtube = build(YOUTUBE_API_SERVICE_NAME, YOUTUBE_API_VERSION, developerKey=API_KEY)

        # 1. Get channel details to find the 'uploads' playlist ID
        channel_request = youtube.channels().list(
            part='contentDetails',
            forHandle=handle
        )
        channel_response = channel_request.execute()
        if not channel_response.get('items'):
            logging.error(f"Channel not found for handle '{handle}'")
            return pd.DataFrame() # Return empty DataFrame
            
        playlist_id = channel_response['items'][0]['contentDetails']['relatedPlaylists']['uploads']

        # 2. Get the list of video IDs from the uploads playlist
        video_ids = []
        next_page_token = None
        while len(video_ids) < max_results:
            logging.info(f"Fetching video page... (collected {len(video_ids)} so far)")
            playlist_request = youtube.playlistItems().list(
                part='contentDetails',
                playlistId=playlist_id,
                maxResults=min(50, max_results - len(video_ids)),
                pageToken=next_page_token
            )
            playlist_response = playlist_request.execute()
            
            video_ids.extend([item['contentDetails']['videoId'] for item in playlist_response['items']])
            
            next_page_token = playlist_response.get('nextPageToken')
            if not next_page_token:
                break
        
        # 3. Get statistics and publication dates for each video
        video_details = []
        for i in range(0, len(video_ids), 50):
            batch_ids = video_ids[i:i+50]
            video_request = youtube.videos().list(
                part='snippet,statistics',
                id=','.join(batch_ids)
            )
            video_response = video_request.execute()
            
            for item in video_response['items']:
                video_details.append({
                    'Date': pd.to_datetime(item['snippet']['publishedAt']).date(),
                    'Views': int(item['statistics'].get('viewCount', 0)),
                    'Likes': int(item['statistics'].get('likeCount', 0))
                })

        df = pd.DataFrame(video_details)
        df = df.sort_values(by='Date').reset_index(drop=True)
        return df

    except HttpError as e:
        logging.error(f"An HTTP error occurred: {e}")
        return pd.DataFrame()
    except Exception as e:
        logging.error(f"An unexpected error occurred: {e}", exc_info=True)
        return pd.DataFrame()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("--username", required=True, help="YouTube channel handle (e.g., 'MrBeast')")
    parser.add_argument("--max-results", type=int, default=5000, help="Maximum number of videos to fetch. Default is 5000.")
    args = parser.parse_args()
    
    time_series_df = get_video_time_series_for_channel(args.username, max_results=args.max_results)
    
    if not time_series_df.empty:
        output_filename = f"{args.username}_youtube_timeseries_data.csv"
        time_series_df.to_csv(output_filename, index=False)
        logging.info(f"Successfully collected {len(time_series_df)} videos and saved data for {args.username} to {output_filename}")
