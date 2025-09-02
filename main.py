import argparse
import logging
import os

# Collector imports are deferred to runtime (imported only when needed) so
# missing credentials for one platform don't break the other.

# --- Logging Configuration ---
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)

def main():
    parser = argparse.ArgumentParser(
        description="Unified data collector for YouTube and TikTok.",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument("platform", choices=['youtube', 'tiktok'], help="The platform to collect data from.")
    parser.add_argument("--username", required=True, help="The username or channel handle for the platform.")
    parser.add_argument("--days", type=int, default=1095, help="[TikTok only] Days back to search. Default is 1095.")
    parser.add_argument("--max-results", type=int, default=5000, help="[YouTube only] Max videos to fetch. Default is 5000.")
    parser.add_argument("--output", help="Optional: Specify a custom output CSV file name.")

    args = parser.parse_args()

    if args.platform == 'youtube':
        from youtube_data_collector_de import get_video_time_series_for_channel
        logging.info(f"Starting YouTube data collection for handle: {args.username}")
        df = get_video_time_series_for_channel(args.username, max_results=args.max_results)
        default_filename = f"{args.username}_youtube_timeseries_data.csv"

    elif args.platform == 'tiktok':
        from tiktok_api_collector_de import get_tiktok_video_time_series
        logging.info(f"Starting TikTok data collection for user: {args.username}")
        df = get_tiktok_video_time_series(args.username, total_days=args.days)
        default_filename = f"{args.username}_tiktok_timeseries_data.csv"

    if df is not None and not df.empty:
        # Respect APP_DATA_DIR environment variable for where to write outputs (default to project root)
        out_dir = os.getenv('APP_DATA_DIR', '.')
        os.makedirs(out_dir, exist_ok=True)
        output_filename = args.output if args.output else default_filename
        output_path = os.path.join(out_dir, output_filename)
        try:
            df.to_csv(output_path, index=False)
            logging.info(f"Successfully collected {len(df)} records and saved to {output_path}")
            print(f"OUTPUT_PATH:{output_path}")
        except Exception as e:
            logging.exception('Failed to write output CSV')
            print(f"ERROR_WRITING_OUTPUT:{e}")
    else:
        logging.warning(f"No data collected for {args.username} on {args.platform}. No file was saved.")

if __name__ == '__main__':
    main()