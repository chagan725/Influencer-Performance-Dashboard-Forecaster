"""refresh_tiktok_token.py

Utility to refresh a TikTok access token using an OAuth refresh-token flow,
persist the new access token (and optionally new refresh token) into a .env file,
and exit with non-zero code on failure so it can be used in cron/systemd scripts.

USAGE
  - Fill in TIKTOK_TOKEN_ENDPOINT, CLIENT_ID and CLIENT_SECRET in your .env
  - Ensure you have a valid REFRESH_TOKEN in .env
  - Run manually or schedule with cron / systemd / APScheduler

Security: prefer storing credentials in a secrets manager or OS keychain rather
than a plaintext .env in production.
"""
import os
import sys
import logging
import requests
import keyring
from dotenv import load_dotenv, set_key, find_dotenv

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')


def refresh_access_token(env_path=None):
    # Load .env
    if env_path is None:
        env_path = find_dotenv()
    if not env_path:
        logging.error('.env file not found. Please create one in the project root.')
        return False

    load_dotenv(env_path)

    # Required configuration (fill these in your .env)
    TOKEN_ENDPOINT = os.getenv('TIKTOK_TOKEN_ENDPOINT')
    # Support both TIKTOK_CLIENT_KEY (used in TikTok docs) and TIKTOK_CLIENT_ID
    CLIENT_KEY = os.getenv('TIKTOK_CLIENT_KEY')
    CLIENT_ID = os.getenv('TIKTOK_CLIENT_ID')
    CLIENT_SECRET = os.getenv('TIKTOK_CLIENT_SECRET')
    REFRESH_TOKEN = os.getenv('TIKTOK_REFRESH_TOKEN')
    # Choose the grant flow: 'client_credentials' or 'refresh_token' (default)
    GRANT_TYPE = os.getenv('TIKTOK_GRANT_TYPE', 'refresh_token')

    # require client secret and token endpoint; require a client id/key
    if not TOKEN_ENDPOINT or not CLIENT_SECRET or not (CLIENT_KEY or CLIENT_ID):
        logging.error('Missing required env vars: TIKTOK_TOKEN_ENDPOINT, TIKTOK_CLIENT_ID, TIKTOK_CLIENT_SECRET')
        return False

    # Build payload depending on grant type. The TikTok API uses different names for
    # client credentials endpoints (client_key/client_secret) vs refresh flow.
    if GRANT_TYPE == 'client_credentials':
        # Matches the curl you provided. Use TIKTOK_CLIENT_KEY if present, else TIKTOK_CLIENT_ID.
        client_key_value = CLIENT_KEY or CLIENT_ID
        payload = {
            'grant_type': 'client_credentials',
            'client_key': client_key_value,
            'client_secret': CLIENT_SECRET,
        }
    else:
        if not REFRESH_TOKEN:
            logging.error('TIKTOK_REFRESH_TOKEN is required for refresh_token grant')
            return False
        payload = {
            'grant_type': 'refresh_token',
            'refresh_token': REFRESH_TOKEN,
            'client_id': CLIENT_ID,
            'client_secret': CLIENT_SECRET,
        }

    try:
        resp = requests.post(TOKEN_ENDPOINT, data=payload, timeout=15)
        # Helpful debug logging: record status, headers and body so API errors are visible.
        logging.debug('Token endpoint response status: %s', resp.status_code)
        logging.debug('Token endpoint response headers: %s', resp.headers)
        logging.debug('Token endpoint response body: %s', resp.text)
        if resp.status_code >= 400:
            # Surface the error body at INFO/ERROR so it's visible without changing global log level
            logging.error('Token endpoint returned error %s: %s', resp.status_code, resp.text)
        resp.raise_for_status()
    except Exception as e:
        logging.exception('Token refresh HTTP request failed')
        return False

    data = resp.json()
    # Expecting keys like access_token, expires_in, refresh_token (optional)
    # Some TikTok endpoints may return different key names; try a few common ones.
    new_access_token = data.get('access_token') or data.get('accessToken')
    new_refresh_token = data.get('refresh_token') or data.get('refreshToken')
    expires_in = data.get('expires_in') or data.get('expiresIn')

    if not new_access_token:
        logging.error('No access_token in refresh response: %s', data)
        return False

    # Persist updated tokens: store access token in OS keyring, and refresh token (if present)
    try:
        # Use the project path as the keyring service name for uniqueness
        service_name = os.path.abspath(env_path)
        keyring.set_password(service_name, 'TIKTOK_ACCESS_TOKEN', new_access_token)
        logging.info('Access token stored in OS keyring under service %s', service_name)
        if new_refresh_token:
            set_key(env_path, 'TIKTOK_REFRESH_TOKEN', new_refresh_token)
        if expires_in is not None:
            set_key(env_path, 'TIKTOK_ACCESS_EXPIRES_IN', str(expires_in))
    except Exception:
        logging.exception('Failed to persist updated token')
        return False

    logging.info('Access token refreshed and securely stored')
    return True


def main():
    env_path = None  # uses find_dotenv()
    ok = refresh_access_token(env_path=env_path)
    if not ok:
        logging.error('Token refresh failed')
        sys.exit(1)
    logging.info('Token refresh succeeded')


if __name__ == '__main__':
    main()
