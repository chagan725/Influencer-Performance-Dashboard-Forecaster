import os
import json
import keyring
import types
from tiktok_api_collector_de import get_tiktok_video_time_series


class DummyResponse:
    def __init__(self, status_code, json_data=None, text=''):
        self.status_code = status_code
        self._json = json_data or {}
        self.text = text

    def json(self):
        return self._json

    def raise_for_status(self):
        if self.status_code >= 400:
            raise Exception(f'Status {self.status_code}: {self.text}')


def test_retry_refresh(monkeypatch, tmp_path):
    # Ensure keyring has a token so initial attempt uses it
    env_path = os.path.abspath('.env')
    keyring.set_password(env_path, 'TIKTOK_ACCESS_TOKEN', 'old_token')

    calls = {'count': 0}

    def fake_post(url, headers=None, data=None):
        calls['count'] += 1
        # First call returns 401, second returns a 200 with empty data
        if calls['count'] == 1:
            return DummyResponse(401, text='Unauthorized')
        else:
            return DummyResponse(200, json_data={'data': {'videos': []}})

    monkeypatch.setattr('requests.post', fake_post)

    # Monkeypatch subprocess.run used to refresh token to simply set a new token in keyring
    def fake_run(cmd, check=False):
        # simulate refresh script storing new token
        keyring.set_password(env_path, 'TIKTOK_ACCESS_TOKEN', 'new_token')
        return types.SimpleNamespace(returncode=0)

    monkeypatch.setattr('subprocess.run', fake_run)

    # Run the collector (it will hit our fake_post and fake_run)
    df = get_tiktok_video_time_series('testuser', total_days=1)
    assert calls['count'] >= 2
    assert df.empty
