Shinyapps.io deployment steps

This file explains how to publish the Shiny app in this repository to shinyapps.io. It intentionally avoids storing secrets in the repo — set them in the shinyapps.io dashboard or via the rsconnect UI.

Checklist before deploying
- Confirm `app.R` and supporting files are on the branch you want to deploy.
- Ensure `requirements.txt` at the project root lists all Python packages your app needs (pandas, requests, keyring, google-api-python-client, python-dotenv, etc.).
- Do NOT commit any secret tokens or client secrets. Use the shinyapps.io dashboard to set environment variables.

Required environment variables (set in shinyapps.io -> Settings -> Environment Variables):
- TIKTOK_CLIENT_KEY
- TIKTOK_CLIENT_SECRET
- TIKTOK_TOKEN_ENDPOINT (recommended default: https://open.tiktokapis.com/v2/oauth/token/)
- TIKTOK_GRANT_TYPE (recommended: client_credentials)
- YOUTUBE_API_KEY (if you use YouTube collection)
- Optionally TIKTOK_ACCESS_TOKEN (for testing only; recommended to let the app request a token at runtime)

High-level deploy steps (RStudio / rsconnect)
1. Install rsconnect (if needed):

```r
install.packages('rsconnect')
```

2. Connect your local R session to your shinyapps.io account. There are two common ways:
   - Use the RStudio GUI (Publishing -> Account -> Connect) and follow the sign-in flow.
   - Or set account info manually (only if you have the token/secret):

```r
rsconnect::setAccountInfo(name='YOUR_ACCOUNT_NAME', token='YOUR_TOKEN', secret='YOUR_SECRET')
```

3. Confirm environment variables are set in the shinyapps.io dashboard for the application (Settings -> Environment Variables).

4. Deploy from the project root (in R):

```r
library(rsconnect)
rsconnect::deployApp(appDir = '.', appName = 'influencer-dashboard')
```

Notes and troubleshooting
- Python packages: shinyapps.io will attempt to install Python packages when `reticulate` is detected and `requirements.txt` exists at the repo root. If the host cannot install a package, include the package in `requirements.txt` and redeploy.
- If your app needs a specific Python interpreter, avoid hardcoding `RETICULATE_PYTHON` in `app.R`; instead rely on the environment and the hosting provider's Python support.
- Keep tokens out of the repo. Use the shinyapps.io dashboard's Environment Variables for production credentials.

If you want, run the `deploy/deploy_shinyapps.R` script in this repo — it checks for common misconfigurations and shows the exact `rsconnect::deployApp()` command to run.
