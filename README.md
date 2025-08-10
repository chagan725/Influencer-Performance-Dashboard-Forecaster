# Influencer Performance Dashboard & Forecaster

This project is a Shiny dashboard that analyzes influencer data from TikTok, YouTube, and Instagram. It also includes a forecasting tool using the Prophet model to predict future video performance.

## Setup

### 1. R Packages
Install the required R packages by running this command in your R console:
```R
install.packages(c("shiny", "shinydashboard", "bslib", "readr", "DT", "ggplot2", "plotly", "dplyr", "janitor", "shinycssloaders", "scales", "prophet", "reticulate"))
```

### 2. Python Packages
Install the required Python packages by running this command in your terminal:
```bash
pip install -r requirements.txt
```

### 3. API Keys
Create a file named `config.py` and add your API keys:
```python
# config.py
TIKTOK_ACCESS_TOKEN = "YOUR_TIKTOK_TOKEN"
YOUTUBE_API_KEY = "YOUR_YOUTUBE_KEY"
```

## How to Run
1.  Configure the Python path at the top of `dashboard_with_prophet.R`.
2.  Open `dashboard_with_prophet.R` in RStudio.
3.  Click "Run App".
