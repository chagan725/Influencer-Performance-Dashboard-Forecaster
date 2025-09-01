#!/usr/bin/env Rscript
# Quick forecast test for a TikTok timeseries CSV
# Usage: Rscript test_forecast_tiktok.R path/to/timeseries.csv [periods]

args <- commandArgs(trailingOnly = TRUE)
csv_path <- if (length(args) >= 1) args[[1]] else stop("Please provide a timeseries CSV file path as the first argument.")
periods <- if (length(args) >= 2) as.integer(args[[2]]) else 30

check_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Package '%s' is required but not installed. Install it in R before running this script." , pkg))
    message(sprintf("In R: install.packages('%s')  or use your package manager. For 'prophet' follow install instructions at https://facebook.github.io/prophet/" , pkg))
    quit(status = 2)
  }
}

check_pkg('readr')
check_pkg('prophet')
check_pkg('dplyr')

library(readr)
library(prophet)
library(dplyr)

message('Reading CSV: ', csv_path)
df <- readr::read_csv(csv_path, show_col_types = FALSE)

# Try to find the date and value columns using common names
cn <- tolower(names(df))
date_col <- names(df)[which(cn %in% c('date','ds','created_at','create_time','created'))[1]]
value_col <- names(df)[which(cn %in% c('views','view_count','y','value','likes'))[1]]

if (is.null(date_col) || is.null(value_col)) {
  message('Could not auto-detect date or value columns. Found columns:')
  message(paste(names(df), collapse = ', '))
  quit(status = 3)
}

message('Using date column: ', date_col, ' and value column: ', value_col)

df2 <- df %>%
  mutate(ds = as.Date(.data[[date_col]]), y = as.numeric(.data[[value_col]])) %>%
  group_by(ds) %>%
  summarize(y = sum(y, na.rm = TRUE)) %>%
  ungroup()

if (nrow(df2) < 10) {
  message('Not enough data points (need at least 10 rows) to train a reliable model.')
  quit(status = 4)
}

message('Fitting Prophet model on ', nrow(df2), ' points...')
m <- prophet::prophet(df2)

message('Creating future dataframe for ', periods, ' periods')
future <- prophet::make_future_dataframe(m, periods = periods)
forecast <- predict(m, future)

out_csv <- file.path(dirname(csv_path), paste0(tools::file_path_sans_ext(basename(csv_path)), '_forecast.csv'))
out_png <- file.path(dirname(csv_path), paste0(tools::file_path_sans_ext(basename(csv_path)), '_forecast.png'))

message('Writing forecast to: ', out_csv)
write.csv(forecast, out_csv, row.names = FALSE)

message('Saving plot to: ', out_png)
png(out_png, width = 1200, height = 800)
plot(m, forecast)
prophet_plot_components(m, forecast)
dev.off()

message('Forecast complete. A few rows of the forecast:')
print(utils::head(forecast[c('ds','yhat','yhat_lower','yhat_upper')]))

message('Done')
