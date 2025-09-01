# Helper script to deploy this Shiny app to shinyapps.io
# Usage: open in RStudio and run interactively, or source() from an R session.

check_and_deploy <- function(appDir = '.', appName = 'influencer-dashboard') {
  required_envs <- c('TIKTOK_CLIENT_KEY','TIKTOK_CLIENT_SECRET','TIKTOK_TOKEN_ENDPOINT','TIKTOK_GRANT_TYPE')
  missing <- required_envs[!nzchar(Sys.getenv(required_envs))]
  if (length(missing) > 0) {
    message('Warning: the following env vars are not set in your local environment: ', paste(missing, collapse = ', '))
    message('Make sure to set these in shinyapps.io Dashboard under the application Settings -> Environment Variables before publishing.')
  }

  if (!file.exists(file.path(appDir, 'app.R'))) stop('app.R not found in project root')
  if (!file.exists(file.path(appDir, 'requirements.txt'))) warning('requirements.txt not found; reticulate may not install needed Python packages on the host')

  message('Ready to deploy. Example deploy command:')
  cat('\nlibrary(rsconnect)\nrsconnect::deployApp(appDir = "', appDir, '", appName = "', appName, '")\n', sep='')
}

# Run when sourced
if (identical(environment(), globalenv())) {
  check_and_deploy()
}
