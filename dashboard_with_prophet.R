# --- 1. Load All Necessary Libraries ---
library(shiny)
library(shinydashboard)
library(bslib)
library(readr)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(janitor)
library(shinycssloaders)
library(scales)
library(prophet)
library(reticulate)

# --- Python Configuration ---
reticulate::use_python("/opt/anaconda3/bin/python", required = TRUE)

# --- 2. UI - Using shinydashboard for a Professional Layout ---
ui <- dashboardPage(
  dashboardHeader(title = "Influence Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("platform", "Select Platform:",
                  choices = c("TikTok", "YouTube", "Instagram")),
      selectInput("country", "Select Country:",
                  choices = NULL),
      conditionalPanel(
        condition = "input.platform == 'YouTube' || input.platform == 'Instagram'",
        selectInput("category", "Select Category:",
                    choices = NULL)
      ),
      hr(),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-line")),
      hr(),
      downloadButton("download_data", "Download Filtered Data", class = "btn-block")
    )
  ),
  dashboardBody(
    theme = bslib::bs_theme(bootswatch = "minty"),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_influencers", width = 4),
                valueBoxOutput("avg_followers", width = 4),
                valueBoxOutput("avg_engagement", width = 4)
              ),
              tabsetPanel(
                id = 'mp',
                tabPanel('Scatterplot', value = 2,
                         fluidRow(
                           box(title = "Plot Controls", width = 3, solidHeader = TRUE, status = "primary",
                               selectInput('x', 'Select X-Axis:', choices = NULL),
                               selectInput('y', 'Select Y-Axis:', choices = NULL),
                               selectInput('color_var', 'Select Color Variable:', choices = NULL),
                               checkboxInput("log_scale", "Use Log Scale", value = FALSE),
                               checkboxInput("show_trendline", "Show Trendline", value = FALSE)
                           ),
                           box(title = "Scatterplot", width = 9, solidHeader = TRUE, status = "primary",
                               withSpinner(plotlyOutput('scatterplot', height = "600px"))
                           )
                         )
                ),
                tabPanel('Histogram', value = 3,
                         fluidRow(
                           box(title = "Plot Controls", width = 3, solidHeader = TRUE, status = "primary",
                               selectInput("hist_var", "Select Numeric Variable:", choices = NULL),
                               checkboxInput("hist_log_scale", "Use Log Scale (X-Axis)", value = FALSE)
                           ),
                           box(title = "Histogram", width = 9, solidHeader = TRUE, status = "primary",
                               withSpinner(plotlyOutput('histogram', height = "600px"))
                           )
                         )
                ),
                tabPanel('Boxplot', value = 4,
                         fluidRow(
                           box(title = "Plot Controls", width = 3, solidHeader = TRUE, status = "primary",
                               selectInput("box_cat_var", "Select Categorical Variable:", choices = NULL),
                               selectInput("box_num_var", "Select Numeric Variable:", choices = NULL),
                               checkboxInput("box_remove_outliers", "Remove Outliers", value = FALSE),
                               checkboxInput("box_log_scale", "Use Log Scale (Y-Axis)", value = FALSE)
                           ),
                           box(title = "Boxplot", width = 9, solidHeader = TRUE, status = "primary",
                               withSpinner(plotlyOutput('boxplot', height = "600px"))
                           )
                         )
                ),
                tabPanel('Barchart', value = 5,
                         fluidRow(
                           box(title = "Plot Controls", width = 3, solidHeader = TRUE, status = "primary",
                               selectInput("bar_var", "Select Categorical Variable:", choices = NULL),
                               sliderInput("top_n", "Number of Categories to Show:", min = 5, max = 50, value = 25, step = 1)
                           ),
                           box(title = "Barchart", width = 9, solidHeader = TRUE, status = "primary",
                               withSpinner(plotlyOutput('barchart', height = "600px"))
                           )
                         )
                )
              )
      ),
      tabItem(tabName = "data_table",
              box(
                title = "Filtered Data", width = 12, solidHeader = TRUE, status = "primary",
                withSpinner(DT::dataTableOutput('datatable'))
              )
      ),
      tabItem(tabName = "forecasting",
              fluidRow(
                box(title = "Forecast Controls", width = 12, solidHeader = TRUE, status = "primary",
                    textInput("username_forecast", "Enter Username for Forecast:"),
                    actionButton("run_forecast", "Generate Forecast", icon = icon("chart-line")),
                    checkboxInput("run_tuning", "Run Hyperparameter Tuning (Slower, More Accurate)", value = FALSE)
                ),
                tabsetPanel(
                  id = "forecast_tabs",
                  tabPanel("Model Performance",
                           fluidRow(
                             box(title = "Forecast vs. Actuals (Test Set)", width = 12, solidHeader = TRUE, status = "primary",
                                 withSpinner(plotlyOutput('performance_plot', height = "500px"))
                             ),
                             box(title = "Accuracy Metrics", width = 12, solidHeader = TRUE, status = "info",
                                 verbatimTextOutput("accuracy_metrics")
                             )
                           )
                  ),
                  tabPanel("Model Trends",
                           box(title = "Forecast Components", width = 12, solidHeader = TRUE, status = "primary",
                               plotOutput("model_components_plot", height = "600px")
                           )
                  ),
                  tabPanel("Prediction",
                           box(title = "90-Day Future Forecast", width = 12, solidHeader = TRUE, status = "primary",
                               withSpinner(plotlyOutput('prediction_plot', height = "600px"))
                           )
                  )
                )
              )
      )
    )
  )
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  # Load datasets and clean names
  tiktok_df <- read_csv("r_tiktok_data.csv", locale = locale(encoding = "UTF-8")) %>% clean_names()
  youtube_df <- read_csv("r_youtube_data.csv", locale = locale(encoding = "UTF-8")) %>% clean_names()
  instagram_df <- read_csv("r_instagram_data.csv", locale = locale(encoding = "UTF-8")) %>% clean_names()
  
  filtered_data <- reactive({
    df <- switch(input$platform,
                 "TikTok" = tiktok_df,
                 "YouTube" = youtube_df,
                 "Instagram" = instagram_df)
    req(input$country)
    if (input$country != "All") {
      df <- switch(input$platform,
                   "TikTok" = df %>% filter(region_code == input$country),
                   "YouTube" = df %>% filter(country == input$country),
                   "Instagram" = df %>% filter(audience_country == input$country)
      )
    }
    if (input$platform %in% c("YouTube", "Instagram")) {
      req(input$category)
      if (input$category != "All") {
        df <- df %>% filter(category_1 == input$category)
      }
    }
    return(df)
  })
  
  observeEvent(input$platform, {
    df <- switch(input$platform, "TikTok" = tiktok_df, "YouTube" = youtube_df, "Instagram" = instagram_df)
    numeric_cols <- names(df %>% select_if(is.numeric))
    categorical_cols <- names(df %>% select_if(is.character) %>% select(-contains("url"), -contains("name")))
    if (input$platform == "TikTok") {
      updateSelectInput(session, "country", choices = c("All", sort(na.omit(unique(df$region_code)))))
    } else if (input$platform == "Instagram") {
      updateSelectInput(session, "country", choices = c("All", sort(na.omit(unique(df$audience_country)))))
      updateSelectInput(session, "category", choices = c("All", sort(na.omit(unique(df$category_1)))))
    } else {
      updateSelectInput(session, "country", choices = c("All", sort(na.omit(unique(df$country)))))
      updateSelectInput(session, "category", choices = c("All", sort(na.omit(unique(df$category_1)))))
    }
    updateSelectInput(session, "x", choices = numeric_cols)
    updateSelectInput(session, "y", choices = numeric_cols)
    updateSelectInput(session, "color_var", choices = c("None", categorical_cols))
    updateSelectInput(session, "hist_var", choices = numeric_cols)
    updateSelectInput(session, "box_cat_var", choices = c("None", categorical_cols))
    updateSelectInput(session, "box_num_var", choices = numeric_cols)
    updateSelectInput(session, "bar_var", choices = categorical_cols)
  })
  
  output$total_influencers <- renderValueBox({ valueBox(value = format(nrow(filtered_data()), big.mark = ","), subtitle = "Total Influencers", icon = icon("users"), color = "purple") })
  output$avg_followers <- renderValueBox({
    follower_col <- switch(input$platform, "TikTok" = "follower_count", "YouTube" = "subscriber_count", "Instagram" = "subscribers")
    avg <- mean(filtered_data()[[follower_col]], na.rm = TRUE)
    valueBox(value = format(round(avg), big.mark = ","), subtitle = "Average Followers/Subscribers", icon = icon("user-plus"), color = "olive")
  })
  output$avg_engagement <- renderValueBox({
    eng_col <- switch(input$platform, "TikTok" = "engagement_rate", "YouTube" = "engagement_rate", "Instagram" = "engagement_rate")
    avg_eng <- mean(filtered_data()[[eng_col]], na.rm = TRUE)
    valueBox(value = paste0(format(round(avg_eng, 2), nsmall = 2), "%"), subtitle = "Average Engagement Rate", icon = icon("heart"), color = "maroon")
  })
  
  # --- Plots ---
  output$scatterplot <- renderPlotly({
    req(input$x, input$y, input$color_var)
    plot_data <- filtered_data()
    if (input$log_scale) {
      plot_data <- plot_data %>% filter(.data[[input$x]] > 0, .data[[input$y]] > 0)
    }
    if (nrow(plot_data) == 0) return(NULL)
    
    name_col <- switch(input$platform,
                       "TikTok" = "display_name", "YouTube" = "channel_name", "Instagram" = "instagram_name")
    
    p <- ggplot(plot_data, aes(x = .data[[input$x]], y = .data[[input$y]]))
    
    if (input$color_var != "None") {
      p <- p + geom_point(aes(color = .data[[input$color_var]], text = paste("Name:", .data[[name_col]])), alpha = 0.7)
    } else {
      p <- p + geom_point(aes(text = paste("Name:", .data[[name_col]])), alpha = 0.6, color = "#1E90FF")
    }
    
    if (input$show_trendline) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "red", fullrange = TRUE)
    }
    
    x_lab <- if (input$log_scale) paste(input$x, "(Log Scale)") else input$x
    y_lab <- if (input$log_scale) paste(input$y, "(Log Scale)") else input$y
    
    if (input$log_scale) {
      p <- p + scale_x_log10(labels = label_comma()) + scale_y_log10(labels = label_comma())
    } else {
      p <- p + scale_x_continuous(labels = label_comma()) + scale_y_continuous(labels = label_comma())
    }
    
    p <- p + theme_minimal() + labs(x = x_lab, y = y_lab)
    ggplotly(p, tooltip = c("x", "y", "text"))
  })
  
  output$histogram <- renderPlotly({
    req(input$hist_var)
    
    plot_data <- filtered_data()
    
    if (input$hist_log_scale) {
      plot_data <- plot_data %>% filter(.data[[input$hist_var]] > 0)
    }
    
    if (nrow(plot_data) == 0) return(NULL)
    
    x_lab <- if (input$hist_log_scale) paste(input$hist_var, "(Log Scale)") else input$hist_var
    
    p <- ggplot(plot_data, aes(x = .data[[input$hist_var]])) +
      geom_histogram(bins = 30, fill = "#6495ED", color = "white") +
      theme_minimal() +
      scale_y_continuous(labels = label_comma()) +
      labs(x = x_lab)
    
    if (input$hist_log_scale) {
      p <- p + scale_x_log10(labels = label_comma())
    } else {
      p <- p + scale_x_continuous(labels = label_comma())
    }
    
    ggplotly(p)
  })
  
  output$boxplot <- renderPlotly({
    req(input$box_cat_var, input$box_num_var)
    
    plot_data <- filtered_data()
    numeric_var_sym <- sym(input$box_num_var)
    
    if (input$box_log_scale) {
      plot_data <- plot_data %>%
        filter(!!numeric_var_sym > 0) %>%
        mutate(!!numeric_var_sym := log10(!!numeric_var_sym))
    }
    
    if (input$box_remove_outliers) {
      if (input$box_cat_var != "None") {
        categorical_var_sym <- sym(input$box_cat_var)
        plot_data <- plot_data %>%
          group_by(!!categorical_var_sym) %>%
          mutate(
            Q1 = quantile(!!numeric_var_sym, 0.25, na.rm = TRUE),
            Q3 = quantile(!!numeric_var_sym, 0.75, na.rm = TRUE),
            IQR = Q3 - Q1,
            lower_bound = Q1 - 1.5 * IQR,
            upper_bound = Q3 + 1.5 * IQR
          ) %>%
          ungroup() %>%
          filter(!!numeric_var_sym >= lower_bound & !!numeric_var_sym <= upper_bound)
      } else {
        Q1 <- quantile(plot_data[[input$box_num_var]], 0.25, na.rm = TRUE)
        Q3 <- quantile(plot_data[[input$box_num_var]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        plot_data <- plot_data %>%
          filter(!!numeric_var_sym >= lower_bound & !!numeric_var_sym <= upper_bound)
      }
    }
    
    if (nrow(plot_data) == 0) return(NULL)
    
    y_lab <- if(input$box_log_scale) paste(input$box_num_var, "(Log10 Scale)") else input$box_num_var
    
    if (input$box_cat_var != "None") {
      p <- ggplot(plot_data, aes(x = .data[[input$box_cat_var]], y = .data[[input$box_num_var]])) +
        geom_boxplot(fill = "#FFC0CB") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      p <- ggplot(plot_data, aes(y = .data[[input$box_num_var]])) +
        geom_boxplot(fill = "#FFC0CB") +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }
    
    p <- p + theme_minimal() + labs(y = y_lab) + scale_y_continuous(labels = label_comma())
    
    ggplotly(p)
  })
  
  output$barchart <- renderPlotly({
    req(input$bar_var)
    p <- filtered_data() %>%
      count(.data[[input$bar_var]], sort = TRUE) %>%
      na.omit() %>% head(input$top_n) %>% 
      ggplot(aes(x = reorder(.data[[input$bar_var]], n), y = n)) +
      geom_bar(stat = "identity", fill = "#90EE90") +
      coord_flip() + theme_minimal() +
      scale_y_continuous(labels = label_comma()) +
      labs(title = paste("Top", input$top_n, "Barchart for", input$bar_var), x = input$bar_var, y = "Count")
    ggplotly(p)
  })
  
  # --- Prediction Logic ---
  prediction_result <- eventReactive(input$predict_btn, {
    # Call the Python function with the user's input
    predict_performance(input$follower_input)
  })
  
  output$prediction_output <- renderText({
    prediction_result()
  })
  
  output$datatable <- DT::renderDataTable({
    data <- filtered_data()
    if (input$platform == "TikTok" && "username" %in% names(data)) {
      data <- data %>% mutate(profile_link = paste0('<a href="https://www.tiktok.com/@', username, '" target="_blank">View Profile</a>'))
    } else if (input$platform == "YouTube" && "channel_name" %in% names(data)) {
      data <- data %>% mutate(channel_link = paste0('<a href="https://www.youtube.com/results?search_query=', utils::URLencode(channel_name), '" target="_blank">Search Channel</a>'))
    } else if (input$platform == "Instagram" && "instagram_name" %in% names(data)) {
      data <- data %>% mutate(profile_link = paste0('<a href="https://www.instagram.com/', utils::URLencode(instagram_name), '" target="_blank">View Profile</a>'))
    }
    DT::datatable(data, escape = FALSE, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste(input$platform, "-filtered-data-", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
  
  forecast_objects <- reactiveValues(test_data = NULL, forecast_on_test_period = NULL, accuracy_text = NULL, final_model = NULL, final_forecast = NULL, full_actuals = NULL)
  
  # --- Forecasting Calculation Logic ---
  observeEvent(input$run_forecast, {
    req(input$username_forecast, input$username_forecast != "", cancelOutput = TRUE)
    showNotification("🚀 Starting forecast generation...", type = "message", duration = 15)
    
    # Reset reactive values on each new run
    forecast_objects$test_data <- NULL; forecast_objects$forecast_on_test_period <- NULL; forecast_objects$accuracy_text <- NULL; forecast_objects$final_model <- NULL
    
    # --- [ Data collection logic is the same ] ---
    script_to_run <- switch(input$platform, "TikTok" = "tiktok_data_collector.py", "YouTube" = "youtube_data_collector.py", "")
    if (script_to_run == "") { showNotification("Forecasting is not available for this platform.", type = "error"); return() }
    tryCatch({
      python_path <- reticulate::py_config()$python
      script_args <- c(script_to_run, "--username", input$username_forecast)
      system2(python_path, args = script_args)
    }, error = function(e) { showNotification(paste("Error running Python script:", e$message), type = "error"); return() })
    filename <- paste0(input$username_forecast, "_timeseries_data.csv")
    if (!file.exists(filename)) { showNotification("Data collection failed.", type = "error"); return() }
    timeseries_data <- read_csv(filename, locale = locale(encoding = "UTF-8"))
    timeseries_data$Date <- as.Date(timeseries_data$Date)
    if (nrow(timeseries_data) < 10) { showNotification("Not enough data to forecast.", type = "warning"); return() }
    
    # --- Data splitting is now done first, as both methods need it ---
    split_point <- floor(0.8 * nrow(timeseries_data))
    train_data <- timeseries_data[1:split_point, ]
    test_data <- timeseries_data[(split_point + 1):nrow(timeseries_data), ]
    prophet_train_data <- data.frame(ds = train_data$Date, y = train_data[[2]])
    
    
    if (input$run_tuning == TRUE) {
      # --- METHOD 1: SLOW BUT ACCURATE HYPERPARAMETER TUNING ---
      showNotification("Running parallel cross-validation...", type = "message")
      
      num_cores <- max(1, detectCores() - 1)
      registerDoParallel(cores = num_cores)
      
      param_grid <- expand.grid(
        changepoint.prior.scale = c(0.01, 0.05, 0.5),
        seasonality.prior.scale = c(1.0, 10.0),
        seasonality.mode = c('additive', 'multiplicative')
      )
      
      all_rmses <- foreach(i = 1:nrow(param_grid), .combine = 'c', .packages = 'prophet') %dopar% {
        params <- param_grid[i, ]
        m <- prophet(changepoint.prior.scale = params$changepoint.prior.scale, seasonality.prior.scale = params$seasonality.prior.scale, seasonality.mode = as.character(params$seasonality.mode))
        m <- fit.prophet(m, prophet_train_data)
        df_cv <- cross_validation(m, initial = 180, period = 90, horizon = 30, units = 'days')
        df_p <- performance_metrics(df_cv)
        return(mean(df_p$rmse))
      }
      stopImplicitCluster()
      
      best_params <- param_grid[which.min(all_rmses), ]
      
      # FIX: Generate the performance plot data using the best parameters
      accuracy_model <- prophet(prophet_train_data, changepoint.prior.scale = best_params$changepoint.prior.scale, seasonality.prior.scale = best_params$seasonality.prior.scale, seasonality.mode = as.character(best_params$seasonality.mode))
      future_dates_for_accuracy <- data.frame(ds = test_data$Date)
      forecast_on_test_period <- predict(accuracy_model, future_dates_for_accuracy)
      forecast_objects$test_data <- test_data
      forecast_objects$forecast_on_test_period <- forecast_on_test_period
      
      # Store accuracy results
      forecast_objects$accuracy_text <- paste(
        "--- Cross-Validation Results ---\n\n",
        "Best Parameters Found:\n",
        "  - Changepoint Scale:", best_params$changepoint.prior.scale, "\n",
        "  - Seasonality Scale:", best_params$seasonality.prior.scale, "\n",
        "  - Seasonality Mode: ", best_params$seasonality.mode, "\n\n",
        "Lowest Average RMSE:", format(round(min(all_rmses), 0), big.mark = ",")
      )
      
      # Define the final model with the best parameters
      final_model <- prophet(changepoint.prior.scale = best_params$changepoint.prior.scale, seasonality.prior.scale = best_params$seasonality.prior.scale, seasonality.mode = as.character(best_params$seasonality.mode))
      
    } else {
      # --- METHOD 2: FAST TRAIN/TEST SPLIT (DEFAULT) ---
      accuracy_model <- prophet::prophet(prophet_train_data, daily.seasonality = FALSE)
      future_dates_for_accuracy <- data.frame(ds = test_data$Date)
      forecast_on_test_period <- predict(accuracy_model, future_dates_for_accuracy)
      
      mae <- mean(abs(forecast_on_test_period$yhat - test_data[[2]])); rmse <- sqrt(mean((forecast_on_test_period$yhat - test_data[[2]])^2))
      
      forecast_objects$accuracy_text <- paste(
        "--- Forecast Accuracy on Test Set ---\n\n", "RMSE:", format(round(rmse, 0), big.mark = ","), "\n", "MAE: ", format(round(mae, 0), big.mark = ","), "\n\n", "Note: Lower values are better."
      )
      
      forecast_objects$test_data <- test_data; forecast_objects$forecast_on_test_period <- forecast_on_test_period
      
      # Define the final model with default parameters
      final_model <- prophet(daily.seasonality = FALSE, weekly.seasonality = TRUE, yearly.seasonality = TRUE)
    }
    
    # --- Common logic for both methods ---
    full_prophet_data <- data.frame(ds = timeseries_data$Date, y = timeseries_data[[2]])
    final_model <- add_country_holidays(final_model, 'US')
    final_model <- fit.prophet(final_model, full_prophet_data)
    future_final <- make_future_dataframe(final_model, periods = 90)
    final_forecast <- predict(final_model, future_final)
    
    forecast_objects$final_model <- final_model
    forecast_objects$final_forecast <- final_forecast
    forecast_objects$full_actuals <- full_prophet_data
    
    showNotification("✅ Forecast complete!", type = "message", duration = 5)
  })
  
  output$performance_plot <- renderPlotly({
    req(forecast_objects$test_data, forecast_objects$forecast_on_test_period)
    plot_ly() %>%
      add_trace(data = forecast_objects$test_data, x = ~Date, y = ~get(names(forecast_objects$test_data)[2]), type = 'scatter', mode = 'markers', name = 'Actual Test Data', marker = list(color = 'rgb(239, 85, 59)')) %>%
      add_trace(data = forecast_objects$forecast_on_test_period, x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines', name = 'Prophet Forecast', line = list(color = 'rgb(22, 96, 167)')) %>%
      layout(title = "Forecast vs. Actuals on Test Data", xaxis = list(title = "Date"), yaxis = list(title = "Views"))
  })
  
  output$accuracy_metrics <- renderPrint({ req(forecast_objects$accuracy_text); cat(forecast_objects$accuracy_text) })
  
  output$model_components_plot <- renderPlot({ req(forecast_objects$final_model); prophet::prophet_plot_components(forecast_objects$final_model, forecast_objects$final_forecast) })
  
  # --- Tab 3: Prediction ---
  output$prediction_plot <- renderPlotly({
    req(forecast_objects$final_forecast)
    
    # --- THIS IS THE FIX ---
    # 1. Isolate only the future 90 days from the forecast dataframe
    future_forecast_data <- tail(forecast_objects$final_forecast, 90)
    
    # 2. Plot ONLY the future data
    plot_ly(future_forecast_data, x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines', name = '90-Day Prediction',
            line = list(color = 'rgb(22, 96, 167)')) %>%
      add_trace(x = ~ds, y = ~yhat_lower, type = 'scatter', mode = 'lines', name = 'Lower Confidence', line = list(color = 'transparent'), showlegend = FALSE) %>%
      add_trace(x = ~ds, y = ~yhat_upper, type = 'scatter', mode = 'lines', name = 'Upper Confidence', fill = 'tonexty', line = list(color = 'transparent'), showlegend = FALSE) %>%
      # 3. The trace for showing historical 'actuals' on this plot has been removed.
      layout(
        title = paste("90-Day Future Performance Forecast for", input$username_forecast),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Predicted Views")
      )
    # --- END OF FIX ---
  })
  
}

# --- 4. Run the Application ---
shinyApp(ui = ui, server = server)