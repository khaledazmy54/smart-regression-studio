library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(bslib)
library(shinyWidgets)

build_house_demo <- function() {
  set.seed(42)
  area <- c(120, 150, 175, 200, 220, 260, 300, 340, 380, 420, 260, NA)
  bedrooms <- c(2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 4, 4)
  age <- c(18, 14, 12, 10, 9, 8, 6, 5, 4, 3, 8, 8)
  distance_to_center <- c(14, 12, 11, 10, 9, 8, 7, 6, 5, 4, 8, 8)
  price <- round(
    25000 +
      area * 1850 +
      bedrooms * 14000 -
      age * 2200 -
      distance_to_center * 3500 +
      rnorm(length(area), 0, 9000),
    0
  )

  data.frame(
    area = area,
    bedrooms = bedrooms,
    age = age,
    distance_to_center = distance_to_center,
    price = price
  )
}

build_car_demo <- function() {
  set.seed(99)
  weight <- round(seq(0.9, 2.5, length.out = 12), 2)
  horsepower <- c(70, 82, 95, 110, 122, 138, 150, 168, 185, 205, 138, NA)
  mileage <- c(26, 24, 23, 21, 20, 19, 18, 16, 15, 14, 19, 19)
  age <- c(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 6, 6)
  price <- round(
    3500 +
      horsepower * 180 +
      mileage * 240 -
      age * 800 -
      weight * 900 +
      rnorm(length(weight), 0, 1800),
    0
  )

  data.frame(
    weight = weight,
    horsepower = horsepower,
    mileage = mileage,
    age = age,
    price = price
  )
}

safe_read_csv <- function(path) {
  tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = TRUE),
    error = function(e) NULL
  )
}

safe_read_text_csv <- function(text_value) {
  tryCatch(
    read.csv(text = text_value, stringsAsFactors = FALSE, check.names = TRUE),
    error = function(e) NULL
  )
}

numeric_columns <- function(df) {
  if (is.null(df)) {
    return(character(0))
  }

  names(df)[vapply(df, is.numeric, logical(1))]
}

safe_mode <- function(values) {
  values <- values[!is.na(values)]
  if (length(values) == 0) {
    return(NA)
  }

  names(sort(table(values), decreasing = TRUE))[1]
}

clean_dataset <- function(df, remove_duplicates, impute_missing, remove_outliers) {
  cleaned <- df
  notes <- c()

  if (remove_duplicates) {
    before <- nrow(cleaned)
    cleaned <- unique(cleaned)
    notes <- c(notes, paste("Removed duplicate rows:", before - nrow(cleaned)))
  }

  if (impute_missing) {
    for (col_name in names(cleaned)) {
      if (is.numeric(cleaned[[col_name]])) {
        replacement <- median(cleaned[[col_name]], na.rm = TRUE)
        if (is.finite(replacement)) {
          cleaned[[col_name]][is.na(cleaned[[col_name]])] <- replacement
        }
      } else {
        replacement <- safe_mode(cleaned[[col_name]])
        cleaned[[col_name]][is.na(cleaned[[col_name]])] <- replacement
      }
    }
    notes <- c(notes, "Imputed missing values using median for numeric columns and mode for categorical columns.")
  }

  if (remove_outliers) {
    numeric_cols <- numeric_columns(cleaned)
    if (length(numeric_cols) > 0) {
      keep_rows <- rep(TRUE, nrow(cleaned))
      for (col_name in numeric_cols) {
        q1 <- quantile(cleaned[[col_name]], 0.25, na.rm = TRUE)
        q3 <- quantile(cleaned[[col_name]], 0.75, na.rm = TRUE)
        iqr_value <- q3 - q1
        if (is.finite(iqr_value) && iqr_value > 0) {
          lower <- q1 - 1.5 * iqr_value
          upper <- q3 + 1.5 * iqr_value
          keep_rows <- keep_rows & ((cleaned[[col_name]] >= lower & cleaned[[col_name]] <= upper) | is.na(cleaned[[col_name]]))
        }
      }
      removed_rows <- sum(!keep_rows)
      cleaned <- cleaned[keep_rows, , drop = FALSE]
      notes <- c(notes, paste("Removed outlier rows:", removed_rows))
    }
  }

  if (length(notes) == 0) {
    notes <- "No cleaning action applied."
  }

  list(data = cleaned, notes = paste(notes, collapse = " "))
}

build_equation <- function(model) {
  coefficients <- coef(model)
  if (length(coefficients) == 0) {
    return("No equation available")
  }

  target_name <- all.vars(formula(model))[1]
  equation_text <- paste0(target_name, " = ", round(coefficients[1], 4))

  if (length(coefficients) > 1) {
    for (i in 2:length(coefficients)) {
      sign_symbol <- if (coefficients[i] >= 0) " + " else " - "
      equation_text <- paste0(
        equation_text,
        sign_symbol,
        round(abs(coefficients[i]), 4),
        " * ",
        names(coefficients)[i]
      )
    }
  }

  equation_text
}

build_quality_report <- function(df) {
  numeric_cols <- numeric_columns(df)
  zero_variance <- if (length(numeric_cols) > 0) {
    numeric_cols[sapply(df[numeric_cols], function(col) {
      col_sd <- sd(col, na.rm = TRUE)
      is.finite(col_sd) && col_sd == 0
    })]
  } else {
    character(0)
  }

  missing_by_column <- data.frame(
    Column = names(df),
    Missing = sapply(df, function(col) sum(is.na(col))),
    MissingPct = round(sapply(df, function(col) mean(is.na(col)) * 100), 2),
    check.names = FALSE
  )

  list(
    overview = data.frame(
      Metric = c("Rows", "Columns", "Numeric columns", "Missing values", "Duplicate rows"),
      Value = c(nrow(df), ncol(df), length(numeric_cols), sum(is.na(df)), sum(duplicated(df))),
      check.names = FALSE
    ),
    missing = missing_by_column,
    zero_variance = zero_variance
  )
}

build_numeric_summary <- function(df) {
  numeric_cols <- numeric_columns(df)
  if (length(numeric_cols) == 0) {
    return(data.frame(Message = "No numeric columns available."))
  }

  numeric_df <- df[, numeric_cols, drop = FALSE]
  data.frame(
    Variable = numeric_cols,
    Mean = round(sapply(numeric_df, mean, na.rm = TRUE), 3),
    Median = round(sapply(numeric_df, median, na.rm = TRUE), 3),
    SD = round(sapply(numeric_df, sd, na.rm = TRUE), 3),
    Min = round(sapply(numeric_df, min, na.rm = TRUE), 3),
    Max = round(sapply(numeric_df, max, na.rm = TRUE), 3),
    check.names = FALSE
  )
}

build_correlation_data <- function(df) {
  numeric_cols <- numeric_columns(df)
  if (length(numeric_cols) < 2) {
    return(NULL)
  }

  corr_matrix <- cor(df[, numeric_cols, drop = FALSE], use = "pairwise.complete.obs")
  corr_df <- as.data.frame(as.table(corr_matrix), stringsAsFactors = FALSE)
  names(corr_df) <- c("Variable1", "Variable2", "Correlation")
  corr_df
}

build_recommendations <- function(model, df, predictors, outcome, rmse_value, r2_value, mae_value) {
  coefficient_rows <- rownames(summary(model)$coefficients)
  predictor_rows <- coefficient_rows[coefficient_rows != "(Intercept)"]

  relationship_strength <- if (r2_value >= 0.85) {
    "Very strong relationship"
  } else if (r2_value >= 0.65) {
    "Good relationship"
  } else if (r2_value >= 0.4) {
    "Moderate relationship"
  } else {
    "Weak relationship"
  }

  suitability <- if (r2_value >= 0.75) {
    "The model is a good fit for this dataset."
  } else if (r2_value >= 0.5) {
    "The model is acceptable, but more feature engineering may improve it."
  } else {
    "The model needs improvement or richer data."
  }

  influential_feature <- "Not enough information"
  if (length(predictor_rows) > 0) {
    standardized_effects <- sapply(predictors, function(feature_name) {
      feature_sd <- sd(df[[feature_name]], na.rm = TRUE)
      outcome_sd <- sd(df[[outcome]], na.rm = TRUE)
      if (is.na(feature_sd) || feature_sd == 0 || is.na(outcome_sd) || outcome_sd == 0) {
        return(NA_real_)
      }
      abs(coef(model)[feature_name] * feature_sd / outcome_sd)
    })

    if (all(is.na(standardized_effects))) {
      influential_feature <- predictor_rows[1]
    } else {
      influential_feature <- names(which.max(standardized_effects))
    }
  }

  target_mean <- mean(df[[outcome]], na.rm = TRUE)
  decision_support <- if (target_mean > 0 && rmse_value <= 0.15 * target_mean) {
    "The current model is reliable enough for practical estimation."
  } else if (target_mean > 0 && rmse_value <= 0.3 * target_mean) {
    "Use the model as a guidance tool, but review predictions manually."
  } else {
    "Improve data quality or add stronger features before relying on automated decisions."
  }

  list(
    relationship_strength = relationship_strength,
    influential_feature = influential_feature,
    suitability = suitability,
    error_note = paste("RMSE:", round(rmse_value, 4), "| MAE:", round(mae_value, 4)),
    decision_support = decision_support
  )
}

build_business_report <- function(metrics, recommendations, cleaning_notes, segmentation) {
  report_lines <- c(
    "Smart Regression, Cleaning, and Segmentation Studio",
    paste("Generated on:", Sys.time()),
    "",
    "Cleaning Summary:",
    cleaning_notes,
    "",
    "Model Evaluation:",
    paste("R-squared:", round(metrics$r2, 4)),
    paste("Adjusted R-squared:", round(metrics$adj_r2, 4)),
    paste("RMSE:", round(metrics$rmse, 4)),
    paste("MAE:", round(metrics$mae, 4)),
    paste("MSE:", round(metrics$mse, 4)),
    paste("Train/Test Split:", metrics$split_summary),
    paste("Equation:", metrics$equation),
    "",
    "Recommendations:",
    paste("Relationship Strength:", recommendations$relationship_strength),
    paste("Most Influential Feature:", recommendations$influential_feature),
    paste("Model Suitability:", recommendations$suitability),
    paste("Decision Support:", recommendations$decision_support),
    paste("Error Summary:", recommendations$error_note)
  )

  if (!is.null(segmentation)) {
    report_lines <- c(
      report_lines,
      "",
      "Segmentation Summary:",
      paste("Features:", paste(segmentation$features, collapse = ", ")),
      paste("Clusters:", length(unique(segmentation$data$Cluster)))
    )
  }

  paste(report_lines, collapse = "\n")
}

format_metric <- function(value) {
  format(round(value, 4), nsmall = 4, big.mark = ",")
}

app_style <- "
body {
  background:
    radial-gradient(circle at top right, rgba(79, 143, 117, 0.16), transparent 28%),
    linear-gradient(180deg, #f4f0e8 0%, #faf8f4 48%, #ffffff 100%);
  color: #172129;
}
.app-header {
  background: linear-gradient(135deg, #123640 0%, #235c6b 58%, #5f946e 100%);
  color: #ffffff;
  padding: 28px 32px;
  border-radius: 24px;
  margin: 20px 0 22px 0;
  box-shadow: 0 24px 54px rgba(18, 54, 64, 0.18);
}
.app-title {
  font-size: 38px;
  font-weight: 800;
  margin-bottom: 8px;
}
.app-subtitle {
  font-size: 16px;
  max-width: 860px;
  opacity: 0.94;
}
.panel-shell {
  background: rgba(255, 255, 255, 0.9);
  border: 1px solid rgba(18, 54, 64, 0.08);
  border-radius: 22px;
  padding: 22px;
  box-shadow: 0 14px 34px rgba(18, 54, 64, 0.08);
}
.section-title {
  font-size: 24px;
  font-weight: 700;
  margin-top: 0;
  margin-bottom: 14px;
  color: #143642;
}
.metric-card {
  background: linear-gradient(180deg, #fffefb 0%, #f7f2e8 100%);
  border: 1px solid rgba(35, 92, 107, 0.12);
  border-radius: 18px;
  padding: 18px;
  min-height: 124px;
  box-shadow: 0 10px 24px rgba(20, 54, 66, 0.07);
  margin-bottom: 16px;
}
.metric-label {
  color: #5f6f76;
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 0.08em;
}
.metric-value {
  color: #143642;
  font-size: 28px;
  font-weight: 700;
  margin-top: 8px;
}
.metric-note {
  color: #516068;
  font-size: 13px;
  margin-top: 8px;
  line-height: 1.5;
}
.card-block {
  background: #fffdf9;
  border: 1px solid rgba(20, 54, 66, 0.1);
  border-radius: 18px;
  padding: 18px;
  margin-bottom: 16px;
}
.tab-content {
  padding-top: 12px;
}
.nav-tabs > li > a {
  color: #235c6b;
  font-weight: 600;
}
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  color: #143642;
  background-color: #fffdf9;
  border-top: 3px solid #c66b3d;
}
.btn-primary,
.btn-default {
  border-radius: 12px;
}
"

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#f7f4ee",
    fg = "#172129",
    primary = "#c66b3d",
    secondary = "#235c6b",
    success = "#4f8f75",
    base_font = font_google("Manrope"),
    heading_font = font_google("Space Grotesk"),
    code_font = font_google("JetBrains Mono")
  ),
  tags$head(tags$style(HTML(app_style))),
  fluidRow(
    column(
      12,
      div(
        class = "app-header",
        div(class = "app-title", "Smart Regression, Cleaning, and Segmentation Studio"),
        div(
          class = "app-subtitle",
          "A full Shiny analytics project with data quality checks, cleaning workflows, interactive visualization, predictive modeling, decision support, and K-Means segmentation."
        )
      )
    )
  ),
  fluidRow(
    column(
      4,
      div(
        class = "panel-shell",
        h3(class = "section-title", "Control Panel"),
        h4("Data Source"),
        radioButtons(
          "data_source",
          "Choose data source",
          choices = c(
            "House demo dataset" = "house",
            "Car demo dataset" = "car",
            "Upload CSV file" = "upload",
            "Manual CSV paste" = "manual"
          )
        ),
        conditionalPanel(
          condition = "input.data_source == 'upload'",
          fileInput("uploaded_file", "Upload CSV file", accept = c(".csv"))
        ),
        conditionalPanel(
          condition = "input.data_source == 'manual'",
          textAreaInput(
            "manual_csv",
            "Paste CSV data",
            rows = 8,
            placeholder = "x1,x2,y\n10,5,100\n20,7,180"
          )
        ),
        actionButton("load_manual_data", "Load Manual Data"),
        tags$hr(),
        h4("Data Quality & Cleaning"),
        checkboxInput("remove_duplicates", "Remove duplicate rows", TRUE),
        checkboxInput("impute_missing", "Impute missing values", TRUE),
        checkboxInput("remove_outliers", "Remove numeric outliers", FALSE),
        actionButton("apply_cleaning", "Apply Cleaning", class = "btn-primary"),
        tags$hr(),
        h4("Regression Model"),
        sliderInput("train_ratio", "Training ratio", min = 0.6, max = 0.9, value = 0.8, step = 0.05),
        selectInput(
          "regression_type",
          "Regression type",
          choices = c("Simple Linear Regression", "Multiple Linear Regression")
        ),
        uiOutput("predictor_ui"),
        uiOutput("outcome_ui"),
        actionButton("train_model", "Train Model", class = "btn-primary"),
        tags$br(),
        tags$br(),
        downloadButton("download_predictions", "Download Predictions"),
        downloadButton("download_cleaned_data", "Download Cleaned Data"),
        tags$br(),
        tags$br(),
        downloadButton("download_report", "Download Insights Report"),
        tags$hr(),
        h4("K-Means Segmentation"),
        uiOutput("cluster_feature_ui"),
        sliderInput("cluster_count", "Number of clusters", min = 2, max = 6, value = 3, step = 1),
        actionButton("run_segmentation", "Run Segmentation", class = "btn-primary")
      )
    ),
    column(
      8,
      fluidRow(
        column(4, uiOutput("dataset_card")),
        column(4, uiOutput("model_card")),
        column(4, uiOutput("insight_card"))
      ),
      div(
        class = "panel-shell",
        tabsetPanel(
          tabPanel(
            "Overview",
            br(),
            fluidRow(
              column(6, div(class = "card-block", h4("Project Scope"), htmlOutput("overview_text"))),
              column(6, div(class = "card-block", h4("Data Health Highlights"), tableOutput("quality_overview_table")))
            ),
            fluidRow(
              column(12, div(class = "card-block", h4("Missing Values by Column"), DTOutput("missing_table")))
            )
          ),
          tabPanel(
            "Data Quality",
            br(),
            h4("Cleaning Log"),
            verbatimTextOutput("cleaning_notes"),
            br(),
            h4("Numeric Summary"),
            DTOutput("summary_table"),
            br(),
            h4("Working Dataset"),
            DTOutput("data_table")
          ),
          tabPanel(
            "Visualizations",
            br(),
            fluidRow(
              column(6, plotlyOutput("scatter_plot", height = "320px")),
              column(6, plotlyOutput("actual_pred_plot", height = "320px"))
            ),
            br(),
            fluidRow(
              column(12, plotOutput("correlation_plot", height = "360px"))
            ),
            br(),
            fluidRow(
              column(6, uiOutput("distribution_var_ui")),
              column(6, uiOutput("boxplot_var_ui"))
            ),
            fluidRow(
              column(6, plotlyOutput("distribution_plot", height = "320px")),
              column(6, plotlyOutput("boxplot_plot", height = "320px"))
            )
          ),
          tabPanel(
            "Model Evaluation",
            br(),
            fluidRow(
              column(3, div(class = "card-block", h4("R-squared"), textOutput("r2_score"))),
              column(3, div(class = "card-block", h4("Adj. R-squared"), textOutput("adj_r2_score"))),
              column(3, div(class = "card-block", h4("RMSE"), textOutput("rmse_score"))),
              column(3, div(class = "card-block", h4("MAE"), textOutput("mae_score")))
            ),
            fluidRow(
              column(6, div(class = "card-block", h4("Train/Test Split"), textOutput("split_summary"))),
              column(6, div(class = "card-block", h4("MSE"), textOutput("mse_score")))
            ),
            div(class = "card-block", h4("Final Regression Equation"), verbatimTextOutput("equation_output")),
            div(class = "card-block", h4("Predictions Preview"), DTOutput("predictions_table")),
            div(class = "card-block", h4("Residual Diagnostics"), plotlyOutput("residual_plot", height = "320px")),
            div(class = "card-block", h4("Statistical Summary"), verbatimTextOutput("model_summary"))
          ),
          tabPanel(
            "Decision Support",
            br(),
            actionButton("show_recommendations", "Generate Intelligent Insights"),
            br(),
            br(),
            tableOutput("recommendations_table"),
            br(),
            h4("What-if Prediction"),
            uiOutput("decision_inputs"),
            actionButton("predict_scenario", "Predict Scenario", class = "btn-primary"),
            br(),
            br(),
            verbatimTextOutput("decision_output")
          ),
          tabPanel(
            "Customer Segmentation",
            br(),
            fluidRow(
              column(6, div(class = "card-block", h4("Segmentation Summary"), tableOutput("segment_summary"))),
              column(6, div(class = "card-block", h4("Cluster Interpretation"), verbatimTextOutput("cluster_note")))
            ),
            div(class = "card-block", plotlyOutput("cluster_plot", height = "360px")),
            div(class = "card-block", h4("Clustered Data"), DTOutput("clustered_data_table"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  raw_dataset_store <- reactiveVal(build_house_demo())
  dataset_store <- reactiveVal(build_house_demo())
  cleaning_notes_store <- reactiveVal("No cleaning action applied yet.")
  model_store <- reactiveVal(NULL)
  predictions_store <- reactiveVal(NULL)
  metrics_store <- reactiveVal(NULL)
  recommendations_store <- reactiveVal(NULL)
  segmentation_store <- reactiveVal(NULL)
  decision_text_store <- reactiveVal("Train the model first, then enter a scenario and click Predict Scenario.")

  reset_model_state <- function() {
    model_store(NULL)
    predictions_store(NULL)
    metrics_store(NULL)
    recommendations_store(NULL)
    decision_text_store("Train the model first, then enter a scenario and click Predict Scenario.")
  }

  reset_segmentation_state <- function() {
    segmentation_store(NULL)
  }

  load_data <- function(df) {
    raw_dataset_store(df)
    dataset_store(df)
    cleaning_notes_store("No cleaning action applied yet.")
    reset_model_state()
    reset_segmentation_state()
  }

  observeEvent(input$data_source, {
    if (input$data_source == "house") {
      load_data(build_house_demo())
    } else if (input$data_source == "car") {
      load_data(build_car_demo())
    } else if (input$data_source == "manual" && isTruthy(input$manual_csv)) {
      manual_data <- safe_read_text_csv(input$manual_csv)
      if (!is.null(manual_data)) {
        load_data(manual_data)
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$uploaded_file, {
    uploaded_data <- safe_read_csv(input$uploaded_file$datapath)
    if (is.null(uploaded_data)) {
      showNotification("Could not read the uploaded CSV file.", type = "error")
      return()
    }

    load_data(uploaded_data)
  })

  observeEvent(input$load_manual_data, {
    manual_data <- safe_read_text_csv(input$manual_csv)
    if (is.null(manual_data)) {
      showNotification("Manual CSV data is invalid. Please check the format.", type = "error")
      return()
    }

    load_data(manual_data)
    showNotification("Manual data loaded successfully.", type = "message")
  })

  observeEvent(input$apply_cleaning, {
    result <- clean_dataset(
      raw_dataset_store(),
      remove_duplicates = isTRUE(input$remove_duplicates),
      impute_missing = isTRUE(input$impute_missing),
      remove_outliers = isTRUE(input$remove_outliers)
    )

    dataset_store(result$data)
    cleaning_notes_store(result$notes)
    reset_model_state()
    reset_segmentation_state()
    showNotification("Cleaning applied to the working dataset.", type = "message")
  })

  quality_report <- reactive({
    build_quality_report(dataset_store())
  })

  output$overview_text <- renderUI({
    HTML(paste(
      "This project now combines <b>data quality diagnostics</b>, <b>cleaning</b>, <b>interactive visual analytics</b>,",
      "<b>regression modeling</b>, <b>decision support</b>, and <b>K-Means segmentation</b> in one dashboard."
    ))
  })

  output$quality_overview_table <- renderTable({
    quality_report()$overview
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$missing_table <- renderDT({
    datatable(quality_report()$missing, options = list(dom = "t", scrollX = TRUE))
  })

  output$cleaning_notes <- renderText({
    cleaning_notes_store()
  })

  output$summary_table <- renderDT({
    datatable(build_numeric_summary(dataset_store()), options = list(dom = "t", scrollX = TRUE))
  })

  output$data_table <- renderDT({
    datatable(dataset_store(), options = list(pageLength = 8, scrollX = TRUE))
  })

  output$predictor_ui <- renderUI({
    available_columns <- numeric_columns(dataset_store())
    req(length(available_columns) >= 2)

    if (identical(input$regression_type, "Simple Linear Regression")) {
      selectInput("predictor_vars", "Select X variable", choices = available_columns, selected = available_columns[1])
    } else {
      pickerInput(
        "predictor_vars",
        "Select X variable(s)",
        choices = available_columns,
        selected = available_columns[seq_len(max(1, min(2, length(available_columns) - 1)))],
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    }
  })

  output$outcome_ui <- renderUI({
    available_columns <- numeric_columns(dataset_store())
    req(length(available_columns) >= 2)
    default_choice <- if ("price" %in% available_columns) "price" else available_columns[length(available_columns)]
    selectInput("outcome_var", "Select Y variable", choices = available_columns, selected = default_choice)
  })

  output$cluster_feature_ui <- renderUI({
    available_columns <- numeric_columns(dataset_store())
    req(length(available_columns) >= 2)
    pickerInput(
      "cluster_features",
      "Segmentation feature(s)",
      choices = available_columns,
      selected = available_columns[seq_len(min(3, length(available_columns)))],
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })

  output$distribution_var_ui <- renderUI({
    available_columns <- numeric_columns(dataset_store())
    req(length(available_columns) >= 1)
    selectInput("distribution_var", "Histogram variable", choices = available_columns, selected = available_columns[1])
  })

  output$boxplot_var_ui <- renderUI({
    available_columns <- numeric_columns(dataset_store())
    req(length(available_columns) >= 1)
    selectInput("boxplot_var", "Boxplot variable", choices = available_columns, selected = available_columns[min(2, length(available_columns))])
  })

  observeEvent(input$train_model, {
    df <- dataset_store()
    predictors <- input$predictor_vars
    outcome <- input$outcome_var

    if (is.null(predictors) || length(predictors) == 0) {
      showNotification("Please choose at least one predictor.", type = "error")
      return()
    }

    if (identical(input$regression_type, "Simple Linear Regression") && length(predictors) != 1) {
      showNotification("Simple regression needs exactly one X variable.", type = "error")
      return()
    }

    if (outcome %in% predictors) {
      showNotification("The output variable must be different from the input variable(s).", type = "error")
      return()
    }

    selected_columns <- unique(c(predictors, outcome))
    model_df <- na.omit(df[, selected_columns, drop = FALSE])

    if (nrow(model_df) < 5) {
      showNotification("At least 5 complete rows are required for train/test evaluation.", type = "error")
      return()
    }

    set.seed(123)
    train_size <- max(2, floor(nrow(model_df) * input$train_ratio))
    train_index <- sample(seq_len(nrow(model_df)), size = train_size)
    train_df <- model_df[train_index, , drop = FALSE]
    test_df <- model_df[-train_index, , drop = FALSE]
    if (nrow(test_df) < 1) {
      test_df <- train_df
    }

    formula_text <- paste(outcome, "~", paste(predictors, collapse = " + "))
    model <- lm(as.formula(formula_text), data = train_df)
    predictions <- predict(model, newdata = test_df)
    residuals_values <- test_df[[outcome]] - predictions
    mse_value <- mean(residuals_values^2)
    rmse_value <- sqrt(mse_value)
    mae_value <- mean(abs(residuals_values))
    r2_value <- summary(model)$r.squared
    adj_r2_value <- summary(model)$adj.r.squared

    model_store(model)
    predictions_store(
      data.frame(
        actual = test_df[[outcome]],
        predicted = predictions,
        residual = residuals_values
      )
    )
    metrics_store(
      list(
        r2 = r2_value,
        adj_r2 = adj_r2_value,
        mse = mse_value,
        rmse = rmse_value,
        mae = mae_value,
        equation = build_equation(model),
        summary_text = capture.output(summary(model)),
        predictors = predictors,
        outcome = outcome,
        training_data = train_df,
        testing_data = test_df,
        split_summary = paste("Train rows:", nrow(train_df), "| Test rows:", nrow(test_df))
      )
    )
    recommendations_store(
      build_recommendations(model, model_df, predictors, outcome, rmse_value, r2_value, mae_value)
    )
    decision_text_store("Model trained. You can now generate recommendations or run a what-if prediction.")
    showNotification("Model trained and evaluated successfully.", type = "message")
  })

  observeEvent(input$run_segmentation, {
    df <- dataset_store()
    cluster_features <- input$cluster_features

    if (is.null(cluster_features) || length(cluster_features) < 2) {
      showNotification("Choose at least two numeric features for segmentation.", type = "error")
      return()
    }

    cluster_df <- na.omit(df[, cluster_features, drop = FALSE])
    if (nrow(cluster_df) < input$cluster_count) {
      showNotification("Not enough complete rows for the selected number of clusters.", type = "error")
      return()
    }

    set.seed(321)
    scaled_df <- scale(cluster_df)
    kmeans_result <- kmeans(scaled_df, centers = input$cluster_count, nstart = 25)
    clustered_df <- cbind(cluster_df, Cluster = factor(kmeans_result$cluster))
    cluster_summary <- aggregate(. ~ Cluster, data = clustered_df, FUN = mean)
    cluster_note <- paste(
      "Segments created using:",
      paste(cluster_features, collapse = ", "),
      "| Cluster sizes:",
      paste(as.integer(table(clustered_df$Cluster)), collapse = ", ")
    )

    segmentation_store(
      list(
        data = clustered_df,
        summary = cluster_summary,
        features = cluster_features,
        note = cluster_note
      )
    )

    showNotification("Customer segmentation completed.", type = "message")
  })

  output$dataset_card <- renderUI({
    df <- dataset_store()
    div(
      class = "metric-card",
      div(class = "metric-label", "Active Dataset"),
      div(class = "metric-value", paste(nrow(df), "rows")),
      div(class = "metric-note", paste(ncol(df), "columns in the working dataset"))
    )
  })

  output$model_card <- renderUI({
    current_metrics <- metrics_store()
    if (is.null(current_metrics)) {
      return(
        div(
          class = "metric-card",
          div(class = "metric-label", "Model Status"),
          div(class = "metric-value", "Waiting"),
          div(class = "metric-note", "Train a model to activate evaluation metrics")
        )
      )
    }

    div(
      class = "metric-card",
      div(class = "metric-label", "Model Status"),
      div(class = "metric-value", "Trained"),
      div(class = "metric-note", paste("RMSE:", format_metric(current_metrics$rmse)))
    )
  })

  output$insight_card <- renderUI({
    current_recommendations <- recommendations_store()
    if (is.null(current_recommendations)) {
      return(
        div(
          class = "metric-card",
          div(class = "metric-label", "Top Insight"),
          div(class = "metric-value", "Ready"),
          div(class = "metric-note", "Insights will appear after training")
        )
      )
    }

    div(
      class = "metric-card",
      div(class = "metric-label", "Top Insight"),
      div(class = "metric-value", current_recommendations$relationship_strength),
      div(class = "metric-note", current_recommendations$decision_support)
    )
  })

  output$scatter_plot <- renderPlotly({
    current_metrics <- metrics_store()
    req(current_metrics)
    predictors <- current_metrics$predictors
    outcome <- current_metrics$outcome
    model_df <- current_metrics$training_data

    plot_obj <- if (length(predictors) == 1) {
      ggplot(model_df, aes_string(x = predictors[1], y = outcome)) +
        geom_point(color = "#235c6b", size = 3.2, alpha = 0.85) +
        geom_smooth(method = "lm", se = FALSE, color = "#c66b3d", linewidth = 1.15) +
        labs(title = "Scatter Plot with Regression Line", x = predictors[1], y = outcome) +
        theme_minimal(base_size = 13)
    } else {
      ggplot(model_df, aes_string(x = predictors[1], y = outcome, color = predictors[2])) +
        geom_point(size = 3.1, alpha = 0.9) +
        labs(title = "Feature Relationship View", subtitle = "Color reflects the second selected predictor", x = predictors[1], y = outcome, color = predictors[2]) +
        theme_minimal(base_size = 13)
    }

    ggplotly(plot_obj)
  })

  output$actual_pred_plot <- renderPlotly({
    prediction_df <- predictions_store()
    req(prediction_df)
    plot_obj <- ggplot(prediction_df, aes(x = actual, y = predicted)) +
      geom_point(color = "#4f8f75", size = 3.1, alpha = 0.85) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#c66b3d", linewidth = 1) +
      labs(title = "Actual vs Predicted", x = "Actual Values", y = "Predicted Values") +
      theme_minimal(base_size = 13)

    ggplotly(plot_obj)
  })

  output$correlation_plot <- renderPlot({
    corr_df <- build_correlation_data(dataset_store())
    req(corr_df)
    ggplot(corr_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Correlation, 2)), size = 4, color = "#13252f") +
      scale_fill_gradient2(low = "#b85450", mid = "#f6f3ec", high = "#3f7d5d", midpoint = 0, limits = c(-1, 1)) +
      labs(title = "Correlation Heatmap", x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
  })

  output$distribution_plot <- renderPlotly({
    req(input$distribution_var)
    plot_obj <- ggplot(dataset_store(), aes_string(x = input$distribution_var)) +
      geom_histogram(fill = "#235c6b", color = "white", bins = 10, alpha = 0.9) +
      labs(title = paste("Distribution of", input$distribution_var), x = input$distribution_var, y = "Count") +
      theme_minimal(base_size = 13)

    ggplotly(plot_obj)
  })

  output$boxplot_plot <- renderPlotly({
    req(input$boxplot_var)
    plot_obj <- ggplot(dataset_store(), aes(x = "", y = .data[[input$boxplot_var]])) +
      geom_boxplot(fill = "#c66b3d", alpha = 0.7, width = 0.3) +
      coord_flip() +
      labs(title = paste("Boxplot of", input$boxplot_var), x = NULL, y = input$boxplot_var) +
      theme_minimal(base_size = 13)

    ggplotly(plot_obj)
  })

  output$residual_plot <- renderPlotly({
    prediction_df <- predictions_store()
    req(prediction_df)
    plot_obj <- ggplot(prediction_df, aes(x = predicted, y = residual)) +
      geom_point(color = "#235c6b", size = 3, alpha = 0.85) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#c66b3d", linewidth = 1) +
      labs(title = "Residual Diagnostics", x = "Predicted Values", y = "Residuals") +
      theme_minimal(base_size = 13)

    ggplotly(plot_obj)
  })

  output$predictions_table <- renderDT({
    prediction_df <- predictions_store()
    req(prediction_df)
    datatable(prediction_df, options = list(pageLength = 6, scrollX = TRUE))
  })

  output$r2_score <- renderText({
    req(metrics_store())
    format_metric(metrics_store()$r2)
  })

  output$adj_r2_score <- renderText({
    req(metrics_store())
    format_metric(metrics_store()$adj_r2)
  })

  output$mse_score <- renderText({
    req(metrics_store())
    format_metric(metrics_store()$mse)
  })

  output$rmse_score <- renderText({
    req(metrics_store())
    format_metric(metrics_store()$rmse)
  })

  output$mae_score <- renderText({
    req(metrics_store())
    format_metric(metrics_store()$mae)
  })

  output$split_summary <- renderText({
    req(metrics_store())
    metrics_store()$split_summary
  })

  output$equation_output <- renderText({
    req(metrics_store())
    metrics_store()$equation
  })

  output$model_summary <- renderText({
    req(metrics_store())
    paste(metrics_store()$summary_text, collapse = "\n")
  })

  observeEvent(input$show_recommendations, {
    req(recommendations_store())
  })

  output$recommendations_table <- renderTable({
    req(input$show_recommendations > 0)
    req(recommendations_store())
    recommendations <- recommendations_store()

    data.frame(
      Insight = c(
        "Strength of relationship",
        "Most influential feature",
        "Model suitability",
        "Error summary",
        "Decision support"
      ),
      Details = c(
        recommendations$relationship_strength,
        recommendations$influential_feature,
        recommendations$suitability,
        recommendations$error_note,
        recommendations$decision_support
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$decision_inputs <- renderUI({
    current_metrics <- metrics_store()
    req(current_metrics)
    training_data <- current_metrics$training_data
    predictors <- current_metrics$predictors

    controls <- lapply(predictors, function(col_name) {
      numericInput(
        inputId = paste0("decision_", col_name),
        label = paste("Enter", col_name),
        value = round(mean(training_data[[col_name]], na.rm = TRUE), 2)
      )
    })

    do.call(tagList, controls)
  })

  observeEvent(input$predict_scenario, {
    current_model <- model_store()
    current_metrics <- metrics_store()
    req(current_model, current_metrics)

    predictors <- current_metrics$predictors
    new_values <- as.data.frame(
      setNames(
        lapply(predictors, function(col_name) input[[paste0("decision_", col_name)]]),
        predictors
      )
    )
    predicted_value <- predict(current_model, newdata = new_values)
    decision_text_store(paste("Predicted", current_metrics$outcome, "=", round(predicted_value[1], 4)))
  })

  output$decision_output <- renderText({
    decision_text_store()
  })

  output$segment_summary <- renderTable({
    segmentation <- segmentation_store()
    req(segmentation)
    segmentation$summary
  }, striped = TRUE, bordered = TRUE, spacing = "m")

  output$cluster_note <- renderText({
    segmentation <- segmentation_store()
    if (is.null(segmentation)) {
      return("Run segmentation to generate cluster interpretation.")
    }
    segmentation$note
  })

  output$cluster_plot <- renderPlotly({
    segmentation <- segmentation_store()
    req(segmentation)
    cluster_df <- segmentation$data
    features <- segmentation$features
    y_feature <- features[min(2, length(features))]

    plot_obj <- ggplot(cluster_df, aes_string(x = features[1], y = y_feature, color = "Cluster")) +
      geom_point(size = 3.2, alpha = 0.85) +
      labs(title = "K-Means Customer Segmentation", x = features[1], y = y_feature, color = "Cluster") +
      theme_minimal(base_size = 13)

    ggplotly(plot_obj)
  })

  output$clustered_data_table <- renderDT({
    segmentation <- segmentation_store()
    req(segmentation)
    datatable(segmentation$data, options = list(pageLength = 6, scrollX = TRUE))
  })

  output$download_predictions <- downloadHandler(
    filename = function() {
      paste0("regression_predictions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      prediction_df <- predictions_store()
      if (is.null(prediction_df)) {
        write.csv(data.frame(Message = "Train the model before downloading predictions."), file, row.names = FALSE)
      } else {
        write.csv(prediction_df, file, row.names = FALSE)
      }
    }
  )

  output$download_cleaned_data <- downloadHandler(
    filename = function() {
      paste0("cleaned_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dataset_store(), file, row.names = FALSE)
    }
  )

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("analytics_report_", Sys.Date(), ".txt")
    },
    content = function(file) {
      metrics <- metrics_store()
      recommendations <- recommendations_store()
      segmentation <- segmentation_store()

      report_text <- if (is.null(metrics) || is.null(recommendations)) {
        paste(
          "Smart Regression, Cleaning, and Segmentation Studio",
          paste("Generated on:", Sys.time()),
          "",
          "Train the regression model first to generate the full analytical report.",
          sep = "\n"
        )
      } else {
        build_business_report(metrics, recommendations, cleaning_notes_store(), segmentation)
      }

      writeLines(report_text, con = file)
    }
  )
}

shinyApp(ui = ui, server = server)
