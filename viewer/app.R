library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(plotly)
library(DT) 
library(htmltools)

met_perc <- read_csv("met_perc.csv", show_col_types = FALSE) %>% 
  mutate(reserve = str_sub(station, 1,3)) %>% 
  select(-station)
wq_perc <- read_csv("wq_perc.csv", show_col_types = FALSE) %>% 
  mutate(reserve = str_sub(station, 1,3))

comb <- left_join(wq_perc, met_perc, by = join_by(reserve, date)) %>% 
  select(-reserve) %>% 
  rename(conductivity = cond, `do_%` = do, turbidity = turb, precipitation = totprcp,
         conductivity_perc = cond_perc, `do_%_perc` = do_perc, turbidity_perc = turb_perc, precipitation_perc = totprcp_perc) %>% 
  group_by(station) %>% 
  mutate(across(c(watertemp, conductivity, `do_%`, ph, turbidity, airtemp, precipitation), ~scale(., center = FALSE), .names = "{.col}_scale")) %>% 
  ungroup() %>% 
  mutate(precipitation_scale = precipitation_scale/10,
         precipitation_perc = if_else(precipitation_perc == 0, NA, precipitation_perc))



ui <- fluidPage(
  
  tags$br(),
  tags$br(),
  
  fluidRow(
    column(2, selectInput("station", "Station:", unique(comb$station))),
    column(2, selectInput("var1", "Variable 1:", c("watertemp", "conductivity", "do_%", "ph", "turbidity", "airtemp", "precipitation"))),
    column(2, selectInput("var2", "Variable 2:", c("watertemp", "conductivity", "do_%", "ph", "turbidity", "airtemp", "precipitation"), selected = "airtemp")),
    column(2, numericInput("ex", "Extreme Definiton (Top/Bottom %):", 10, 1, 50, 1)),
    column(2, numericInput("lag1", "Variable 1 Lag (Days):", 0, 0, 30, 1)),
    column(2, numericInput("lag2", "Variable 2 Lag (Days):", 0, 0, 30, 1))
  ),
  
  fluidRow(
    column(12, plotlyOutput("plot"))
  ),
  
  
  tags$br(),
  fluidRow(
    column(12, DTOutput("category_stats"))
  )
  
)

server <- function(input, output, session) {
  
  filt_data <- reactive({
    req(input$lag1)
    req(input$lag2)
    
    filt_data <- comb %>% 
      filter(station == input$station) %>% 
      mutate(
        var1_cat = case_when(
          .data[[str_c(input$var1, "_perc")]] > (100 - input$ex) ~ "high",
          .data[[str_c(input$var1, "_perc")]] <= (0 + input$ex) ~ "low",
          .default = NA),
        var2_cat = case_when(
          .data[[str_c(input$var2, "_perc")]] > (100 - input$ex) ~ "high",
          .data[[str_c(input$var2, "_perc")]] <= (0 + input$ex) ~ "low",
          .default = NA)
      )
    

    if (input$lag1 > 0) {
      lag1 <- round(input$lag1)
      filt_data <- filt_data %>% 
        mutate(
          !!str_c(input$var1, "_scale") := lag(.data[[str_c(input$var1, "_scale")]], lag1),
          !!str_c(input$var1) := lag(.data[[str_c(input$var1)]], lag1),
          var1_cat = lag(var1_cat, lag1),
          var1_orig_date = lag(date, lag1)
        )
    } else {
      filt_data$var1_orig_date <- filt_data$date
    }
    

    if (input$lag2 > 0) {
      lag2 <- round(input$lag2)
      filt_data <- filt_data %>% 
        mutate(
          !!str_c(input$var2, "_scale") := lag(.data[[str_c(input$var2, "_scale")]], lag2),
          !!str_c(input$var2) := lag(.data[[str_c(input$var2)]], lag2),
          var2_cat = lag(var2_cat, lag2),
          var2_orig_date = lag(date, lag2)
        )
    } else {
      filt_data$var2_orig_date <- filt_data$date
    }
    
    filt_data
  })
  
  # Store plot axis ranges to maintain zoom
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Reset ranges when station changes
  observeEvent(input$station, {
    ranges$x <- NULL
    ranges$y <- NULL
  })
  
  # Capture the plotly_relayout event to store zoom levels
  observeEvent(event_data("plotly_relayout", source = "plot"), {
    ed <- event_data("plotly_relayout", source = "plot")

    # Extract axis ranges when available
    if (!is.null(ed$`xaxis.range[0]`) && !is.null(ed$`xaxis.range[1]`)) {
      ranges$x <- c(ed$`xaxis.range[0]`, ed$`xaxis.range[1]`)
    }
    if (!is.null(ed$`yaxis.range[0]`) && !is.null(ed$`yaxis.range[1]`)) {
      ranges$y <- c(ed$`yaxis.range[0]`, ed$`yaxis.range[1]`)
    }

    # Handle double-click to reset zoom
    if (!is.null(ed$'xaxis.autorange') && ed$'xaxis.autorange') {
      ranges$x <- NULL
    }
    if (!is.null(ed$'yaxis.autorange') && ed$'yaxis.autorange') {
      ranges$y <- NULL
    }
  })
  
  output$plot <- renderPlotly({
    req(filt_data())
    
    var1_name <- input$var1
    var2_name <- input$var2
    lag1_val <- input$lag1
    lag2_val <- input$lag2
    

    plot_data <- filt_data() %>%
      mutate(

        hover_text1 = case_when(
          lag1_val > 0 ~ str_c(
            var1_name, 
            "<br>Value: ", round(.data[[input$var1]], 2),
            "<br>Date: ", format(date, "%Y-%m-%d"),
            "<br>Data from: ", format(var1_orig_date, "%Y-%m-%d")
          ),
          TRUE ~ str_c(
            var1_name, 
            "<br>Value: ", round(.data[[input$var1]], 2),
            "<br>Date: ", format(date, "%Y-%m-%d")
          )
        ),

        hover_text2 = case_when(
          lag2_val > 0 ~ str_c(
            var2_name, 
            "<br>Value: ", round(.data[[input$var2]], 2),
            "<br>Date: ", format(date, "%Y-%m-%d"),
            "<br>Data from: ", format(var2_orig_date, "%Y-%m-%d")
          ),
          TRUE ~ str_c(
            var2_name, 
            "<br>Value: ", round(.data[[input$var2]], 2),
            "<br>Date: ", format(date, "%Y-%m-%d")
          )
        ),

        hover_text1_extreme = case_when(
          !is.na(var1_cat) & lag1_val > 0 ~ str_c(
            var1_name, 
            "<br>", var1_cat,
            "<br>Value: ", round(.data[[input$var1]], 2),
            "<br>Date: ", format(date, "%Y-%m-%d"),
            "<br>Value from: ", format(var1_orig_date, "%Y-%m-%d")
          ),
          !is.na(var1_cat) ~ str_c(
            var1_name, 
            "<br>", var1_cat,
            "<br>Value: ", round(.data[[input$var1]], 2),
            "<br>Date: ", format(date, "%Y-%m-%d")
          ),
          TRUE ~ NA_character_
        ),

        hover_text2_extreme = case_when(
          !is.na(var2_cat) & lag2_val > 0 ~ str_c(
            var2_name, 
            "<br>", var2_cat,
            "<br>Value: ", round(.data[[input$var2]], 2),
            "<br>Date: ", format(date, "%Y-%m-%d"),
            "<br>Value from: ", format(var2_orig_date, "%Y-%m-%d")
          ),
          !is.na(var2_cat) ~ str_c(
            var2_name, 
            "<br>", var2_cat,
            "<br>Value: ", round(.data[[input$var2]], 2),
            "<br>Date: ", format(date, "%Y-%m-%d")
          ),
          TRUE ~ NA_character_
        )
      ) %>%
      mutate(label1 = str_c(input$var1, ": ", var1_cat),
             label2 = str_c(input$var1, ": ", var1_cat),
             var1_scale = .data[[str_c(input$var1, "_scale")]],
             var2_scale = .data[[str_c(input$var2, "_scale")]])
    
  
  

    p <- plot_ly(source = "plot") %>%
      add_lines(data = plot_data, x = ~date,
                y = ~var1_scale,
                text = ~hover_text1,
                opacity = 0.7,
                line = list(color = "#999933"),
                name = input$var1,
                hoverinfo = "text"
      ) %>%
      add_lines(data = plot_data, x = ~date,
                y = ~var2_scale,
                text = ~hover_text2,
                opacity = 0.7,
                line = list(color = "#ddcc77"),
                name = input$var2,
                hoverinfo = "text"
      ) %>%
      add_markers(data = filter(plot_data, var1_cat == "high"), x = ~date,
                  y = ~var1_scale,
                  name = str_c(input$var1, " : high"),
                  text = ~hover_text1_extreme,
                  marker = list(symbol = "circle", size = 8, color = "#cc3311"),
                  hoverinfo = "text"
      ) %>%
      add_markers(data = filter(plot_data, var1_cat == "low"), x = ~date,
                  y = ~var1_scale,
                  name = str_c(input$var1, " : low"),
                  text = ~hover_text1_extreme,
                  marker = list(symbol = "circle", size = 8, color = "#0077bb"),
                  hoverinfo = "text"
      ) %>% 
      add_markers(data = filter(plot_data, var2_cat == "high"), x = ~date,
                  y = ~var2_scale,
                  name = str_c(input$var2, " : high"),
                  text = ~hover_text2_extreme,
                  marker = list(symbol = "triangle-up", size = 9, color = "#ee3377"),
                  hoverinfo = "text"
      ) %>%
      add_markers(data = filter(plot_data, var2_cat == "low"), x = ~date,
                  y = ~var2_scale,
                  name = str_c(input$var2, " : low"),
                  text = ~hover_text2_extreme,
                  marker = list(symbol = "triangle-up", size = 9, color = "#33bbee"),
                  hoverinfo = "text"
      )  %>% 
      layout(xaxis = list(
        title = ""
      ),
      yaxis = list(
        title = "Variables Scaled to Overlap",
        ticks = "",   
        showticklabels = FALSE
      ),
      legend = list(
        orientation = "h",     # horizontal orientation
        x = 0.5,               # center the legend horizontally
        xanchor = "center",    # center anchor for the legend
        y = -0.1               # position the legend above the plot area; adjust as needed
      ))
    
    
    if (!is.null(ranges$x)) {
      p <- p %>% layout(xaxis = list(range = ranges$x))
    }
    if (!is.null(ranges$y)) {
      p <- p %>% layout(yaxis = list(range = ranges$y))
    }
    
    
    
   p %>% 
     event_register('plotly_relayout')
   
  })
  
  
  
  # Create a reactive for category statistics
  category_stats <- reactive({
    req(filt_data())
    
    # Get counts for individual extremes
    var1_high_count <- sum(!is.na(filt_data()$var1_cat) & filt_data()$var1_cat == "high", na.rm = TRUE)
    var1_low_count <- sum(!is.na(filt_data()$var1_cat) & filt_data()$var1_cat == "low", na.rm = TRUE)
    var2_high_count <- sum(!is.na(filt_data()$var2_cat) & filt_data()$var2_cat == "high", na.rm = TRUE)
    var2_low_count <- sum(!is.na(filt_data()$var2_cat) & filt_data()$var2_cat == "low", na.rm = TRUE)
    
    # Create a new variable for combined categories
    data_with_combos <- filt_data() %>%
      mutate(combo = case_when(
        var1_cat == "high" & var2_cat == "high" ~ "Both High",
        var1_cat == "low" & var2_cat == "low" ~ "Both Low",
        var1_cat == "high" & var2_cat == "low" ~ str_c(input$var1, " High, ", input$var2, " Low"),
        var1_cat == "low" & var2_cat == "high" ~ str_c(input$var1, " Low, ", input$var2, " High"),
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(combo))
    
    # Get combo counts
    total_categorized <- nrow(data_with_combos)
    
    if(total_categorized > 0) {
      counts <- data_with_combos %>%
        count(combo, var1_cat, var2_cat) %>%
        mutate(
          days = n,
          var1_pct = case_when(
            var1_cat == "high" ~ round(n / var1_high_count * 100, 1),
            var1_cat == "low" ~ round(n / var1_low_count * 100, 1),
            TRUE ~ NA_real_
          ),
          var2_pct = case_when(
            var2_cat == "high" ~ round(n / var2_high_count * 100, 1),
            var2_cat == "low" ~ round(n / var2_low_count * 100, 1),
            TRUE ~ NA_real_
          )
        ) %>%
        select(combo, days, var1_pct, var2_pct)
    } else {
      # Return empty dataframe with correct structure if no categorized data
      counts <- data.frame(
        combo = character(),
        days = integer(),
        var1_pct = numeric(),
        var2_pct = numeric()
      )
    }
    
    
    # Calculate total rows with any categorization (high or low)
    total_rows <- nrow(filt_data())
    
    # Add information as attributes
    attr(counts, "total_rows") <- total_rows
    attr(counts, "var1_high_count") <- var1_high_count
    attr(counts, "var1_low_count") <- var1_low_count
    attr(counts, "var2_high_count") <- var2_high_count
    attr(counts, "var2_low_count") <- var2_low_count
    
    counts
  })
  
  # Render the category statistics table
  output$category_stats <- renderDT({
    req(category_stats())
    
    stats_df <- category_stats()
    
    # Rename the columns to make them more descriptive
    colnames(stats_df) <- c(
      "Combination", 
      "Days", 
      str_c("% of ", input$var1, " days in category"), 
      str_c("% of ", input$var2, " days in category")
    )
    
    # Get attributes with additional information
    total_rows <- attr(stats_df, "total_rows")
    var1_high_count <- attr(stats_df, "var1_high_count")
    var1_low_count <- attr(stats_df, "var1_low_count")
    var2_high_count <- attr(stats_df, "var2_high_count")
    var2_low_count <- attr(stats_df, "var2_low_count")
    
    # Add caption with additional information
    caption <- str_c(
      "Total Days: ", total_rows, 
      " | ", input$var1, " High: ", var1_high_count,
      " | ", input$var1, " Low: ", var1_low_count,
      " | ", input$var2, " High: ", var2_high_count,
      " | ", input$var2, " Low: ", var2_low_count
    )
    
    # Format table
    datatable(
      stats_df,
      options = list(
        dom = 't',
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE
      ),
      caption = tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        caption
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Combination',
        target = 'row',
        fontWeight = styleEqual('Total', 'bold')
      )
  })
}

shinyApp(ui = ui, server = server)