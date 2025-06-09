# Load required libraries
library(shiny)
library(bslib)
library(purrr)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(duckdb)
library(tidyr)


theme = bs_theme(brand_yml = "_brand.yml")

# Initialize DuckDB connection and load data
con <- dbConnect(duckdb())

# Source the tips dataset
source("load_tips.R")

# Ensure the dataset has the required structure
if (!all(c("total_bill", "tip", "sex", "smoker", "day", "time", "size") %in% names(tips))) {
  stop("Tips dataset is missing required columns")
}

# Create the tips table in DuckDB with enhanced structure
tips_enhanced <- tips %>%
  mutate(
    tip_percent = round((tip / total_bill) * 100, 2),
    day = factor(day, levels = c("Thur", "Fri", "Sat", "Sun")),
    day_order = case_when(
      day == "Thur" ~ 1,
      day == "Fri" ~ 2,
      day == "Sat" ~ 3,
      day == "Sun" ~ 4
    )
  )

# Write data to DuckDB
dbWriteTable(con, "tips", tips_enhanced, overwrite = TRUE)

# Define UI-----
ui <- page_sidebar(
  title = "Restaurant Tipping Dashboard with Filters",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#3498db"
  ),
  
  # Sidebar with Filter Controls----
  sidebar = sidebar(
    width = 350,
    title = "Filter & Sort Controls",
    
    # Bill Amount Range Slider
    div(
      style = "margin-bottom: 20px;",
      h5("Total Bill Range", style = "color: #2c3e50; margin-bottom: 10px;"),
      sliderInput(
        "bill_range",
        label = NULL,
        min = floor(min(tips_enhanced$total_bill)),
        max = ceiling(max(tips_enhanced$total_bill)),
        value = c(floor(min(tips_enhanced$total_bill)), ceiling(max(tips_enhanced$total_bill))),
        step = 1,
        pre = "$"
      )
    ),
    
    # Tip Percentage Range Slider
    div(
      style = "margin-bottom: 20px;",
      h5("Tip Percentage Range", style = "color: #2c3e50; margin-bottom: 10px;"),
      sliderInput(
        "tip_percent_range",
        label = NULL,
        min = floor(min(tips_enhanced$tip_percent)),
        max = ceiling(max(tips_enhanced$tip_percent)),
        value = c(floor(min(tips_enhanced$tip_percent)), ceiling(max(tips_enhanced$tip_percent))),
        step = 1,
        post = "%"
      )
    ),
    
    # Party Size Range Slider
    div(
      style = "margin-bottom: 20px;",
      h5("Party Size", style = "color: #2c3e50; margin-bottom: 10px;"),
      sliderInput(
        "size_range",
        label = NULL,
        min = min(tips_enhanced$size),
        max = max(tips_enhanced$size),
        value = c(min(tips_enhanced$size), max(tips_enhanced$size)),
        step = 1
      )
    ),
    
    hr(),
    
    # Gender Filter (Radio Buttons)
    div(
      style = "margin-bottom: 20px;",
      h5("Gender Filter", style = "color: #2c3e50; margin-bottom: 10px;"),
      radioButtons(
        "gender_filter",
        label = NULL,
        choices = list(
          "All" = "all",
          "Male Only" = "Male",
          "Female Only" = "Female"
        ),
        selected = "all",
        inline = TRUE
      )
    ),
    
    # Smoker Filter (Radio Buttons)
    div(
      style = "margin-bottom: 20px;",
      h5("Smoker Filter", style = "color: #2c3e50; margin-bottom: 10px;"),
      radioButtons(
        "smoker_filter",
        label = NULL,
        choices = list(
          "All" = "all",
          "Smokers" = "Yes",
          "Non-Smokers" = "No"
        ),
        selected = "all",
        inline = TRUE
      )
    ),
    
    # Day Filter (Checkboxes)
    div(
      style = "margin-bottom: 20px;",
      h5("Day Filter", style = "color: #2c3e50; margin-bottom: 10px;"),
      checkboxGroupInput(
        "day_filter",
        label = NULL,
        choices = c("Thursday" = "Thur", "Friday" = "Fri", "Saturday" = "Sat", "Sunday" = "Sun"),
        selected = c("Thur", "Fri", "Sat", "Sun"),
        inline = FALSE
      )
    ),
    
    # Time Filter (Dropdown)
    div(
      style = "margin-bottom: 20px;",
      h5("Meal Time", style = "color: #2c3e50; margin-bottom: 10px;"),
      selectInput(
        "time_filter",
        label = NULL,
        choices = list(
          "All Times" = "all",
          "Lunch Only" = "Lunch",
          "Dinner Only" = "Dinner"
        ),
        selected = "all"
      )
    ),
    
    hr(),
    
    # Sort Options
    div(
      style = "margin-bottom: 20px;",
      h5("Sort Data By", style = "color: #2c3e50; margin-bottom: 10px;"),
      selectInput(
        "sort_by",
        label = NULL,
        choices = list(
          "Total Bill (Low to High)" = "total_bill_asc",
          "Total Bill (High to Low)" = "total_bill_desc",
          "Tip Amount (Low to High)" = "tip_asc",
          "Tip Amount (High to Low)" = "tip_desc",
          "Tip Percentage (Low to High)" = "tip_percent_asc",
          "Tip Percentage (High to Low)" = "tip_percent_desc",
          "Party Size (Small to Large)" = "size_asc",
          "Party Size (Large to Small)" = "size_desc",
          "Day of Week" = "day_order"
        ),
        selected = "total_bill_desc"
      )
    ),
    
    # Reset Button
    div(
      style = "margin-top: 20px;",
      actionButton(
        "reset_filters",
        "Reset All Filters",
        class = "btn-outline-secondary btn-sm",
        style = "width: 100%;"
      )
    ),
    
    hr(),
    
    # Status Display
    div(
      style = "background: #f8f9fa; padding: 10px; border-radius: 5px; font-size: 0.85em;",
      strong("Filtered Results: "), 
      span(textOutput("filtered_count", inline = TRUE), style = "color: #007bff; font-weight: bold;"),
      " of ", 
      span(nrow(tips_enhanced), style = "color: #6c757d;"), 
      " total records"
    )
  ),
  
  # Main content####
  div(
    # KPI Cards Row
    fluidRow(
      class = "mb-4",
      
      # Total Records Card
      column(3,
             div(
               class = "card h-100 bg-primary",
               style = "border: none;",
               div(
                 class = "card-body text-white text-center",
                 div(
                   style = "font-size: 3rem; margin-bottom: 0.5rem;",
                   icon("users")
                 ),
                 h4("Filtered Records", style = "margin-bottom: 0.5rem;"),
                 h2(textOutput("total_tippers"), style = "font-weight: bold; margin: 0;")
               )
             )
      ),
      
      # Average Tip Percentage Card
      column(3,
             div(
               class = "card h-100 bg-success",
               style = "border: none;",
               div(
                 class = "card-body text-white text-center",
                 div(
                   style = "font-size: 3rem; margin-bottom: 0.5rem;",
                   icon("percentage")
                 ),
                 h4("Average Tip %", style = "margin-bottom: 0.5rem;"),
                 h2(textOutput("avg_tip_percent"), style = "font-weight: bold; margin: 0;")
               )
             )
      ),
      
      # Average Bill Card
      column(3,
             div(
               class = "card h-100 bg-info",
               style = "border: none;",
               div(
                 class = "card-body text-white text-center",
                 div(
                   style = "font-size: 3rem; margin-bottom: 0.5rem;",
                   icon("dollar-sign")
                 ),
                 h4("Average Bill", style = "margin-bottom: 0.5rem;"),
                 h2(textOutput("avg_bill"), style = "font-weight: bold; margin: 0;")
               )
             )
      ),
      
      # Average Tip Amount Card
      column(3,
             div(
               class = "card h-100 bg-warning",
               style = "border: none;",
               div(
                 class = "card-body text-white text-center",
                 div(
                   style = "font-size: 3rem; margin-bottom: 0.5rem;",
                   icon("money-bill-wave")
                 ),
                 h4("Average Tip", style = "margin-bottom: 0.5rem;"),
                 h2(textOutput("avg_tip"), style = "font-weight: bold; margin: 0;")
               )
             )
      )
    ),
    
    # Charts Row
    fluidRow(
      # Data Table
      column(6,
             div(
               class = "card h-100",
               style = "overflow: hidden;",
               div(
                 class = "card-header",
                 h4("Filtered Data", class = "card-title mb-0")
               ),
               div(
                 class = "card-body p-0",
                 style = "height: calc(100% - 60px); overflow: hidden;",
                 div(
                   style = "height: 100%; padding: 15px;",
                   DT::DTOutput("tips_table", height = "100%")
                 )
               )
             )
      ),
      
      ## Scatter Plot----
      column(6,
             div(
               class = "card",
               div(
                 class = "card-header",
                 h4("Total Bill vs. Tip", class = "card-title mb-0")
               ),
               div(
                 class = "card-body",
                 plotlyOutput("scatter_plot", height = "400px")
               )
             )
      )
    ),
    
    # Additional Charts Row-----
    fluidRow(
      class = "mt-4",
      
      ## Tip Distribution by Day----
      column(6,
             div(
               class = "card",
               div(
                 class = "card-header",
                 h4("Tip Distribution by Day", class = "card-title mb-0")
               ),
               div(
                 class = "card-body",
                 plotlyOutput("tip_by_day", height = "350px")
               )
             )
      ),
      
      ## Tip by Time and Gender----
      column(6,
             div(
               class = "card",
               div(
                 class = "card-header",
                 h4("Tips by Time & Gender", class = "card-title mb-0")
               ),
               div(
                 class = "card-body",
                 plotlyOutput("tip_by_time_gender", height = "350px")
               )
             )
      )
    )
  )
)

# Define Server----
server <- function(input, output, session) {
  
  # Reactive function to filter and sort data
  filtered_data <- reactive({
    data <- tips_enhanced
    
    # Apply bill range filter
    data <- data %>%
      filter(total_bill >= input$bill_range[1] & total_bill <= input$bill_range[2])
    
    # Apply tip percentage range filter
    data <- data %>%
      filter(tip_percent >= input$tip_percent_range[1] & tip_percent <= input$tip_percent_range[2])
    
    # Apply party size filter
    data <- data %>%
      filter(size >= input$size_range[1] & size <= input$size_range[2])
    
    # Apply gender filter
    if (input$gender_filter != "all") {
      data <- data %>% filter(sex == input$gender_filter)
    }
    
    # Apply smoker filter
    if (input$smoker_filter != "all") {
      data <- data %>% filter(smoker == input$smoker_filter)
    }
    
    # Apply day filter
    if (!is.null(input$day_filter) && length(input$day_filter) > 0) {
      data <- data %>% filter(day %in% input$day_filter)
    }
    
    # Apply time filter
    if (input$time_filter != "all") {
      data <- data %>% filter(time == input$time_filter)
    }
    
    # Apply sorting
    data <- switch(input$sort_by,
                   "total_bill_asc" = data %>% arrange(total_bill),
                   "total_bill_desc" = data %>% arrange(desc(total_bill)),
                   "tip_asc" = data %>% arrange(tip),
                   "tip_desc" = data %>% arrange(desc(tip)),
                   "tip_percent_asc" = data %>% arrange(tip_percent),
                   "tip_percent_desc" = data %>% arrange(desc(tip_percent)),
                   "size_asc" = data %>% arrange(size),
                   "size_desc" = data %>% arrange(desc(size)),
                   "day_order" = data %>% arrange(day_order),
                   data
    )
    
    return(data)
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSliderInput(session, "bill_range", 
                      value = c(floor(min(tips_enhanced$total_bill)), ceiling(max(tips_enhanced$total_bill))))
    updateSliderInput(session, "tip_percent_range", 
                      value = c(floor(min(tips_enhanced$tip_percent)), ceiling(max(tips_enhanced$tip_percent))))
    updateSliderInput(session, "size_range", 
                      value = c(min(tips_enhanced$size), max(tips_enhanced$size)))
    updateRadioButtons(session, "gender_filter", selected = "all")
    updateRadioButtons(session, "smoker_filter", selected = "all")
    updateCheckboxGroupInput(session, "day_filter", selected = c("Thur", "Fri", "Sat", "Sun"))
    updateSelectInput(session, "time_filter", selected = "all")
    updateSelectInput(session, "sort_by", selected = "total_bill_desc")
  })
  
  # Output filtered count
  output$filtered_count <- renderText({
    as.character(nrow(filtered_data()))
  })
  
  ## KPI Calculations----
  output$total_tippers <- renderText({
    as.character(nrow(filtered_data()))
  })
  
  output$avg_tip_percent <- renderText({
    data <- filtered_data()
    if (nrow(data) > 0) {
      avg_percent <- mean(data$tip_percent, na.rm = TRUE)
      paste0(round(avg_percent, 1), "%")
    } else {
      "N/A"
    }
  })
  
  output$avg_bill <- renderText({
    data <- filtered_data()
    if (nrow(data) > 0) {
      avg_bill <- mean(data$total_bill, na.rm = TRUE)
      paste0("$", round(avg_bill, 2))
    } else {
      "N/A"
    }
  })
  
  output$avg_tip <- renderText({
    data <- filtered_data()
    if (nrow(data) > 0) {
      avg_tip <- mean(data$tip, na.rm = TRUE)
      paste0("$", round(avg_tip, 2))
    } else {
      "N/A"
    }
  })
  
  ## Data Table----
  output$tips_table <- DT::renderDT({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "No data matches current filters")))
    }
    
    # Create the data table
    dt <- DT::datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "350px",
        dom = 'frtip',
        ordering = TRUE,
        searching = TRUE,
        info = TRUE,
        lengthChange = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "80px", targets = "_all")
        ),
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().container()).find('.dataTables_scrollBody').css('max-height', '350px');",
          "}"
        )
      ),
      class = "compact cell-border stripe hover",
      rownames = FALSE
    ) %>%
      DT::formatCurrency('total_bill', '$') %>%
      DT::formatCurrency('tip', '$') %>%
      DT::formatRound('tip_percent', 1)
    
    return(dt)
  })
  
  ## Scatter Plot----
  output$scatter_plot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data matches current filters") +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- data %>%
      ggplot(aes(x = total_bill, y = tip, colour = time, size = size)) +
      geom_point(alpha = 0.7) +
      scale_colour_manual(values = c("Dinner" = "#CB6874", "Lunch" = "#97CEA1")) +
      geom_smooth(method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
      labs(
        x = "Total Bill ($)",
        y = "Tip ($)"
      ) +
      theme_minimal() +
      theme(legend.position = "top")
    
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1, yanchor = "top"))
  })
  
  ## Tip Distribution by Day----
  output$tip_by_day <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data matches current filters") +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- data %>%
      ggplot(aes(x = day, y = tip_percent, fill = day)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_brewer(type = "qual", palette = "Set2") +
      labs(
        x = "Day of Week",
        y = "Tip Percentage (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  ## Tips by Time and Gender----
  output$tip_by_time_gender <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data matches current filters") +
        theme_void()
      return(ggplotly(p))
    }
    
    summary_data <- data %>%
      group_by(time, sex) %>%
      summarise(
        avg_tip = mean(tip, na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      )
    
    p <- summary_data %>%
      ggplot(aes(x = time, y = avg_tip, fill = sex)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("Female" = "#CB6874", "Male" = "#97CEA1")) +
      labs(
        x = "Time",
        y = "Average Tip ($)",
        fill = "Gender"
      ) +
      theme_minimal() +
      theme(legend.position = "top")
    
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1, yanchor = "top"))
  })
  
  # Clean up DuckDB connection when session ends
  onStop(function() {
    dbDisconnect(con)
  })
}

# Run the application
shinyApp(ui = ui, server = server)