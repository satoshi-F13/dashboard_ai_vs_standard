# Load required libraries
library(shiny)
library(bslib) # Designing layout of web app
library(ellmer) # Chat configulation
library(purrr) #effective formula
library(DT)  # Data table
library(plotly) # Intractive visualization
library(dplyr)  # Data cleaninig
library(ggplot2) # Static visualization
library(shinychat) #AI assistant
library(duckdb)  #SQL package
library(tidyr)  # for data cleaning

theme = bs_theme(brand_yml = "_brand.yml")

# Initialize DuckDB connection and load data
con <- dbConnect(duckdb())


tips <- read.csv(file = "tips.csv")


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

# Initialize DuckDB connection and load data
con <- dbConnect(duckdb())
dbWriteTable(con, "tips", tips_enhanced, overwrite = TRUE)

# Define the system prompt for SQL generation----
sql_system_prompt <- "
You are a helpful SQL assistant for a restaurant tips database. 
The database contains a table called 'tips' with the following columns:
- total_bill (DOUBLE): The total bill amount in dollars
- tip (DOUBLE): The tip amount in dollars  
- sex (VARCHAR): Gender of the customer ('Male' or 'Female')
- smoker (VARCHAR): Whether customer smokes ('Yes' or 'No')
- day (VARCHAR): Day of the week ('Thur', 'Fri', 'Sat', 'Sun')
- time (VARCHAR): Time of meal ('Lunch' or 'Dinner')
- size (INTEGER): Number of people in the party
- tip_percent (DOUBLE): Calculated tip percentage
- day_order (INTEGER): Numeric day order (1=Thur, 2=Fri, 3=Sat, 4=Sun)

Always respond with valid SQL SELECT statements only. Do not include explanations.
Use proper SQL syntax for DuckDB.

Examples:
- 'Show weekend data' → SELECT * FROM tips WHERE day IN ('Sat', 'Sun')
- 'Filter by female customers' → SELECT * FROM tips WHERE sex = 'Female'
- 'Average tip for dinner vs lunch' → SELECT time, AVG(tip) as avg_tip FROM tips GROUP BY time
"

# Configure ellmer for SQL generation
ellmer_chat <- ellmer::chat_anthropic(
  model = "claude-3-5-sonnet-20241022",
  system_prompt = sql_system_prompt
)

# Define UI-----
ui <- page_sidebar(
  title = "Restaurant Tipping Dashboard with AI Asistant",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#3498db"
  ),
  
  # Sidebar with AI chat----
  sidebar = sidebar(
    width = 350,
    title = "AI SQL Assistant",
    p("Ask questions in natural language to query the tips database:", 
      style = "font-size: 0.9em; color: #666; margin-bottom: 15px;"),
    div(
      style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px; font-size: 0.8em;",
      strong("Example queries:"), br(),
      "• Show only weekend data", br(),
      "• Filter by female customers", br(),
      "• What's the average tip for dinner?", br(),
      "• Show high tip percentages over 20%"
    ),
    chat_ui("chat"),
    hr(),
    p("Status:", strong(textOutput("query_status", inline = TRUE)), 
      style = "font-size: 0.8em; margin-top: 10px;")
  ),
  
  # Main content####
  div(
    # KPI Cards Row
    fluidRow(
      class = "mb-4",
      
      # Total Tippers Card
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
                 h4("Total Records", style = "margin-bottom: 0.5rem;"),
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
    
    # Query Display Row (shows generated SQL)
    conditionalPanel(
      condition = "output.current_sql != ''",
      fluidRow(
        class = "mb-3",
        column(12,
               div(
                 class = "alert alert-info",
                 style = "margin-bottom: 0;",
                 strong("Current Query: "),
                 code(textOutput("current_sql", inline = TRUE))
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
                 h4("Query Results", class = "card-title mb-0")
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
  
  # Initialize reactive values
  values <- reactiveValues(
    current_data = tips_enhanced,
    current_sql = "SELECT * FROM tips",
    query_status = "Ready"
  )
  
  
  
  # Handle chat input using shinychat pattern
  observeEvent(input$chat_user_input, {
    user_message <- input$chat_user_input
    
    tryCatch({
      values$query_status <- "Processing..."
      
      # Generate SQL using ellmer
      sql_query <- ellmer_chat$chat(user_message)
      
      # Clean up the SQL (remove any markdown formatting)
      sql_query <- gsub("```sql\n|```\n|```", "", sql_query)
      sql_query <- trimws(sql_query)
      
      # Validate that it's a SELECT statement
      if (!grepl("^SELECT", toupper(sql_query))) {
        values$query_status <- "Error: Only SELECT queries allowed"
        chat_append("chat", "I can only help with SELECT queries to retrieve data.")
        return()
      }
      
      # Check if connection exists and is valid, if not recreate it
      if (!exists("con") || !dbIsValid(con)) {
        con <<- dbConnect(duckdb())
        dbWriteTable(con, "tips", tips_enhanced, overwrite = TRUE)
      }
      
      # Execute the query
      result <- dbGetQuery(con, sql_query)
      
      if (nrow(result) > 0) {
        values$current_data <- result
        values$current_sql <- sql_query
        values$query_status <- paste("Success:", nrow(result), "rows returned")
        
        response_message <- paste("✅ Query executed successfully! Retrieved", nrow(result), "rows.")
        chat_append("chat", response_message)
      } else {
        values$query_status <- "No results found"
        chat_append("chat", "Query executed but returned no results.")
      }
      
    }, error = function(e) {
      values$query_status <- paste("Error:", e$message)
      
      # Try to reconnect if there's a connection error
      if (grepl("connection|rapi_prepare|Invalid connection", e$message, ignore.case = TRUE)) {
        tryCatch({
          # Force close existing connection if it exists
          if (exists("con")) {
            try(dbDisconnect(con), silent = TRUE)
          }
          
          # Create new connection
          con <<- dbConnect(duckdb())
          dbWriteTable(con, "tips", tips_enhanced, overwrite = TRUE)
          
          # Retry the query
          result <- dbGetQuery(con, sql_query)
          
          if (nrow(result) > 0) {
            values$current_data <- result
            values$current_sql <- sql_query
            values$query_status <- paste("Success (after reconnect):", nrow(result), "rows returned")
            
            response_message <- paste("✅ Query executed successfully after reconnection! Retrieved", nrow(result), "rows.")
            chat_append("chat", response_message)
          } else {
            values$query_status <- "No results found (after reconnect)"
            chat_append("chat", "Query executed after reconnection but returned no results.")
          }
          
        }, error = function(reconnect_error) {
          values$query_status <- paste("Reconnection failed:", reconnect_error$message)
          chat_append("chat", paste("❌ Failed to reconnect to database:", reconnect_error$message))
        })
      } else {
        chat_append("chat", paste("❌ Error executing query:", e$message))
      }
    })
  })
  
  ## Output current SQL query----
  output$current_sql <- renderText({
    values$current_sql
  })
  
  ## Output query status----
  output$query_status <- renderText({
    values$query_status
  })
  
  ## KPI Calculations----
  output$total_tippers <- renderText({
    as.character(nrow(values$current_data))
  })
  
  output$avg_tip_percent <- renderText({
    if ("tip_percent" %in% names(values$current_data)) {
      avg_percent <- mean(values$current_data$tip_percent, na.rm = TRUE)
      paste0(round(avg_percent, 1), "%")
    } else {
      "N/A"
    }
  })
  
  output$avg_bill <- renderText({
    if ("total_bill" %in% names(values$current_data)) {
      avg_bill <- mean(values$current_data$total_bill, na.rm = TRUE)
      paste0("$", round(avg_bill, 2))
    } else {
      "N/A"
    }
  })
  
  output$avg_tip <- renderText({
    if ("tip" %in% names(values$current_data)) {
      avg_tip <- mean(values$current_data$tip, na.rm = TRUE)
      paste0("$", round(avg_tip, 2))
    } else {
      "N/A"
    }
  })
  
  ## Data Table----
  output$tips_table <- DT::renderDT({
    data <- values$current_data
    
    if (nrow(data) == 0) {
      return(DT::datatable(data.frame(Message = "No data to display")))
    }
    
    # Create the data table with available columns
    dt <- DT::datatable(
      data,
      options = list(
        pageLength = 10, # numbers of rows
        scrollX = TRUE,  # card width
        scrollY = "350px",  # card height
        dom = 'frtip',
        ordering = TRUE,
        searching = TRUE,
        info = TRUE,
        lengthChange = FALSE,  # Remove length change dropdown
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "80px", targets = "_all")  # Set consistent column width
        ),
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().container()).find('.dataTables_scrollBody').css('max-height', '350px');",
          "}"
        )
      ),
      class = "compact cell-border stripe hover",  # Better styling for compact view
      rownames = FALSE  # Remove row numbers to save space
    )
    
    ## Apply formatting if columns exist----
    if ("total_bill" %in% names(data)) {
      dt <- dt %>% DT::formatCurrency('total_bill', '$')
    }
    if ("tip" %in% names(data)) {
      dt <- dt %>% DT::formatCurrency('tip', '$')
    }
    if ("tip_percent" %in% names(data)) {
      dt <- dt %>% DT::formatRound('tip_percent', 1)
    }
    
    return(dt)
  })
  
  ## Scatter Plot----
  output$scatter_plot <- renderPlotly({
    data <- values$current_data
    
    # Check if required columns exist
    if (!all(c("total_bill", "tip") %in% names(data)) || nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for scatter plot") +
        theme_void()
      return(ggplotly(p))
    }
    
    ## Create base plot----
    aes_mapping <- aes(x = total_bill, y = tip)
    
    # Add color if time column exists
    if ("time" %in% names(data)) {
      aes_mapping$colour <- quote(time)
    }
    
    # Add size if size column exists
    if ("size" %in% names(data)) {
      aes_mapping$size <- quote(size)
    }
    
    p <- data %>%
      ggplot(aes_mapping) +
      geom_point(alpha = 0.7) +
      scale_colour_manual(values = c("Dinner" = "#CB6874", "Lunch" = "#97CEA1")) +
      geom_smooth(method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
      labs(
        x = "Total Bill ($)",
        y = "Tip ($)"
      ) +
      theme_minimal()+
      theme(legend.position = "top")
    
    ggplotly(p)%>% 
      #When ggplotly() converts a ggplot2 object to an interactive plotly chart, it doesn't always preserve ggplot2 theme settings
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1, yanchor = "top")) # Legend position as top
  })
  
  ## Tip Distribution by Day----
  output$tip_by_day <- renderPlotly({
    data <- values$current_data
    
    if (!all(c("day", "tip_percent") %in% names(data)) || nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for day analysis") +
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
    data <- values$current_data
    
    if (!all(c("time", "sex", "tip") %in% names(data)) || nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for time/gender analysis") +
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
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1, yanchor = "top")) # Legend position as top
  })
  
  # Clean up DuckDB connection when session ends
  onStop(function() {
    dbDisconnect(con)
  })
}

# Run the application
shinyApp(ui = ui, server = server)