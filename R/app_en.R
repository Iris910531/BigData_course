library(shinydashboard)
library(shiny)
library(ggplot2)
library(reticulate)
library(stringr)
library(plotly)
library(gridExtra)
library(colourpicker)
library(showtext)

# Load system fonts
#showtext_auto()

# Specify the font to use, for example "PingFang TC"
#font_add("PingFang", "PingFang.ttc")

# Apply font
#showtext_opts(dpi = 96)

# Use conda environment name to set Python
use_condaenv("bigdata", required = TRUE)

# Or if you want to set manually, you can use the line below
# use_python("/path/to/your/miniforge3/envs/bigdata/bin/python")

find_stable_regions <- function(signal, lower_bound, upper_bound, min_length) {
  stable_indices <- which(signal >= lower_bound & signal <= upper_bound)
  groups <- split(stable_indices, cumsum(c(1, diff(stable_indices) != 1)))
  stable_groups <- groups[sapply(groups, length) >= min_length]
  heads <- c()
  tails <- c()
  for (group in stable_groups) {
    head_index <- head(group, 1)
    tail_index <- tail(group, 1)
    before_range <- pmax(1, head_index - min_length/2)
    after_range <- pmin(length(signal), tail_index + min_length/2)
    if (mean(signal[before_range:head_index] >= lower_bound & signal[before_range:head_index] <= upper_bound) > 0.5) {
      heads <- c(heads, head_index)
    }
    if (mean(signal[tail_index:after_range] >= lower_bound & signal[tail_index:after_range] <= upper_bound) > 0.5) {
      tails <- c(tails, tail_index)
    }
  }
  return(list(headss = tails, tailss = heads))
}

trim_ts_data <- function(ts_data, head_indices, tail_indices) {
  start_index <- head_indices[1]
  end_index <- tail_indices[length(tail_indices)]
  trimmed_data <- ts_data[start_index:end_index]
  return(trimmed_data)
}

extract_3axis_segments_to_one_csv <- function(data_XYZ, head_indices, tail_indices, output_file) {
  all_segments_df <- data.frame()
  for (i in seq_along(head_indices)) {
    segment_head <- head_indices[i]
    segment_tail <- tail_indices[i]
    segment_X <- data_XYZ$X[segment_head:segment_tail]
    if (length(segment_X) <= 500) {
      print("The number of indices within the wave is too small, so this wave is deleted")
      next
    } else {
      segment_Y <- data_XYZ$Y[segment_head:segment_tail]
      segment_Z <- data_XYZ$Z[segment_head:segment_tail]
      df <- data.frame(
        Segment_Head = rep(segment_head, length(segment_X)),
        Segment_Tail = rep(segment_tail, length(segment_X)),
        Index = segment_head:segment_tail, 
        Xaxis = segment_X,
        Yaxis = segment_Y,
        Zaxis = segment_Z
      )
    }
    all_segments_df <- rbind(all_segments_df, df)
  }
  if (nrow(all_segments_df) < 1) {
    message(paste0("This data is incorrect, so ", basename(output_file), " is not generated"))
  } else {
    return(all_segments_df)
  }
}

process_file <- function(file_path, file_name, lower_bound, upper_bound, min_length, model_path, position) {
  f_name <<- c(f_name, file_name)
  content <- read.table(file_path, header = FALSE, stringsAsFactors = FALSE)
  numeric_df <- data.frame(lapply(content, as.numeric))
  clean_df <- na.omit(numeric_df)
  data_axis <- clean_df[, 1]
  stable_region_indices <- find_stable_regions(data_axis, lower_bound, upper_bound, min_length)
  head_indices <- stable_region_indices$headss
  tail_indices <- stable_region_indices$tailss
  if (length(head_indices) > 0 & length(tail_indices) > 0) {
    trimmed_ts_data <- trim_ts_data(data_axis, head_indices, tail_indices)
    trimmed_stable_region_indices <- find_stable_regions(trimmed_ts_data, lower_bound, upper_bound, min_length)
    trimmed_head_index <- c(1, trimmed_stable_region_indices$heads)
    trimmed_tail_index <- c(trimmed_stable_region_indices$tails, length(trimmed_ts_data))
    data_Xaxis <- clean_df[, 1]
    trimmed_X <- trim_ts_data(data_Xaxis, head_indices, tail_indices)
    data_Yaxis <- clean_df[, 2]
    trimmed_Y <- trim_ts_data(data_Yaxis, head_indices, tail_indices)
    data_Zaxis <- clean_df[, 3]
    trimmed_Z <- trim_ts_data(data_Zaxis, head_indices, tail_indices)
    df <- data.frame(X = unlist(trimmed_X), Y = unlist(trimmed_Y), Z = unlist(trimmed_Z))
    if (length(trimmed_head_index) > 1 & length(trimmed_tail_index) > 1) {
      prepro_done <- extract_3axis_segments_to_one_csv(df, trimmed_head_index, trimmed_tail_index, file_name)
    } else {
      message(paste0(file_name, " data (pre-processing) has a problem, so it is not generated"))
      pie_count <<- c(pie_count, 0)
      return(NULL)
    }
  } else {
    message(paste0(file_name, " raw data is too little to find head and tail, so it is not generated"))
    pie_count <<- c(pie_count, 0)
    return(NULL)
  }
  # Use reticulate to call Python function
  source_python("python/module.py")
  # Absolute Path
  #source_python("/path/to/your/python/module.py")
  
  result <- input_data(prepro_done, position, model_path, file_name)
  p_message <<- c(p_message, result[[1]])
  pie_count <<- c(pie_count, result[[2]])
}

ui <- dashboardPage(
  dashboardHeader(title = "Vibration Condition of Robotic Arm"),
  dashboardSidebar(
    width = 350,  # Set sidebar width
    selectInput("option", "Select Actuation Position:", choices = c("Horizontal Actuation on Motor Side (Xa)", "Horizontal Actuation on Idler Side (Xb)", "Vertical Actuation on Motor Side (Ya)", "Vertical Actuation on Idler Side (Yb)")),
    fileInput("file", "Upload Files (up to 25)", multiple = TRUE, accept = c(".txt")),
    actionButton("process", "Process Files")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        #fileInfo { display: none; }
      "))
    ),
    fluidRow(
      column(width = 12,
             textOutput("displayText")  # Display a line of text on the page
      )
    ),
    fluidRow(
      box(title = "Document Information", status = "primary", solidHeader = TRUE,
          verbatimTextOutput("fileInfo"),
          textOutput("file_count"),
          textOutput("warning_message")
      )
    ),
    fluidRow(
      column(width = 12,
             box(title = "Pie Chart", status = "primary", solidHeader = TRUE, plotlyOutput("pieChart"))
      ),
      column(width = 12,
             box(title = "Results for Each Figure", status = "primary", solidHeader = TRUE, plotlyOutput("gridPlot"))
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$process, {
    # Initialize variables
    pie_count <<- c()
    f_name <<- c()
    p_message <<- c()
    
    req(input$file)
    files <- input$file
    if (nrow(files) > 25) {
      output$warning_message <- renderText("The number of uploaded files cannot exceed 25.")
      output$file_count <- renderText("")
      output$fileInfo <- renderText("")
    } else {
      output$warning_message <- renderText("")
      output$file_count <- renderText(paste("Number of uploaded files:", nrow(files)))
      
      lapply(1:nrow(files), function(i) {
        file <- files[i,]
        file_path <- file$datapath
        file_name <- file$name
        par(mfrow = c(1, 2))
        
        switch(input$option,
               "Horizontal Actuation on Motor Side (Xa)" = process_file(file_path, file_name, -0.2, 0.1, 1000, 
                                                                        "python/trained_models/Xa_model_24.joblib", "Xa"),
               "Horizontal Actuation on Idler Side (Xb)" = process_file(file_path, file_name, -2, -1.2, 1000, 
                                                                        "python/trained_models/Xb_model_20.joblib", "Xb"),
               "Vertical Actuation on Motor Side (Ya)" = process_file(file_path, file_name, -3, 3, 1000, 
                                                                      "python/trained_models/Ya_model_4.joblib", "Ya"),
               "Vertical Actuation on Idler Side (Yb)" = process_file(file_path, file_name, -2, 2, 1000, 
                                                                      "python/trained_models/Yb_model_20.joblib", "Yb")
        )
      })
      # Absolute Path
      # "/path/to/your/python/trained_models/Xa_model_24.joblib"
      # "/path/to/your/python/trained_models/Xb_model_20.joblib"
      # "/path/to/your/python/trained_models/Ya_model_4.joblib"
      # "/path/to/your/python/trained_models/Yb_model_20.joblib"
      
      # Define color map
      color_map <- c("Data Error" = "#272727", "65" = "#EE6363", "80" = "#76EE00", "95" = "#FFD700", "130" = "#CE0000", "220" = "#EE6363", "260" = "#76EE00", "300" = "#FFD700", "380" = "#CE0000")
      if (all(pie_count == 0)) {
        showModal(modalDialog(
          title = "Error",
          "The selected actuation position does not match the uploaded data",
          easyClose = TRUE,
          footer = NULL
        ))
      } 
      
      # Generate pie chart based on selected actuation position
      output$pieChart <- renderPlotly({
        data <- table(pie_count)
        pie_data <- data.frame(
          labels = names(data),
          values = as.numeric(data)
        )
        pie_data$labels[pie_data$labels == "0"] <- "Data Error"
        
        # Ensure correct color order
        colors <- sapply(pie_data$labels, function(label) color_map[label])
        
        plot_ly(pie_data, labels = ~labels, values = ~values, type = 'pie',
                textinfo = 'none',  # Remove default display
                text = ~paste("Load:", labels, "<br>Count:", values, "<br>Percentage:", round(values / sum(values) * 100, 2), "%"),
                hoverinfo = 'text',  # Set hover information
                textinfo = 'label+percent', insidetextorientation = 'radial',
                marker = list(colors = colors)) %>%
          layout(title = list(text = paste("Pie Chart -", input$option), font = list(family = "Heiti TC")))
      })
      
      # Generate color grid plot
      output$gridPlot <- renderPlotly({
        grid_data <- data.frame(
          x = rep(1:5, times = 5),
          y = rep(5:1, each = 5),
          labels = rep("", 25),
          file = rep("", 25)
        )
        
        # Fill in labels and file names
        all_labels <- 1:(nrow(files))
        grid_data$labels[all_labels] <- pie_count[all_labels]
        grid_data$labels[grid_data$labels == 0] <- "Data Error"  # Replace 0 with "Data Error"
        grid_data$file[all_labels] <- f_name[all_labels]
        
        p <- ggplot(grid_data, aes(x = x, y = y, fill = labels, text = paste("File:", file, "<br>Load:", labels))) +
          geom_tile(color = "white") +
          scale_fill_manual(values = color_map) +
          theme_minimal() +
          theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
        
        ggplotly(p, tooltip = "text") %>%
          layout(legend = list(title = list(text = "Results for Each Figure"), traceorder = "normal"))
      })
      
      # Display a line of text on the page
      output$displayText <- renderText({
        "For assistance, please contact Mr. Li. Phone: 0912345678 Email: abc@gmail.com"
      })
    }
  })
}

# Launch the application
shinyApp(ui, server)
