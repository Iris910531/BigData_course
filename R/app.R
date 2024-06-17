library(shinydashboard)
library(shiny)
library(ggplot2)
library(reticulate)
library(stringr)
library(plotly)
library(gridExtra)
library(colourpicker)
library(showtext)

# 加載系統字體
showtext_auto()

# 指定要使用的字體，例如 "PingFang TC"
font_add("PingFang", "PingFang.ttc")

# 應用字體
showtext_opts(dpi = 96)

# 使用 conda 環境名稱來設置 Python
use_condaenv("bigdata", required = TRUE)

# 或者如果你想手動設置，也可以使用下面這行
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
      print("有波內的index數量太少了，故刪掉此波")
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
    message(paste0("此資料有誤，故不生成", basename(output_file)))
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
      message(paste0(file_name, "資料（前處理）有問題，故不生成"))
      pie_count <<- c(pie_count, 0)
      return(NULL)
    }
  } else {
    message(paste0(file_name, "raw data太少以至於找不到頭尾，故不生成"))
    pie_count <<- c(pie_count, 0)
    return(NULL)
  }
  # 使用 reticulate 來調用 Python 函數
  source_python("python/module.py")
  result <- input_data(prepro_done, position, model_path, file_name)
  p_message <<- c(p_message, result[[1]])
  pie_count <<- c(pie_count, result[[2]])
}

ui <- dashboardPage(
  dashboardHeader(title = "機械手臂的振動情況"),
  dashboardSidebar(
    width = 350,  # 設置側邊欄寬度
    selectInput("option", "選擇作動位置：", choices = c("水平作動下馬達側(Xa)", "水平作動下惰輪側(Xb)", "垂直作動下馬達側(Ya)", "垂直作動下惰輪側(Yb)")),
    fileInput("file", "上傳文件（至多25個）", multiple = TRUE, accept = c(".txt")),
    actionButton("process", "處理文件")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        #fileInfo { display: none; }
      "))
    ),
    fluidRow(
      column(width = 12,
             textOutput("displayText")  # 在頁面上顯示一行文字
      )
    ),
    fluidRow(
      box(title = "文件資訊", status = "primary", solidHeader = TRUE,
          verbatimTextOutput("fileInfo"),
          textOutput("file_count"),
          textOutput("warning_message")
      )
    ),
    fluidRow(
      column(width = 12,
             box(title = "圓餅圖", status = "primary", solidHeader = TRUE, plotlyOutput("pieChart"))
      ),
      column(width = 12,
             box(title = "各個結果圖", status = "primary", solidHeader = TRUE, plotlyOutput("gridPlot"))
      )
    )
  )
)


# 定義伺服器邏輯
server <- function(input, output) {
  
  observeEvent(input$process, {
    # 初始化變數
    pie_count <<- c()
    f_name <<- c()
    p_message <<- c()
    
    req(input$file)
    files <- input$file
    if (nrow(files) > 25) {
      output$warning_message <- renderText("上傳的文件數量不能超過25個。")
      output$file_count <- renderText("")
      output$fileInfo <- renderText("")
    } else {
      output$warning_message <- renderText("")
      output$file_count <- renderText(paste("上傳的文件數量為:", nrow(files)))
      
      lapply(1:nrow(files), function(i) {
        file <- files[i,]
        file_path <- file$datapath
        file_name <- file$name
        par(mfrow = c(1, 2))
        
        switch(input$option,
               "水平作動下馬達側(Xa)" = process_file(file_path, file_name, -0.2, 0.1, 1000, 
                                             "/python/trained_models/Xa_model_24.joblib", "Xa"),
               "水平作動下惰輪側(Xb)" = process_file(file_path, file_name, -2, -1.2, 1000, 
                                             "/python/trained_models/Xb_model_20.joblib", "Xb"),
               "垂直作動下馬達側(Ya)" = process_file(file_path, file_name, -3, 3, 1000, 
                                             "/python/trained_models/Ya_model_4.joblib", "Ya"),
               "垂直作動下惰輪側(Yb)" = process_file(file_path, file_name, -2, 2, 1000, 
                                             "/python/trained_models/Yb_model_20.joblib", "Yb")
        )
      })
      
      # 定義顏色對應表
      color_map <- c("資料異常" = "#272727", "65" = "#EE6363", "80" = "#76EE00", "95" = "#FFD700", "130" = "#CE0000", "220" = "#EE6363", "260" = "#76EE00", "300" = "#FFD700", "380" = "#CE0000")
      if (all(pie_count == 0)) {
        showModal(modalDialog(
          title = "錯誤",
          "選擇的作動位置與上傳的資料不匹配",
          easyClose = TRUE,
          footer = NULL
        ))
      } 
      
      # 根據選擇的作動位置生成圓餅圖
      output$pieChart <- renderPlotly({
        data <- table(pie_count)
        pie_data <- data.frame(
          labels = names(data),
          values = as.numeric(data)
        )
        pie_data$labels[pie_data$labels == "0"] <- "資料異常"
        
        # 確保顏色順序正確
        colors <- sapply(pie_data$labels, function(label) color_map[label])
        
        plot_ly(pie_data, labels = ~labels, values = ~values, type = 'pie',
                textinfo = 'none',  # 移除預設顯示
                text = ~paste("負荷：", labels, "<br>數量：", values, "<br>百分比：", round(values / sum(values) * 100, 2), "%"),
                hoverinfo = 'text',  # 設定hover資訊
                textinfo = 'label+percent', insidetextorientation = 'radial',
                marker = list(colors = colors)) %>%
          layout(title = list(text = paste("圓餅圖 -", input$option), font = list(family = "Heiti TC")))
      })
      
      # 生成顏色格子圖
      output$gridPlot <- renderPlotly({
        grid_data <- data.frame(
          x = rep(1:5, times = 5),
          y = rep(5:1, each = 5),
          labels = rep("", 25),
          file = rep("", 25)
        )
        
        # 填充標籤和文件名
        all_labels <- 1:(nrow(files))
        grid_data$labels[all_labels] <- pie_count[all_labels]
        grid_data$labels[grid_data$labels == 0] <- "資料異常"  # 替換 0 為 "異常"
        grid_data$file[all_labels] <- f_name[all_labels]
        
        p <- ggplot(grid_data, aes(x = x, y = y, fill = labels, text = paste("資料:", file, "<br>負荷:", labels))) +
          geom_tile(color = "white") +
          scale_fill_manual(values = color_map) +
          theme_minimal() +
          theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())
        
        ggplotly(p, tooltip = "text") %>%
          layout(legend = list(title = list(text = "各個結果圖"), traceorder = "normal"))
      })
      
      # 在頁面上顯示一行文字
      output$displayText <- renderText({
        "如需協助，請洽李先生 電話：0912345678 信箱：abc@gmail.com"
      })
    }
  })
}

# 啟動應用程式
shinyApp(ui, server)
