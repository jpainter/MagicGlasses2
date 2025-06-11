
# Yearly Summary with Percent Change - Flextable Generator
# This script creates a formatted flextable showing yearly totals and percent changes
# with color coding for positive (red) and negative (green) changes

# Function to create yearly summary flextable
yearly_summary_table <- function(data, 
                                       date_col = "Month", 
                                       value_col = "total",
                                       country_col = "National",
                                       table_title = "Yearly Summary with Percent Change",
                                       positive_color = "#e74c3c",  # Red for positive
                                       negative_color = "#27ae60") { # Green for negative
  
  # Validate inputs
  if (!inherits(data, "tbl_ts")) {
    stop("Data must be a tsibble object")
  }
  
  if (!all(c(date_col, value_col) %in% colnames(data))) {
    stop("Specified columns not found in data")
  }
  
  # Extract year from date column and filter out NA values
  
  yearly_data = 
    data %>% ungroup %>%
    index_by( Year = year(!!index(.))) %>%
    summarise(
      Total = sum(!!sym(value_col), na.rm = TRUE),
      months_count = n(),
      .groups = "drop"
    ) %>%
    arrange(Year) %>%
    mutate(
      `Percent Change` = round((Total - lag(Total)) / lag(Total) * 100, 1),
      `Change Direction` = case_when(
        is.na(`Percent Change`) ~ "baseline",
        `Percent Change` > 0 ~ "positive",
        `Percent Change` < 0 ~ "negative",
        TRUE ~ "zero"
      ),
      `Formatted Change` = case_when(
        is.na(`Percent Change`) ~ "—",
        `Percent Change` > 0 ~ paste0("+", `Percent Change`, "%"),
        TRUE ~ paste0(`Percent Change`, "%")
      )
    )
  
  # Create the flextable
  ft <- yearly_data %>%
    select( Year, Total, `Formatted Change`) %>%
    flextable() %>%
    
    # Set column names
    set_header_labels(
      Year = "Year",
      Total = "Total",
      `Formatted Change` = "% Change from Previous Year"
    ) %>%
    
    # Format numbers with commas
    colformat_double(j = "Total", big.mark = ",", digits = 0) %>%
    colformat_double(j = "Year", big.mark = "", digits = 0) %>%
    
    # Apply conditional formatting for percent change column
    bg(
      i = which(yearly_data$`Change Direction` == "positive"),
      j = "Formatted Change",
      bg = positive_color
    ) %>%
    bg(
      i = which(yearly_data$`Change Direction` == "negative"), 
      j = "Formatted Change",
      bg = negative_color
    ) %>%
    bg(
      i = which(yearly_data$`Change Direction` == "baseline"),
      j = "Formatted Change", 
      bg = "#95a5a6"
    ) %>%
    
    # Set text color to white for colored cells
    color(
      i = which(yearly_data$`Change Direction` %in% c("positive", "negative", "baseline")),
      j = "Formatted Change",
      color = "white"
    ) %>%
    
    # Style the table
    theme_vanilla() %>%
    
    # Header styling
    bg(part = "header", bg = "#2c3e50") %>%
    color(part = "header", color = "white") %>%
    bold(part = "header") %>%
    
    # Title styling
    bg(i = 1, part = "header", bg = "#34495e") %>%
    align(i = 1, part = "header", align = "center") %>%
    fontsize(i = 1, part = "header", size = 14) %>%
    bold(i = 1, part = "header") %>%
    
    # Body styling
    align(j = c("Total", "Formatted Change"), align = "center", part = "body") %>%
    align(j = "Year", align = "left", part = "body") %>%
    fontsize(part = "body", size = 11) %>%
    
    # Column widths
    width(j = "Year", width = 1) %>%
    width(j = "Total", width = 1.5) %>%
    width(j = "Formatted Change", width = 2) %>%
    
    # Add borders
    border_outer(border = fp_border(color = "#2c3e50", width = 2)) %>%
    border_inner_h(border = fp_border(color = "#bdc3c7", width = 1)) %>%
    border_inner_v(border = fp_border(color = "#bdc3c7", width = 1))
  
  # Print summary statistics
  # cat("\n=== SUMMARY STATISTICS ===\n")
  valid_changes <- yearly_data$`Percent Change`[!is.na(yearly_data$`Percent Change`)]
  
  # if(length(valid_changes) > 0) {
  #   cat(sprintf("Average yearly change: %.1f%%\n", mean(valid_changes)))
  #   cat(sprintf("Largest increase: +%.1f%%\n", max(valid_changes)))
  #   cat(sprintf("Largest decrease: %.1f%%\n", min(valid_changes)))
  #   cat(sprintf("Years with increases: %d\n", sum(valid_changes > 0)))
  #   cat(sprintf("Years with decreases: %d\n", sum(valid_changes < 0)))
  # }
  # cat(sprintf("Total years analyzed: %d\n", nrow(yearly_data)))
  # cat("=========================\n\n")
  # 
  return( ft )
}


# Run the demo

# demo_table <- create_yearly_summary_table(
#   mable_Data, 
#   table_title = "Demo: Gambia Yearly Summary"
# )
# 
# # Display the table
# demo_table
