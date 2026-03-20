###CHA0S - NYC PROPERTY SALES DATA LOADING v1###

setwd("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Team Project")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(corrplot)
library(purrr)

raw_files <- list.files("Raw data", 
                        pattern = "\\.xls$|\\.xlsx$", 
                        full.names = TRUE, 
                        recursive = TRUE)

cat("Total files found:", length(raw_files), "\n\n")

raw_files_df <- data.frame(file = raw_files) %>%
  mutate(
    year = gsub(".*([0-9]{4}).*", "\\1", file),
    format = ifelse(grepl("\\.xlsx$", file), "xlsx", "xls")
  ) %>%
  group_by(year, format) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(year)

read_sales_file_safe <- function(file_path) {
  cat("Reading:", file_path, "\n")
  data <- read_excel(file_path, skip = 3)  # Skip first 3 rows to get to actual headers
  return(data)
}

all_sales <- map_df(raw_files, read_sales_file_safe)

all_sales <- all_sales %>%
  mutate(
    across(c(BOROUGH, BLOCK, LOT, `RESIDENTIAL UNITS`, `COMMERCIAL UNITS`, 
             `TOTAL UNITS`, `LAND SQUARE FEET`, `GROSS SQUARE FEET`, 
             `YEAR BUILT`, `SALE PRICE`), as.numeric),
    SALE_DATE = as.Date(SALE_DATE, format = "%m/%d/%Y")
  )

cat("\n", "="*60, "\n")
cat("Combined dataset loaded successfully!\n")
cat("Dimensions:", dim(all_sales), "\n")
cat("="*60, "\n\n")

names(all_sales) <- tolower(gsub(" ", "_", names(all_sales)))

cat("Standardized column names:\n")
print(names(all_sales))
cat("\n")

cat("Data structure:\n")
str(all_sales, max.level = 1)
cat("\n")

cat("First 5 rows:\n")
head(all_sales, 5)
cat("\n")