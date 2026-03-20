###CHA0S - NYC PROPERTY SALES DATA LOADING v2###

setwd("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Team Project")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(corrplot)
library(purrr)


# ============================================================================
# STEP 1: Get all Excel files
# ============================================================================

raw_files <- list.files("Raw data", 
                        pattern = "\\.xls$|\\.xlsx$", 
                        full.names = TRUE, 
                        recursive = TRUE)

cat("Total files found:", length(raw_files), "\n\n")

# ============================================================================
# STEP 2: Load files with robust type handling
# ============================================================================

# Read each file and suppress the type combination warnings
read_sales_file_robust <- function(file_path) {
  cat("Reading:", file_path, "\n")
  
  # Use suppressWarnings to ignore the type mismatch warnings during read
  data <- suppressWarnings(
    read_excel(file_path, skip = 3)
  )
  
  return(data)
}

# Load all files as character (text) first
all_sales_list <- map(raw_files, read_sales_file_robust)

cat("\nCombining all datasets...\n")

# Convert all columns to character to avoid type conflicts, then bind
all_sales <- all_sales_list %>%
  map(~mutate(.x, across(everything(), as.character))) %>%
  bind_rows()

cat("\n", strrep("=", 60), "\n")
cat("Combined dataset loaded successfully!\n")
cat("Dimensions:", dim(all_sales), "\n")
cat(strrep("=", 60), "\n\n")

all_sales <- all_sales %>%
  mutate(
    BOROUGH = as.numeric(BOROUGH),
    BLOCK = as.numeric(BLOCK),
    LOT = as.numeric(LOT),
    `RESIDENTIAL UNITS` = as.numeric(`RESIDENTIAL UNITS`),
    `COMMERCIAL UNITS` = as.numeric(`COMMERCIAL UNITS`),
    `TOTAL UNITS` = as.numeric(`TOTAL UNITS`),
    `LAND SQUARE FEET` = as.numeric(`LAND SQUARE FEET`),
    `GROSS SQUARE FEET` = as.numeric(`GROSS SQUARE FEET`),
    `YEAR BUILT` = as.numeric(`YEAR BUILT`),
    `SALE PRICE` = as.numeric(`SALE PRICE`),
    `SALE DATE` = as.Date(`SALE DATE`, format = "%m/%d/%Y")
  )

cat("Data types converted back to proper formats.\n\n")

# ============================================================================
# STEP 3: Standardize column names and convert types
# ============================================================================

# Lowercase and replace spaces with underscores
names(all_sales) <- tolower(gsub(" ", "_", names(all_sales)))

cat("Standardized column names:\n")
print(names(all_sales))
cat("\n")

all_sales <- all_sales %>%
  select(borough:sale_date)  # Keep only first 21 data columns

cat("Removed note columns. Final column count:", ncol(all_sales), "\n")
print(names(all_sales))
cat("\n")

# Convert key numeric columns
all_sales <- all_sales %>%
  mutate(
    borough = as.numeric(borough),
    block = as.numeric(block),
    lot = as.numeric(lot),
    residential_units = as.numeric(residential_units),
    commercial_units = as.numeric(commercial_units),
    total_units = as.numeric(total_units),
    land_square_feet = as.numeric(land_square_feet),
    gross_square_feet = as.numeric(gross_square_feet),
    year_built = as.numeric(year_built),
    sale_price = as.numeric(sale_price),
    
    # Convert sale date (handle different formats)
    sale_date = suppressWarnings(
      as.Date(sale_date, format = "%m/%d/%Y")
    )
  )

cat("Data types converted successfully!\n\n")

#XOXXOXOXO
# Check total rows
#cat("Total rows in dataset:", nrow(all_sales), "\n")

# Look at actual values in these columns
#cat("\nFirst 10 SALE_PRICE values:\n")
#print(head(all_sales$sale_price, 10))

#cat("\nFirst 10 SALE_DATE values:\n")
#print(head(all_sales$sale_date, 10))

# Check data types
#cat("\nData types:\n")
#cat("sale_price class:", class(all_sales$sale_price), "\n")
#cat("sale_date class:", class(all_sales$sale_date), "\n")

# Look at sample raw data
#cat("\nFirst 5 rows of relevant columns:\n")
#print(all_sales %>% select(address, sale_price, sale_date) %>% head(5))

#XOXO
# Test on first file - read WITHOUT skipping to see exact structure
#test_raw <- read_excel(raw_files[1], col_names = FALSE)

# Print rows 1-10 to see where actual data is
#cat("First 10 rows (raw):\n")
#print(head(test_raw, 10))

# Print column names at different row levels
#cat("\nRow 4 (potential header):\n")
#print(test_raw[4, ])

#cat("\nRow 5 (potential first data):\n")
#print(test_raw[5, ])

###XOXOXOXOX
# Check total rows
cat("Total rows:", nrow(all_sales), "\n\n")

# Look at actual values
cat("First 20 SALE_PRICE values:\n")
print(head(all_sales$sale_price, 20))
cat("\n")

cat("First 20 SALE_DATE values:\n")
print(head(all_sales$sale_date, 20))
cat("\n")

# Check for empty strings or weird values
cat("Unique non-NA SALE_PRICE values (first 20):\n")
print(head(unique(na.omit(all_sales$sale_price)), 20))
cat("\n")

# Check raw data before conversion
cat("First 20 raw SALE_DATE strings:\n")
print(head(all_sales %>% pull(sale_date), 20))
###########################################
# Look at the raw data BEFORE conversion to see what's in each column
cat("Checking raw data structure - first 3 rows:\n")
print(all_sales_list[[1]][1:3, ])

# Check all column 21 (should be SALE DATE based on our understanding)
cat("\n\nColumn 21 (sale_date):\n")
print(head(all_sales_list[[1]] %>% select(21)))

# Check if dates might be in a different column
cat("\n\nAll columns with dates/times:\n")
date_cols <- names(all_sales_list[[1]])[grep("DATE|DATE|date", names(all_sales_list[[1]]), ignore.case = TRUE)]
print(date_cols)

# Print those columns
if(length(date_cols) > 0) {
  print(head(all_sales_list[[1]] %>% select(all_of(date_cols))))
}


####XOXOXOXOXO
# Check for any remaining NA values from conversion
cat("Conversion summary:\n")
cat("Rows with non-numeric SALE_PRICE:", sum(is.na(all_sales$sale_price)), "\n")
cat("Rows with invalid SALE_DATE:", sum(is.na(all_sales$sale_date)), "\n\n")

# ============================================================================
# STEP 4: Verify the data
# ============================================================================

cat("Data structure:\n")
str(all_sales, max.level = 1)
cat("\n")

cat("First 5 rows:\n")
print(head(all_sales %>% select(borough, address, sale_price, sale_date), 5))
cat("\n")

cat("Summary statistics:\n")
summary(all_sales$sale_price)
cat("\n")

cat("Sale date range:", min(all_sales$sale_date, na.rm = TRUE), 
    "to", max(all_sales$sale_date, na.rm = TRUE), "\n")
