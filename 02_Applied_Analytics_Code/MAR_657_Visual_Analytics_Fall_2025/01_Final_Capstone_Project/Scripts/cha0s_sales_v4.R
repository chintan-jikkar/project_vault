###CHA0S - NYC PROPERTY SALES DATA LOADING v4###
# ADAPTIVE SKIP HANDLING FOR ALL YEARS (2003-2025)

# ============================================================================
# NYC PROPERTY SALES DATA - LOADING AND INITIAL PROCESSING
# ============================================================================

# Set working directory
setwd("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Team Project")

# Load libraries
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
# STEP 2: Load files with ADAPTIVE skip handling (2003-2025)
# ============================================================================

read_sales_file_robust <- function(file_path) {
  cat("Reading:", file_path, "\n")
  
  # Read first 10 rows to detect skip value
  test_read <- read_excel(file_path, col_names = FALSE, n_max = 10)
  
  # Check which row has BOROUGH (the header row)
  header_row <- which(sapply(1:nrow(test_read), function(i) {
    any(grepl("BOROUGH", test_read[i, ], ignore.case = TRUE))
  }))
  
  # Determine skip value
  skip_val <- ifelse(length(header_row) > 0, header_row[1] - 1, 3)
  
  # Now read the file with the correct skip value
  data <- suppressWarnings(
    read_excel(file_path, skip = skip_val)
  )
  
  # Coerce ALL character/numeric columns to character to avoid type conflicts
  # This handles APARTMENT NUMBER and any other columns that vary in type
  data <- data %>%
    mutate(across(where(function(x) is.character(x) | is.numeric(x) | is.logical(x)),
                  as.character))
  
  return(data)
}

# Load all files
all_sales_list <- map(raw_files, read_sales_file_robust)

cat("\nCombining all datasets...\n")

# ============================================================================
# STEP 2b: Combine all files
# ============================================================================

all_sales <- bind_rows(all_sales_list, .id = "source")

cat("\n", strrep("=", 60), "\n")
cat("Combined dataset loaded successfully!\n")
cat("Dimensions:", dim(all_sales), "\n")
cat(strrep("=", 60), "\n\n")

# ============================================================================
# STEP 3: Standardize column names and clean up
# ============================================================================

# Lowercase and replace spaces with underscores
names(all_sales) <- tolower(gsub(" ", "_", names(all_sales)))

# Remove the source column (not needed)
all_sales <- all_sales %>%
  select(-source)

cat("Standardized column names:\n")
print(names(all_sales))
cat("\n")

# Keep only essential data columns (remove any extra note columns)
all_sales <- all_sales %>%
  select(borough, neighborhood, building_class_category, tax_class_at_present,
         block, lot, `ease-ment`, building_class_at_present, address,
         apartment_number, zip_code, residential_units, commercial_units,
         total_units, land_square_feet, gross_square_feet, year_built,
         tax_class_at_time_of_sale, building_class_at_time_of_sale,
         sale_price, sale_date)

cat("Removed extra columns. Final column count:", ncol(all_sales), "\n\n")

# ============================================================================
# STEP 4: Fix date format and ensure proper data types
# ============================================================================

all_sales <- all_sales %>%
  mutate(
    # Convert datetime to date
    sale_date = as.Date(sale_date),
    
    # Ensure numeric columns are numeric
    borough = as.numeric(borough),
    block = as.numeric(block),
    lot = as.numeric(lot),
    residential_units = as.numeric(residential_units),
    commercial_units = as.numeric(commercial_units),
    total_units = as.numeric(total_units),
    land_square_feet = as.numeric(land_square_feet),
    gross_square_feet = as.numeric(gross_square_feet),
    year_built = as.numeric(year_built),
    sale_price = as.numeric(sale_price)
  )

cat("Data types converted successfully!\n\n")

# ============================================================================
# STEP 5: Initial data inspection
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

cat("Rows with NA sale_price:", sum(is.na(all_sales$sale_price)), "\n")
cat("Rows with NA sale_date:", sum(is.na(all_sales$sale_date)), "\n\n")

# ============================================================================
# STEP 6: Data cleaning and feature engineering
# ============================================================================

all_sales_clean <- all_sales %>%
  # Extract temporal features
  mutate(
    sale_year = year(sale_date),
    sale_month = month(sale_date),
    sale_quarter = quarter(sale_date),
    
    # Map borough codes to borough names
    borough_name = case_when(
      borough == 1 ~ "Manhattan",
      borough == 2 ~ "Bronx",
      borough == 3 ~ "Brooklyn",
      borough == 4 ~ "Queens",
      borough == 5 ~ "Staten Island",
      TRUE ~ "Unknown"
    ),
    
    # Price per square foot
    price_per_sqft = ifelse(
      !is.na(gross_square_feet) & gross_square_feet > 0,
      sale_price / gross_square_feet,
      NA
    ),
    
    # Property age
    property_age = ifelse(
      !is.na(year_built),
      sale_year - year_built,
      NA
    ),
    
    # New construction indicator
    is_new_construction = ifelse(property_age <= 2, 1, 0)
  ) %>%
  # Remove non-arms-length sales (price < $10,000)
  filter(sale_price >= 10000, !is.na(sale_price), !is.na(sale_date)) %>%
  # Remove extreme outliers in price_per_sqft
  filter(is.na(price_per_sqft) | (price_per_sqft >= 10 & price_per_sqft <= 10000)) %>%
  # Remove unrealistic property ages
  filter(is.na(property_age) | (property_age >= -5 & property_age <= 150))

cat(strrep("=", 60), "\n")
cat("Data after cleaning:\n")
cat("Original rows:", nrow(all_sales), "\n")
cat("Clean rows:", nrow(all_sales_clean), "\n")
cat("Rows removed:", nrow(all_sales) - nrow(all_sales_clean), "\n")
cat(strrep("=", 60), "\n\n")

# ============================================================================
# STEP 7: Summary statistics by borough
# ============================================================================

cat("Summary Statistics by Borough:\n")
borough_summary <- all_sales_clean %>%
  group_by(borough_name) %>%
  summarise(
    Count = n(),
    Avg_Price = round(mean(sale_price, na.rm = TRUE), 2),
    Median_Price = round(median(sale_price, na.rm = TRUE), 2),
    Avg_Price_Per_SqFt = round(mean(price_per_sqft, na.rm = TRUE), 2),
    Std_Price = round(sd(sale_price, na.rm = TRUE), 2),
    Min_Price = min(sale_price, na.rm = TRUE),
    Max_Price = max(sale_price, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Avg_Price))

print(borough_summary)
cat("\n")

# ============================================================================
# STEP 8: Sales by year
# ============================================================================

cat("Sales count by year:\n")
year_summary <- all_sales_clean %>%
  group_by(sale_year) %>%
  summarise(
    Count = n(),
    Avg_Price = round(mean(sale_price, na.rm = TRUE), 2),
    Avg_Price_Per_SqFt = round(mean(price_per_sqft, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(sale_year)

print(year_summary)
cat("\n")

# ============================================================================
# STEP 9: Create output directory and save cleaned dataset
# ============================================================================

# Create folder if it doesn't exist
dir.create("processed_data", showWarnings = FALSE)

# Save the cleaned data
write.csv(
  all_sales_clean,
  "processed_data/nyc_sales_clean_2003_2025.csv",
  row.names = FALSE
)

cat(strrep("=", 60), "\n")
cat("Cleaned data saved to: processed_data/nyc_sales_clean_2003_2025.csv\n")
cat("Year range:", min(all_sales_clean$sale_year, na.rm = TRUE), 
    "to", max(all_sales_clean$sale_year, na.rm = TRUE), "\n")
cat("Total records:", nrow(all_sales_clean), "\n")
cat(strrep("=", 60), "\n")
