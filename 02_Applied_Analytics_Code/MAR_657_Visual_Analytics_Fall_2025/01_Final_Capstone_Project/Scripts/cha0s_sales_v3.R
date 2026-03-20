###CHA0S - NYC PROPERTY SALES DATA LOADING v3###

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
# STEP 2: Load files and coerce types before combining
# ============================================================================

read_sales_file_robust <- function(file_path) {
  cat("Reading:", file_path, "\n")
  
  data <- suppressWarnings(
    read_excel(file_path, skip = 3)
  )
  
  # Coerce APARTMENT NUMBER to character ONLY if it exists
  if("APARTMENT NUMBER" %in% names(data)) {
    data <- data %>%
      mutate(`APARTMENT NUMBER` = as.character(`APARTMENT NUMBER`))
  }
  
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
    sale_date = as.Date(sale_date),
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
# STEP 9: Save cleaned dataset
# ============================================================================

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

# ============================================================================
# DIAGNOSTIC: Check which files have date format issues
# ============================================================================

cat("Checking all files for date parsing issues...\n\n")

date_issues <- data.frame()

for(i in seq_along(all_sales_list)) {
  file_name <- raw_files[i]
  df <- all_sales_list[[i]]
  
  na_count <- sum(is.na(df$`SALE DATE`))
  total_rows <- nrow(df)
  
  if(na_count > 0 | total_rows == 0) {
    date_issues <- rbind(date_issues, data.frame(
      File = basename(file_name),
      Total_Rows = total_rows,
      NA_Dates = na_count,
      NA_Percent = round(100 * na_count / total_rows, 2)
    ))
  }
}

if(nrow(date_issues) > 0) {
  cat("Files with date issues:\n")
  print(date_issues)
  cat("\n")
} else {
  cat("No date issues found!\n\n")
}

# Now check what the sale_date column looks like in a 2011+ file
cat("Checking 2011 file structure:\n")
test_2011 <- read_excel(raw_files[grep("2011", raw_files)[1]], skip = 3)
cat("\nColumn names in 2011 file:\n")
print(names(test_2011))

cat("\nFirst 5 SALE DATE values from 2011:\n")
print(head(test_2011$`SALE DATE`, 5))

cat("\nData type of SALE DATE column:\n")
print(class(test_2011$`SALE DATE`))


# Check the RAW structure of 2011 file (no skip)
cat("2011 file raw structure (first 10 rows):\n")
test_2011_raw <- read_excel(raw_files[grep("2011", raw_files)[1]], col_names = FALSE)
print(head(test_2011_raw, 10))

cat("\n\nColumn count in 2011 file:", ncol(test_2011_raw), "\n")
cat("Row count in 2011 file:", nrow(test_2011_raw), "\n")
