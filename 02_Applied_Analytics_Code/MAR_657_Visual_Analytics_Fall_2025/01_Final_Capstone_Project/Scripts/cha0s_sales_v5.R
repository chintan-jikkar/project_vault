# ======================================================================
# CHA0S :: NYC Property Sales — Robust Loader + Cleaner (2003–2025)
# Author: GPT-5 Implementation
# ======================================================================
install.packages("janitor")

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(janitor)
  library(purrr)
  library(readr)
  library(stringr)
})

# ------------------------------- WORKING DIRECTORY --------------------
setwd("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Team Project")

# ------------------------------- CONFIG --------------------------------
INPUT_DIR  <- "Raw data"   # folder containing all .xls/.xlsx files (recursively)
OUT_DIR    <- "processed_data"
OUT_CSV    <- file.path(OUT_DIR, "nyc_sales_clean_2003_2025.csv")

MIN_PRICE  <- 10000           # drop non–arms-length
PPSF_MIN   <- 10              # $/sqft lower bound
PPSF_MAX   <- 10000           # $/sqft upper bound
AGE_MIN    <- -5
AGE_MAX    <- 150

# ---------------------------- HELPERS ----------------------------------

detect_header_skip <- function(path, lookfor = "BOROUGH") {
  probe <- suppressWarnings(read_excel(path, col_names = FALSE, n_max = 10))
  hit <- which(
    sapply(seq_len(nrow(probe)), function(i)
      any(grepl(lookfor, probe[i, ], ignore.case = TRUE, useBytes = TRUE)))
  )
  if (length(hit)) max(0, hit[1] - 1) else 3L
}

read_sales_one <- function(path) {
  skip <- detect_header_skip(path)
  cat(sprintf("Reading: %s (skip=%s)\n", path, skip))
  df <- suppressWarnings(read_excel(path, skip = skip))
  df <- clean_names(df)
  df <- df %>%
    mutate(across(everything(), as.character)) %>%
    rename(easement = any_of(c("ease_ment", "ease-ment", "easement"))) %>%
    rename(
      building_class_category        = any_of(c("building_class_category")),
      tax_class_at_present           = any_of(c("tax_class_at_present")),
      building_class_at_present      = any_of(c("building_class_at_present")),
      tax_class_at_time_of_sale      = any_of(c("tax_class_at_time_of_sale")),
      building_class_at_time_of_sale = any_of(c("building_class_at_time_of_sale")),
      apartment_number               = any_of(c("apartment_number","apartment","apt")),
      gross_square_feet              = any_of(c("gross_square_feet","gross_sq_feet","gross_sqft","gross_sf")),
      land_square_feet               = any_of(c("land_square_feet","land_sq_feet","land_sqft","land_sf")),
      sale_price                     = any_of(c("sale_price","saleprice")),
      sale_date                      = any_of(c("sale_date","saledate"))
    ) %>%
    mutate(source_file = basename(path))
  df
}

want_cols <- c(
  "borough","neighborhood","building_class_category","tax_class_at_present",
  "block","lot","easement","building_class_at_present","address",
  "apartment_number","zip_code","residential_units","commercial_units",
  "total_units","land_square_feet","gross_square_feet","year_built",
  "tax_class_at_time_of_sale","building_class_at_time_of_sale",
  "sale_price","sale_date","source_file"
)

# --------------------------- LOAD ALL FILES -----------------------------
files <- list.files(INPUT_DIR, pattern = "\\.(xlsx|xls)$", full.names = TRUE, recursive = TRUE)
cat("Total files found:", length(files), "\n\n")
stopifnot(length(files) > 0)

raw_list <- map(files, read_sales_one)

cat("\nBinding rows…\n")
raw_all <- bind_rows(raw_list)

cat("\n================ LOAD SUMMARY ================\n")
cat("Rows (raw):", nrow(raw_all), "\n")
cat("Cols (raw):", ncol(raw_all), "\n")
cat("=============================================\n\n")

raw_all <- raw_all %>% select(any_of(want_cols))

# ------------------------ TYPE FIXES & PARSING -------------------------
parse_sale_date <- function(x) {
  out <- suppressWarnings(as.Date(x))
  bad <- is.na(out) & !is.na(x)
  if (any(bad)) {
    out2 <- suppressWarnings(parse_date_time(x[bad], orders = c("mdy","ymd","dmy")))
    out[bad] <- as.Date(out2)
  }
  out
}

num_cols <- c("borough","block","lot","residential_units","commercial_units",
              "total_units","land_square_feet","gross_square_feet","year_built")

clean_all <- raw_all %>%
  mutate(
    sale_date = parse_sale_date(sale_date),
    sale_price = parse_number(sale_price)
  ) %>%
  mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))))

# ------------------------ FEATURE ENGINEERING --------------------------
map_borough <- function(b) {
  bnum <- suppressWarnings(as.numeric(b))
  case_when(
    !is.na(bnum) & bnum == 1 ~ "Manhattan",
    !is.na(bnum) & bnum == 2 ~ "Bronx",
    !is.na(bnum) & bnum == 3 ~ "Brooklyn",
    !is.na(bnum) & bnum == 4 ~ "Queens",
    !is.na(bnum) & bnum == 5 ~ "Staten Island",
    TRUE ~ str_to_title(as.character(b))
  )
}

clean_all <- clean_all %>%
  mutate(
    borough_name   = map_borough(borough),
    sale_year      = year(sale_date),
    sale_month     = month(sale_date),
    sale_quarter   = quarter(sale_date),
    price_per_sqft = ifelse(!is.na(gross_square_feet) & as.numeric(gross_square_feet) > 0,
                            sale_price / as.numeric(gross_square_feet), NA_real_),
    property_age   = ifelse(!is.na(year_built),
                            sale_year - as.numeric(year_built), NA_real_),
    is_new_construction = ifelse(!is.na(property_age) & property_age <= 2, 1L, 0L)
  )

# ------------------------------ FILTERS --------------------------------
clean_all <- clean_all %>%
  filter(!is.na(sale_date), !is.na(sale_price)) %>%
  filter(sale_price >= MIN_PRICE) %>%
  filter(is.na(price_per_sqft) | (price_per_sqft >= PPSF_MIN & price_per_sqft <= PPSF_MAX)) %>%
  filter(is.na(property_age) | (property_age >= AGE_MIN & property_age <= AGE_MAX))

# ----------------------------- VALIDATION ------------------------------
cat("\n================ CLEAN SUMMARY ================\n")
cat("Rows (clean):", nrow(clean_all), "\n")
yrng <- range(clean_all$sale_year, na.rm = TRUE)
cat(sprintf("Year range: %s — %s\n", yrng[1], yrng[2]))
cat("NA in borough_name:", sum(is.na(clean_all$borough_name)), "\n")
cat("NA in sale_price:",   sum(is.na(clean_all$sale_price)),   "\n")
cat("NA in sale_date:",    sum(is.na(clean_all$sale_date)),    "\n")
cat("================================================\n\n")

by_boro <- clean_all %>%
  group_by(borough_name) %>%
  summarise(
    n = n(),
    median_price = median(sale_price, na.rm = TRUE),
    mean_price   = mean(sale_price,   na.rm = TRUE),
    mean_ppsf    = mean(price_per_sqft, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_price))

by_year <- clean_all %>%
  group_by(sale_year) %>%
  summarise(
    n = n(),
    avg_price = mean(sale_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(sale_year)

cat("By borough (head):\n"); print(head(by_boro, 10)); cat("\n")
cat("By year (head):\n");    print(head(by_year, 10));  cat("\n")

# ------------------------------- OUTPUT --------------------------------
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
write.csv(clean_all, OUT_CSV, row.names = FALSE)
cat(sprintf("✅ Saved cleaned dataset → %s\n", OUT_CSV))
