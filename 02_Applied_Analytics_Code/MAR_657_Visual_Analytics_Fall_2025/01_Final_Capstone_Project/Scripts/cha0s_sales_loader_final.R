# ======================================================================
# CHA0S :: NYC Property Sales — Full-Market Loader + Flags + Summaries
# Scope: 2003–2025 (residential + commercial + land) • R v4+
# ======================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(tibble)
})

# ------------------------------- WORKING DIRECTORY ---------------------
setwd("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Team Project")

# ------------------------------- CONFIG --------------------------------
INPUT_DIR  <- "Raw data"                         # recursive: .xls/.xlsx
OUT_DIR    <- "Processed data"
OUT_CSV    <- file.path(OUT_DIR, "nyc_sales_clean_2003_2025.csv")

# Core sanity thresholds (keep dataset broad but sane)
MIN_PRICE  <- 10000      # drop $0 / symbolic transfers
PPSF_MIN   <- 10         # $/sqft lower bound (avoid bogus huge sqft)
PPSF_MAX   <- 10000      # $/sqft upper bound (avoid 1 sqft divisions)
AGE_MIN    <- -5         # allow slight negatives for new builds paperwork
AGE_MAX    <- 150

# Luxury tail indicator (flag only; do not drop)
LUXURY_PPSF_CUT <- 5000

# ---------------------------- HELPERS ----------------------------------

# Detect header row (contains "BOROUGH"); fallback = 3
detect_header_skip <- function(path, lookfor = "BOROUGH") {
  probe <- suppressWarnings(read_excel(path, col_names = FALSE, n_max = 10))
  hit <- which(
    sapply(seq_len(nrow(probe)), function(i)
      any(grepl(lookfor, probe[i, ], ignore.case = TRUE, useBytes = TRUE)))
  )
  if (length(hit)) max(0, hit[1] - 1) else 3L
}

# Minimal name cleaner (lowercase; spaces/hyphens -> underscores)
clean_names_min <- function(df) {
  names(df) <- names(df) |>
    tolower() |>
    gsub("[[:space:]]+", "_", x = _) |>
    gsub("-", "_", x = _)
  df
}

# Robust sale_date parser: Excel serials + ISO + mdy/ymd/dmy
parse_sale_date <- function(x) {
  out <- rep(as.Date(NA), length(x))
  
  # numeric-like -> try Excel serial (origin 1899-12-30)
  is_numlike <- suppressWarnings(!is.na(as.numeric(x))) & !is.na(x)
  if (any(is_numlike)) {
    xn <- suppressWarnings(as.numeric(x[is_numlike]))
    is_excel <- !is.na(xn) & xn > 20000 & xn < 60000  # ~1954–2064
    out[is_numlike & is_excel] <- as.Date(xn[is_excel], origin = "1899-12-30")
  }
  
  # strings / anything still NA -> try ISO then parse_date_time
  need_str <- is.na(out) & !is.na(x)
  if (any(need_str)) {
    xs <- as.character(x[need_str])
    try1 <- suppressWarnings(as.Date(xs))   # ISO-like
    out[need_str] <- try1
    still <- is.na(out[need_str])
    if (any(still)) {
      xs2 <- xs[still]
      try2 <- suppressWarnings(parse_date_time(xs2, orders = c("ymd","mdy","dmy")))
      out[need_str][still] <- as.Date(try2)
    }
  }
  out
}

# Read one workbook robustly; coerce to character to avoid bind_rows type clashes
read_sales_one <- function(path) {
  skip <- detect_header_skip(path)
  cat(sprintf("Reading: %s (skip=%s)\n", path, skip))
  df <- suppressWarnings(read_excel(path, skip = skip))
  df <- clean_names_min(df)
  df <- df %>% mutate(across(everything(), as.character))
  
  # ---- normalize headers with stray suffixes/spacing (fixes 2012–2019) ----
  normalize_to <- function(nms, pattern, target) sub(pattern, target, nms, perl = TRUE)
  nm <- names(df)
  nm <- normalize_to(nm, "^(sale_date)(\\W|_)*$",                               "sale_date")
  nm <- normalize_to(nm, "^(saledate)(\\W|_)*$",                                "sale_date")
  nm <- normalize_to(nm, "^(sale_price)(\\W|_)*$",                              "sale_price")
  nm <- normalize_to(nm, "^(gross_square_feet|gross_sq[_]?ft|gross_sf)(\\W|_)*$","gross_square_feet")
  nm <- normalize_to(nm, "^(land_square_feet|land_sq[_]?ft|land_sf)(\\W|_)*$",   "land_square_feet")
  nm <- normalize_to(nm, "^(apartment_number|apartment|apt)(\\W|_)*$",           "apartment_number")
  nm <- normalize_to(nm, "^(building_class_category)(\\W|_)*$",                  "building_class_category")
  nm <- normalize_to(nm, "^(building_class_at_time_of_sale)(\\W|_)*$",           "building_class_at_time_of_sale")
  nm <- normalize_to(nm, "^(tax_class_at_time_of_sale)(\\W|_)*$",                "tax_class_at_time_of_sale")
  nm <- normalize_to(nm, "^(building_class_at_present)(\\W|_)*$",                "building_class_at_present")
  nm <- normalize_to(nm, "^(tax_class_at_present)(\\W|_)*$",                     "tax_class_at_present")
  nm <- normalize_to(nm, "^(borough|boro)(\\W|_)*$",                              "borough")
  nm <- normalize_to(nm, "^(zip_code|zip)(\\W|_)*$",                              "zip_code")
  nm <- normalize_to(nm, "^(residential_units)(\\W|_)*$",                         "residential_units")
  nm <- normalize_to(nm, "^(commercial_units)(\\W|_)*$",                          "commercial_units")
  nm <- normalize_to(nm, "^(total_units)(\\W|_)*$",                               "total_units")
  nm <- normalize_to(nm, "^(year_built)(\\W|_)*$",                                "year_built")
  nm <- normalize_to(nm, "^(address)(\\W|_)*$",                                   "address")
  nm <- normalize_to(nm, "^(neighborhood)(\\W|_)*$",                              "neighborhood")
  nm <- normalize_to(nm, "^(block)(\\W|_)*$",                                     "block")
  nm <- normalize_to(nm, "^(lot)(\\W|_)*$",                                       "lot")
  nm <- normalize_to(nm, "^(ease[_-]?ment)(\\W|_)*$",                             "easement")
  names(df) <- nm
  # -------------------------------------------------------------------------
  
  # Normalize common variants (no-ops if already normalized)
  df <- df %>%
    rename(
      easement                       = any_of(c("easement","ease_ment","ease-ment")),
      building_class_category        = any_of("building_class_category"),
      tax_class_at_present           = any_of("tax_class_at_present"),
      building_class_at_present      = any_of("building_class_at_present"),
      tax_class_at_time_of_sale      = any_of("tax_class_at_time_of_sale"),
      building_class_at_time_of_sale = any_of("building_class_at_time_of_sale"),
      apartment_number               = any_of(c("apartment_number","apartment","apt")),
      gross_square_feet              = any_of(c("gross_square_feet","gross_sq_feet","gross_sqft","gross_sf")),
      land_square_feet               = any_of(c("land_square_feet","land_sq_feet","land_sqft","land_sf")),
      sale_price                     = any_of(c("sale_price","saleprice")),
      sale_date                      = any_of(c("sale_date","saledate"))
    ) %>%
    mutate(source_file = basename(path))
  
  df
}

# Columns we prefer to keep (safe-select with any_of)
want_cols <- c(
  "borough","neighborhood","building_class_category","tax_class_at_present",
  "block","lot","easement","building_class_at_present","address",
  "apartment_number","zip_code","residential_units","commercial_units",
  "total_units","land_square_feet","gross_square_feet","year_built",
  "tax_class_at_time_of_sale","building_class_at_time_of_sale",
  "sale_price","sale_date","source_file"
)

# Borough mapping: numeric codes or already-labeled strings
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

# --------------------------- LOAD ALL FILES -----------------------------
files <- list.files(INPUT_DIR, pattern = "\\.(xlsx|xls)$", full.names = TRUE, recursive = TRUE)
cat("Total files found:", length(files), "\n\n")
stopifnot(length(files) > 0)

raw_list <- map(files, read_sales_one)

cat("\nBinding rows…\n")
raw_all <- bind_rows(raw_list) %>% select(any_of(want_cols))

cat("\n================ LOAD SUMMARY ================\n")
cat("Rows (raw):", nrow(raw_all), "\n")
cat("Cols (raw):", ncol(raw_all), "\n")
cat("=============================================\n\n")

# ------------------------ TYPE FIXES & PARSING -------------------------
num_cols <- c("borough","block","lot","residential_units","commercial_units",
              "total_units","land_square_feet","gross_square_feet","year_built")

clean_all <- raw_all %>%
  mutate(
    sale_date  = parse_sale_date(sale_date),
    sale_price = parse_number(sale_price)
  ) %>%
  mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))))

# ------------------------ FEATURE ENGINEERING --------------------------
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
  filter(is.na(property_age)   | (property_age >= AGE_MIN & property_age <= AGE_MAX))

# ------------------------------ FLAGS ----------------------------------
res_patterns <- "(1 FAMILY|2 FAMILY|3 FAMILY|4 FAMILY|APARTMENT|CONDO|COOP|CO-OP|DWELL|RESIDENTIAL)"
q_hi <- quantile(clean_all$sale_price, 0.995, na.rm = TRUE)   # 99.5th pct for winsor/flag

clean_all <- clean_all %>%
  mutate(
    is_residential   = if_else(
      !is.na(building_class_category) & grepl(res_patterns, building_class_category, ignore.case = TRUE) |
        (!is.na(total_units) & suppressWarnings(as.numeric(total_units)) >= 1),
      TRUE, FALSE, missing = FALSE
    ),
    has_ppsf         = !is.na(price_per_sqft),
    is_luxury_ppsf   = has_ppsf & price_per_sqft > LUXURY_PPSF_CUT,
    is_extreme_price = sale_price >= q_hi,
    price_winsor     = pmin(sale_price, q_hi)
  )

# ----------------------------- VALIDATION ------------------------------
cat("\n================ CLEAN SUMMARY ================\n")
cat("Rows (clean):", nrow(clean_all), "\n")
yrng <- range(clean_all$sale_year, na.rm = TRUE)
cat(sprintf("Year range: %s — %s\n", yrng[1], yrng[2]))
cat("NA in borough_name:", sum(is.na(clean_all$borough_name)), "\n")
cat("NA in sale_price:",   sum(is.na(clean_all$sale_price)),   "\n")
cat("NA in sale_date:",    sum(is.na(clean_all$sale_date)),    "\n")
cat("================================================\n\n")

# Assert full-year coverage (2003–2025)
missing_years <- setdiff(2003:2025, sort(unique(clean_all$sale_year)))
if (length(missing_years)) {
  warning("Missing years in cleaned data: ", paste(missing_years, collapse = ", "))
}

# ------------------------- SUMMARIES (MARKET) --------------------------
borough_market <- clean_all %>%
  group_by(borough_name) %>%
  summarise(
    n                 = n(),
    median_price      = median(sale_price, na.rm = TRUE),
    mean_price        = mean(sale_price,   na.rm = TRUE),
    trimmed_mean_p01  = mean(sale_price, trim = 0.01, na.rm = TRUE),
    winsor_mean_p995  = mean(price_winsor, na.rm = TRUE),
    mean_ppsf_all     = mean(price_per_sqft, na.rm = TRUE),
    share_res         = mean(is_residential),
    share_has_ppsf    = mean(has_ppsf),
    luxury_ppsf_share = mean(is_luxury_ppsf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_price))


cat("=== Borough Market Summary ===\n"); print(borough_market); cat("\n")

by_year <- clean_all %>%
  group_by(sale_year) %>%
  summarise(
    n = n(),
    median_price = median(sale_price, na.rm = TRUE),
    avg_price    = mean(sale_price, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(sale_year)

cat("=== Year Summary ===\n"); print(by_year, n = 30); cat("\n")

# -------------------- SUMMARIES (RESIDENTIAL SLICE) --------------------
borough_res <- clean_all %>%
  filter(is_residential) %>%
  group_by(borough_name) %>%
  summarise(
    n_res                = n(),
    median_price_res     = median(sale_price, na.rm = TRUE),
    mean_price_res       = mean(sale_price,   na.rm = TRUE),
    mean_ppsf_res        = mean(price_per_sqft, na.rm = TRUE),
    luxury_ppsf_share_res= mean(is_luxury_ppsf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_price_res))

cat("=== Borough Residential Summary ===\n"); print(borough_res); cat("\n")

# --------------------------- YEAR × BOROUGH GRID -----------------------
year_boro <- clean_all %>%
  group_by(sale_year, borough_name) %>%
  summarise(
    n = n(),
    median_price = median(sale_price, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(sale_year, borough_name)

cat("=== Year x Borough (head) ===\n"); print(head(year_boro, 20)); cat("\n")

# --------------------------- TAIL HEALTH CHECKS ------------------------
tail_check <- clean_all %>%
  summarise(
    n = n(),
    p995_price = quantile(sale_price, 0.995, na.rm = TRUE),
    share_extreme_price = mean(is_extreme_price),
    share_luxury_ppsf   = mean(is_luxury_ppsf, na.rm = TRUE)
  )
cat("=== Tail Checks (city-wide) ===\n"); print(tail_check); cat("\n")

bx_qn <- clean_all %>%
  filter(borough_name %in% c("Bronx","Queens")) %>%
  group_by(borough_name) %>%
  summarise(
    n = n(),
    median_price = median(sale_price, na.rm = TRUE),
    mean_price   = mean(sale_price, na.rm = TRUE),
    p99_price    = quantile(sale_price, 0.99,  na.rm = TRUE),
    p995_price   = quantile(sale_price, 0.995, na.rm = TRUE),
    luxury_ppsf_share = mean(is_luxury_ppsf, na.rm = TRUE),
    .groups = "drop"
  )
cat("=== Bronx vs Queens Tail Comparison ===\n"); print(bx_qn); cat("\n")

# ---------------------- FILE–YEAR COVERAGE DIAGNOSTICS -----------------
files_list <- list.files(INPUT_DIR, pattern="\\.(xlsx|xls)$", full.names=TRUE, recursive=TRUE)

file_years <- tibble(
  file = basename(files_list),
  y4   = as.integer(str_extract(basename(files_list), "(?<!\\d)(20\\d{2}|200\\d)(?!\\d)")),
  y2   = suppressWarnings(as.integer(str_match(basename(files_list), "_(\\d{2})(?=\\D*$)")[,2]))
) %>%
  mutate(year_hint = dplyr::coalesce(y4, ifelse(!is.na(y2), 2000 + y2, NA_integer_))) %>%
  select(file, year_hint)

by_file_from_data <- clean_all %>%
  group_by(source_file) %>%
  summarise(min_year = min(sale_year, na.rm = TRUE),
            max_year = max(sale_year, na.rm = TRUE),
            .groups = "drop")

year_coverage <- by_file_from_data %>%
  left_join(file_years, by = c("source_file" = "file")) %>%
  mutate(year_final = dplyr::coalesce(year_hint, min_year)) %>%
  arrange(coalesce(year_final, min_year), source_file)

cat("=== File-Year Coverage (sample) ===\n"); print(head(year_coverage, 30)); cat("\n")

yrs_present <- sort(unique(clean_all$sale_year))
cat("Missing years vs 2003:2025 -> ", paste(setdiff(2003:2025, yrs_present), collapse=", "), "\n\n")

# ------------------------------- OUTPUT --------------------------------
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
write.csv(clean_all, OUT_CSV, row.names = FALSE)
cat(sprintf("✅ Saved cleaned dataset → %s\n", OUT_CSV))
