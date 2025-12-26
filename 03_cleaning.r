# ==============================================================================
# 03_cleaning_FINAL_FIXED.R
# ==============================================================================

process_data <- function(df) {
  cat("Cleaning data and calculating prices...\n")
  
  # 1. REMOVE BAD 'AGENCY' COLUMN IF IT EXISTS
  # The previous loader created a wrong 'agency' column. We delete it.
  if("agency" %in% names(df)) {
    cat("  -> Removing incorrect 'agency' column...\n")
    df <- df %>% select(-agency)
  }

  # 2. RENAME CORRECT COLUMNS
  # We use b6q11 (found in scan) for Agency
  cat("  -> Renaming 'b6q11' to 'agency'...\n")
  
  df_renamed <- df %>%
    rename(
      agency   = b6q11,              # The correct Agency Column
      qty_sold = any_of(c("b6q12")), # Quantity
      val_sold = any_of(c("b6q13"))  # Value
    )

  # 3. SAFETY CHECK
  if(!"qty_sold" %in% names(df_renamed)) stop("Error: Qty column missing (b6q12)")
  if(!"val_sold" %in% names(df_renamed)) stop("Error: Value column missing (b6q13)")
  
  df_clean <- df_renamed %>%
    # 4. CONVERT TYPES (Strip Labels)
    mutate(
      social_group = as.numeric(as.character(social_group)),
      agency       = as.numeric(as.character(agency)),
      qty_sold     = as.numeric(as.character(qty_sold)),
      val_sold     = as.numeric(as.character(val_sold))
    ) %>%
    
    # 5. FILTER VALID SALES
    filter(qty_sold > 0, val_sold > 0) %>%
    
    # 6. CALCULATE PRICES
    mutate(
      unit_price = val_sold / qty_sold,
      log_unit_price = log(unit_price)
    ) %>%
    
    # 7. CASTE CATEGORIES
    mutate(
      caste_cat = case_when(
        social_group == 1 ~ "ST",
        social_group == 2 ~ "SC",
        social_group == 3 ~ "OBC",
        TRUE ~ "General"
      )
    ) %>%
    
    # 8. DEFINE AGENCY (Correct NSS Codes)
    # 1 = Local Private Trader (Predatory)
    # 2 = Mandi (Regulated)
    mutate(
      sold_to_trader = ifelse(agency == 1, 1, 0),
      sold_to_mandi  = ifelse(agency == 2, 1, 0)
    ) %>%
    
    # 9. OUTLIER REMOVAL
    group_by(crop_code) %>%
    mutate(
      p01 = quantile(unit_price, 0.01, na.rm=TRUE),
      p99 = quantile(unit_price, 0.99, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    filter(unit_price >= p01 & unit_price <= p99)

  cat(paste("Success! Final dataset ready with", nrow(df_clean), "records.\n"))
  return(df_clean)
}

# Run the function
df_final <- process_data(df_raw)
