compute_AI <- function(data = sample_data,
                       left_hemisphere = "lh",
                       right_hemisphere = "rh",
                       start = 2,
                       end = 15) {
  # Extract the column names for lh and rh




  lh_col <- colnames(data)[start + 1]
  rh_col <- colnames(data)[start + 2]

  # Pivot the data from wide to long format
  data_long <- reshape(
    data,
    varying = list(c(lh_col, rh_col)),
    v.names = c("AI"),
    times = c("lh", "rh"),
    timevar = "region",
    direction = "long"
  )

  # Calculate AI
  data_long$AI <- 100 * (data_long$lh - data_long$rh) / (data_long$lh + data_long$rh)

  # Remove lh and rh columns
  data_long <- data_long[, !(names(data_long) %in% c(lh_col, rh_col))]

  # Pivot the data back to wide format
  data_wide <- reshape(
    data_long,
    idvar = "ID",
    timevar = "region",
    v.names = "AI",
    direction = "wide"
  )

  return(data_wide)
}



## second choice

compute_AI <- function(data = sample_data,
                       left_hemisphere = "lh",
                       right_hemisphere = "rh",
                       ID="ID",
                       start = 2,
                       end = 15) {

  colnames(data)

  data2 <- reshape(data,
                   idvar = "ID",
                   varying = colnames[,start:end],
                   v.names = "AI",
                   timevar = "region",
                   times = substring(names(data)[-1], 4),
                   direction = "long")

  # Calculate the AI column
  data2$AI <- 100 * (data2$lh - data2$rh) / (data2$lh + data2$rh)

  # Clean up variable names

  data_long2 <- reshape(
    data,
    varying = data[,start:end],
    v.names = c("AI"),
    times = c(left_hemisphere, right_hemisphere),
    timevar = "region",
    direction = "long"
  )


  # Calculate the AI for each row
  for (i in seq_len(nrow(data))) {
    lh <- sum(data_mat[i, lh_cols])
    rh <- sum(data_mat[i, rh_cols])
    data[i, paste0("AI_", lh_cols)] <- 100 * (lh - rh) / (lh + rh)
  }

  # Remove the original "lh" and "rh" columns
  data <- data[, -c((start + 1):ncol(data))]

  return(data)
}
