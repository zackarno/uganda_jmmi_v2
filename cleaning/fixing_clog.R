change_df_by_variables_and_ids <- function (data, change_variables, change_ids, data_id_column, 
                                                   new_values, change) 
{
  change_variables <- as.character(change_variables)
  change_ids <- as.character(change_ids)
  change <- as.logical(change)
  change[is.na(change)] <- TRUE
  data_ids <- data[[data_id_column]]
  data_ids <- as.character(data_ids)
  data_id_column <- make.names(data_id_column)
  names(data) <- make.names(names(data))
  change_variables <- make.names(change_variables)
  cl <- tibble::tibble(change_variables, change_ids, data_id_column, 
                       new_values, change)
  cl <- purrr::map(cl, function(x) {
    if (is.factor(x)) {
      as.character(x)
    }
    x
  }) %>% as_tibble
  non_unique_change_ids <- change_ids[change_ids %in% data_ids[duplicated(data_ids)]] %>% 
    unique
  if (length(non_unique_change_ids) != 0) {
    warning("some change_ids are not unique in the data ids - ignoring these.")
    cl <- cl[!(change_ids %in% non_unique_change_ids), ]
    change_ids <- change_ids[!(change_ids %in% non_unique_change_ids)]
  }
  unknown_ids <- (!(change_ids %in% data[[data_id_column]]))
  if (any(unknown_ids)) {
    warning(paste("ignoring", length(which(unknown_ids)), 
                  "cleaning log rows because ID not found in data"))
    cl <- cl[!unknown_ids, ]
    change_ids <- change_ids[!unknown_ids]
  }
  unknown_variables <- (!(cl$change_variables %in% names(data)))
  if (any(unknown_variables)) {
    warning(paste("ignoring", length(which(unknown_variables)), 
                  "cleaning log rows because variable name not found in data"))
    cl <- cl[!unknown_variables, ]
    change_ids <- change_ids[!unknown_variables]
  }
  cl_to_change <- cl[cl$change, ]
  change_ids <- cl_to_change$change_ids
  change_variables <- cl_to_change$change_variables
  new_values <- cl_to_change$new_values
  change <- cl_to_change$change
  rows_to_change <- match(change_ids, data_ids)
  cols_to_change <- match(change_variables, colnames(data))
  old_values <- purrr:::pmap(list(rows_to_change, cols_to_change), 
                             function(row, col) {
                               data[row, col]
                             }) %>% unlist
  change_value <- function(row, col, new) {
    data[row, col] <<- new
  }
  mapply(change_value, rows_to_change, cols_to_change, new_values)
  changelog <- tibble(change_variables, change_ids, old_values, 
                      new_values, change, data_id_column)
  attributes(data)$changelog <- rbind(attributes(data)$changelog, 
                                      changelog)
  class(data) <- c("clog_modified_data", class(data)) %>% 
    unique
  data
}


clog_clean <- function (df, cleaninglog) 
{
  df <- tibble::as_tibble(df)
  attributes(df)$raw_data <- df
  df <- change_df_by_variables_and_ids(data = df, change_variables = cleaninglog$variables, 
                                       change_ids = cleaninglog$ids, data_id_column = attributes(cleaninglog)$data_id_column_name, 
                                       new_values = cleaninglog$new_values, change = cleaninglog$change)
  class(df) <- c("clog_modified_data", class(df)) %>% 
    unique
  df
}
