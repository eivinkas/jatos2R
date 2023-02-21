#' Convert from JATOS.txt to .rds and .csv.
#' @param data String: The PATH of the .txt data file
#' @param filename Vector/character
#' @param trial_name Vector/character
#' @param col_name Vector/character
#' @param type Vector/character
#' @return raw_data.rds, raw_data.csv, data.rds, data.csv
#'
#' @export

jatos2r = function(
    data = "dat.txt",
    filename = c("dat1.rds",
                 "dat1.csv"),
    from_col = c('trialName',
                 'plugin'),
    trial_name = list(c('all'),
                   'store_config_data'),
    col_name = list(c('all'),
                    'all'),
    allow_duplicates = c('rt', 'time_elapsed', 'response'),
    output_raw = TRUE,
    output_clean = TRUE)

{

  library(jsonlite)
  library(dplyr)

  # Make raw data frame
  text_data <- readLines(data, warn = FALSE)
  text_data <- paste0(text_data, collapse = "")
  new_data <- gsub("][", ",", text_data, fixed = TRUE)
  json_data <- fromJSON(new_data)
  df <- as.data.frame(json_data)

  # Save raw data
  if (output_raw == TRUE){
    write.csv(df, file = paste("raw_", filename[2], sep = ""), row.names = FALSE)
    saveRDS(df, paste("raw_", filename[1], sep = ""))
  }

  # Record every column that is added
  colRec = NULL

  # Make vector of all elements
  if (trial_name[[1]][1] == 'all') {
    trial_name[[1]] = unique(df[,names(df) == from_col[1]])[!is.na(unique(df[,names(df) == from_col[1]]))]
  }

  # Make df for first element
  if (col_name[[1]][1] != 'all') {
    col1 = dplyr::select(col1, col_name[[1]])
  }
  col1 = dplyr::filter(df, !!sym(from_col[1]) == trial_name[[1]][1])
  col1 = col1[,(names(col1) != 'config')]

  newDF = as.data.frame(col1)
  newDF <- newDF[, !sapply(newDF, function(x) all(is.na(x) | is.null(x) | (is.list(x) && length(x) == 0)))]
  colRec = c(colRec, names(newDF))
  for (k in 1:length(names(newDF))) names(newDF)[k] = paste(trial_name[[1]][1], ':', names(newDF)[k], sep="")


  # Add data for all the other elements
  if (length(trial_name[[1]]) > 1) {
    for (i in 2:length(trial_name[[1]])) {
      col1 = dplyr::filter(df, !!sym(from_col[1]) == trial_name[[1]][i])
      col1 = col1[,(names(col1) != 'config')]
      dummyDF = as.data.frame(col1)
      dummyDF <- dummyDF[, !sapply(dummyDF, function(x) all(is.na(x) | is.null(x) | (is.list(x) && length(x) == 0)))]

      # check if column names are duplicated
      delDuplicates = NULL
      for (k in 1:length(names(dummyDF))) {
        if (names(dummyDF)[k] %in% colRec) {
          if ((names(dummyDF)[k] %in% allow_duplicates) == FALSE) delDuplicates = c(delDuplicates, k)
        }
      }
      for (k in 1:length(names(dummyDF))) names(dummyDF)[k] = paste(trial_name[[1]][i], ':', names(dummyDF)[k], sep="")

      # Get columns
      if (length(delDuplicates > 0)) dummyDF = dummyDF[,-delDuplicates]
      cnames = names(dummyDF)

      # Check length of new data
      len1 = length(dummyDF[,1])

      if (len1 < length(newDF[,1])) {
        dummyDF = dummyDF[rep(seq_len(nrow(dummyDF)), each = length(newDF[,1])/length(dummyDF[,1])), ]
      }

      if (len1 > length(newDF[,1])) {
        newDF = newDF[rep(seq_len(nrow(newDF)), each = length(dummyDF[,1])/length(newDF[,1])), ]
      }

      if (length(dummyDF[,1]) != length(newDF[,1])) {
        return ("Stopped : Column lengths do not match.")
      }

      dummyDF = as.data.frame(dummyDF)
      names(dummyDF) = cnames

      newDF = cbind(newDF, dummyDF)

      }
  }

  if (length(from_col) > 1){
    for (j in 2:length(from_col)) {

      if (trial_name[[j]][1] == 'all') {
        trial_name[[j]] = unique(df[,names(df) == from_col[j]])[!is.na(unique(df[,names(df) == from_col[j]]))]
      }

      for (i in 1:length(trial_name[[j]])) {
        col1 = dplyr::filter(df, !!sym(from_col[j]) == trial_name[[j]][i])
        if (trial_name[[j]] != 'store_config_data') col1 = col1[,(names(col1) != 'config')]
        dummyDF = as.data.frame(col1)
        dummyDF <- dummyDF[, !sapply(dummyDF, function(x) all(is.na(x) | is.null(x) | (is.list(x) && length(x) == 0)))]

        # check if column names are duplicated
        delDuplicates = NULL
        for (k in 1:length(names(dummyDF))) {
          if (names(dummyDF)[k] %in% colRec) {
            if ((names(dummyDF)[k] %in% allow_duplicates) == FALSE) delDuplicates = c(delDuplicates, k)
          }
        }
        for (k in 1:length(names(dummyDF))) names(dummyDF)[k] = paste(trial_name[[j]][i], ':', names(dummyDF)[k], sep="")


        # Get columns
        if (length(delDuplicates > 0)) dummyDF = dummyDF[,-delDuplicates]
        cnames = names(dummyDF)

        # Check length of new data
        len1 = length(dummyDF[,1])

        if (len1 < length(newDF[,1])) {
          dummyDF = dummyDF[rep(seq_len(nrow(dummyDF)), each = length(newDF[,1])/length(dummyDF[,1])), ]
        }

        if (len1 > length(newDF[,1])) {
          newDF = newDF[rep(seq_len(nrow(newDF)), each = length(dummyDF[,1])/length(newDF[,1])), ]
        }

        if (length(newDF[,1]) !=  length(dummyDF[,1])) {
          return ("Stopped : Column lengths do not match.")
        }

        dummyDF = as.data.frame(dummyDF)
        names(dummyDF) = cnames

        newDF = cbind(newDF, dummyDF)

      }
    }
  }

  # Fix class
  for (i in 1:length(newDF[1,])) {
    test1 = grepl("^[0-9]+$", newDF[,i])
    if (all(test1) == TRUE) newDF[,i] = as.integer(newDF[,i])
  }

  # Save new df
  if (output_clean == TRUE) {
    write.csv(newDF, file = filename[2], row.names = FALSE)
    saveRDS(newDF, filename[1])
  }

  return(newDF)

}
