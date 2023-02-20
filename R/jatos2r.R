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
                  filename = c("dat1.rds", "dat1.csv",
                               "backGr.rds", "backGr.csv"),
                  filterCol = NULL,
                  filterID = NULL,
                  getCol = NULL,
                  output_raw = FALSE,
                  output_clean = FALSE,
                  output_background = FALSE)

{

library(jsonlite)
library(dplyr)
library(tidyr)

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

# Get worker IDs
workers = unique(df$workerID)
workers = workers[!is.na(workers)]
workers = workers[workers != "0"]

# Get col names
columnID = 'trialName'

# Get trial Names
trial_name = unique(df[[columnID[1]]])
trial_name = trial_name[!is.na(trial_name)]

# Check if data is 1 per workerID
singleDat = NULL
workerDF = dplyr::filter(df, workerID == workers[1])
for (i in 1:length(trial_name)) {
  testForSingle = length(dplyr::filter(workerDF, trialName == trial_name[i])[,1])
  if (testForSingle == 1) {
    singleDat = c(singleDat, 1)
  } else {singleDat = c(singleDat, 0)
    }
}

# DF for repeating responses
trial_name_repeat = trial_name[singleDat == 0]

# DF for single responses
trial_name_single = trial_name[singleDat == 1]

# Get new DF
col1 = dplyr::filter(df, trialName == trial_name_repeat[1])
col1 <- col1[, !sapply(col1, function(x) all(is.na(x) | is.null(x) | (is.list(x) && length(x) == 0)))]

# Unlist config
col1 = col1[,colnames(col1) != 'config']
newDF = data.frame(col1)
col1 = dplyr::filter(df, plugin == 'store_config_data')
col1 = col1[rep(row.names(col1), each = length(newDF[,1])/length(workers)), ]
col1 = col1$config
col1 = as.data.frame(unnest(col1, all_of(config)))
col1 <- col1[, !sapply(col1, function(x) all(is.na(x) | is.null(x) | (is.list(x) && length(x) == 0)))]
names(col1) = paste("config:", names(col1), sep = "")
newDF = cbind(newDF, col1)

# Set correct class
for (i in 1:length(newDF[1,])) {
  testDF <- newDF[,i]
  test = grepl("^[0-9]+$", testDF)
  if (all(test == TRUE)) newDF[,i] = as.integer(newDF[,i])
  if (all(test == FALSE)) newDF[,i] = as.character(newDF[,i])
}

# Save new df
if (output_clean == TRUE) {
write.csv(newDF, file = filename[2], row.names = FALSE)
saveRDS(newDF, filename[1])
}

return(newDF)
}
