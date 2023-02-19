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

                  output_raw = TRUE,

                  output_clean = TRUE,

                  output_background = TRUE,

                  filename = c("dat1.rds", "dat1.csv"),

                  trial_name = c('dotMaskTrial',
                                 'dotMaskTrial',
                                 'dotMaskTrial',
                                 'dotMaskTrial',
                                 'textResponse',
                                 'textResponse',
                                 'multipleChoice'),

                   col_name= c('workerID',
                               'numerosity',
                               'imageNumber',
                               'numGroups',
                               'response',
                               'rt',
                               'response'),

                  type = c("character",
                           "integer",
                           "character",
                           "integer",
                           "integer",
                           "integer",
                           "character"))

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

# Get new DF
col1 = dplyr::filter(df, trialName == trial_name[1])
col1 = col1[,colnames(col1) == col_name[1]]
col1 = unlist(col1)
newDF = data.frame(col1)

for (i in 2:length(col_name)) {
  newCol = col1 = dplyr::filter(df, trialName == trial_name[i])
  newCol = col1[,colnames(newCol) == col_name[i]]
  newCol = unlist(newCol)
  newDF = cbind(newDF, newCol)
}
colnames(newDF) = col_name

# Set correct class
for (i in 1:length(newDF[,1])) {
  newDF[,i] = as(newDF[,i], type[i])
}

# Save new df
if (output_clean == TRUE) {
write.csv(newDF, file = filename[2], row.names = FALSE)
saveRDS(newDF, filename[1])
}

}
