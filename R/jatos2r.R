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
                  trial_name = c('dotmaskTrial',
                                 'dotmaskTrial',
                                 'dotmaskTrial',
                                 'dotmaskTrial',
                                 'dotmaskTrial',
                                 'dotmaskTrial'),
                   col_name = c('workerID',
                               'numerosity',
                               'imageNumber',
                               'numGroups',
                               'response',
                               'rt'),
                   type = c("character",
                           "integer",
                           "character",
                           "integer",
                           "integer",
                           "integer"),
                  output_raw = TRUE,
                  output_clean = TRUE,
                  output_background = TRUE)

{

library(jsonlite)
library(dplyr)

# Make raw data frame
text_data <- readLines(data, warn = FALSE)
text_data <- paste0(text_data, collapse = "")
new_data <- gsub("][", ",", text_data, fixed = TRUE)
json_data <- fromJSON(new_data)
df <- as.data.frame(json_data)

# Make background data
if (output_background == TRUE) {
  backgrDF = data.frame(workerID = 0,
                        age = 0,
                        gender = 0,
                        os = 0,
                        browser = 0,
                        avg_frame_time = 0,
                        strategies = 0,
                        feedback = 0
                        )
  # Get worker IDs
  workers = unique(df$workerID)
  workers = workers[!is.na(workers)]
  workers = workers[workers != "0"]

  for (i in 1:length(workers)) {

    if (i>1) backgrDF = rbind(backgrDF, rep(0, 8))


    workerDF = dplyr::filter(df, workerID == workers[i])

    # get workerID
    backgrDF[i,1] = unlist(workers[i])

    # get age
    age1 = dplyr::filter(workerDF, trialName == "age")$response
    age1 = as.numeric(unlist(age1))
    backgrDF[i,2] = age1

    # get gender
    gender1 = dplyr::filter(workerDF, trialName == "gender")$response
    gender1 = unlist(gender1)
    backgrDF[i,3] = gender1

    # get OS
    os1 = dplyr::filter(workerDF, trialName == "browserCheck")$os
    os1 = unlist(os1)
    backgrDF[i,4] = os1

    # get browser
    browser1 = dplyr::filter(workerDF, trialName == "browserCheck")$browser
    browser1 = unlist(browser1)
    backgrDF[i,5] = browser1

    # get avg frame time
    frameTime1 = mean(workerDF$avg_frame_time, na.rm = TRUE)
    backgrDF[i,6] = frameTime1

    # get strategies
    strat1 = dplyr::filter(workerDF, trialName == "responseStrategies")$response
    strat1 = unlist(strat1)
    backgrDF[i,7] = strat1

    # get feedback
    feedback1 = dplyr::filter(workerDF, trialName == "feedback")$response
    feedback1 = unlist(feedback1)
    backgrDF[i,8] = feedback1

  }
  saveRDS(backgrDF, filename[3])
  write.csv(backgrDF, file = filename[4], row.names = FALSE)
}

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
for (i in 1:length(newDF[1,])) {
  newDF[,i] = as(newDF[,i], type[i])
}

# Save new df
if (output_clean == TRUE) {
write.csv(newDF, file = filename[2], row.names = FALSE)
saveRDS(newDF, filename[1])
}

}
