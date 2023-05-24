library(rjson)
library(ggplot2)
library(dplyr)
library(data.table)
library(e1071)

dt <- data.table(semesterName = character(),
                 gpas = vector(),
                 n = numeric(),
                 mean = numeric(),
                 sd = numeric(),
                 median = numeric(),
                 trimmedmean = numeric(),
                 mad = numeric(),
                 min = numeric(),
                 max = numeric(),
                 firstQ = numeric(),
                 thirdQ = numeric(),
                 nas = numeric(),
                 skewness = numeric(),
                 kurtosis = numeric(),
                 se = numeric())

for (year in 2015:2021) {
  if (year == 2020) next
  for (semester in 1:2) {
    semesterName <- paste0(year, semester)
    dataset <- fromJSON(file = paste0(paste0("offerings/", semesterName), "sections.json"))
    
    morning_gpas <- list()
    non_morning_gpas <- list()
    
    
    for (i in seq_along(dataset)) {
      if (any(grepl("^\\w{3}_0830$", dataset[[i]]$schedule))) {
        morning_gpas <- c(morning_gpas, dataset[[i]]$gpa)
      } else {
        non_morning_gpas <- c(non_morning_gpas, dataset[[i]]$gpa)
      }
    }
    
    morning_gpas_2 <- unlist(morning_gpas)
    non_morning_gpas_2 <- unlist(non_morning_gpas)
    
    # Calculate descriptive statistics
    n_morning <- length(morning_gpas_2)
    mean_value_morning <- mean(morning_gpas_2)
    sd_value_morning <- sd(morning_gpas_2)
    median_value_morning <- median(morning_gpas_2)
    trimmed_mean_morning <- mean(morning_gpas_2, trim = 0.1)
    mad_value_morning <- mad(morning_gpas_2)
    min_value_morning <- min(morning_gpas_2)
    max_value_morning <- max(morning_gpas_2)
    first_quartile_morning <- quantile(morning_gpas_2, 0.25)
    third_quartile_morning <- quantile(morning_gpas_2, 0.75)
    n_missing_morning <- sum(is.na(morning_gpas_2))
    skewness_value_morning <- skewness(morning_gpas_2)
    kurtosis_value_morning <- kurtosis(morning_gpas_2)
    se_value_morning <- sd_value_morning / sqrt(length(morning_gpas_2))
    
    # Calculate descriptive statistics
    n_non_morning <- length(non_morning_gpas_2)
    mean_value_non_morning <- mean(non_morning_gpas_2)
    sd_value_non_morning <- sd(non_morning_gpas_2)
    median_value_non_morning <- median(non_morning_gpas_2)
    trimmed_mean_non_morning <- mean(non_morning_gpas_2, trim = 0.1)
    mad_value_non_morning <- mad(non_morning_gpas_2)
    min_value_non_morning <- min(non_morning_gpas_2)
    max_value_non_morning <- max(non_morning_gpas_2)
    first_quartile_non_morning <- quantile(non_morning_gpas_2, 0.25)
    third_quartile_non_morning <- quantile(non_morning_gpas_2, 0.75)
    n_missing_non_morning <- sum(is.na(non_morning_gpas_2))
    skewness_value_non_morning <- skewness(non_morning_gpas_2)
    kurtosis_value_non_morning <- kurtosis(non_morning_gpas_2)
    se_value_non_morning <- sd_value_non_morning / sqrt(length(non_morning_gpas_2))
    
    dt <- rbindlist(list(dt, list(semesterName = paste0(sub("(\\d{4})2", "\\1 Spring", sub("(\\d{4})1", "\\1 Fall", semesterName)), " Morning Classes"),
                                  gpas = list(morning_gpas),
                                  n = n_morning,
                                  mean = mean_value_morning,
                                  sd = sd_value_morning,
                                  median = median_value_morning,
                                  trimmedmean = trimmed_mean_morning,
                                  mad = mad_value_morning,
                                  min = min_value_morning,
                                  max = max_value_morning,
                                  firstQ = first_quartile_morning,
                                  thirdQ = third_quartile_morning,
                                  nas = n_missing_morning,
                                  skewness = skewness_value_morning,
                                  kurtosis = kurtosis_value_morning,
                                  se = se_value_morning)), fill = TRUE)
    
    dt <- rbindlist(list(dt, list(semesterName = paste0(sub("(\\d{4})2", "\\1 Spring", sub("(\\d{4})1", "\\1 Fall", semesterName)), " Non-Morning Classes"), gpas = list(non_morning_gpas),
                                  n = n_non_morning,
                                  mean = mean_value_non_morning,
                                  sd = sd_value_non_morning,
                                  median = median_value_non_morning,
                                  trimmedmean = trimmed_mean_non_morning,
                                  mad = mad_value_non_morning,
                                  min = min_value_non_morning,
                                  max = max_value_non_morning,
                                  firstQ = first_quartile_non_morning,
                                  thirdQ = third_quartile_non_morning,
                                  nas = n_missing_non_morning,
                                  skewness = skewness_value_non_morning,
                                  kurtosis = kurtosis_value_non_morning,
                                  se = se_value_non_morning)), fill = TRUE)
  }
}

dt_copy <- dt
dt_copy$gpas <- NULL

write.csv(dt_copy, "descriptive_statistics.csv", row.names = TRUE)

