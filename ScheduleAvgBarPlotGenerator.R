# Install packages if needed
install.packages("ggplot2")
install.packages("rjson")
install.packages("dplyr")
install.packages("stringr")
# Load libraries
library(rjson)
library(ggplot2)
library(dplyr)
library(stringr)

# Create empty data frame to populate later on
df <- data.frame()

# Populate the data frame by loading JSON files of past offerings files
for (year in 2015:2021) {
  if (year == 2020) next
  for (semester in 1:2) {
    semesterName <- paste0(year, semester)
    dataset <- fromJSON(file = paste0(paste0("offerings/", semesterName), "sections.json"))
    
    local_df <- bind_rows(dataset)
    
    df <- rbind(df, local_df)
  }
}

# Check for NA values
has_na <- any(is.na(df))

if (has_na) {
  # Drop rows with NA values
  df <- na.omit(df)
} else {
  print("Data set doesn't have any NA values!")
}

# Convert schedule names into human readable names
df$schedule <- str_replace_all(df$schedule, "^\\w{3}_", "")
df$schedule <- paste(substr(df$schedule, 1, 2), ":", substr(df$schedule, 3, 4), sep = "")

schedule_avg <- aggregate(gpa ~ schedule, data = df, FUN = mean)

ggplot(schedule_avg, aes(x = schedule, y = gpa)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(2.215, 2.28)) +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.5), linetype = "dotted"),
        panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.5), linetype = "dotted"),
        axis.line = element_line(color = "black")) +
  xlab("Start of Lecture") +
  ylab("GPA")

  