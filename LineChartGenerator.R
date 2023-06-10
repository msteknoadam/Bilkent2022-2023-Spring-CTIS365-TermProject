install.packages("rjson")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")
library(rjson)
library(ggplot2)
library(dplyr)
library(stringr)

means <- data.frame(semesterName = character(), morning_mean_gpa = numeric(), non_morning_mean = numeric())

for (year in 2015:2021) {
  if (year == 2020) next
  for (semester in 1:2) {
    semesterName <- paste0(year, semester)
    dataset <- fromJSON(file = paste0(paste0("offerings/", semesterName), "sections.json"))
    
    df <- bind_rows(dataset)
    
    df <- df %>%
      group_by(courseCode, gpa) %>%
      summarize(schedule = list(schedule))
    
    morning_classes <- df %>%
      filter(sapply(schedule, function(x) any(str_detect(x, "^\\w{3}_0830$")))) %>%
      select(courseCode, gpa)
    
    non_morning_classes <- df %>%
      filter(!sapply(schedule, function(x) any(str_detect(x, "^\\w{3}_0830$")))) %>%
      select(courseCode, gpa)
    
    # Calculate mean GPA for morning classes
    morning_mean_gpa <- mean(morning_classes$gpa)
    
    # Calculate mean GPA for non-morning classes
    non_morning_mean_gpa <- mean(non_morning_classes$gpa)
    
    means <- rbind(means, data.frame(semesterName = sub("(\\d{4})2", "\\1 Spring", sub("(\\d{4})1", "\\1 Fall", semesterName)), morning_mean_gpa = morning_mean_gpa, non_morning_mean = non_morning_mean_gpa))
  }
}

# Create the line chart using ggplot2
ggplot(means, aes(x = semesterName)) +
  geom_line(aes(y = morning_mean_gpa, color = "Morning Mean GPA", group = 1)) +
  geom_line(aes(y = non_morning_mean, color = "Non-Morning Mean GPA", group = 1)) +
  labs(x = "Semester", y = "Mean GPA", color = "Variable") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()
