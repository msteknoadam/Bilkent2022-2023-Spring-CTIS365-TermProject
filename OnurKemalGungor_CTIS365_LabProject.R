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
    
    local_df <- local_df %>%
      group_by(courseCode, gpa) %>%
      summarize(schedule = list(schedule))
    
    semesterReadable <- paste0(year, "-", year + 1, ifelse(semester == 1, " Fall", " Spring"))
    
    local_df$semester <- semesterReadable
    
    local_df$courseCode <- str_replace_all(local_df$courseCode, "_", "-")
    
    local_df$course <- str_replace_all(local_df$courseCode, "-\\d+", "")
    
    local_df$dept <- str_replace_all(local_df$course, "\\d+", "")
    
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

# Minimum number of sample count
sample_count <- 6

# Loop until finding at least sample_count number of samples
repeat {
  sample_semester_and_depts <- df[df$semester == sample(df$semester, 1) &
                                    df$dept %in% sample(unique(df$dept), 2), ]
  
  uniq_schedules <- data.frame(
    schedule = I(unique(sample_semester_and_depts$schedule)),
    matchCount = numeric(length(unique(sample_semester_and_depts$schedule))))
  
  for(i in 1:nrow(uniq_schedules)) {
    dept1 <- sample_semester_and_depts[1,]$dept
    selected_schedule <- sample_semester_and_depts[1,]$schedule
    
    dup_schedules_dept1 <- sample_semester_and_depts[sample_semester_and_depts$dept == dept1 & mapply(setequal, sample_semester_and_depts$schedule, selected_schedule), ]
    dup_schedules_dept2 <- sample_semester_and_depts[sample_semester_and_depts$dept != dept1 & mapply(setequal, sample_semester_and_depts$schedule, selected_schedule), ]
    
    min_sample_count <- min(nrow(dup_schedules_dept1), nrow(dup_schedules_dept2))
    
    uniq_schedules[i, ]$matchCount <- min_sample_count * 2
  }
  
  max_match_row_idx <- which.max(uniq_schedules$matchCount)
  
  dept1 <- sample_semester_and_depts[1,]$dept
  selected_schedule <- uniq_schedules[max_match_row_idx,]$schedule
  
  dup_schedules_dept1 <- sample_semester_and_depts[sample_semester_and_depts$dept == dept1 & mapply(setequal, sample_semester_and_depts$schedule, selected_schedule), ]
  dup_schedules_dept2 <- sample_semester_and_depts[sample_semester_and_depts$dept != dept1 & mapply(setequal, sample_semester_and_depts$schedule, selected_schedule), ]
  
  min_sample_count <- min(nrow(dup_schedules_dept1), nrow(dup_schedules_dept2))
  
  dup_schedules_dept1 <- dup_schedules_dept1[1:min_sample_count, ] %>% arrange(gpa)
  dup_schedules_dept2 <- dup_schedules_dept2[1:min_sample_count, ] %>% arrange(gpa)
  
  if(nrow(dup_schedules_dept1) + nrow(dup_schedules_dept2) >= sample_count) {
    break
  }
}

# Graph 1 - Different Courses With Same Schedule From 2 Departments
combined <- data.frame(
  courseNum = as.factor(paste0("Course ", seq(nrow(dup_schedules_dept1)))),
  gpa1 = dup_schedules_dept1$gpa,
  gpa2 = dup_schedules_dept2$gpa,
  dept1 = dup_schedules_dept1$dept,
  dept2 = dup_schedules_dept2$dept
)

# Find difference between max and min values here so we can create a dynamically adjusted graph limit
diff <- max(combined$gpa1, combined$gpa2) - min(combined$gpa1, combined$gpa2)

ggplot(combined, aes(x = courseNum)) +
  geom_line(aes(y = gpa1, color = paste("GPA of course of", dept1, "dept"), group = 1)) +
  geom_line(aes(y = gpa2, color = paste("GPA of course of", dept2, "dept"), group = 1)) +
  labs(x = "Course", y = "GPA", color = "Variable") +
  scale_color_manual(values = c("red", "blue")) +
  coord_cartesian(ylim = c(min(combined$gpa1, combined$gpa2) - (diff / 4), max(combined$gpa1, combined$gpa2) + (diff / 4))) +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.5), linetype = "dotted"),
        panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.5), linetype = "dotted"),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
  ggtitle(paste("GPAs of", nrow(combined) ,"Different Courses With Same Schedule From 2 Departments"))

# Graph 2 - Mean GPA of Morning Classes vs Non-Morning Classes of 2 Random Departments
# Find mean GPA of Morning and Non-Morning classes of 2 random departments and make sure their semester data are same
repeat {
  selected_depts <- sample(unique(df$dept), 2)
  
  morning_df <- df %>%
    filter(dept %in% selected_depts, grepl("0830", schedule)) %>%
    group_by(dept, semester) %>%
    summarise(morning_mean_gpa = mean(gpa))
  
  non_morning_df <- df %>%
    filter(dept %in% selected_depts, !grepl("0830", schedule)) %>%
    group_by(dept, semester) %>%
    summarise(non_morning_mean_gpa = mean(gpa))
  
  dept1 <- morning_df$dept[1]
  
  if(nrow(morning_df[morning_df$dept == dept1, ]) == nrow(morning_df[morning_df$dept != dept1, ]) &&
     all(morning_df[morning_df$dept == dept1, ]$semester == morning_df[morning_df$dept != dept1, ]$semester)) {
    break
  }
}

combined <- inner_join(morning_df, non_morning_df, by = c("dept", "semester"))

# Find difference between max and min values here so we can create a dynamically adjusted graph limit
diff <- max(combined$morning_mean_gpa, combined$non_morning_mean_gpa) - min(combined$morning_mean_gpa, combined$non_morning_mean_gpa)

ggplot(combined, aes(x = semester, y = morning_mean_gpa, color = dept, group = dept)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y = non_morning_mean_gpa), linetype = "dashed") +
  geom_point(aes(y = non_morning_mean_gpa), shape = 16) +
  labs(x = "Semester", y = "Mean GPA", title = "Mean GPA of Morning Classes vs Non-Morning Classes of Each Department", subtitle = "Non-Dashed = Morning Classes / Dashed = Non-Morning Classes", color = "Department") +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.5), linetype = "dotted"),
        panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.5), linetype = "dotted"),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top")
