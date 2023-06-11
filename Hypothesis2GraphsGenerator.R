install.packages("ggplot2")
install.packages("rjson")
install.packages("dplyr")
install.packages("stringr")
library(rjson)
library(ggplot2)
library(dplyr)
library(stringr)

df <- data.frame()

for (year in 2015:2021) {
  if (year == 2020) next
  for (semester in 1:2) {
    semesterName <- paste0(year, semester)
    dataset <- fromJSON(file = paste0(paste0("offerings/", semesterName), "sections.json"))
    
    local_df <- bind_rows(dataset)
    
    local_df <- local_df %>%
      group_by(courseCode, gpa) %>%
      summarize(schedule = list(str_replace_all(schedule, "^\\w{3}_", "")))
    
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

sample_count <- 8

repeat {
  sample_semester_and_dept_and_course <- df[df$semester == sample(df$semester, 1) &
                                            df$dept == sample(df$dept, 1) &
                                            df$course == sample(df$course, 1), ]
  
  dup_schedules <- sample_semester_and_dept_and_course[duplicated(sample_semester_and_dept_and_course$schedule), ]
  
  if(nrow(dup_schedules) >= sample_count) {
    break
  }
}

schedule_grouped_dups <- dup_schedules %>%
  group_by(schedule) %>%
  summarise(courseCodes = list(courseCode),
            gpas = list(gpa),
            courses = list(course),
            depts = list(dept))

for(i in 1:nrow(schedule_grouped_dups)) {
  row <- schedule_grouped_dups[i, ]
  courses_split <- data.frame(
    courseCode = row$courseCodes,
    gpa = row$gpas
  )
  colnames(courses_split) <- c("courseCode", "gpa")
  diff <- max(courses_split$gpa) - min(courses_split$gpa)
  print(ggplot(courses_split, aes(x = courseCode, y = gpa)) +
          geom_bar(stat = "identity") +
          coord_cartesian(ylim = c(min(courses_split$gpa) - (diff / 2), max(courses_split$gpa) + (diff / 2))) +
          theme(panel.background = element_rect(fill = "transparent"),
                plot.background = element_rect(fill = "transparent"),
                panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.5), linetype = "dotted"),
                panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.5), linetype = "dotted"),
                axis.line = element_line(color = "black"),
                plot.title = element_text(hjust = 0.5)) +
          xlab("Course") +
          ylab("GPA") +
          ggtitle(paste("Hypothesis 2 - Ex. ", i)))
  invisible(readline(prompt="Press [enter] to see next graph"))
}
