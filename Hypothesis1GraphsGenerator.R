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
      summarize(schedule = list(schedule))
    
    semesterReadable <- paste0(year, "-", year + 1, ifelse(semester == 1, " Fall", " Spring"))
    
    local_df$semester <- semesterReadable
    
    local_df$courseCode <- str_replace_all(local_df$courseCode, "_", "-")
    
    local_df$course <- str_replace_all(local_df$courseCode, "-\\d+", "")
    
    local_df$dept <- str_replace_all(local_df$course, "\\d+", "")
    
    df <- rbind(df, local_df)
  }
}

# Filter courses with at least one lecture starting at 0830
morning_classes <- df %>%
  filter(grepl("0830", schedule))

# Remove duplicate courses, leave only one instance of the same course
morning_classes <- morning_classes[!duplicated(morning_classes[c("course", "semester")]), ]

# Filter courses with no lectures starting at 0830
non_morning_classes <- df %>%
  filter(!grepl("0830", schedule))

# Remove duplicate courses, leave only one instance of the same course
non_morning_classes <- non_morning_classes[!duplicated(non_morning_classes[c("course", "semester")]), ]

# Find the matching courses (morning and non-morning class for same course)
matching_classes <- merge(x = morning_classes, y = non_morning_classes, by = c("course", "semester", "dept")) %>%
  arrange(semester, course)

sample_count <- 4

repeat {
  sample_semester_and_dept <- matching_classes[matching_classes$semester == sample(matching_classes$semester, 1) &
                                                 matching_classes$dept == sample(matching_classes$dept, 1), ]
  
  if(nrow(sample_semester_and_dept) >= sample_count) {
    break
  }
}

sample_courses <- sample_semester_and_dept[sample(nrow(sample_semester_and_dept), 4), ]

for(i in 1:nrow(sample_courses)) {
  course <- sample_courses[i,]
  courseCodes <- c(paste(course$courseCode.x, course$semester, "(Morning)"), paste(course$courseCode.y, course$semester, "(Non-Morning)"))
  courses_split <- data.frame(
    courseCode = as.factor(courseCodes),
    gpa = c(course$gpa.x, course$gpa.y)
  )
  diff <- max(courses_split$gpa) - min(courses_split$gpa)
  print(ggplot(courses_split, aes(x = courseCode, y = gpa)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(limits = courseCodes) +
    coord_cartesian(ylim = c(min(courses_split$gpa) - (diff / 2), max(courses_split$gpa) + (diff / 2))) +
    theme(panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          panel.grid.major = element_line(color = rgb(0, 0, 0, alpha = 0.5), linetype = "dotted"),
          panel.grid.minor = element_line(color = rgb(0, 0, 0, alpha = 0.5), linetype = "dotted"),
          axis.line = element_line(color = "black"),
          plot.title = element_text(hjust = 0.5)) +
    xlab("Course") +
    ylab("GPA") +
    ggtitle(paste("Hypothesis 1 - Ex. ", i)))
  invisible(readline(prompt="Press [enter] to see next graph"))
}
