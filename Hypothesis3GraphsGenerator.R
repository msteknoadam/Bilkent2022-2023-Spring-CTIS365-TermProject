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

sample_count <- 6

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

combined <- data.frame(
  courseNum = as.factor(paste0("Course ", seq(nrow(dup_schedules_dept1)))),
  gpa1 = dup_schedules_dept1$gpa,
  gpa2 = dup_schedules_dept2$gpa,
  dept1 = dup_schedules_dept1$dept,
  dept2 = dup_schedules_dept2$dept
)

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
  ggtitle("Hypothesis 3")