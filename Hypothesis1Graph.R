data <- data.frame(
  c("CTIS480-1 2021-2022 Fall","CTIS480-2 2021-2022 Fall"),
  c(2.41, 2.68)
)

colnames(data) <- c("Course and Section", "Section GPA")

barplot(data$`Section GPA`,
        main = "Hypothesis 1",
        names.arg = data$`Course and Section`,
        ylim = c(0, 4),
        width = 0.5,
        yaxt = "n")

axis(side = 2, at = pretty(range(0, 4), n= 8))

