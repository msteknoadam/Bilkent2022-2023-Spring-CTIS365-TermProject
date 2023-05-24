data <- data.frame(
  c("ECON103-1 2021-2022 Fall","ECON103-4 2021-2022 Fall"),
  c(2.16, 2.36)
)

colnames(data) <- c("Course and Section", "Section GPA")

barplot(data$`Section GPA`,
        main = "Hypothesis 2",
        names.arg = data$`Course and Section`,
        ylim = c(0, 4),
        width = 0.5,
        yaxt = "n")

axis(side = 2, at = pretty(range(0, 4), n= 8))

