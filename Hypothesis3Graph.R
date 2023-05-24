data <- data.frame(
  c("ECON103-4 2021-2022 Fall","CS121-3 2021-2022 Fall"),
  c(2.36, 2.77)
)

colnames(data) <- c("Course and Section", "Section GPA")

barplot(data$`Section GPA`,
        main = "Hypothesis 3",
        names.arg = data$`Course and Section`,
        ylim = c(0, 4),
        width = 0.5,
        yaxt = "n")

axis(side = 2, at = pretty(range(0, 4), n= 8))

