library(dplyr)
library(kableExtra)

# Data frame formatted with one row for question number, additional rows for each
# response, question accuracy represented as '0' - incorrect or '1' - correct
dat <- read.csv("/Users/kta5166/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Classes/BBH 440 - Epidemiology/2026 SP/Exams/Exam 2 Scores.csv")

q <- names(dat)

n <- nrow(dat)
cut <- round(0.27 * n)

dat_sorted <- dat %>% arrange(desc(total))

top <- dat_sorted[1:cut, ]
bottom <- dat_sorted[(n - cut + 1):n, ]

data.frame(
  question = paste0("q", 1:44),
  disc = sapply(names(dat)[grepl("^q", names(dat))], function(q) {
    mean(top[[q]]) - mean(bottom[[q]])
  })
) %>% 
  mutate(class = case_when(disc < 0 ~ "poor",
                           disc < 0.2 ~ "weak",
                           disc < 0.4 ~ "acceptable",
                           disc >= 0.4 ~ "excellent")) %>%
  kbl()
