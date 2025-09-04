library(dplyr)
data1 <- read.table("Partial_report_Sub1_S1.csv", header=TRUE, sep=",")
data1 <- data1 %>% filter(phase == "experiment")
data1 <- data1[, -which(names(data1) == "matrix")]
data1 <- data1[, -which(names(data1) == "trial")]

data1
