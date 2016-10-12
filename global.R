
rm(list = ls())

dir <- dir("fits/")

names_target <- gsub("fit_", "", dir)
names_target <- gsub("_step.RData", "", names_target)








