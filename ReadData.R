# setwd("~/Desktop/AACT201509_pipe_delimited_txt")
data <- read.table(file="nct_aliases.txt",
                   header=TRUE,
                   sep="|",
                   na.strings="",
                   colClasses=c("integer","character","character"),
                   comment.char = "",
                   quote="",
                   fill=FALSE,
                   nrows=2600)
View(data)
