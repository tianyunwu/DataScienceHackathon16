# Delete embeded soft returns
fileTransName = paste(unlist(strsplit(fileName,split='.',fixed = TRUE))[1],"_nolf.txt",sep="");
command = paste("tr -d '\012' <",fileName,"> ",fileTransName,sep="")
fileTrans = try(system(command))
# Count the number of lines in the file
command = paste("wc -l < ~/Desktop/AACT201509_pipe_delimited_txt/",fileName,sep="")
lineNumber = try(system(command))
# try to read the first 100 lines in the table
d2 <- read.table(file=fileTransName,
                 header=TRUE,
                 sep="|",
                 na.strings="",
                 stringsAsFactors = FALSE,
                 comment.char = "",
                 quote="",
                 fill=FALSE,
                 nrows=100);
# Estimate the memory usage
sz <- object.size(d2)
print(sz*160033/1000,units = "Mb")
# Extract the column structures
classes <- sapply(d2,class)
# Apply the column structure on reading the entire the data file
d2 <- read.table(file=fileTransName,
                 header=TRUE,
                 sep="|",
                 na.strings="",
                 colClasses = classes,
                 comment.char = "",
                 quote="",
                 fill=FALSE,
                 nrows=lineNumber);