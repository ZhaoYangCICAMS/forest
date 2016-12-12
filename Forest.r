
library("sqldf")

install.packages("CITAN", repos = "http://cran.us.r-project.org")

if (file.exists("Test.sqlite") == TRUE) file.remove("Forest_1.sqlite")

db <- dbConnect(SQLite(), dbname="Forest_1.sqlite")

mydata <- read.csv("EM_WoS.csv")

mydata

article <- mydata[,c(3, 2, 4,37, 44, 42  )]
head(article)

colnames(mydata)

head(mydata$type)

class(mydata)

author <- mydata[, c(3,8)]
head(author)

dbWriteTable(conn = db, name = "article", value = article)

dbListTables(db)
dbReadTable(db, "article")
