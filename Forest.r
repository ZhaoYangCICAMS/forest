
library("sqldf")

# install.packages("CITAN", repos = "http://cran.us.r-project.org")

if (file.exists("Test.sqlite") == TRUE) file.remove("Forest_1.sqlite")

#### Create database ####
db <- dbConnect(SQLite(), dbname="Forest_1.sqlite")


#### Read file ####
mydata <- read.csv("EM_WoS.csv")
mydata$X <- NULL 

#### Tidying data ####

article <- mydata[,c(2, 1, 3,36, 43, 41  )]


colnames(mydata)

head(mydata$type)

class(mydata)

#### Separate authors from mydata ####

author <- mydata[, c(2,7)]

## For to authors ###
author_1 <- data.frame(ID = character(), author = character(), stringsAsFactors = FALSE)
list_ids <- unique(author$ID)

for (i in list_ids) {
  row_1 =  author[author$ID == i,]
  newrow = data.frame(ID= i, strsplit(as.character(row_1$author), "and"))
  colnames(newrow) = c("ID", "author")
  author_1 = rbind(author_1, newrow)
}


#### Others ####
dbWriteTable(conn = db, name = "article", value = article)

dbListTables(db)
dbReadTable(db, "article")


