
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

author_1 <- data.frame(ID = "", author = "")


dbWriteTable(conn = db, name = "article", value = article)

dbListTables(db)
dbReadTable(db, "article")
