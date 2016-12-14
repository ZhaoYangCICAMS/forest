library("sqldf")
library("combinat" )

# install.packages("CITAN", repos = "http://cran.us.r-project.org")

if (file.exists("Test.sqlite") == TRUE) file.remove("Forest_1.sqlite")

#### Create database ####
db <- dbConnect(SQLite(), dbname="Forest_1.sqlite")


#### Read file ####
mydata <- read.csv("EM_WoS.csv")
mydata$X <- NULL 

#### Tidying data ####

article <- mydata[,c(2, 1, 3,36, 43, 41  )]



#### Separate authors from mydata ####

author <- mydata[, c(2,7)]

## For to authors ###
author_1 <- data.frame(ID = character(), author = character(), stringsAsFactors = FALSE)
list_ids <- unique(author$ID)

for (i in list_ids) {
  row_1 =  author[author$ID == i,]
  newrow = data.frame(ID= i, strsplit(as.character(row_1$author), "and "))
  colnames(newrow) = c("ID", "author")
  author_1 = rbind(author_1, newrow)
}

#### From list to graph of authors ####

edgelist <- data.frame(Source = character(), Target = character(), stringsAsFactors = FALSE)
table_ids <- table(author_1$ID)
table_ids_0 <- data.frame(table_ids)
table_ids_1 <- table_ids[table_ids_0$Freq > 1,]
list_ids_1 <- unique(table_ids_1$Var1)

for (i in list_ids_1) {
  df_1 = author_1[author_1$ID == i,]
  df_2 = combn(df_1$author, 2)
  df_3 = data.frame(t(df_2))
  colnames(df_3) = c("Source", "Target")
  edgelist = rbind(edgelist, df_3)
}


#### Others ####
dbWriteTable(conn = db, name = "article", value = article)

dbListTables(db)
dbReadTable(db, "article")


