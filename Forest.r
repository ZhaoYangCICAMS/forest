library("sqldf")
library("combinat" )
library(igraph)

# upload Data

edgelist <- read.csv2("edgeslit.csv")

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
  newrow = data.frame(ID= i, strsplit(as.character(row_1$author), ".and."))
  colnames(newrow) = c("ID", "author")
  author_1 = rbind(author_1, newrow)
}

#### From list to graph of authors ####


edgelist_1 <- data.frame(Source = character(), Target = character(), stringsAsFactors = FALSE)
table_ids <- table(author_1$ID)
table_ids_0 <- data.frame(table_ids)
table_ids_1 <- table_ids_0[table_ids_0$Freq == 2,]
list_ids_1 <- unique(table_ids_1$Var1)

for (i in list_ids_1) {
  df_1 = author_1[author_1$ID == i,]
  df_2 = combn(df_1$author, 2)
  df_3 = data.frame(t(data.frame(df_2)))
  colnames(df_3) = c("Source", "Target")
  edgelist_1 = rbind(edgelist_1, df_3)
}

edgelist_2 <- data.frame(Source = character(), Target = character(), stringsAsFactors = FALSE)
table_ids_2 <- table(author_1$ID)
table_ids_0_2 <- data.frame(table_ids_2)
table_ids_1_2 <- table_ids_0_2[table_ids_0_2$Freq > 2,]
list_ids_1_2 <- unique(table_ids_1_2$Var1)

for (i in list_ids_1_2) {
  df_1 = author_1[author_1$ID == i,]
  df_2 = combn(df_1$author, 2)
  df_3 = data.frame(t(df_2))
  colnames(df_3) = c("Source", "Target")
  edgelist_2 = rbind(edgelist_2, df_3)
}

edgelist <- rbind(edgelist_1, edgelist_2)


#### Others ####
dbWriteTable(conn = db, name = "article", value = article)

dbListTables(db)
dbReadTable(db, "article")

## Test ###

network <- graph.data.frame(edgelist, directed = FALSE)

## Extracting References ##

ref_0 <- mydata[,c("ID","cited.references") ]
ref_1 <- data.frame(ID = character(), ref = character(), stringsAsFactors = FALSE)
list_ids <- unique(author$ID)

for (i in list_ids) {
  row_1 =  author[author$ID == i,]
  newrow = data.frame(ID= i, strsplit(as.character(row_1$author), ".and."))
  colnames(newrow) = c("ID", "author")
  author_1 = rbind(author_1, newrow)
}

y <- data.frame(ref_0$cited.references)

newrow <- data.frame(strsplit(as.character(y), "[.]"))
