setwd("C:/Users/sebas/Google Drive/DOCTORADO/PAPERS/F. OBJECTIVE 1 - Simulation ABM/D. MODEL 4 - Viviana/17. Forest_R/forest_2")


library(sqldf)
library(combinat)
library(igraph)
library(XML)
library(xml2)
library(plyr)
library ("RSQLite")
library("proto")
library ("gsubfn")
library("combinat" )
library(tcltk2)


# upload Data

edgelist <- read.csv2("edgelist.csv", sep = ",")
ref_1 <- read.csv("ref_1.csv")
edgelist$X <- NULL
ref_1$X <- NULL
EM_authors_ref <- read.csv2("EM_authors_ref.csv", sep = ",")
edgelist_ref <- read.csv2("edgelist_ref.csv", sep = ",")
edgelist_ref$X <- NULL
edgelist_total <- read.csv2("edgelist_total.csv", sep = ",")
edgelist_total$X <- NULL

#  merge data of Complex Networks

data_1 <- read.csv2("CN.csv", sep = ",")
data_1$X <- NULL
author_0 <- data_1[, c("ID","author")]
data_1 <- data_1[,c("ID","cited.references" )]
data_2 <- read.csv2("CN (1).csv", sep = ",")
data_2$X <- NULL
author_2 <- data_2[, c("ID","author")]
data_2 <- data_2[,c("ID","cited.references"  )]
data_3 <- read.csv2("CN (2).csv", sep = ",")
data_3$X <- NULL
author_3 <- data_3[, c("ID","author")]
data_3 <- data_3[,c("ID","cited.references" )]
data_4 <- read.csv2("CN (3).csv", sep = ",")
data_4$X <- NULL
author_4 <- data_4[, c("ID","author")]
data_4 <- data_4[,c("ID","cited.references" )]
data_5 <- read.csv2("CN (4).csv", sep = ",")
data_5$X <- NULL
author_5 <- data_5[, c("ID","author")]
data_5 <- data_5[,c("ID","cited.references" )]

mydata <- do.call("rbind", list(data_1, data_2, data_3, data_4, data_5))
author <- do.call("rbind", list(author_0, author_2, author_3, author_4, author_5))

# install.packages("CITAN", repos = "http://cran.us.r-project.org")

if (file.exists("Test.sqlite") == TRUE) file.remove("Forest_1.sqlite")

#### Create database ####
db <- dbConnect(SQLite(), dbname="Forest_1.sqlite")


#### Read file #### (for the topic of Entrepenurial Marketing)
## mydata <- read.csv("EM_WoS.csv")
## mydata$X <- NULL 

#### Tidying data ####

## article <- mydata[,c(2, 1, 3,36, 43, 41  )]  # revisar para borrar

#### Separate authors from mydata (for the topic of Entrepenurial Marketing) #### 

## author <- mydata[, c(2,7)] 

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
## dbWriteTable(conn = db, name = "article", value = article)

## dbListTables(db)
## dbReadTable(db, "article")

## Test ###

## network <- graph.data.frame(edgelist, directed = FALSE)

## Extracting References ##

ref_0 <- mydata[!(mydata$cited.references==""),c("ID","cited.references") ]
ref_1 <- data.frame(ID = character(), ref = character(), stringsAsFactors = FALSE)
list_ids <- unique(ref_0$ID)

for (i in list_ids) {
  row_2 = ref_0[ref_0$ID == i,]
  newrow_1 = data.frame(ID= i, strsplit(as.character(row_2$cited.references), "\n", fixed = TRUE))
  colnames(newrow_1) = c("ID", "ref")
  ref_1 = rbind(ref_1, newrow_1)
}

## References that have DOI

ref_DOI <- sqldf("select * from ref_1 where ref LIKE '%DOI%'")

## ID vs DOI of the references 

ref_ID_DOI <- data.frame(ID= ref_DOI$ID, ref= gsub(".*DOI.", "", ref_DOI$ref) )
ref_ID_DOI <- data.frame(ID = ref_DOI$ID, DOI=gsub('.{1}$', '', ref_ID_DOI$ref))

## Extracted authors from references 
authors <- data.frame(doi = character(), author = character(), stringsAsFactors = FALSE)
ref_ID_DOI_UNKNOWN <- data.frame(doi = character(),stringsAsFactors = FALSE)

## Ensayo
#ensayo <- ref_ID_DOI[400:500,]
##ensayo <- "10.1162/153244303321897735"

for (i in ref_ID_DOI$DOI) {
  ##for (i in ensayo) {
  doi <- i
  url <- paste0("http://api.crossref.org/works/", doi, ".xml")
  xml_data_1 = try(xmlParse(url), silent = TRUE);
  ##xml_data_1 <- xmParse(url)
  if (class(xml_data_1) == "try-error") {
    ref_ID_DOI_UNKNOWNS <- data.frame(doi = i)
    ref_ID_DOI_UNKNOWN = rbind(ref_ID_DOI_UNKNOWN, ref_ID_DOI_UNKNOWNS)
  } else  {
    xml_data_2 <- xmlToList(xml_data_1)
    
    if (as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article"){
      if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]]))
      {next} else {
      author_ref <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]])
      
      author_1_ref <- ldply(author_ref, data.frame)
      author_2_ref <- author_1_ref[author_1_ref$.attrs == "author", c(2,3) ]
      
      authorss <- data.frame(doi= doi, author = paste0(author_2_ref$surname, ", ", author_2_ref$given_name))
      
      authors = rbind(authors, authorss)
      } 
      }else {next}
  }
}

#### From list to graph of authors of references ####

edgelist_ref_1 <- data.frame(Source = character(), Target = character(), stringsAsFactors = FALSE)
table_dois <- table(EM_authors_ref$doi)
table_dois_0 <- data.frame(table_dois)
table_dois_1 <- table_dois_0[table_dois_0$Freq == 2,]
list_dois_1 <- unique(table_dois_1$Var1)

for (i in list_dois_1) {
  df_1 = EM_authors_ref[EM_authors_ref$doi == i,]
  df_2 = combn(df_1$author, 2)
  df_3 = data.frame(t(data.frame(df_2)))
  colnames(df_3) = c("Source", "Target")
  edgelist_ref_1 = rbind(edgelist_ref_1, df_3)
}

edgelist_ref_2 <- data.frame(Source = character(), Target = character(), stringsAsFactors = FALSE)
table_dois_2 <- table(EM_authors_ref$doi)
table_dois_0_2 <- data.frame(table_dois_2)
table_dois_1_2 <- table_dois_0_2[table_dois_0_2$Freq > 2,]
list_dois_1_2 <- unique(table_dois_1_2$Var1)

for (i in list_dois_1_2) {
  df_1_1 = EM_authors_ref[EM_authors_ref$doi == i,]
  df_2_2 = combn(df_1_1$author, 2)
  df_3_3 = data.frame(t(data.frame(df_2_2)))
  colnames(df_3_3) = c("Source", "Target")
  edgelist_ref_2 = rbind(edgelist_ref_2, df_3_3)
}

edgelist_ref <- rbind(edgelist_ref_1, edgelist_ref_2)

## List of total edges

edgelist_total <- rbind(edgelist, edgelist_ref)
write.csv(edgelist_total, file = "edgelist_total.csv")

### Network Stats ####

## Convert links to graph object ##

net <- graph.data.frame(edgelist_total, directed = FALSE)

## Find Giant Component ##

net_sim <- simplify(net)

## Topology ##

net_gml <- read.graph("CN/net_final.gml", format = "gml")
d.net_gml <- degree(net_gml)
dd.net_gml <- degree.distribution(net_gml)
d <- 1:max(d.net_gml) - 1
ind <- (dd.net_gml != 0)
plot(d[ind], dd.net_gml[ind], log="xy", col = "blue", 
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

## Degree vs Neighbors ##

a.nn.deg.yeast <- graph.knn(net_gml, V(net_gml))$knn
plot(d.net_gml, a.nn.deg.yeast, log="xy",
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"),
     main="Average Neighbor Degree vs Node Degree")





 