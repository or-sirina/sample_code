library(xml2)
library(rvest)  
library(tibble)
library (tidyverse)
library(stringr)


ves <- "https://www.vestnik.mgimo.ru/jour/user/setLocale/en_US?source=%2Fjour%2Fissue%2Farchive"
ves <- read_html(ves)
links <- ves %>% html_nodes("div#issue h4 a") %>% html_attr("href")
links <- data.frame(links)
vlinks <- tibble::rowid_to_column(links, "ID")             
vlinks <- vlinks[2:83,]
vfin <- data.frame(matrix(ncol = 35, nrow = 0))
colnames(vfin) <- c("title",	"fulltext_url",	"keywords",	"abstract",	"author1_fname",	"author1_mname",	"author1_lname",	"author1_suffix",	"author1_email",	"author1_institution",	"author2_fname",	"author2_mname",	"author2_lname",	"author2_suffix",	"author2_email",	"author2_institution",	"author3_fname",	"author3_mname",	"author3_lname",	"author3_suffix",	"author3_email",	"author3_institution",	"author4_fname",	"author4_mname",	"author4_lname",	"author4_suffix",	"author4_email",	"author4_institution",	"acknowledgements",	"disciplines",	"cover_paste",	"document_type",	"erratum",	"peer_reviewed",	"short_title")

for (i in 1:14){
  urln <- paste(vlinks[i,2])
  urln <- read_html(urln)
  urli <- urln %>% html_nodes(".title a") %>% html_attr("href")
  urli <- data.frame(urli)
  urli <- tibble::rowid_to_column(data.frame(urli), "ID")
  j <- nrow(urli)
  
  for (h in 1:j){
    tryCatch({
    urlh <- paste(urli[h,2])
    urlh <- read_html(urlh)
    
    fin <- tibble(
    title = urlh %>% html_nodes("div#articleTitle") %>% html_text(),
    fulltext_url = urli[h,2],
    keywords = paste(urlh %>% html_nodes("div#articleSubject div a") %>% html_text(), collapse = ", "),
    abstract = paste (urlh %>% html_nodes("div#articleAbstract div p") %>% html_text(), collapse = "/n"),
    author1_fname = word(urlh %>% html_nodes(".stepleft span p") %>% html_text(), 1)[1],
    author1_mname = word(urlh %>% html_nodes(".stepleft span p") %>% html_text(), 2)[1],
    author1_lname = word(urlh %>% html_nodes(".stepleft span p") %>% html_text(), 3)[1],
    author1_suffix = 1,
    author1_email = 1, 
    author1_institution = data.frame(urlh %>% html_nodes(".stepleft") %>% html_text())[1,1],
    acknowledgements = 1, 
    disciplines = 1, 
    cover_paste = 1, 
    document_type = if (grepl("review", urlh %>% html_nodes("div#articleAbstract div p") %>% html_text(), fixed = TRUE) == TRUE) {paste("Review")} else {paste("Article")},
    erratum = 1,
    peer_reviewed = paste("??"),
    short_title = substring(urlh %>% html_nodes("div#articleTitle") %>% html_text(), first=1, last=65)
    )
    vfin<- rbind(vfin, fin)
    })
  }
}