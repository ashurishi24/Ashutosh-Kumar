if (!require(tm)) {install.packages("tm")}
if (!require(wordcloud)) {install.packages("wordcloud")}
library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)


#code for task 1 according to tutorial

setwd("C:/Users/Ashutosh Kumar/Desktop/GroupAssignment")



create_DTM = function(text)
{
  
  
  require(tibble)
  textdf = data_frame(text = text) # yields 120x1 tibble. i.e., each doc = 1 row here.
  
  # Tokenizing ops. Words first.
  textdf %>% unnest_tokens(word, text)   # try ?unnest_tokens
  
  
  # First, build a datafame
  ak_text <- textdf %>% 
    unnest_tokens(word, text) %>%     # word tokenization 
    anti_join(stop_words)    # run ?join::dplyr 
  
  # First convert text corpus to tidy format, i.e. a tibble with token and count per row per document.
  
  tidy_text = textdf %>%   
    mutate(doc = row_number()) %>%
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    group_by(doc) %>%
    count(word, sort=TRUE)
  
  tidy_text
  
  text_dtm <- tidy_text %>% cast_sparse(doc, word, n)
  # class(nokia_dtm)  # Matrix
  return(text_dtm)
  
}


years = 2005:2014

for (year in years) {
  df= readRDS(paste0('C:/Users/Ashutosh Kumar/Desktop/GroupAssignment/bd.df.30firms.',year,'.Rds'))
  text = df$bd.text #creating business description
  cik = sapply(strsplit(df$file,"_"),'[[',1)
  dtm=create_DTM(text=text,docID=cik) #user defined function created above
  saveRDS(dtm,paste0('C:/Users/Ashutosh Kumar/Desktop/GroupAssignment/df',year,'.Rds'))
}

head(dtm)
dim(dtm)

str(df)

text[1]

d1=as.matrix(readRDS("C:/Users/Ashutosh Kumar/Desktop/GroupAssignment/df2005.Rds"))
d2= as.matrix(readRDS("C:/Users/Ashutosh Kumar/Desktop/GroupAssignment/df2006.Rds"))


d11 = d1[,colSums(d1)>0]
d22=d2[,colSums(d2) >0]


add.words=setdiff(colnames(d22),colnames(d11))
df=data.frame(new.words=add.words,year=2006, stringsAsFactors=F)
all.words=unique(c(colnames(d11),colnames(d22)))


for  (k in 3:10) {
  d=as.matrix(readRDS(paste0('C:/Users/Ashutosh Kumar/Desktop/GroupAssignment/df',years[k],'.Rds')))
                             d=d[,colSums(d)>0]
                             
                             new.t = setdiff(colnames(d),all.words)
                             all.words=unique(c(all.words,colnames(d)))
                             df1=data.frame(new.words =new.t,year=years[k],stringsAsFactors=F)
                             df=rbind(df,df1)
}

dim(df)
head(df)

new_terms_dtm_2006=d2[,colnames(d2) %in% df$new.words[df$year==2006]]
freq=colSums(new_terms_dtm_2006)

head(freq)

#Plot Word Cloud
x11()
wordcloud::wordcloud(words = names(freq),freq=freq,random.color = T,colors = 100:150)
title('Normal DTM')

new_terms_dtm_2006[1:5,1:5]

a= new_terms_dtm_2006[row.names(new_terms_dtm_2006)=='128776',]
colSums(a)
head(a)
a[a!=0]


