  #Create a text file(.txt) of the speech.
  
  #Install and load the libraries
  library(tm)
  library(SnowballC)
  library(wordcloud2)
  library(RColorBrewer)
  
  #Text mining
  'Corpus is a list of a document (in our case, we only have one document).'
  
  'Import txt file'
  mltxt <- readLines("ml.txt")
  
  'Load data as corpus'
  '**VectorSource() function creates a corpus of character vectors'
  mldoc <- Corpus(VectorSource(mltxt))
  
  'Inspect the content of the document'
  inspect(mldoc)
  
  #Text transformation
  '**creating a content transformer (toSpace) to eplace special characters with blanks'
  toSpace <- content_transformer(function(x, pattern)gsub(pattern," ",x))
  mldoc <- tm_map(mldoc, toSpace, "/")
  mldoc <- tm_map(mldoc, toSpace, "@")
  mldoc <- tm_map(mldoc, toSpace, "\\|")
  
  'Remove english common stopwords'
  mldoc <- tm_map(mldoc, removeWords, stopwords("en"))
  
  'Remove your own stop word'
  'specify your stopwords as a character vector'
  mldoc <- tm_map(mldoc, removeWords, c("will","let","day"))
  
  'Remove punctuations'
  mldoc <-tm_map(mldoc, removePunctuation)
  
  'Remove numbers'
  mldoc <-tm_map(mldoc, removeNumbers)
  
  'Convert the text to lower case'
  mldoc <- tm_map(mldoc, content_transformer(tolower))
  
  'Eliminate extra white spaces'
  mldoc <- tm_map(mldoc, stripWhitespace)
  
  'Text Stemming'
  mldoc <- tm_map(mldoc,stemDocument)
  
  
  #Build a TDM (Term Document Matrix)
  
  mldtm<-TermDocumentMatrix(mldoc)
  m <- as.matrix(mldtm)
  v <- sort(rowSums(m),decreasing = TRUE)
  d <- data.frame(word=names(v),freq=v)
  d
  head(d,50)
  
  
  #Generate the word cloud
  
  mypalette<-brewer.pal(7,"Greens")
  set.seed(1234)
  wordcloud2(d,size=0.5,shuffle = FALSE, rotateRatio = 0.5, shape = 'circle')
  
  
  
