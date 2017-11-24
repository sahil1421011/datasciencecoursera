#pooja_bavishi


#categorize words as very negative to very positive and add some movie-specific words

negTerms <-c("excellent","good")
posTerms <-c("out","rough","Poor","not")

review<- read.delim(file="~/Downloads/review.txt", header=FALSE, stringsAsFactors=FALSE)
review <- review$V1
review <- unlist(lapply(review, function(x) { str_split(x, "\n") }))




#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, negTerms, posTerms){
  final_scores <- matrix('', 0, 3)
  scores <- laply(sentences, function(sentence, negTerms, posTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
   
    posMatches <- match(words, posTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    posMatches <- sum(!is.na(posMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c( negMatches, posMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, negTerms, posTerms)
  return(scores)
}    



#build tables of positive and negative sentences with scores
posResult <- as.data.frame(sentimentScore(review,  negTerms, posTerms))
negResult <- as.data.frame(sentimentScore(review, negTerms, posTerms))
posResult <- cbind(posResult)
colnames(posResult) <- c('sentence', 'neg', 'pos')
posResult
#negResult <- cbind(negResult, 'negative')
#colnames(negResult) <- c('sentence', 'neg', 'pos', 'sentiment')    
#combine the positive and negative tables
#results <- rbind(posResult, negResult)
#results

a=posResult$pos
b=posResult$neg
a=as.vector(a)
b=as.vector(b)
pos=length(a[a >= 1])
neg=length(a[b >= 1])

result=cbind(pos,neg)
result


