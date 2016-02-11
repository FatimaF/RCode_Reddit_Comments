require(dplyr)
require(ggplot2)
require(tm)
require(wordcloud)
require(SnowballC)
require(lubridate)
require(scales)
db <- src_sqlite('C:/Users/Fateme_2/Desktop/reddit-comments-may-2015/database.sqlite', create = F)
MyTable <- tbl(db, "May2015")
start <- as.numeric(as.POSIXct("2015-05-01 00:00:00 ET"))
end <- as.numeric(as.POSIXct("2015-05-01 23:59:59 ET"))
df <- filter(MyTable,start <= created_utc & created_utc < end)
df <- as.data.frame(df, n = -1)

**************Top 10 subreddits based on number of posts*************
e <- unique(df$subreddit)
SortedData <- data.frame(subreddit = sort(e))
SortedData$posts <- tapply(df$score,df$subreddit, length)
SortedData <- SortedData[order(SortedData$posts, decreasing = T),]
s <- SortedData[1:10,]
ggplot(s, aes(reorder(subreddit,posts),posts, fill=subreddit)) + geom_bar(stat="identity")+
ggtitle("Top 10 subreddits") + labs(x="Subreddit",y="Posts")+
coord_flip()

*************Frequency of words appearing in “nfl” subreddit comments**********
DataSubset <- df[df$subreddit=="nfl",]
Most used words in body of comments!
Texts <- VCorpus(VectorSource(DataSubset$body))
Texts <- tm_map(Texts, tolower)
Texts <- tm_map(Texts, PlainTextDocument)
Texts <- tm_map(Texts, removePunctuation)
Texts <- tm_map(Texts, removeWords, c("the","that","please","will","didnt","let","still","look","get","say","make","now","know","think","one","want","take","cant","can","dont","wasnt","this","thank","you","if","was","just","like","else","jpg","need",stopwords("english")))
Texts <- tm_map(Texts, removeNumbers)
Texts <- tm_map(Texts, stemDocument)
wordcloud(Texts, max.words = 50, random.order = FALSE,color = brewer.pal(5,"Dark2"))
matrix_terms <- DocumentTermMatrix(Texts)
removeSparseTerms(matrix_terms,0.99)
matrix_terms <- as.matrix(matrix_terms)


***********Number of comments per hour***********
DataSubset <- df[,c(1,9)]
DataSubset$date <- as.POSIXct(DataSubset$created_utc, origin="1970-01-01")
DataSubset$hour <- hour(DataSubset$date)+1
Perhour <- aggregate(created_utc ~ hour, data = DataSubset, FUN = function(x){NROW(x)})
b <- 1:24
ggplot(data = Perhour, aes(x=hour, y=created_utc)) + 
    geom_bar(stat="identity", fill = "light blue")+
    ggtitle("Total Number of Comments Per Hour on May 1st") + labs(x="Hour",y="Number of Comments")+scale_x_discrete(breaks=b) 

***********Top 10 subreddits with high score comments***********
DataSubset <- df[df$score > 100,]
HighScoreSubreddits <- aggregate(score ~ subreddit, data = DataSubset, FUN = function(x){NROW(x)})
Sorted <- HighScoreSubreddits[order(HighScoreSubreddits$score, decreasing = T),]
s <- Sorted[1:10,]
ggplot(s, aes(reorder(subreddit,score),score, fill=subreddit)) + geom_bar(stat="identity")+
ggtitle("Top 10 subreddits with high score comments") + labs(x="Subreddit",y="Number of high score comments")+
coord_flip()

**********Number of users per subreddits(Top 10 subbredits)******
e <- unique(df$subreddit)
SortedData <- data.frame(subreddit = sort(e))
SortedData$posts <- tapply(df$score,df$subreddit, length)
SortedData <- SortedData[order(SortedData$posts, decreasing = T),]
s <- SortedData[1:10,]
SelectedData <- df[df$subreddit %in% s$subreddit,]
AuthorCount <- aggregate(author ~ subreddit, data = SelectedData, FUN = function(x){NROW(x)})
ggplot(AuthorCount, aes(reorder(subreddit,author),author, fill=subreddit)) + geom_bar(stat="identity",width=0.5)+
ggtitle("Number of authors for top 10 subreddits") + labs(x="Subreddit",y="Number of authors")+
scale_fill_brewer(palette="Spectral")+
coord_flip()


**********Number of links shared in comments******
SelectedData <- df[grep("www.", df$body),]
SelectedData <- df[df$subreddit %in% s$subreddit,]
SelectedData$LinkShared <- grepl('www.', SelectedData$body)
LinkCount <- aggregate(author ~ subreddit+LinkShared, data = SelectedData, FUN = function(x){NROW(x)})
All <- summaryBy(author~subreddit, data=LinkCount,FUN=sum)
total <- merge(LinkCount,All,by="subreddit")
total$ratio <- 100*round(total$author/total$author.sum,digits=2)
ggplot(total, aes(x=subreddit, y=ratio, fill=LinkShared)) + geom_bar(position="dodge",stat="identity")+
ggtitle("Percentage of comments with shared links for top 10 subreddits") + labs(x="Subreddit",y="Percentage of comments")+
scale_y_discrete(breaks=c("2","5","10","20","30","40","50","60","70","80","90","100"))

*********Percentage of comments that shared youtube links***********
LinkData <- SelectedData[SelectedData$LinkShared=="TRUE",]
LinkData$youtube <- grepl('youtube.com', LinkData$body)
Youtube <- LinkData[grep("youtube.com", LinkData$body),]
L <- aggregate(author ~ subreddit+youtube, data = LinkData, FUN = function(x){NROW(x)})
All <- summaryBy(author~subreddit, data=L,FUN=sum)
total <- merge(L,All,by="subreddit")
total$percentage <- 100*round(total$author/total$author.sum,digits=2)
ggplot(total, aes(x=subreddit, y=percentage, fill=youtube)) + geom_bar(position="dodge",stat="identity")+
ggtitle("Percentage of comments sharing youtube links for top 10 subreddits") + labs(x="Subreddit",y="Percentage of comments")


***********Funny subreddit (What people said on top rated commenst) ***********
DataSubset <- df[df$subreddit=="funny" & df$score > 100,]
Texts <- VCorpus(VectorSource(DataSubset$body))
Texts <- tm_map(Texts, tolower)
Texts <- tm_map(Texts, PlainTextDocument)
Texts <- tm_map(Texts, removePunctuation)
Texts <- tm_map(Texts, stemDocument)
Texts <- tm_map(Texts, removeWords, c("fuck","day","later","becaus","peopl","much","your","you","never","put","thing","tell","the","that","please","will","didnt","let","still","look","get","say","make","now","know","think","one","want","take","cant","can","dont","wasnt","this","thank","you","if","was","just","like","else","jpg","need",stopwords("english")))
Texts <- tm_map(Texts, removeNumbers)
wordcloud(Texts, max.words = 20, random.order = FALSE,color = brewer.pal(7,"Dark2"))


