library(randomForest)
library(caret)
source('src/api/status.R')
source('src/app/services.R')
source('src/utils/utils.R')
token <- 'CAACEdEose0cBAPGOxKRD2tlSePAySGlvOch2YuDDC9lhUPzzAtvC5uZCQoUN8EA7oKkA4wYrlLjQvOr1nPUadmBSt0ZC4gazB1nZBIqDVG6YI1FHBshl2ZCV1fmOnCjcZARc7eYl9BTAzqZAaKUMGIZAT36lDYphuIV3WYHMIXg275QXFK9VN0dRoLyJ6ta4pYZD'

likeData <- getStatuses(token, c('likes'))
likeData <- likeData[likeData$user_id != '-1',]
likeData$user_id <- factor(likeData$user_id)

isLiked <- cbind(rep(1, nrow(likeData)))
isCommented <- cbind(rep(0, nrow(likeData)))
likeData <- cbind(likeData, isLiked, isCommented)

commentData <- getStatuses(token, c('comments'))
commentData <- commentData[commentData$user_id != '-1',]
commentData$user_id <- factor(commentData$user_id)

isCommented <- cbind(rep(1, nrow(commentData)))
isLiked <- cbind(rep(0, nrow(commentData)))
commentData <- cbind(commentData, isLiked, isCommented)

statusData <- rbind(likeData, commentData)

likeCount <- tapply(statusData$isLiked, statusData$user_id, sum)
commentCount <- tapply(statusData$isCommented, statusData$user_id, sum)

meta <- unique(data.frame(user_id = statusData$user_id,
                          user_name = statusData$user_name))
like_count <- likeCount[meta$user_id]
meta <- cbind(meta, like_count = like_count)
comment_count <- commentCount[meta$user_id]
meta <- cbind(meta, comment_count = comment_count)
rownames(meta) <- c(1:nrow(meta))
rownames(meta$comment_count) <- c(1:nrow(meta))
rownames(meta$like_count) <- c(1:nrow(meta))
meta[95,]$like_count <- 79
lico <- meta[,c(3,4)]
rfModel <- randomForest(comment_count ~ like_count,
                   proximity = TRUE,
                   data = lico)
plotClusters <- function(rfModel,jitterCoef=1000, nClus = 3){
    dist <- jitter(cmdscale(1-rfModel$proximity), jitterCoef)*100
    km <- kmeans(dist, centers = nClus, nstart = nClus^3)
    plot(dist, col=km$cluster)
    print(km$cluster)
}
clusters <- NULL
manipulate(withVisible(plotClusters(rfModel, jitterCoef, nClus)),
           jitterCoef = slider(1000, 4000, step = 500),
           nClus = slider(2,5, step = 1))

plot(dist)
text(dist[95,1], dist[95,2], label='S', col='red')

c1 <- dist[ ( dist[,2] < 0 & dist[,2] > -40),]
c2 <- dist[dist[,2] < -40,]
c3 <- dist[dist[,1] > 30 ,]
c4 <- dist[dist[,1] < -20,]

c1Index <- as.numeric(rownames(c1))
c1Names <- meta$user_name[c1Index]

c2Index <- as.numeric(rownames(c2))
c2Names <- meta$user_name[c2Index]

c3Index <- as.numeric(rownames(c3))
c3Names <- meta$user_name[c3Index]

c4Index <- as.numeric(rownames(c4))
c4Names <- meta$user_name[c4Index]

labelPoints <- c(c1Index[1], c2Index[1], c3Index[3], c4Index[4])
labels <- c('C1', 'C2', 'C3', 'C4')
text(dist[labelPoints,1], dist[labelPoints,2], label=labels, col='red')

meta[c1Index, ]
meta[c2Index, ]
meta[c3Index, ]
meta[c4Index, ]


user_id <- '100001066687353'
access_token <- 'CAACEdEose0cBAPAWHBHFcgGcpMZAhojZBnQWypbreMS5miwpLamp6qTFUNjGSpYZBoZB4BduMWYWfftxKYflKOLgVndgoA6NBBoymcPX3ETZATMlUMstevzUOQVQTk6iNUGSipJ0kaau8RZBf4jxMUkZAwgo0CiIKPORtIDTVhTSGG6ZAbEGCyC4aybYCogF23sZD'

rawData <- getStatusesData(user_id, access_token)
write.csv(rawData, 'sush.csv')
df <- read.csv('sush.csv')[,-1]
save(rawData,file="sush.rda")
meta <- preProcessStatusData(rawData, user_id)
lico <- meta[,c(3,4)]
rfModel <- randomForest(comment_count ~ like_count,
                        proximity = TRUE,
                        data = lico)
rfd <- jitter(cmdscale(1-rfModel$proximity), 300)*100

userIndex <- which(meta$user_id == user_id)
d <- getDistance(as.numeric(userIndex), meta)
userCluster <- km$cluster[userIndex]

