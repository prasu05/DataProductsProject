getStatusesData <- function(user_id, access_token){
    library(Rfacebook)
    library(randomForest)
    library(caret)
    source('src/api/status.R')
    likeData <- getStatuses(user_id, access_token, c('likes'))
    likeData <- likeData[likeData$user_id != '-1',]
    likeData$user_id <- factor(likeData$user_id)
    
    isLiked <- cbind(rep(1, nrow(likeData)))
    isCommented <- cbind(rep(0, nrow(likeData)))
    likeData <- cbind(likeData, isLiked, isCommented)
    
    commentData <- getStatuses(user_id, access_token, c('comments'))
    commentData <- commentData[commentData$user_id != '-1',]
    commentData$user_id <- factor(commentData$user_id)
    
    isCommented <- cbind(rep(1, nrow(commentData)))
    isLiked <- cbind(rep(0, nrow(commentData)))
    commentData <- cbind(commentData, isLiked, isCommented)
    rbind(likeData, commentData)
}

preProcessStatusData <- function(statusData, user_id){
    names(statusData)
    numOfStatuses <- length(unique(statusData$status_id))
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
    userIndex <- which(meta$user_id == user_id)
    meta[userIndex,]$like_count <- numOfStatuses
    meta
}