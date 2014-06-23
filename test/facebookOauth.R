getGraphApiUrl <- function(access_token){
    paste('https://graph.facebook.com/v2.0/10152490386058480/statuses?limit=500&access_token=',access_token, sep="")
    
}

getLikeMatrixForStatus <- function(likes){
    if(!is.null(likes)){
        df <- lapply(likes, as.data.frame)
        df <- Reduce(rbind, df, data.frame())
        colnames(df) <- c('user_id', 'user_name')
        df    
    }
}

getCommentMatrixForStatus <- function(comments){
    if(!is.null(comments)){
        df <- lapply(comments, function(comment){
            temp <- as.data.frame(comment)
            data.frame(user_id=temp$from.id, user_name=temp$from.name)
        })
        Reduce(rbind, df, data.frame())
    }
}

getLikesFromStatus <- function(status){
    status <- unlist(status, recursive = FALSE)
    likeDf <- getLikeMatrixForStatus(status$likes.data)
    commentDf <- getCommentMatrixForStatus(status$comments.data)
    likeDf <- rbind(likeDf, commentDf)
    if( is.null(likeDf) ){
        likeDf <- data.frame(user_id = '-1',user_name = 'XXX')    
    }
    status_id <- cbind(status_id = rep(status$id, nrow(likeDf)))
    status_message <- cbind(status_message = rep(status$message, nrow(likeDf)))
    status_time <- cbind(status_time = rep(status$updated_time, nrow(likeDf)))
    cbind(status_id, status_message, status_time,likeDf)
}

#app_id <- '314262048733373'
#app_secret <- 'b57dab3fb4352f433b7e392ea8b37a66'
#fbToken <- fbOAuth(app_id, app_secret, extended_permissions = TRUE)
fbToken <- 'CAACEdEose0cBAByQXUlBcC3l7t5ZAQK4z3vZBG5VOFA4GkwdmiUD5lF6oBBz9AcgENuZA3brOd3sFlGtzyjvydzIoHY0IGtmsuzBI8r7VlCXdpwfxBAZCFwBipyvlWZBoztZAUvKpi5eUitI4CV3TXIMKCb8t03DHxKDOTxj5IIby68XNnZCLF6Kv14NDJ1JmsZD'
httpRes <- GET( getGraphApiUrl( fbToken ) )
if(httpRes$status_code == 200){
    res = fromJSON( as.character(httpRes) )    
}
likeDataFrame <- data.frame()
likeList <- lapply(res$data, getLikesFromStatus)
likeDataFrame <- Reduce(rbind, likeList, data.frame())
count <- length(res$data)

while( !is.null(res$paging) ) {
    httpRes <- GET( as.character( res$paging[2] ) )
    if(httpRes$status_code == 200){
        res = fromJSON( as.character(httpRes) )
        likeList <- lapply(res$data, getLikesFromStatus)
        likeDataFrame <- rbind(likeDataFrame, Reduce(rbind, likeList, data.frame()))
        count <- count + length(res$data)
    }
}



