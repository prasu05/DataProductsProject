
getGraphApiUrl <- function(user_id, access_token){
    paste('https://graph.facebook.com/v2.0/',
          user_id,
          '/statuses?limit=500&access_token=',access_token, sep="")
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

getLikesFromStatus <- function(status, metadata){
    status <- unlist(status, recursive = FALSE)
    if('likes' %in% metadata){
        likeDf <- getLikeMatrixForStatus(status$likes.data)    
    }
    if('comments' %in% metadata){
        likeDf <- getCommentMatrixForStatus(status$comments.data)
    }
    
    if( is.null(likeDf) ){
        likeDf <- data.frame(user_id = '-1',user_name = 'XXX')    
    }
    status_id <- cbind(status_id = rep(status$id, nrow(likeDf)))
    status_message <- cbind(status_message = rep(status$message, nrow(likeDf)))
    status_time <- cbind(status_time = rep(status$updated_time, nrow(likeDf)))
    cbind(status_id, status_message, status_time,likeDf)
}

getStatuses <- function(user_id, access_token, metadata = c('likes')){
    url <- getGraphApiUrl( user_id, access_token )
    print(url)
    httpRes <- GET( url )
    if(httpRes$status_code == 200){
        res = fromJSON( as.character(httpRes) )    
    }
    likeDataFrame <- data.frame()
    likeList <- lapply(res$data, getLikesFromStatus, metadata)
    likeDataFrame <- Reduce(rbind, likeList, data.frame())
    count <- length(res$data)
    
    while( !is.null(res$paging) ) {
        httpRes <- GET( as.character( res$paging[2] ) )
        if(httpRes$status_code == 200){
            res = fromJSON( as.character(httpRes) )
            likeList <- lapply(res$data, getLikesFromStatus, metadata)
            likeDataFrame <- rbind(likeDataFrame, Reduce(rbind, likeList, data.frame()))
            count <- count + length(res$data)
        }
    }
    likeDataFrame
}