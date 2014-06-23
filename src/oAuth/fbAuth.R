fbAuth <- function (app_id, app_secret, extended_permissions = TRUE) 
{
    full_url <- oauth_callback()
    full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x = full_url, 
                     replacement = "\\1")
    facebook <- oauth_endpoint(authorize = "https://www.facebook.com/dialog/oauth", 
                               access = "https://graph.facebook.com/oauth/access_token")
    myapp <- oauth_app("facebook", app_id, app_secret)
    if (extended_permissions == TRUE) {
        scope <- paste("user_birthday,user_hometown,user_location,user_relationships,", 
                       "friends_birthday,friends_hometown,friends_location,friends_relationships,publish_actions,", 
                       "user_status,user_checkins,friends_status,friends_checkins,user_likes,friends_likes,read_stream,export_stream", 
                       collapse = "")
    }
    else {
        scope <- NULL
    }
    if (packageVersion("httr")$minor < 3) {
        facebook_token <- oauth2.0_token(facebook, myapp, scope = scope,
                                         type = "application/x-www-form-urlencoded")
        fb_oauth <- sign_oauth2.0(facebook_token$access_token)
        if (GET("https://graph.facebook.com/me", config = fb_oauth)$status == 
                200) {
            message("Authentication successful.")
        }
    }
    if (packageVersion("httr")$minor >= 3) {
        fb_oauth <- oauth2.0_token(facebook, myapp, scope = scope,
                                   type = "application/x-www-form-urlencoded", cache = FALSE)
        if (GET("https://graph.facebook.com/me", config(token = fb_oauth))$status == 
                200) {
            message("Authentication successful.")
        }
    }
    return(fb_oauth)
}