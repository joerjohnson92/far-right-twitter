#This file will write Trump's followers to the SQL file as the function runs
#can restart from previous cursor in case of a crash
getFollowers_Trump <- function (screen_name = NULL, oauth_folder, cursor = -1, user_id = NULL, 
          verbose = TRUE, sleep = 1, database) 
{
  creds <- list.files(oauth_folder, full.names = T)
  cr <- sample(creds, 1)
  if (verbose) {
    message(cr)
  }
  load(cr)
  limit <- smappR::getLimitFollowers(my_oauth)
  if (verbose) {
    message(limit, " API calls left")
  }
  while (limit == 0) {
    cr <- sample(creds, 1)
    if (verbose) {
      message(cr)
    }
    load(cr)
    Sys.sleep(sleep)
    rate.limit <- smappR::getLimitRate(my_oauth)
    if (rate.limit < 100) {
      Sys.sleep(300)
    }
    limit <- smappR::getLimitFollowers(my_oauth)
    if (verbose) {
      message(limit, " API calls left")
    }
  }
  url <- "https://api.twitter.com/1.1/followers/ids.json"
  followers <- c()
  while (cursor != 0) {
    if (!is.null(screen_name)) {
      params <- list(screen_name = screen_name, cursor = cursor, 
                     stringify_ids = "true")
    }
    if (!is.null(user_id)) {
      params <- list(user_id = user_id, cursor = cursor, 
                     stringify_ids = "true")
    }
    url.data <- my_oauth$OAuthRequest(URL = url, params = params, 
                                      method = "GET", cainfo = system.file("CurlSSL", 
                                                                           "cacert.pem", package = "RCurl"))
    Sys.sleep(sleep)
    limit <- limit - 1
    json.data <- rjson::fromJSON(url.data)
    if (length(json.data$error) != 0) {
      if (verbose) {
        message(url.data)
      }
      stop("error! Last cursor: ", cursor)
    }
    followers <- c(as.character(json.data$ids))
    prev_cursor <- json.data$previous_cursor_str
    cursor <- json.data$next_cursor_str
    
    #Write to DB
    followers_df <- as.data.frame(cbind(followers, "cand100270"))
    names(followers_df) <- c("TWitter_ID", "bonica_rid")
    
    db_insert_into( con = database$con, table = "candidate_followers", values = followers_df)

    save(cursor, file = "cursor.RData")
    
    message(length(followers), " followers. Next cursor: ", 
            cursor)
    if (verbose) {
      message(limit, " API calls left")
    }
    while (limit == 0) {
      cr <- sample(creds, 1)
      if (verbose) {
        message(cr)
      }
      load(cr)
      Sys.sleep(sleep)
      rate.limit <- smappR::getLimitRate(my_oauth)
      if (rate.limit < 100) {
        Sys.sleep(300)
      }
      limit <- smappR::getLimitFollowers(my_oauth)
      if (verbose) {
        message(limit, " API calls left")
      }
    }
  }
  return(followers)
}

