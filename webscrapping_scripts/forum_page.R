# return forum page html -----------------------------
forum_page_url <- "https://moodle.ncku.edu.tw/mod/forum/index.php?id=113277"
forum_page_html <- login_read_html(forum_page_url)


# return url  of each forum as a vector  ----------------------------
get_forum_links <- function() {
  ## extract sections names
  forum_links <- html_nodes(forum_page_html, "a") %>% html_attr("href")
  ## filter out strings contains "https"
  forum_links <- forum_links[-grep("https", forum_links)]
  ## filter out strings do not contains "view*"
  forum_links <- forum_links[grep("view", forum_links)]
  ## get unique link
  forum_links <- unique(forum_links)
  ## paste the full URL
  return( paste("https://moodle.ncku.edu.tw/mod/forum",
               forum_links, sep = "/") )
}


# return topics(dates) of each forum as a vector --------------------------
get_forum_topics <- function() {
  ## extract forum topic
  forum_topics <- html_nodes(forum_page_html, ".c0") %>% html_text()
  ## remove unrelated elements
  return( forum_topics <- forum_topics[-c(1, 3)] )
}

# return the number of posts ----------------------------------------------
get_forum_posts_number <- function() {
  ## extract the number of posts of annoucement
  posts_number_announcement <- html_nodes(forum_page_html, ".c2") %>% html_text()
  posts_number_announcement <- posts_number_announcement[2]
  ## extract the number of posts
  posts_number_discussion <- html_nodes(forum_page_html, ".c3") %>% html_text()
  posts_number_discussion <- posts_number_discussion[-c(1:3)]
  return(c(posts_number_announcement, posts_number_discussion))
}