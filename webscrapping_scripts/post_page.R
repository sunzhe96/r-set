# return post page html
get_post_html <- function(url) {
  return( login_read_html(url) )
}

# return a vector of author names of the post page
get_postAndReply_author <- function(url) {
    get_post_html(url) %>%  html_nodes(".author") %>% html_node("a") %>% html_text()  
}

# return reply content of the post page
get_postAndReply_content <- function(url) {
  get_post_html(url) %>% html_nodes(".fullpost") %>% html_text()
}

# remove the first element (leave only reply)
# make data frame
make_postsAndReply_table <- function(url) {
  data.frame("replyBy" = get_postAndReply_author(url)[-1],
             "replyContent" = get_postAndReply_content(url)[-1])
}
