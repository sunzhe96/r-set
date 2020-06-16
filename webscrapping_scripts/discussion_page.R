# return a table of a discussion page given url
get_discussion_table <- function(url) {
  discussion_table <- login_read_html(url) %>% html_table()
  # return as a dataframe
  return( as.data.frame(discussion_table) )
}

# return a vector of links which are the url of the posts in one discussion page
get_post_links <- function(url) {
  login_read_html(url) %>%
    html_nodes(".starter") %>% 
    html_node("a") %>% 
    html_attr("href")
}

# join links in the discussion table
make_discussion_table <- function(url) {
  table <- get_discussion_table(url)
  table$links <- get_post_links(url)
  return(table)
}