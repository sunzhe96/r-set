source("login.R")
source("forum_page.R")
source("discussion_page.R")
source("post_page.R")

# forum_page_table --------------------------------------------------------

# create forum_page_table
forum_page_table <- data.frame(get_forum_topics(),
                               get_forum_links(),
                               get_forum_posts_number())
# change colnames
colnames(forum_page_table) <- c("topics", "links", "posts_number")


# discussion table --------------------------------------------------------

# create an empty data frame for storing discussion table later
discussion_table <- data.frame()

# read each discussion table
for (i in 1:nrow(forum_page_table)) {
  
  new_discussion_table <- 
    make_discussion_table(forum_page_table[i, 2])
  
  # if no posts in a discussion page
  # create a data frame with the the same header like other pages that have posts
  # and NA value
  if (length(names(new_discussion_table)) < 7) {
    new_discussion_table <- data.frame(
      "議題" = NA,
      "開始於" = NA,
      "開始於.1" = NA,
      "回應" = NA,
      "未閱讀" = NA,
      "最新貼文" = NA,
      "links" = NA
    )
  }  
  # add column date
  new_discussion_table$date <- forum_page_table[i, 1]
  
  discussion_table <- rbind(discussion_table,
                            new_discussion_table)
}


# posts_table --------------------------------------------------------------
## creat an empty data frame for storing posts_table
posts_table <- data.frame()
## loop every row in discuss_table except 287 which has different html structure
## add row 287 manually after the for loop
loops <- 1:nrow(discussion_table)
loops <- loops[-287]
for (v in loops) {
  if (discussion_table[v, ]$回應 != 0 & !is.na(discussion_table[v, ]$回應)) {
    new_post_table <- make_postsAndReply_table(discussion_table[v, 7])
  } else {
   new_post_table <- data.frame("replyBy" = 0,
                                "replyContent" = 0)
  }
  new_post_table$topic <- discussion_table[v, 1]
  new_post_table$postAuthor <- discussion_table[v, 3]
  new_post_table$reply <- discussion_table[v, 4]
  new_post_table$date <- discussion_table[v, 8]
  posts_table <- rbind(posts_table, new_post_table)
  print(new_post_table)
}

## add special case manually (row 287)
row_287_author <- get_postAndReply_author(discussion_table[287, 7])
row_287_content <- get_postAndReply_content(discussion_table[287, 7])
row_287_post_table <- data.frame("replyBy" = row_287_author,
                                 "replyContent" = row_287_content[-1])
row_287_post_table$topic <- discussion_table[287, 1]
row_287_post_table$postAuthor <- discussion_table[287, 3]
row_287_post_table$reply <- discussion_table[287, 4]
row_287_post_table$date <- discussion_table[287, 8]
posts_table <- rbind(posts_table, row_287_post_table)

# save table objects as files ---------------------------------------------
saveRDS(posts_table, "posts_table")
saveRDS(posts_table, "posts_table_complete")
saveRDS(forum_page_table, "forum_page_table")
saveRDS(discussion_table, "discussion_table")