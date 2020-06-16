library(tidyverse)
discussion_table <- readRDS("data/discussion_table")
posts_table <- readRDS("data/posts_table_complete")

# create student posts table ----------------------------------------------

students_posts_table <- posts_table %>% 
  # covert column date into date type data
  mutate(date = as.Date(date, "%m/%d")) %>%
  # remove na value
  na.omit() %>%
  # remove professor and unregistered classmate
  filter(!(postAuthor %in% c("康佩如", "許清芳")))


# barplot: number of posts ------------------------------------------------

  # compute frequency of posts of each student 
  students_freq <- students_posts_table %>% group_by(postAuthor) %>% count()

  # barplot
  ggplot(data = students_freq, mapping = aes(x = n, y = reorder(postAuthor, n))) + 
  geom_col(na.rm = TRUE, orientation = "y") + 
  geom_vline(xintercept = median(students_freq$n), color = "red") +
  geom_text(aes(x = 43, y = 3, label = "median reference")) +
  labs( x = "numbers of posts", y = "student",
        title = "Number of Posts by Students") +
  theme_bw()
  

# barplot: number of times reply to others --------------------------------
  
  # frequency of reply to other 
  replyToOther_freq <- students_posts_table %>%
      mutate(replyToOther = ifelse(replyBy != postAuthor, TRUE, FALSE)) %>%
      group_by(replyBy) %>%
      summarise(replyToOther_times = sum(replyToOther)) %>%
      filter(!(replyBy %in% c(0, "許清芳"))) %>%
      arrange(desc(replyToOther_times))

  # barplot
  ggplot(data = replyToOther_freq, mapping = aes(x = replyToOther_times, 
                                                 y = reorder(replyBy, replyToOther_times))) + 
  geom_col(na.rm = TRUE, orientation = "y") + 
  geom_vline(xintercept = median(replyToOther_freq$replyToOther_times), color = "red") +
  geom_text(aes(x = 3, y = 3, label = "median reference")) +
  labs( x = "times student reply to the posts of other classmates", y = "students",
        title = "Number of Times Reply to The Post of Other Classmate by Studetns") +
  theme_bw()


# positive reply by professor ---------------------------------------------

  
  # get posts reply by professor
  replyBy_prof <- students_posts_table %>% filter(replyBy == "許清芳")

  # get positive reply with pattern(sentences contain "nice", "excellent", "good", "great")
  nice <- grep("nice", x = replyBy_prof$replyContent, ignore.case = TRUE)
  excellent <- grep("excellent", x = replyBy_prof$replyContent, ignore.case = TRUE)
  good <- grep("good", x = replyBy_prof$replyContent, ignore.case = TRUE)
  great <- grep("great", x = replyBy_prof$replyContent, ignore.case = TRUE)

  # combine keywords and remove duplicate elements
  positive <- unique(c(nice, excellent, good, great))
  positive_table <- replyBy_prof[positive, ]

  positivie_freq <- positive_table %>% group_by(postAuthor) %>% count() %>% arrange(desc(n))

# barplot
  ggplot(data = positivie_freq, mapping = aes(x = n, 
                                                 y = reorder(postAuthor, n))) + 
  geom_col(na.rm = TRUE, orientation = "y") + 
  geom_vline(xintercept = median(replyToOther_freq$replyToOther_times), color = "red") +
  geom_text(aes(x = 2, y = 3, label = "median reference")) +
  labs( x = "number of times professor gave positive comment", y = "students",
        title = "Number of Positive commnets Given by Professor") +
  theme_bw()


# number of posts by weeks ------------------------------------------------

post_freq_byDate <- posts_table  %>% filter(postAuthor != "許清芳") %>% 
  mutate(date = as.Date(date, "%m/%d")) %>% group_by(date)%>% count()

ggplot(data = post_freq_byDate, mapping = aes(x = date, y = n,
                                              group = 1))  +
  geom_point() + 
  geom_line() +
  scale_x_date(date_breaks = "1 week") + 
  theme_bw()
  
# number of posts of each student by weeks
post_freq_student_byDate <- students_posts_table  %>% 
  group_by(date, postAuthor) %>% 
  count() %>% 
  na.omit()

post_freq_median <- post_freq_student_byDate %>% group_by(date) %>% summarise(median = median(n))

index <- sapply(post_freq_student_byDate$date, function(x) {which(x == post_freq_median$date)})
post_freq_student_byDate$median <- pull(post_freq_median[index, ], median)


ggplot(data = post_freq_student_byDate[post_freq_student_byDate$postAuthor == "黃凱揚",],
       mapping = aes(x = date, y = n))  +
  geom_point() + 
  geom_line(aes(color = "selected student")) +
  geom_point(aes(x = date, y = median)) +
  geom_line(aes(x = date, y = median, color = "median reference")) +
  scale_x_date(date_breaks = "1 week") +
  ylim(min = 0, max = 20) +
  labs(x = "course presented date",
       y = "number of posts",
       title = "Number of Posts of Each Student by Course Session",
       color = "line color") +
  theme_bw()


# number of comments each student replies to other by date ----------------
replyToOther_byDate <- students_posts_table %>%
  mutate(replyToOther = ifelse(replyBy != postAuthor, TRUE, FALSE)) %>%
  filter(replyToOther == TRUE & !(replyBy %in% c(0, "許清芳"))) %>%
  group_by(replyBy, date) %>%
  count()

ggplot(data = replyToOther_byDate[replyToOther_byDate$replyBy == "廖傑恩",],
       mapping = aes(x = date, y = n))  +
  geom_point() + 
  geom_line(aes(color = "selected student")) +
  scale_x_date(date_breaks = "1 week") +
  ylim(min = 0, max = 10) +
  labs(x = "course presented date",
       y = "number of posts",
       title = "Number of Posts of Each Student by Course Session",
       color = "line color") +
  theme_bw()
