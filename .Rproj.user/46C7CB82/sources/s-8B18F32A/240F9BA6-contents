library(shiny)
library(tidyverse)
source("anonymization.R")

## app.R ##
shinyApp(ui = tagList(
  # shinythemes::themeSelector(),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "student engagement visulization",
    tabPanel(
      "About",
      h1("Data"),
      p(
        "Data was extracted from the data manegement course on moodle using rvest pacakge."
      ),
      h1("Measurement"),
      p("Student engagement is measured by 3 variables in this project."),
      tags$ol(
        tags$li("number of posts"),
        tags$li("times each student replies to the posts of other classmates"),
        tags$li("number of postive comments given by professor to each student")
      ),
      p(
        "Positive comments means comments contain keywords such as good, excellent, great and nice."
      )
    ),
    tabPanel("Course",
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   "Number of Posts by Students",
                   h2("Number of Posts by Students"),
                   plotOutput("number_posts")
                 ),
                 tabPanel(
                   "Reply to The Posts of Other Classmates",
                   h2("Number of Times Reply to The Post of Other Classmates by Students"),
                   plotOutput("reply_to_other")
                 ),
                 tabPanel(
                   "Positive Comments Given by Professor",
                   h2("Number of Positive Comments Each Student Recived From Professor"),
                   p(
                     "Positive comments means comments contain keywords such as good, excellent, great, nice"
                   ),
                   plotOutput("positive_prof")
                 )
               )
             )),
    
    tabPanel(
      "Student",
      sidebarPanel(
        selectInput(
          "student",
          choices = unique(students_posts_table$postAuthor_anony),
          label = "select student name",
          selected = "O"
        )
      ),
      mainPanel(
        plotOutput("student_posts_byDate"),
        plotOutput("replyToOther_byDate")
      )
    )
  )
  
), 

  server = function(input, output) {
    output$number_posts <- renderPlot({
        # barplot
  ggplot(data = students_freq, mapping = aes(x = n, y = reorder(postAuthor_anony, n))) + 
  geom_col(na.rm = TRUE, orientation = "y") + 
  geom_vline(xintercept = median(students_freq$n), color = "red") +
  geom_text(aes(x = 43, y = 3, label = "median reference")) +
  labs( x = "numbers of posts", y = "student") +
  theme_bw()
    })
    output$reply_to_other <- renderPlot({
      ggplot(data = replyToOther_freq, mapping = aes(x = replyToOther_times, 
                                                     y = reorder(replyBy_anony, replyToOther_times))) + 
        geom_col(na.rm = TRUE, orientation = "y") + 
        geom_vline(xintercept = median(replyToOther_freq$replyToOther_times), color = "red") +
        geom_text(aes(x = 3, y = 3, label = "median reference")) +
        labs( x = "times student reply to the posts of other classmates", y = "students") +
        theme_bw()
    })
    output$positive_prof <- renderPlot({
      ggplot(data = positivie_freq, mapping = aes(x = n, 
                                                  y = reorder(postAuthor_anony, n))) + 
        geom_col(na.rm = TRUE, orientation = "y") + 
        geom_vline(xintercept = median(replyToOther_freq$replyToOther_times), color = "red") +
        geom_text(aes(x = 2, y = 3, label = "median reference")) +
        labs( x = "number of times professor gave positive comment", y = "students") +
        theme_bw()
      
    })
    output$student_posts_byDate <- renderPlot({
      ggplot(data = post_freq_student_byDate[post_freq_student_byDate$postAuthor_anony == input$student,],
             mapping = aes(x = date, y = n))  +
        geom_point() + 
        geom_line(aes(color = "selected student")) +
        geom_point(aes(x = date, y = median)) +
        geom_line(aes(x = date, y = median, color = "median reference")) +
        scale_x_date(date_breaks = "1 week") +
        ylim(min = 0, max = 20) +
        labs(y = "number of posts",
             title = "Number of Posts of Each Student by Course Session",
             color = "line color") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1))
    })
    output$replyToOther_byDate <- renderPlot({
      ggplot(data = replyToOther_byDate[replyToOther_byDate$replyBy_anony == input$student,],
             mapping = aes(x = date, y = n))  +
        geom_col() +
        scale_x_date(date_breaks = "1 week") +
        ylim(min = 0, max = 10) +
        labs(x = "course presented date",
             y = "number of posts",
             title = "number of comments each student replies to other by date",
             color = "line color") +
        theme_bw()
    })
  }
)