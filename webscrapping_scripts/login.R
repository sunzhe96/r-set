# web scraping library
library("xml2", "rvest")

# login to the moodle website ---------------------------------------------
login <- function(username, password) {
  ## this is the login page of NCKU moodle
  login_page <- "https://moodle.ncku.edu.tw/login/index.php"
  ## simulate a session
  login_session <- html_session(login_page)
  ## parse login form which is the second element here
  login_form <- html_form(login_session)[[2]]
  ## fill in account and password
  filled_form <- set_values(login_form,
                            username = username,
                            password = password)
  ## login procedure
  submit_form(login_session, filled_form)
  assign("login_session", login_session, envir = globalenv())
}

# login with NCKU moodle username and password
login("username", "password")

# return html by url after login-----------------------------
login_read_html <- function(url) {
  ## simulate open course forum page after login
  page <- jump_to(login_session,
                        url,
                  options = "RECOVER")
  return(read_html(page))
}