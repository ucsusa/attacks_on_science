##Washington Post scraping
library(polite)

url_test <- polite::bow("https://www.washingtonpost.com/nation/2025/08/03/tennessee-quadruple-murder-manhunt/", force = TRUE, user_agent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36') %>% 
  scrape(., content = "text/html", accept = "html") %>%
  html_node("body") %>%
  html_text2() %>%
  as.character()

library(rvest)

library(httr)
members  <-  GET("https://www.washingtonpost.com/nation/2025/08/03/tennessee-quadruple-murder-manhunt/"), authenticate("jbarbatidajches@ucs.org", "j9NLhmU7X2!5jPU"))
members_html <- html(members)

library(RCurl)
members_html <- getURL("https://www.washingtonpost.com/subscribe/signin/?next_url=https%3A%2F%2Fwww.washingtonpost.com%2F&nid=top_pb_signin&arcId=&account_location=ONSITE_HEADER_HOMEPAGE&itid=nav_sign_in", userpwd = "jbarbatidajches@ucs.org:j9NLhmU7X2!5jPU")
members_html

handle <- handle("https://www.washingtonpost.com/")
path   <- "subscribe/signin/?next_url=https%3A%2F%2Fwww.washingtonpost.com%2F&nid=top_pb_signin&arcId=&account_location=ONSITE_HEADER_HOMEPAGE&itid=nav_sign_in"

login <- list(
  amember_login = "jbarbatidajches@ucs.org"
  ,amember_pass  = "j9NLhmU7X2!5jPU"
  ,amember_redirect_url = 
    "https://www.washingtonpost.com/nation/2025/08/03/alabama-child-sex-trafficking-bunker-brent/"
)


url <- "https://www.washingtonpost.com/nation/2025/08/03/tennessee-quadruple-murder-manhunt/"

file <- download(url)
txt <- read_html(url)

require(RCurl)
require(XML)
webpage <- getURL("https://www.washingtonpost.com/nation/2025/08/03/tennessee-quadruple-murder-manhunt/") ##Doesn't work, gets this error -- Error in function (type, msg, asError = TRUE)  : 
#HTTP/2 stream 1 was not closed cleanly: INTERNAL_ERROR (err 2)

webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)


webpage <- read_html(url, skip = 3)
article_body <- webpage %>% html_nodes("body") %>% html_text() %>% paste(collapse = "\n")

response <- POST(handle = handle, path = path, body = login)




url <- "https://www.washingtonpost.com/subscribe/signin/?next_url=https%3A%2F%2Fwww.washingtonpost.com%2F&nid=top_pb_signin&arcId=&account_location=ONSITE_HEADER_HOMEPAGE&itid=nav_sign_in"

pgsession <- session_jump_to(session(url))

pgform  <- html_form(pgsession)

filled_form <- set_values(pgform, username = "jbarbatidajches@ucs.org", password <- "j9NLhmU7X2!5jPU")

submit_form(pgsession, filled_form)
article <- jump_to(pgsession, "https://www.washingtonpost.com/nation/2025/08/03/alabama-child-sex-trafficking-bunker-brent/")

page <- html(article)

usernames <- html_nodes(x = page) 

data_usernames <- html_text(usernames, trim = TRUE) 




library(chromote)
grab_text3 <- function(url_test) {
  
  b <- ChromoteSession$new()
  #b$view() # Opens a browser window where you can log in
  #b$go_to("https://www.washingtonpost.com/subscribe/signin") 
  
  #b <- ChromoteSession$new()
  #cookies <- readRDS("C:/Users/kellickson/OneDrive - Union of Concerned Scientists/data_analysis/aos_analysis/data/authentication_cookies.rds")
  #b$Network$setCookies(cookies)
  #b$go_to("https://www.washingtonpost.com/subscribe/signin/?next_url=https%3A%2F%2Fwww.washingtonpost.com%2F&nid=top_pb_signin&arcId=&itid=nav_sign_in")
  Sys.sleep(1)
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")
  Sys.sleep(1)
  b$Page$navigate("https://www.washingtonpost.com/nation/2025/08/03/tennessee-quadruple-murder-manhunt/")
  Sys.sleep(1)
  
  with_user_agent <- b$Runtime$evaluate("document.querySelector('html').outerHTML")$result$value
  Sys.sleep(1)
  url_test <- read_html(with_user_agent) %>% html_nodes("p") %>% html_text() %>% as.character() %>% paste(., collapse = ". ")
  b$close(wait = FALSE)
  Sys.sleep(1)
  
  
  #New York Times scraping
  
  #This did not work:
  
  
  grab_text4 <- function(url_test){
    nyt_url <- screened_rss_feed_db_split$URL
    nyt_username <- "kmellickson@gmail.com"
    nyt_password <- "Run2BetterProtections!"
    
    # Create a list of form data
    login_data <- list(
      username_field_name = nyt_username, password_field_name = nyt_password)
    
    # Send the POST request to log in
    session <- httr::POST(nyt_url, body = login_data, encode = "form")
    
    url_test <- polite::bow(session, force = TRUE, user_agent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36') %>% 
      scrape(.) %>%
      html_node("body") %>%
      html_text2() %>%
      as.character()}
  
  grab_text5 <- function(url_test){
    possibly(grab_text4, otherwise = NA)
  }
  
  
  ##Didn't work for E&E News
  grab_text <- function(url_test){
    url_test <- polite::bow(url_test, force = TRUE, user_agent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36') %>% 
      scrape(., content = "text/html") %>%
      html_node("body") %>%
      html_text2() %>%
      as.character()}
  
  grab_text2 <- possibly(grab_text, otherwise = NA)
  
  
  
  ##More words
  
  
  
  ##More words
  #GET("https://thehill.com/homenews/administration/5405741-mike-pence-donald-trump-epstein-files/", user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36")))
  
  ##Set up user agent
  #headers = c(
  # `user-agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36')
  
  #GET("https://thehill.com/homenews/administration/5405741-mike-pence-donald-trump-epstein-files/", user_agent("'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36'"))
  
  # new sessio, set userAgent, value grabbed from current Chrome wof Windows
  #master_link <- "https://thehill.com/homenews/administration/5405741-mike-pence-donald-trump-epstein-files/"
  