### Purpose ### 

# This file contains the code to scrape and download most popular NY Times articles 
# on the Palestinian issue between the beginning and ending of First Intifada 

apikey = 'rWleicWrIuYJL2RGu6e4TCWQLK0JI45x'

term <- "palestine" # Any article that mentions Palestine
begin_date <- "19871208" #Intifada begins
end_date <- "19930913" #Intifada ends

baseurl <- "http://api.nytimes.com/svc/search/v2/articlesearch.json"


nytimes_search <- function(term, n, begin_date, end_date, apikey){ 
  
  # Function to download n (most popular) articles mentioning the term 
  # Palestine between the begin date and end date 
  
  baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term, 
                    "&begin_date=",begin_date,"&end_date=",end_date,
                    "&facet_filter=true&api-key=",apikey, sep="") # Constructed URL

  n <- ceiling(n/10) # Results come in batches of 10. 
  x <- vector('list', n) # Empty list to store results 
  
  for (i in seq_len(n)){
    
    # Iterate over all pages 
    search <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame()
    # Store results
    x[[i+1]] <- search
    # Prevent Crash
    Sys.sleep(10)
}
  x <- rbind_pages(x) #Bind together the rows 
  return(x)
}

articles <- nytimes_search(term = 'Palestine', n = 2000, begin_date = begin_date, end_date = end_date, apikey = apikey)


x <- lapply(articles$response.docs.keywords, function(x) {x[1, ]})
keywords <- bind_rows(x)

write_csv(articles, "C:\\Users\\SARTHAK\\Desktop\\LSE ASDS\\Assignments\\MY472\\final-assignment-SarthakSaluja1-main\\articles.csv")
write_csv(keywords, "C:\\Users\\SARTHAK\\Desktop\\LSE ASDS\\Assignments\\MY472\\final-assignment-SarthakSaluja1-main\\keywords.csv")
