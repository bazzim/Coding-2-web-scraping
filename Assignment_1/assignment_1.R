library(rvest)
library(data.table)

rm(list=ls())

# website of interest:  https://www.sciencenews.org/

## create a function which downloads information from a url to dataframe (from sciencenews.org)
get_sciencenews_page <- function(my_url){
  print(my_url)
  t <- read_html(my_url)
  
  boxes <- t %>% html_nodes('.post-item-river__content___2Ae_0')
  
  x <- boxes[[1]]
  boxes_dfs <- lapply(boxes, function(x){
    tl <- list()
  
    tl[['title']] <- paste0( x %>% html_nodes('.post-item-river__title___J3spU') %>% html_text(), collapse = ' ')
    tl[['link']] <- paste0( x %>% html_nodes('.post-item-river__title___J3spU > a') %>% html_attr('href'))
    tl[['excerpt']] <- paste0( x %>% html_nodes('.post-item-river__excerpt___3ok6B') %>% html_text(), collapse = ' ')
    tl[[ 'date' ]] <- paste0( x %>%  html_nodes('.published') %>% html_text())
    tl[[ 'author' ]] <-  paste0( x %>% html_nodes('.n') %>% html_text())
    tl[[ 'topic' ]] <-  paste0( x %>% html_nodes('.post-item-river__eyebrow___33ASW')%>% html_text())
    
    return(tl)
  })
  df <- rbindlist(boxes_dfs, fill = T)
  return(df)
}


# create a function which requires two arguments. First a keyword then a number of pages to download.
get_searched_pages <- function(searchterm, pages_to_download) {
  
  # concat the search terms together according to url
  searchterm <- gsub(' ','+',searchterm)
  
  # create links
  if (pages_to_download == 1){
    
    links_to_get <- paste0('https://www.sciencenews.org/?s=',searchterm)
    
  }
  else{
    links_to_get <- c(paste0('https://www.sciencenews.org/?s=', searchterm),
                      paste0('https://www.sciencenews.org/page/', 2:pages_to_download, '?s=', searchterm))
  }
  ret_df <- rbindlist(lapply(links_to_get, get_sciencenews_page))
  return(ret_df)
}

# testing function get_searched_pages
df2 <- get_searched_pages('artificial intelligence',2)

my_url <- "https://www.sciencenews.org/?s=machine+learning"
# apply function 1 "get_sciencenews_page"
df <- get_sciencenews_page(my_url)


# save the outputs of get_sciencenews_page() to a csv file
write.csv(df, 'sciencenews_output.csv')

# save a single object to file
saveRDS(df, "sciencenews_output.rds")

# apply function 2 "get_searched_pages()"
df2 <- get_searched_pages('machine learning',3)

# save the outputs of get_searched_pages() to a csv file
write.csv(df2, 'searched_pages_output.csv')

# save a single object to file
saveRDS(df2, "searched_pages_output.rds")
