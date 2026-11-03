library(httr2)
library(rvest)
urls<-c('https://en.wikipedia.org/wiki/Conservatism',
        'https://en.wikipedia.org/wiki/Liberalism'
        )
filterFunction<-function(page){
  links<-page|>
    html_elements("#mw-content-text p a")|>
    html_attr('href')
  outlinks<-links[str_which(links, "https://en.wikipedia.org/wiki/")]|>
    str_replace("#.+$", "")|>
    unique()
  
  newlinks<-table(outlinks)|>
    sort(decreasing=T)
  
  newlinks<-names(newlinks)
  return(newlinks)
}


visitFunction <- function() {
  visited_urls <- c()
  function(urls, maxurls=100) {
    linkdata <- list()
    
    while(length(urls)>0 & length(visited_urls)<maxurls) {
      i<-urls[1]
      if (!i %in% visited_urls) {

        page <- read_html(i)
        visited_urls <<- c(visited_urls, i)
        newlinks <- filterFunction(page)
        linkdata[[i]] <- newlinks
        urls<-urls[-1L]
      }
    }
    return(linkdata)
  }
  
}


pageVisitor<-visitFunction()



out<-pageVisitor(urls)
for(i in 1:length(out)){
  out2<-pageVisitor(out[[i]][1:5])
}
d<-c(out, out2, out3)
