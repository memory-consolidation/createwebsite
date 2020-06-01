library(readr)
library (dplyr)

#additional libraries for images stuff
library (magick)
library (data.table)

options(download.file.method="libcurl")

nanull <- function (isnullterm){
  if (is.null(isnullterm)){NA}else {isnullterm}
}

expandorcid <- function(orcid_numberslist){
  #to test:  orcid_numberslist =orcidnumberlist
  orcidlist =data.frame(orcid= orcid_numberslist)
  
  orcidlist$givenn_name = NA
  orcidlist$family_name = NA
  orcidlist$people_code = NA
  ## get links
  orcidlist$bio_fo = NA
  ## links
  orcidlist$githublink_fo=NA
  orcidlist$twitterlink_fo = NA
  orcidlist$picturelink_fo = NA
  orcidlist$lablink_fo = NA
  
  for (i in c(1: nrow(orcidlist))){
    a= as.character(orcidlist$orcid[i])
    print(a)
    b=rorcid::orcid_id(a)[[1]]
    urlname=b$`researcher-urls`$`researcher-url`$`url-name`
    url=b$`researcher-urls`$`researcher-url`$url.value
    
    # name
    orcidlist$givenn_name [i]= nanull(b$name$`given-names`)
    orcidlist$family_name [i]=nanull(b$name$`family-name`)
    ## add code without space and '
    orcidlist$people_code_orcid [i]=gsub("[^a-zA-Z0-9]", "-",tolower(paste0(b$name$`given-names`,"-",b$name$`family-name`)))
    
    orcidlist$bio_fo[i] = nanull(b$biography$content)
    ## links
    orcidlist$githublink_fo[i]= nanull(url[grepl("github", url)][1])
    orcidlist$twitterlink_fo[i] = nanull(url[grepl("twitter", url)][1])
    orcidlist$picturelink_fo[i] = nanull(url[grepl("picture", urlname)][1])
    orcidlist$lablink_fo[i] = nanull(url[grepl("lab", urlname)][1])
  }
  return(orcidlist)
}

repl_from_orcid <- function (ori,orcid){
  if (is.na(orcid)){ori} else {orcid}
}