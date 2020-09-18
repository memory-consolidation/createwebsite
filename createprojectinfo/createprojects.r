# serve website:
HUGO_VERSION = "0.75.1"
if (blogdown::hugo_version() != HUGO_VERSION) blogdown::install_hugo(version =HUGO_VERSION, force =TRUE, use_brew = FALSE)

setwd("createprojectinfo")
###--------
source ("funct-dependencies.r")


###----------------------------------PROJECTS--PROJECTS--projects(with links to people)--
##get folder with the information sheets

projects<- read_delim("projectlist.csv",
                       ",", trim_ws = TRUE, skip = 1, na=character())

people<- read_delim("peoplelist.csv",
                         ",", trim_ws = TRUE, skip = 0, na=c("", "NA"))

##  get orcid information:
orcidlist1 = expandorcid(unique(people$orcidnum[!is.na (people$orcidnum)]))


people_sfb2 = left_join(people, orcidlist1, by = c("orcidnum" = "orcid"))
# update info with orcid information as default.
people_sfb = people_sfb2 %>%
  mutate (orcid = paste0("https://orcid.org/",orcidnum))%>%
  mutate (github = ifelse (is.na(githublink_fo), github,githublink_fo)) %>%
  mutate (bio = ifelse (is.na(bio_fo), bio, bio_fo)) %>%
  mutate (twitter = ifelse (is.na(twitterlink_fo), twitter, twitterlink_fo)) %>%
  mutate (homepage = ifelse (is.na(lablink_fo), homepage, lablink_fo)) %>%
  mutate (avatar = ifelse (is.na(picturelink_fo),avatar ,picturelink_fo))%>%
  #mutate (people_code = ifelse (!is.na(people_code_orcid), people_code_orcid,peoplecode))%>%
  mutate (people_code = paste0("sfb-",gsub("[^a-zA-Z0-9]", "",people_code))) %>%
  mutate (avatar= ifelse (grepl(".jpg",avatar),avatar,""))
  

# get avatar out if not an jpg image


#View(people_sfb)




##---------------------------------------- make projects



template = readLines("projects_template.md")

for (i in c(1: nrow(projects))){
  templatenew = template
  templatenew =sub ("THISISTHETITLE", projects$Title[i],templatenew)
  ### this needs to be changed:
  peoproj =people_sfb %>% filter(grepl (substring(projects$hash[i],9), Project)) %>%
    select(peoplecode)%>%
    pull()
  
  print(paste0( '"',paste0(peoproj, collapse = '","'), '"'))
  
  templatenew =sub ("heretheautors", paste0( '"',paste0(peoproj, collapse = '","'), '"'),templatenew)
  templatenew =sub ("IMAGECAPTION", projects$featured_image_caption[i],templatenew)
  #### erase second line when RG gets the right function (parametrised url)
  MAINTEXT2 = paste0('<iframe src ="https://sdash.sourcedata.io/dashboard?search=',projects$hash[i],'" height=1000px width=90% ></iframe>')
  
  #MAINTEXT2 = paste0('<iframe src ="https://sdash.sourcedata.io/dashboard" height=1000px width=90% ></iframe>')
  templatenew =sub ("maintexthere", projects$description [i],templatenew )
  templatenew =sub ("SFgallerylink", MAINTEXT2,templatenew )
  
  outdirectory= paste0("../website/content/project/",substring(projects$hash[i],9))
  dir.create(outdirectory, showWarnings = FALSE)
  writeLines(templatenew, paste0(outdirectory,"/index.md") )
  # deprecated, new images created
  #if (isTRUE(projects$new_image[i] == "1")) file.copy (paste0(seafilefolder,"projectsimages/",substring(projects$hash[i],9),".png"),
  #                                             paste0(outdirectory,"/featured.png"), overwrite = TRUE)
}



###----------------------------------authors--authors--authors-- Make authors (only ones with update and code set)
## avatar will be in order: default avatar, twitter avatar, orcid linked avatar, manually added avatar in folder



update =  as.data.frame(people_sfb) #%>% filter (people_code != "") #%>% filter(update == "yes")

update[is.na(update)] <- ""

p_template =  readLines("authors_template.md")

for (i in c(1: nrow(update))){
  # create directory
  pdirectory =paste0("../website/content/authors/",update$peoplecode[i])
  dir.create(pdirectory, showWarnings=FALSE)
  
  # create index
  templatenew = p_template
  if (update$Name[i]== "") {templatenew = sub ("DISPLAYNAME",paste0(update$`Last name`[i], " ", update$`First name`[i]),templatenew)}
  templatenew =sub ("DISPLAYNAME", update$Name[i],templatenew)
  templatenew =sub ("USERNAME", update$peoplecode[i],templatenew)
  templatenew =sub ("HEREROLE", update$role[i],templatenew)
  templatenew =sub ("HERESHORTBIO", gsub("\"","'",update$bio [i]),templatenew)
  
  SOCIAL = paste0("\n- icon: globe \n  icon_pack: fas \n  link: ",update$homepage[i])
  HERETEXT = update$bio [i] # bigraphy text is either orcid bio, twitter description  (in order of preference)
  
  ## twitter info integration, most rewritten if orcid info exists
  if (update$twitter [i] != ""){
    SOCIAL = paste0(SOCIAL,"\n- icon: twitter \n  icon_pack: fab \n  link: ",update$twitter [i])
    
    ## add twitter description, will be rewritten if there are orcid information
    tweetname = substring(update$twitter[i],21)
    a=rtweet::lookup_users(tweetname)
    HERETEXT = a$description
    
    ## add twitter picture, will be rewritten if there are orcid information, will not write if there is already a picture
    if (!file.exists(paste0(pdirectory,"/avatar.jpg"))){
      
      download.file(sub("_normal.", ".",a$profile_image_url),paste0(pdirectory,"/avatar.jpg"), mode ="wb")
    }
    
  }
  
  ## orcid info link + bio
  if (update$orcidnum[i] != ""){
    SOCIAL = paste0(SOCIAL,"\n- icon: orcid \n  icon_pack: ai \n  link: ",update$orcid[i])
    if (!is.na(update$bio_fo [i]))    HERETEXT = update$bio_fo [i] # bigraphy text is either orcid bio, twitter description  (in order of preference) or default text
  }
  
  ## add github link
  if (update$github[i] != ""){
    SOCIAL = paste0(SOCIAL,"\n- icon: github \n  icon_pack: fab \n  link: ",update$github[i])
  }
  
  ## add avatar picture
  if (update$avatar[i] != "" & update$avatar[i] != "done"){
    download.file(update$avatar[i],paste0(pdirectory,"/avatar.jpg"), mode ="wb")
  }
  
  templatenew =sub ("HERESOCIAL", SOCIAL,templatenew)
  
  templatenew =sub ("HERETEXT", HERETEXT,templatenew)
  
  writeLines(templatenew, paste0(pdirectory,"/_index.md") )
  
  # add default avatar image if none present:
  if (!file.exists(paste0(pdirectory,"/avatar.jpg"))){
    file.copy ("avatar.jpg",
               paste0(pdirectory,"/avatar.jpg"))
  }
  
  
}

# for tests
#writeLines(templatenew, "test.md" )


##---------------------------------------- Featured images (based on data read above)


# function to create image from the main image given on seafile and avatars  given in website (can be set with createprojects.r)
featureimage <- function(project,people_sfb = people,   heightfeature = 230,
                         border =3,
                         widthfeature = 450) {
  
  ## getting people slide:
  # selecting people from that project, who have an author page:
  goodone =lapply (people_sfb$Project, function (x){ project %in% names(fread(text=paste0("\n ",x)))})
  selectedpeople =people_sfb[unlist(goodone),] %>% filter (peoplecode != "")
  # get and append all people images, + resiz
  
  imagep = image_blank (77,heightfeature)
  if (length (selectedpeople$peoplecode)> 0){
    peoplefaces_path = paste0("../website/content/authors/",selectedpeople$peoplecode, "/avatar.jpg")
    
    imagep =
      image_read(peoplefaces_path) %>%
      image_modulate( saturation = 10)%>%
      image_resize("100x")%>%
      image_crop ("100x100", gravity ="Center")%>%
      image_annotate(selectedpeople$Name, gravity = "south", size = "9", boxcolor = "light grey")%>%
      image_append( stack = TRUE) %>%
      image_resize(paste0("77x", heightfeature)) %>%
      image_extent (paste0("77x", heightfeature), gravity = "North")
  }
  
  
  Pwidth= image_info(imagep)$width
  ## get main image, box around it, and append with people slider
  imagemain = image_blank (widthfeature-Pwidth-2*border, heightfeature-2*border)
  if (file.exists(paste0("projectsimages/", project,".png"))) {
    imagemain=
      paste0("projectsimages/", project,".png") %>%
      image_read() %>%
      image_resize(paste0(widthfeature-Pwidth-2*border,"x", heightfeature-2*border)) %>%
      image_extent (paste0(widthfeature-Pwidth-2*border,"x", heightfeature-2*border), gravity = "Center")%>%
      image_border(geometry = paste0(border,"x",border))
  }
  
  
  Image = image_append(c(imagemain, imagep))
  return (Image)
}

# for testing
# featureimage ("A04")


## create and save the file
for (theproject in substring (projects$hash,9)) {
  #print (theproject)
  theproject %>%
    featureimage(people_sfb,border = 2) %>%
    image_write(path = paste0("../website/content/project/",theproject,"/featured.png"), format = "png")
}
#
#
#
#
# ### helper functions used before
# ### add project to people list
#
# # people_sfb$project = NA
# #
# # projectlists= substring (projects$hash,9)
# # for (project in projectlists){
# #   print (project)
# #   people_sfb$project[grep(project, people_sfb$bio)] = project
# # }
# #
# # people_sfb$project
# # write_delim(people_sfb,paste0(seafilefolder,"sfb1315_people2.csv"),
# #             delim="\t")
#
# i=1
# substring(projects$hash [i],9)
#
# peoproj =people_sfb %>% filter(grepl (substring(projects$hash [i],9), project)) %>%
#   select(people_code)%>%
#   pull()
#
# paste0( '"',paste0(peoproj, collapse = '","'), '"')
#
setwd(../")
