#setting up website

## install dependencies in terminal:
# brew install git golang hugo


## get packages
renv::restore()

library(blogdown)

## install academic website
dir.create("website")
blogdown::install_hugo(force=TRUE) # to make sure the latest version of hugo is used

blogdown::new_site(dir = "website",theme="wowchemy/starter-academic")



# note to get to an older hugo version, for instance 0.58.3:
#if (blogdown::hugo_version() != "0.58.3") blogdown::install_hugo(version ="0.58.3", force =TRUE, use_brew = FALSE)

# moving and modifying folders

file.rename("website/content/home", "trash/website")
file.rename("home", "website/content/home")
#file.rename("website/layouts", "trash/layout")
file.rename("layouts", "website/layouts")
file.rename("website/content/project", "trash/project")
dir.create("website/content/project")

file.rename("website/config/_default/menus.toml", "trash/menus.toml")
file.rename("website_individualisation/menus.toml", "website/config/_default/menus.toml")



## change website name:



# # run website
# blogdown::stop_server()
# setwd("website")
# blogdown::build_site()
# blogdown::serve_site()
# setwd("../")

#file.rename( "trash", "website/content/home", recursive=TRUE)
