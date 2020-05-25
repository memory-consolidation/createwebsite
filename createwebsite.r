#setting up website
## get packages
packrat::restore()

library(blogdown)

## install academic website
dir.create("website")
blogdown::install_hugo(force=TRUE) # to make sure the latest version of hugo is used
blogdown::new_site(dir = "website",theme="gcushen/hugo-academic")

# note to get to an older hugo version, for instance 0.58.3:
#if (blogdown::hugo_version() != "0.58.3") blogdown::install_hugo(version ="0.58.3", force =TRUE, use_brew = FALSE)

# moving and modifying folders

file.rename("website/content/home", "trash")
file.rename("home", "website/content/home")
file.rename("layouts", "website/layouts")


blogdown::serve_site()

#file.rename( "trash", "website/content/home", recursive=TRUE)
