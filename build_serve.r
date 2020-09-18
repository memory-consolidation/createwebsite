# build_serve.r

# run website
blogdown::stop_server()
setwd("website")
blogdown::build_site()
blogdown::serve_site()
setwd("../")