# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 

########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################


golem::fill_desc(
  pkg_name = "zipfJuxtaposer", # The Name of the package containing the App 
  pkg_title = "zipfJuxtaposer", # The Title of the package containing the App 
  pkg_description = "Package to compare two zipfian ranked lists of components.", # The Description of the package containing the App 
  author_first_name = "Jonathan", # Your First Name
  author_last_name = "St-Onge", # Your Last Name
  author_email = "jonathanstonge7@gmail.com", # Your Email
  repo_url = "https://github.com/jstonge/zipfJuxtaposer" # The URL of the GitHub Repo (optional) 
)     

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license( "Golem User" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## Favicon ----
# If you want to change the favicon (default is golem's one)
# golem::use_favicon() # path = "path/to/ico". Can be an online file. 
# golem::remove_favcicon()

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

