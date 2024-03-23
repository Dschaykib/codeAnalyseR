# script to create the DESCRIPTION file

# Remove default DESC and NEWS.md
unlink("DESCRIPTION")
unlink("NEWS.md")


# initial files -----------------------------------------------------------

# Create a new description object
my_desc <- desc::description$new("!new")
my_news <- newsmd::newsmd()

# Set your package name
my_desc$set("Package", "codeAnalyseR")
# Set license
my_desc$set("License", "MIT + file LICENSE")

# Remove some author fields
my_desc$del("Maintainer")
# Set the version
my_desc$set_version("0.0.0.9000")
# The title of your package
my_desc$set(Title = "Creation of NEWS.md File")
# The description of your package
my_desc$set(Description =
  paste0("Analyse your custom R scripts."))
# The urls
# my_desc$set("URL", "https://github.com/Dschaykib/newsmd")
# my_desc$set("BugReports",
#             "https://github.com/Dschaykib/newsmd/issues")

#Set authors
my_desc$set("Authors@R",
            paste0("person('Jakob', 'Gepp',",
                   "email = 'jakob.gepp@yahoo.de',",
                   "role = c('cre', 'aut'))"))

# set R version
my_desc$set_dep("R", type = desc::dep_types[2], version = ">= 4.2.1")

# set suggests
my_desc$set_dep("testthat", type = desc::dep_types[3], version = "*")
my_desc$set_dep("newsmd", type = desc::dep_types[3], version = "*")
my_desc$set_dep("desc", type = desc::dep_types[3], version = "*")
my_desc$set_dep("origin", type = desc::dep_types[3], version = "*")
my_desc$set_dep("roxygen2", type = desc::dep_types[3], version = "*")
my_desc$set_dep("usethis", type = desc::dep_types[3], version = "*")
my_desc$set_dep("devtools", type = desc::dep_types[3], version = "*")

# set dependencies
# my_desc$set_dep("R6", type = desc::dep_types[1])



# initial functions -------------------------------------------------------

my_desc$bump_version("minor")
my_news$add_version(my_desc$get_version())

my_news$add_subtitle("Initial function setup")
my_news$add_bullet(c("add findredundance and get_network",
                     "setup package"))


# WIP ---------------------------------------------------------------------

# bump dev version
#my_desc$bump_version("dev")
#my_news$add_version(my_desc$get_version())
#my_news$add_bullet(c("current dev version"))


# save everything ---------------------------------------------------------

my_desc$set("Date", Sys.Date())
my_desc$write(file = "DESCRIPTION")
my_news$write(file = "NEWS.md")

# set CRAN version number in README
my_readme <- readLines("README.md")
my_readme[1] <- paste0(
  "# codeAnalyseR - ", my_desc$get_version(),
  " <img src=\"misc/news.png\" width=170 align=\"right\" />")

writeLines(my_readme, "README.md")

# update renv packages if needed
renv::clean()
renv::snapshot(prompt = TRUE)


# set pkg names
origin::originize_pkg()

# update documentation
roxygen2::roxygenise()
# tidy DESCRIPTON
usethis::use_tidy_description()

