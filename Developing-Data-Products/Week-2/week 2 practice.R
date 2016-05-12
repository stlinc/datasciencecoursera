library(devtools)

# Install slidify libraries
# options(download.file.method = "wininet")
# install_github('slidify', 'ramnathv')
# install_github('slidifyLibraries', 'ramnathv')
# Failed, probably caused by proxy control
# Download zip from https://api.github.com/repos/ramnathv/slidify/zipball/master
# Unzip it and rename folder names as slidify
# Build the package
build("./slidify", binary=FALSE)
# Install package locally
install.packages("D:/Coursera Data Science Specialization/Developing Data Products/Week 2/slidify_0.5.tar.gz", repos = NULL, type = "source")
# Download zip from https://api.github.com/repos/ramnathv/slidifyLibraries/zipball/master
# Unzip it and rename folder names as slidifyLibraries
# Build the package
build("./slidifyLibraries", binary=FALSE)
# Install package locally
install.packages("D:/Coursera Data Science Specialization/Developing Data Products/Week 2/slidifyLibraries_0.3.1.tar.gz", repos = NULL, type = "source")

# load Slidify
library(slidify)

# Set the working directory to create the Slidify project
setwd("./Slidify Presentation")
author("first_deck")
slidify("index.Rmd")
browseURL("index.html")

# First, log in to GitHub and create a new empty repository.
# Use the following command, but replace user with your username and repo with the name of your new repository (both arguments are strings).
publish_github(stlinc, datasciencecoursera)

# Interactive slidify examples
# http://slidify.github.io/dcmeetup/demos/interactive/

# RStudio Presenter guide
# https://support.rstudio.com/hc/en-us/articles/200486468-Authoring-R-Presentations
# Quick start is file then New File then R Presentation

# Quiz
# Q 1. In the slidify YAML text. Changing the framework from io2012 to shower does what?
# A: It changes the html5 framework thus changing the style of the slides.

# Q 2.You wrote R code in a slidify document 
# If you want to hide the results of the CODEsummaryCODE statement (yet still have it run) what should you do?
# A: Add a results = 'hide' option in the {r} call of the code chunk

# Q 3. You wrote R code in a slidify document. If you want to display the results, but not the actual code, what should you do?
# A: Add a echo = FALSE option in the {r} call of the code chunk

# Q 4. R studio presentation tool does what?
# A: Creates HTML5 slides using a generalized markdown format having an extention Rpres and creates reproducible presentations by embedding and running the R code from within the presentation document.

# Q 5. In Rstudio presenter, if you do not want the code to be evaluated, what option do you need to add to the {r} options?
# A: eval = FALSE

# Q 6. When presenting data analysis to a broad audience, which of the following should be done?
# A: Present results in the chronological order in which it was performed. # Wrong answer, but don't know why.
