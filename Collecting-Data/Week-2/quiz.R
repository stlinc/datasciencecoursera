install.packages("httpuv"")
library(httr)
require(httpuv)
require(jsonlite)

oauth_endpoints("github")
myapp <- oauth_app("github", key = "5c6295466425f8509b9f", secret = "734f8f363b12ab27e5d3970a8d6797164e641751")
# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)
