install.packages("httpuv"")
library(httr)
require(httpuv)
require(jsonlite)

oauth_endpoints("github")
myapp <- oauth_app("github", key = "x", secret = "x")
# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)
