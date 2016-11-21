loginurl    = "https://www.optionslam.com/accounts/login/"
agent       = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36"
#accept      = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
#accept_enc  = "gzip, deflate, br"
#accept_lang = "en-US,en;q=0.8"
#conn        = "keep-alive"
dataurl     = "https://www.optionslam.com/earnings/stocks/AAPL?page=-1"
# Plan:
# GET base loginpage to establish cookies
# Read csrftoken cookie value from cookie jar
# POST base loginpage with username, password, csrfmiddlewaretoken, next=/
# GET dataurl, assign to variable
# run readHTMLTable on previous variable

if(require(RCurl) && url.exists(loginurl)) {
  curl = getCurlHandle()
  curlSetOpt(cookiejar="cookies.txt",
             cookiefile="cookies.txt",
             useragent = agent,
             followlocation = TRUE,
             curl=curl)
  
  #tt =  getURL(u, userpwd = "jecosgro:<pass>")
  #myTable = readHTMLTable(tt)
  response = getURL(loginurl, curl=curl, write=basicTextGatherer())
  rm(curl)
  gc()
  csrftoken = sub(".*(csrftoken)\t", "", grep("csrftoken",readLines("cookies.txt"),value=T))
  
  curl = getCurlHandle()
  curlSetOpt(cookiejar="cookies.txt",
             cookiefile="cookies.txt",
             useragent = agent,
             followlocation = TRUE,
             curl=curl)
  
  params = list(username="username", #change me
             password="password", #change me
             csrfmiddlewaretoken=csrftoken)
  postForm(loginurl, .params=params, curl=curl)
}