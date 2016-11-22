library(httr)
library(xml2)
library(XML)
library(dplyr)
r = GET("https://www.optionslam.com/accounts/login/")
sys.sleep(1)
cookies(r)
token=cookies(r)[1,]$value
mypass = .rs.askForPassword("Optionslam password:")
body = list(username="username", #change me
            password=mypass,
            csrfmiddlewaretoken=token)
p = POST("https://www.optionslam.com/accounts/login/", 
         body = body,
         encode = "form", # defaults to multipart but optionslam wants form
         add_headers('Referer' = "https://www.optionslam.com/accounts/login/",
                     'Origin'  = "https://www.optionslam.com"))
rm(mypass)
sys.sleep(1)
#debug
#http_status(p)
#headers(p)
r2 = GET("https://www.optionslam.com/earnings/stocks/SBUX?page=-1")
sys.sleep(1)
#http_status(r2)
t = readHTMLTable(content(r2, "text"))

# Clean up the raw results
raw_past_earnings = t[[5]]
trim1_past_earnings = slice(raw_past_earnings, 22:(nrow(raw_past_earnings)-5))
past_earnings = select(trim1_past_earnings, -V4, -V11, -V16)

# Add names for easy subsetting (and next call)
table_headings = c("EARNINGS DATE",	"PRE EARNINGS CLOSE", "POST EARNINGS OPEN",
                   "OPEN", "HIGH", "LOW", "CLOSE", "CLOSE%", "MAX%", "MEAN", "MEDIAN",
                   "MEAN RAW", "MEDIAN RAW")
colnames(past_earnings) = table_headings

# Grab the last 12 dates for later...
foo = as.numeric(sub("%", "", past_earnings$`CLOSE%`[1:12]))/100