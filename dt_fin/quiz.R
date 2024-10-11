library(tidyverse)
library(lubridate)
library(inspectdf)
library(stringr)

# q1
data("diamonds")
diamonds[150,]
diamonds[5,4]
unique(diamonds$cut)

data('presidential')
dim(presidential)

data('mpg')
mpg

#q2
data(iris)
iris <- gather(iris, key = 'variable', value = 'measurement',`Sepal.Length`:`Petal.Width`)
iris
as_tibble(iris)

share_prices <- tibble(
  bank=c("ANZ", "CBA", "NAB", "WBC"),
  june=c(28.2, 82.8, 26.7, 28.4),
  july=c(27.9, 82.3, 28.5, 28.6)
)
share_prices  
gather(share_prices, key='month', value='price', june:july)

dmy('01-01-2000')
as.integer(dmy("29-02-2020")- dmy("01-01-2000"))

messy_text <- c("I am 175 cm tall and I'm 24 years old.",
                "My father is 52 years old and 185 cm tall.", 
                "We're both taller than Queen Elizabeth II, who
                  stands at 163 cm, but is way older than us at 93 years.")

tibble(height= str_match(messy_text, " (\\d+) cm")[,2], ages= str_match(messy_text, " (\\d+) years")[,2])

data("gss_cat")
unique(gss_cat$partyid)
counts <- count(gss_cat,partyid)
filter(counts,n>=3000)

test_string <- "abc (15) xyz"
str_match(test_string, "abc (\\(\\d*)")[,2]
