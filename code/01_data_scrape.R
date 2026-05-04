library(rvest)
library(tidyverse)

base_url <- "https://locations.chipotle.com"

ohio_page <- read_html("https://locations.chipotle.com/oh")

# city links
links <- ohio_page %>% html_elements("a")

city_links <- tibble(
  city = html_text2(links),
  url = html_attr(links, "href")
) %>%
  filter(!is.na(url)) %>%
  mutate(url = url_absolute(url, base_url)) %>%
  filter(str_detect(url, "locations\\.chipotle\\.com/oh/[^/]+$"))

city_links <- city_links[!duplicated(city_links$url), ]

# function for store links
get_store_links <- function(city_url) {
  page <- read_html(city_url)
  links <- page %>% html_elements("a")
  
  df <- tibble(
    store_name = html_text2(links),
    store_url = html_attr(links, "href")
  ) %>%
    filter(!is.na(store_url)) %>%
    mutate(store_url = url_absolute(store_url, base_url)) %>%
    filter(str_detect(store_url, "locations\\.chipotle\\.com/oh/[^/]+/[^/]+$"))
  
  df[!duplicated(df$store_url), ]
}

store_links <- map_dfr(city_links$url, get_store_links)

# function for store details
get_store_info <- function(store_url) {
  page <- read_html(store_url)
  text <- page %>% html_text2()
  
  address <- str_extract(text, "\\d+[^\\n]+, OH \\d{5}|\\d+[^\\n]+ OH \\d{5}")
  
  tibble(
    store_url = store_url,
    address = address,
    city = str_extract(address, "(?<= )[^,]+(?=, OH)|(?<= )[A-Za-z ]+(?= OH \\d{5})"),
    state = "OH",
    zip = str_extract(address, "\\d{5}")
  )
}

chipotle_locations <- map_dfr(store_links$store_url, get_store_info)

chipotle_locations <- chipotle_locations[!duplicated(chipotle_locations$store_url), ]

chipotle_locations
nrow(chipotle_locations)

write_csv(chipotle_locations, "chipotle_ohio_locations.csv")