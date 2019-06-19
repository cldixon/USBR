library(tidyverse)
library(rvest)
library(here)

## try except - read html
try_to_read_html <- purrr::possibly(read_html, otherwise = NA)

## web scrape of USBR dams data --
## contains catalog of dams managed
## by US Bureau of Reclamation.

## create index
root_url <- "https://www.usbr.gov/projects"

catalog_html <- root_url %>% 
  str_c('/facilities.php?type=Dam') %>% 
  read_html()
## parse html, collect html for dam info pages
dam_index <- catalog_html %>% 
  html_nodes('td > a') %>% 
  html_attr('href') %>% 
  enframe(name = NULL, value = 'link') %>% 
  filter(str_detect(link, '\\?id\\=\\d{1,}$')) %>% 
  mutate(br_id = str_extract(link, '(?<=\\?id\\=)\\d{1,}$')) %>% 
  distinct(br_id) %>% 
  ## create url to dam info page
  mutate(url = str_c(root_url, '/index.php?id=', br_id),
         html = map(url, try_to_read_html))
## view
dam_index

## functions to parse info pages ------

## name ----
get_name <- function(html) {
  name <- html %>% 
    html_node('div.Main-content > div.Main-well') %>% 
    html_node('div > h1.text-center') %>% 
    html_text(trim = TRUE)
  if (is_empty(name)) {
    NA
  } else {
    name
  }
}
try_to_get_name <- purrr::possibly(get_name, otherwise = NA)

## state_region ----
get_state_region <- function(html) {
  st_reg <- html %>% 
    html_node('div.Main-content > div.Main-well') %>% 
    html_node('div > div > p') %>% 
    html_text(trim = TRUE) %>% 
    str_remove_all('\n') %>% 
    str_replace_all('\\s{2,}', ' ')
  if (is_empty(st_reg)) {
    NA
  } else {
    st_reg
  }
}
try_to_get_state_region <- purrr::possibly(get_state_region, otherwise = NA)

## image ----
get_images <- function(html) {
  img_src <- html %>% 
    html_node('div.Main-content > div.Main-well') %>% 
    html_nodes('img') %>% 
    html_attr('src')
  if (is_empty(img_src)) {
    NA
  } else {
    img_src
  }
}
try_to_get_images <- purrr::possibly(get_images, otherwise = NA)

## overview ----
get_overview <- function(html) {
  overview <- html %>% 
    html_node('div.Main-content > div.Main-well') %>% 
    html_nodes('div#Overview > p') %>% 
    html_text(trim = TRUE) %>% 
    str_c(collapse = ' ') %>% 
    str_trim()
  if (is_empty(overview)) {
    NA
  } else {
    overview
  }
}
try_to_get_overview <- purrr::possibly(get_overview, otherwise = NA)

## details ----
get_details <- function(html) {
  deats <- html %>% 
    html_node('div.Main-content > div.Main-well') %>% 
    html_nodes('div#Details > table') %>% 
    html_table() %>% 
    bind_rows() %>% 
    spread('X1', 'X2') %>% 
    as_tibble() %>% 
    janitor::clean_names()
  if (is_empty(deats)) {
    tibble()
  } else {
    deats
  }
}
try_to_get_details <- purrr::possibly(get_details, otherwise = tibble())


## owner ----
get_owner_name <- function(html) {
  owner_name <- html %>% 
    html_node('div.Main-content > div.Main-well') %>% 
    html_node('div.contactRow > div:nth-child(1)') %>% 
    html_text(trim = FALSE) %>% 
    str_extract('(?<=Organization:).*(?=Address:)') %>% 
    str_trim()
  if (is_empty(owner_name)) {
    NA
  } else {
    owner_name
  }
}
try_to_get_owner_name <- purrr::possibly(get_owner_name, otherwise = NA)

## operator ----
get_operator_name <- function(html) {
  oper_name <- html %>% 
    html_node('div.Main-content > div.Main-well') %>% 
    html_node('div.contactRow > div:nth-child(2)') %>% 
    html_text(trim = FALSE) %>% 
    str_extract('(?<=OperatorOrganization:).*(?=Address:)') %>% 
    str_trim()
  if (is_empty(oper_name)) {
    NA
  } else {
    oper_name
  }
}
try_to_get_operator_name <- purrr::possibly(get_operator_name, otherwise = NA)


## parse info pages -----
dams_data <- dam_index %>% 
  mutate(br_id = str_c('dam_', br_id),
         name = map_chr(html, try_to_get_name),
         state_region = map_chr(html, try_to_get_state_region),
         state = str_extract(state_region, '(?<=State:).*(?=Region)'),
         region = str_extract(state_region, '(?<=Region:).*'),
         overview = map_chr(html, try_to_get_overview),
         details = map(html, try_to_get_details),
         owner = map_chr(html, try_to_get_owner_name),
         operator = map_chr(html, try_to_get_operator_name),
         images = map(html, try_to_get_images)) %>% 
  mutate_at(vars(state, region), str_trim) %>% 
  select(-c(html, state_region))

## unpack details
details_data <- dams_data %>% 
  select(br_id, details) %>% 
  unnest(details) %>% 
  select(br_id, location, longitude, latitude,
       project, dam_type, reservoir, national_id = national_id_number, 
       watercourse, original_construction, spillway_type, crest_elevation)

# join details to main data 
dams_data <- dams_data %>% 
  select(-details) %>% 
  left_join(details_data, by = 'br_id')
# view data
dams_data

## save data to three separate files
## 1. tabular dataset
## 2. text dataset (id, name, overview, etc.)
## 3. image index

## 1. tabular dataset ----
dams_data %>% 
  select(br_id, name, state, region, owner, operator, 
         location, longitude, latitude, project, dam_type, 
         reservoir,national_id, watercourse, original_construction, 
         spillway_type, crest_elevation, url) %>% 
  mutate(crest_elevation = str_extract(crest_elevation, '[\\d]{1,}\\.{0,1}\\d{0,}')) %>% 
  mutate_at(vars(longitude, latitude, crest_elevation), as.numeric) %>% 
  write_csv(here('data/dams/dams_data.csv'))

## 2. text dataset ----
dams_data %>% 
  select(br_id, name, overview, url) %>% 
  write_csv(here('data/dams/dams_text.csv'))

## 3. image index ----
## images are in base64 encoded format
## will require processing
dams_data %>% 
  select(br_id, images) %>% 
  unnest(images) %>% 
  drop_na() %>% 
  ## parse img url data
  separate(images, c('meta', 'src'), sep = ',') %>% 
  mutate(filetype = str_extract(meta, '(?<=image/).*(?=;)')) %>% 
  ## create unique img name
  group_by(br_id) %>% 
  mutate(img = str_c(br_id, '_', row_number())) %>% 
  ungroup() %>% 
  ## add file ext to img name
  mutate(img = str_c(img, '.', filetype)) %>% 
  select(src, img) %>% 
  ## save to file
  write_csv(here('data/dams/img_index.csv'))
  
