library(tidyverse)
library(base64enc)
library(here)

## decode base64 image files

## load image index
img_index <- read_csv(here('data/dams/img_index.csv'))
# view
img_index


## function: decode images --
decode_img <- function(img, src) {
  outconn <- file(img, 'wb')
  base64decode(src, outconn)
}
try_to_decode_img <- purrr::possibly(decode_img, otherwise = NA)


## decode images
img_index %>% 
  mutate(img_dwn = map2_dbl(img, src, ~try_to_decode_img(here('data/dams/images', .x), .y)))
