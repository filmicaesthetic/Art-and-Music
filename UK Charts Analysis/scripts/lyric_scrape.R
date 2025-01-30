## scrape

pacman::p_load(dplyr, rvest, stringr)

all_years_list <- "https://en.wikipedia.org/wiki/Lists_of_UK_top-ten_singles"

url_2023 <- "https://en.wikipedia.org/wiki/List_of_UK_top-ten_singles_in_2023"

html_2023 <- url_2023 |>
  read_html()

tbl_2023 <- (html_2023 |>
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[4]') |>
  html_table())[[1]] |>
  filter(grepl("Singles", Weeksintop10) == FALSE) |>
  mutate(Single = gsub('\\"', "", str_extract(Single, '\\".*\\"'))) |>
  mutate(Artist_nofeat = gsub("( featuring).*", "", Artist))

srch_list <- tbl_2023 |>
  mutate(search_url = paste0("https://search.azlyrics.com/search.php?q=", gsub(" ", "+", tolower(paste(Single, Artist_nofeat, sep = " "))), "&x=e77a0ee1c54110fde67430e34b4619ed31e54257d84089667ed043e046719173"))

## get song lyric url

search_url <- "https://search.azlyrics.com/search.php?q=anti-hero+taylor+swift&x=e77a0ee1c54110fde67430e34b4619ed31e54257d84089667ed043e046719173"

get_lyric_url <- function(search_url) {
  
  lyric_url <- search_url |>
    read_html() |>
    html_node(".table.table-condensed") |>
    html_node("a") |>
    html_attr("href")
  
  return(lyric_url)
  
}

lyric_list <- srch_list |>
  rowwise() |>
  mutate(lyric_url = get_lyric_url(search_url))

search_url |>
  read_html() |>
  html_elements("a") |>
  html_attr("ng-href")

## get lyrics

song_url <- "https://genius.com/Oliver-tree-and-robin-schulz-miss-you-lyrics"

song_url |>
  read_html() |>
  html_nodes(".Lyrics__Container-sc-1ynbvzw-6.YYrds") |>
  html_text2()
