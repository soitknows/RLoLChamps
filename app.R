library(rjson)

vers_url <- "https://ddragon.leagueoflegends.com/api/versions.json"
versions <- fromJSON(vers_url)
versions <- versions[!grepl("lolpatch", versions)]


GetChampStats <- function (patch) {
    url_beg <- "http://ddragon.leagueoflegends.com/cdn/"
    url_end <- "/data/en_US/champion.json"
    url <- paste(url_beg, patch, url_end, sep = "")
    stats <- fromJSON(url)
}