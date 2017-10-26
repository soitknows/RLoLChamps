library(rjson)

GetPatches <- function (){
  vers_url <- "https://ddragon.leagueoflegends.com/api/versions.json"
  versions <- fromJSON(file = vers_url)
  versions <- versions[!grepl("lolpatch", versions)]
}

GetPatchStats <- function (patch) {
    url_beg <- "http://ddragon.leagueoflegends.com/cdn/"
    url_end <- "/data/en_US/champion.json"
    url <- paste(url_beg, patch, url_end, sep = "")
    stats <- fromJSON(file = url)
}

GetChampStats <- function(stats,champ) {
  stats <- data[['data']]
  champ_stats <- stats[champ]
}