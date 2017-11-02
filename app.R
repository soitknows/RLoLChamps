# app.R
#
# Copyright (C) 2017 Andrew Tarver
# Author: Andrew Tarver (http://github.com/soitknows)
# Style Guide: https://google.github.io/styleguide/Rguide.xml
#
# RLoLChamps
#
# Description: R Shiny app to compare League of Legends champion stats. Two 
#     champions can be compared at a time. Patch version is also selectable.

library(rjson)
library(shiny)
library(ggplot2)

# Get list of patch versions.
GetPatches <- function (){
  vers_url <- "https://ddragon.leagueoflegends.com/api/versions.json"
  versions <- fromJSON(file = vers_url)
  # Extract only the valid patch versions, e.g. exclude ones prefixed with lolpatch.
  versions <- versions[!grepl("lolpatch", versions)]
}

# Get champion data for specific patch version.
GetPatchData <- function (patch) {
    url_beg <- "http://ddragon.leagueoflegends.com/cdn/"
    url_end <- "/data/en_US/champion.json"
    url <- paste(url_beg, patch, url_end, sep = "")
    data <- fromJSON(file = url)
    # Extract the data node where each champ's base stats are stored.
    data <- data[['data']]
}

GetChampData <- function (patchdata) {
  # Empty data frame to be populated.
  champdata <- data.frame(champ = character(), category = character(), vlaue = numeric())
  # Populate data empty data frame. 
  for (i in patchdata) {
    # Create vector of champ's name that is same length as number of categories.
    champ <- rep(i$name, length(i$stats))
    # Create vectors of stat categories and values.
    category <- names(i$stats)
    value <- i$stats
    # Create vector of log transformations of stat values because stat orders of magnitued car vary greatly.
    normalized <- sapply(value, function(i) { log10(i + 10) })
    #normalized <- sapply(normalized, log10)
    # Bind columns together.
    newdata <- as.data.frame(cbind(champ, category, value, normalized))
    # Clear out superfluos names not needed in data frame.  
    row.names(newdata) <- c()
    # Bind rows to final data frame to complete construction.
    champdata <- rbind(champdata, newdata)
  }
  # Unlist each column to aid in plotting.
  champdata$champ <- unlist(champdata$champ)
  champdata$category <- unlist(champdata$category)
  champdata$value <- unlist(champdata$value)
  champdata$normalized <- unlist(champdata$normalized)
  champdata
}

# Get list of patches for ui pick list.
patches <- GetPatches()
# Get patch data for most recent patch.
currentpatch <- GetPatchData(patches[1])
# Get list of champ names for pui pick list.
champs <-  names(currentpatch)

ui <- fluidPage(
  selectInput(inputId = "patch",
              label = "Patch",
              choices = patches),

  selectInput(inputId = "champ1",
              label = "Champion 1",
              choices = champs),
  
  selectInput(inputId = "champ2",
              label = "Champion 2",
              choices = champs),

  plotOutput(outputId = "stats_orig"),
  plotOutput(outputId = "stats_log")
)

server <- function (input, output) {
  df <- reactive({
    patchdata <- GetPatchData(input$patch)
    champdata <- GetChampData(patchdata)
    champdata <- champdata[champdata$champ == input$champ1 | 
                             champdata$champ == input$champ2,]
  })
  # Plot champ base statistic comparisons.
  output$stats_orig <- renderPlot({
    ggplot(data = df(), aes(category, value, shape = champ, color = champ, group = champ)) + 
      geom_point(size = 5) +
      geom_line() +
      theme(axis.text.x  = element_text(angle=50, vjust=0.5, size=12)) +
      scale_x_discrete(breaks=c(unique(df()$category)),
                       labels=c("Health", "Health / Level", "Mana Pool", 
                                "Mana Pool / Level","Move Speed", "Armor", 
                                "Armor / Level", "Spellblock", "Spellblock / Level",
                                "Range", "Health Regen", "Health Regen / Level", 
                                "Mana Regen", "Mana Regen / Level", "Crit", 
                                "Crit / Level", "Attack Damage", "Attack Damage / Level", 
                                "Attack Speed Offset", "Attack Speed / Level"))
  })
  # Plot log transformation of champ base statistic comparisons.
  output$stats_log <- renderPlot({
    ggplot(data = df(), aes(category, normalized, shape = champ, color = champ, group = champ)) + 
      geom_point(size = 5) +
      geom_line() +
      theme(axis.text.x  = element_text(angle=50, vjust=0.5, size=12)) +
      scale_x_discrete(breaks=c(unique(df()$category)),
                       labels=c("Health", "Health / Level", "Mana Pool", 
                                "Mana Pool / Level","Move Speed", "Armor", 
                                "Armor / Level", "Spellblock", "Spellblock / Level",
                                "Range", "Health Regen", "Health Regen / Level", 
                                "Mana Regen", "Mana Regen / Level", "Crit", 
                                "Crit / Level", "Attack Damage", "Attack Damage / Level", 
                                "Attack Speed Offset", "Attack Speed / Level"))
  })
}

shinyApp(ui = ui, server = server)