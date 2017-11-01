library(rjson)
library(shiny)
library(ggplot2)

GetPatches <- function (){
  vers_url <- "https://ddragon.leagueoflegends.com/api/versions.json"
  versions <- fromJSON(file = vers_url)
  versions <- versions[!grepl("lolpatch", versions)]
}

GetPatchData <- function (patch) {
    url_beg <- "http://ddragon.leagueoflegends.com/cdn/"
    url_end <- "/data/en_US/champion.json"
    url <- paste(url_beg, patch, url_end, sep = "")
    data <- fromJSON(file = url)
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
  champdata$champ <- unlist(champdata$champ)
  champdata$category <- unlist(champdata$category)
  champdata$value <- unlist(champdata$value)
  champdata$normalized <- unlist(champdata$normalized)
  champdata
}

patches <- GetPatches()
currentpatch <- GetPatchData(patches[1])
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

  plotOutput(outputId = "stats")
)

server <- function (input, output) {
  df <- reactive({
    patchdata <- GetPatchData(input$patch)
    champdata <- GetChampData(patchdata)
    champdata <- champdata[champdata$champ == input$champ1 | 
                             champdata$champ == input$champ2,]
  })
  output$stats <- renderPlot({
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