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
  print(champdata)
  # Populate data empty data frame. 
  for (i in patchdata) {
    # Create vector of champ's name same length as number of categories.
    champ <- rep(i$name, length(i$stats))
    # Create vectors of stat categories and values.
    value <- i$stats
    category <- names(i$stats)
    # Bind columns together.
    x <- as.data.frame(cbind(champ,category,value))
    # Clear out superfluos names.
    row.names(x) <- c()
    # Bind rows to final data frame to complete construction.
    champdata <- rbind(champdata,x)
  }
  champdata$champ <- unlist(champdata$champ)
  champdata$category <- unlist(champdata$category)
  champdata$value <- unlist(champdata$value)
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
    ggplot(data = df(), aes(category,value, shape = champ, color = champ)) + 
      geom_point(size = 5)
  })
}

shinyApp(ui = ui, server = server)