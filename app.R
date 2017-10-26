library(rjson)
library(shiny)

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

GetChamps <- function (data) {
  champs <- names(data)
}
  
GetChampStats <- function (data,champ) {
  champ_stats <- data[[champ]][['stats']]
}

patches <- GetPatches()
data <- GetPatchData(patches[1])
champs <- GetChamps(data)



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
  
  plotOutput(outputId = "stats1"),
  plotOutput(outputId = "stats2")
)

server <- function (input, output) {
  df1 <- reactive({
    stats <- GetChampStats(data, input$champ1)
    stats <- unlist(stats)
  })
  df2 <- reactive({
    stats <- GetChampStats(data, input$champ2)
    stats <- unlist(stats)
  })
  
  output$stats1 <- renderPlot({
    plot(df1())
  })
  output$stats2 <- renderPlot({
    plot(df2())
  })
}

shinyApp(ui = ui, server = server)