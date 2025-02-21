library(shiny)
library(kmeansClustering)

ui <- fluidPage(
  titlePanel('k-means clustering'),
  sidebarLayout(
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(iris)),
      selectInput('ycol', 'Y Variable', names(iris), selected=names(iris)[[2]]),
      numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
    ),
    mainPanel(plotOutput('plot1'))
  )
)
server <- function(input, output, session) {
  # Reactive expression to select the relevant data based on user input
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]  # Select the specified columns from the iris dataset
  })

  # Reactive expression to compute K-means clustering using your custom function
  clusters <- reactive({
    kmeans_clustering(selectedData(), input$clusters)
  })

  # Render the plot using the custom plotting function
  output$plot1 <- renderPlot({
    plot_clusters(selectedData(), clusters())
  })
}
shinyApp(ui, server)
