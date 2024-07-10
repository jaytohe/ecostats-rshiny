library(shiny)
library(shinyFiles)
library(fs)
library(purrr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Select File"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      shinyFilesButton("spectoCSV", "File select", "Please select a file", multiple = TRUE, viewtype = "detail"),
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("spectoCSVTable"),
      uiOutput("images")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "spectoCSV", roots = volumes, session = session)
  
  #Get CSV metadata i.e. name, size and datapath of a file.
  csvMetaData <- reactive({
    req(!is.integer(input$spectoCSV))
    parseFilePaths(volumes, input$spectoCSV)
  })
  
  # Extract absolute path of csv file and save it as reactive expression.
  csvData <- reactive({
    read.csv(csvMetaData()$datapath)
  })
  
  # Construct name of images to be used as IDs for each imageOutput
  imgNames <- reactive({
    purrr::map_chr(csvData()$otherColumn, \(p) paste0("image", p))
  })
  
  output$spectoCSVTable <- renderTable({
    csvData()
  })
  
  # Render as many image output html elements as there are image names
  # For each imageOutput element, give it the image name as id.
  output$images <- renderUI({
    purrr::map(imgNames(), imageOutput)
  })
  
  # Observe because we are dynamically registering output
  # This is considered a side effect and it doesn't work with reactive (Trust me, I tried and it didn't work lol)
  observe({
    for (i in seq_along(imgNames())) {
      # Create a local environment for each iteration to force i to be in scope of reactive output
      # Without it, i is just the final loop value
      local({
        my_i <- i # Assign i to my_i so that each iteration has unique value of i
        outId <- imgNames()[[my_i]]
        output[[outId]] <- renderImage({
          
          # Get the absolute path to the image according to csv file path provided by shinyFiles
          imgAbsPath <- path(csvMetaData()$datapath) %>% 
            path_dir() %>% 
            path(csvData()$filePath[[my_i]])
          
          list(src = imgAbsPath,
               contentType = "image/png",
               deleteFile = FALSE,
               width = 200,
               height = 200)
        }, deleteFile = FALSE)
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
