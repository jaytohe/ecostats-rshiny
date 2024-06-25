library(shiny)
ui <- fluidPage(
  titlePanel("Wizard UI Example"),
  mainPanel(
    tabsetPanel(
      id="wizardTabs",
      type="pills",
      tabPanel("Import Data",
        uiOutput("step1UI"),
        textOutput("step1Output")
      ),
      tabPanel("Date Select",
        uiOutput("step2UI"),
        textOutput("step2Output"),
      ),
      tabPanel("Match Calls",
        uiOutput("step3UI"),
        textOutput("step3Output"),
      ),
      tabPanel("Export Calls",
        uiOutput("step4UI"),
        textOutput("step4Output"),
      )
    )
  )
)

server <- function(input, output, session) {

  # Step 1 UI and Logic
  output$step1UI <- renderUI({
    tagList(
      textInput("step1Input", "Step 1: Enter your name"),
      actionButton("step1Next", "Next")
    )
  })

  output$step1Output <- renderText({
    req(input$step1Input)
    paste("Hello", input$step1Input)
  })

  observeEvent(input$step1Next, {
    req(input$step1Input)
    updateTabsetPanel(session, "wizardTabs", selected = "Date Select")
  })

  # Step 2 UI and Logic
  output$step2UI <- renderUI({
    req(input$step1Input) # Ensure step 1 is completed
    tagList(
      textInput("step2Input", "Step 2: Enter your age"),
      actionButton("step2Next", "Next")
    )
  })

  output$step2Output <- renderText({
    req(input$step2Input)
    paste("Your age is", input$step2Input)
  })

  observeEvent(input$step2Next, {
    updateTabsetPanel(session, "wizardTabs", selected = "Match Calls")
  })

  # Step 3 UI and Logic
  output$step3UI <- renderUI({
    req(input$step2Next > 0) # Ensure step 2 is completed
    tagList(
      textInput("step3Input", "Step 3: Enter your email"),
      actionButton("step3Next", "Next")
    )
  })

  observeEvent(input$step3Next, {
    updateTabsetPanel(session, "wizardTabs", selected = "Export Calls")
  })

  output$step3Output <- renderText({
    req(input$step3Input)
    paste("Your email is", input$step3Input)
  })


  output$step4UI <- renderUI({
    req(input$step3Next > 0) # Ensure step 2 is completed
    tagList(
      textInput("step4Input", "Step 4: Enter your address"),
      actionButton("step4Finish", "Finish")
    )
  })

  output$step4Output <- renderText({
    req(input$step4Input)
    paste("Your address is", input$step4Input)
  })

  observeEvent(input$step4Finish, {
    showModal(modalDialog(
      title = "Wizard Complete",
      "You have successfully completed all steps!",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
