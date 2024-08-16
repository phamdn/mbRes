# Load the shiny and mbRes packages
library(shiny)
library(mbRes)

# Define the UI
ui <- fluidPage(
  # Title
  titlePanel("mbRes: Exploration of Multiple Biomarker Responses using Effect Size"),

  # Sidebar with file input and run button
  sidebarLayout(
    sidebarPanel(
      # File input widget
      fileInput("file", "Choose a CSV file",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Run button
      actionButton("run", "Run analysis"),
      # Figure width input
      numericInput("width", "Figure width (inches)", value = 15),
      # Figure height input
      numericInput("height", "Figure height (inches)", value = 4),
      # Figure resolution input
      numericInput("res", "Figure resolution (dpi)", value = 1200)
    ),
    # Main panel with mess, es, idx, fig.delta, fig.sval and fig.avg outputs and download buttons
    mainPanel(
      textOutput("mess"),
      tableOutput("es"),
      downloadButton("download_es", "Download es"),
      tableOutput("idx"),
      downloadButton("download_idx", "Download idx"),
      plotOutput("fig.delta"),
      downloadButton("download_fig.delta", "Download fig.delta"),
      plotOutput("fig.sval"),
      downloadButton("download_fig.sval", "Download fig.sval"),
      plotOutput("fig.avg"),
      downloadButton("download_fig.avg", "Download fig.avg")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Reactive expression to read the csv file
  data <- reactive({
    req(input$file) # Require a file to be uploaded
    read.csv(input$file$datapath) # Read the csv file
  })

  # Reactive expression to run the analysis using mbRes
  analysis <- eventReactive(input$run, {
    req(data()) # Require data to be available
    mbRes::mbr(data()) # Run the mbr function using mbRes
  })

  # Render the mess output
  output$mess <- renderText({
    req(analysis()) # Require analysis to be done
    analysis()$mess # Show the mess from the analysis
  })

  # Render the es output
  output$es <- renderTable({
    req(analysis()) # Require analysis to be done
    analysis()$es # Show the es from the analysis
  })

  # Render the idx output
  output$idx <- renderTable({
    req(analysis()) # Require analysis to be done
    analysis()$idx # Show the idx from the analysis
  })

  # Render the fig.delta output
  output$fig.delta <- renderPlot({
    req(analysis()) # Require analysis to be done
    mbRes::visual(analysis(), rotate = TRUE)$fig.delta # Plot the fig.delta from the analysis using visual function
  })

  # Render the fig.sval output
  output$fig.sval <- renderPlot({
    req(analysis()) # Require analysis to be done
    mbRes::visual(analysis(), rotate = TRUE)$fig.sval # Plot the fig.sval from the analysis using visual function
  })

  # Render the fig.avg output
  output$fig.avg <- renderPlot({
    req(analysis()) # Require analysis to be done
    mbRes::visual(analysis(), rotate = TRUE)$fig.avg # Plot the fig.avg from the analysis using visual function
  })

  # Download handler for the es table
  output$download_es <- downloadHandler(
    filename = function() {
      paste("es-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(analysis()$es, file) # Save the es table as a csv file
    }
  )

  # Download handler for the idx table
  output$download_idx <- downloadHandler(
    filename = function() {
      paste("idx-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(analysis()$idx, file) # Save the idx table as a csv file
    }
  )

  # Download handler for the fig.delta plot
  output$download_fig.delta <- downloadHandler(
    filename = function() {
      paste("fig.delta-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$width, height = input$height, units = "in", res = input$res) # Save the fig.delta plot as a png file
      print(mbRes::visual(analysis(), rotate = TRUE)$fig.delta) # Plot the fig.delta from the analysis using visual function
      dev.off() # Turn off the device
    }
  )

  # Download handler for the fig.sval plot
  output$download_fig.sval <- downloadHandler(
    filename = function() {
      paste("fig.sval-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$width, height = input$height, units = "in", res = input$res) # Save the fig.sval plot as a png file
      print(mbRes::visual(analysis(), rotate = TRUE)$fig.sval) # Plot the fig.sval from the analysis using visual function
      dev.off() # Turn off the device
    }
  )

  # Download handler for the fig.avg plot
  output$download_fig.avg <- downloadHandler(
    filename = function() {
      paste("fig.avg-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = input$width, height = input$height, units = "in", res = input$res) # Save the fig.avg plot as a png file
      print(mbRes::visual(analysis(), rotate = TRUE)$fig.avg) # Plot the fig.avg from the analysis using visual function
      dev.off() # Turn off the device
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
