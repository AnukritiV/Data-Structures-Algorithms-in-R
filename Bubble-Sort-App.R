# app.R
# Bubble Sort Visualizer using R Shiny

library(shiny)

# ---------------------------
# Bubble Sort Function
# ---------------------------
bubble_sort <- function(arr) {
  
  # Store steps for visualization
  steps <- list()
  step_count <- 1
  
  n <- length(arr)
  
  # Save initial array
  steps[[step_count]] <- paste("Initial Array:", paste(arr, collapse = ", "))
  
  for (i in 1:(n - 1)) {
    
    swapped <- FALSE
    
    for (j in 1:(n - i)) {
      
      # Compare adjacent elements
      if (arr[j] > arr[j + 1]) {
        
        # Swap
        temp <- arr[j]
        arr[j] <- arr[j + 1]
        arr[j + 1] <- temp
        
        swapped <- TRUE
        
        # Save each sorting step
        step_count <- step_count + 1
        steps[[step_count]] <- paste(
          "Step", step_count - 1, ":",
          paste(arr, collapse = ", ")
        )
      }
    }
    
    # Optimization:
    # Stop if already sorted
    if (!swapped) {
      break
    }
  }
  
  list(
    sorted_array = arr,
    steps = steps
  )
}

# ---------------------------
# User Interface
# ---------------------------
ui <- fluidPage(
  
  titlePanel("Bubble Sort Visualizer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput(
        inputId = "array_input",
        label = "Enter numbers separated by commas:",
        #value = "23,1,26,3,13,20,9"
      ),
      
      actionButton(
        inputId = "sort_btn",
        label = "Sort Array",
        class = "btn-primary"
      ),
      
      br(), br(),
      
      helpText("Example: 23,1,26,3,13,20,9")
    ),
    
    mainPanel(
      
      h3("Sorted Array"),
      verbatimTextOutput("sorted_output"),
      
      h3("Sorting Steps"),
      verbatimTextOutput("steps_output")
    )
  )
)

# ---------------------------
# Server Logic
# ---------------------------
server <- function(input, output) {
  
  observeEvent(input$sort_btn, {
    
    # Convert input string into numeric vector
    arr <- as.numeric(trimws(unlist(strsplit(input$array_input, ","))))
    
    # Validate input
    validate(
      need(
        !any(is.na(arr)),
        "Please enter valid numeric values separated by commas."
      )
    )
    
    # Run Bubble Sort
    result <- bubble_sort(arr)
    
    # Display sorted array
    output$sorted_output <- renderPrint({
      result$sorted_array
    })
    
    # Display sorting steps
    output$steps_output <- renderPrint({
      cat(unlist(result$steps), sep = "\n")
    })
  })
}

# ---------------------------
# Run the App
# ---------------------------
shinyApp(ui = ui, server = server)