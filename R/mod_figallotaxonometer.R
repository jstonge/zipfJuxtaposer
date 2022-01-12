#' figallotaxonometer UI Function
#'
#' @description A shiny Module.
#'
# Builds theme object to be supplied to ui
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_figallotaxonometer_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      div(id = "header",
          labeled_input("prev_city_btn", "Prev.",
                        actionButton(ns('prev_sys'), icon('arrow-left'))),
          labeled_input(
            'sys-selector1', "System 1",
            selectizeInput(ns('system1'), label = NULL, choices = NULL)
            ),
          labeled_input(
            "sys-selector2", "System 2",
            selectizeInput(ns('system2'), label = NULL, choices = NULL)
            ),
          labeled_input("next_city_btn", "Next",
                        actionButton(ns('next_sys'), icon('arrow-right'))),
      ),
      fluidRow(
        column(1,
          br(),br(),br(),br(),br(),br(),
          selectInput(ns("example"), "Example", choices = c("Girl baby names", "Boy baby names")),
          br(),
          fileInput(ns("upload"), NULL, buttonLabel = "Upload...",  accept = c(".csv", ".tsv"), multiple = TRUE),
          br(),
          selectInput(ns("instrument"), "Instrument",  choices = "Rank"),
          br(),
          shinyWidgets::sliderTextInput(
            inputId = ns("a"),
            label = "alpha",
            choices = c(round(1:18/12, 2), 1, 2, 5, Inf),
            selected = 1
          ),
          br(),
          selectInput(ns("shift"), "Type Shifts",  
                      choices = list("Proportion", "Shannon", "Tsallis", "KLD", "JSD"), 
                      selected = "JSD")
        ),
        column(7,
            plotOutput(ns("plot"), height = "95vh")
        ),
        column(4, br(), 
            plotOutput(ns("shifterator_plot"), height = "84vh")
        )
          
      )
    )
  )
}
    
#' figallotaxonometer Server Functions
#'
#' @noRd
mod_figallotaxonometer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    n <- reactive({ 
      if (isTruthy(input$upload)) { input$upload$name } 
      else { 
        if (input$example == "Girl baby names") { names(elem_girls) }
        else { names(elem_boys) }
        }
      })

    # Actualize choices according to provided files
    observeEvent(n(), {
      updateSelectInput(inputId = "system1", choices = n()[1:(length(n())-1)], selected = n()[1]) 
      updateSelectInput(inputId = "system2", choices = n()[2:length(n())],     selected = n()[2]) 
    })

    # Keep track of index to filter provided files
    system_idx <- reactive({
      req(input$system1)
      req(input$system2)
      idx_1 <- which(n() == input$system1)
      idx_2 <- which(n() == input$system2)
      c(idx_1, idx_2)
      })
    
    # If next btn is clicked, update selected to next index.
    observeEvent(input$next_sys, {
      current_idx <-  system_idx()
      len_all_idx <- length(n())
      if ((current_idx[2]+1) <= len_all_idx) {
        updateSelectizeInput(inputId = "system1", choices = n()[1:(len_all_idx-1)], selected = n()[current_idx[1]+1])
        updateSelectizeInput(inputId = "system2", choices = n()[2:len_all_idx],     selected = n()[current_idx[2]+1])
      } 
    })
    
    # If prev btn is clicked, update selected to previous index.
    observeEvent(input$prev_sys, {
      current_idx = system_idx()
      len_all_idx <- length(n())
      if ((current_idx[1]-1) >= 1) {
        updateSelectizeInput(inputId = "system1", choices = n()[1:(len_all_idx-1)], selected = n()[current_idx[1]-1])
        updateSelectizeInput(inputId = "system2", choices = n()[2:len_all_idx],     selected = n()[current_idx[2]-1])
      } 
    })
    
    # Main function to load and combine distributions
    data <- reactive({
      
      if (isTruthy(input$upload)) {
        # checks
        req(input$upload)
        stopifnot(length(input$upload$datapath) >= 2)
        
        validate(
          need(system_idx()[1] != system_idx()[2], "Please select different systems")
        )
        
        read_dat <- function(path) {
          DT <- data.table::fread(path)
          validate(
            need(all(c("types", "counts") %in% colnames(DT)), "Data provided must have types and counts as column names")
          )
          data.table::setcolorder(DT, c("types", "counts"))
          DT[, ("totalunique") := nrow(DT)]
          DT[, ("probs") := DT$counts / sum(DT$counts)]
          
          return(DT)
        }
        
        e1 <- read_dat(input$upload$datapath[system_idx()[1]])
        e2 <- read_dat(input$upload$datapath[system_idx()[2]])
      
      } else { # load examples
        if (input$example == "Girl baby names") {
          e1 <- elem_girls[[system_idx()[1]]]
          e2 <- elem_girls[[system_idx()[2]]]
      } else {
          e1 <- elem_boys[[system_idx()[1]]]
          e2 <- elem_boys[[system_idx()[2]]]
        }
      }
      
      combine_distributions(e1, e2)
      
    })
    
    system_names <- function() {
      if (isTruthy(input$upload)) { files <- input$upload$datapath[1] }
      else { files <- names(elem_boys)[1] } # boy or girl doesnt matter
      ext <- tools::file_ext(files)
      tmp_fname <- gsub(glue::glue("\\.{ext}"), "", c(input$system1, input$system2))
      gsub("-", " ", tmp_fname)
    }
    
    # Actual plotting
    output$plot <- renderPlot({
      diamond_plot(data(), as.double(input$a), system_names())
      }, res = 96) |>
      bindCache(data(), as.double(input$a))

    output$shifterator_plot <- renderPlot({
      shifterator_plot(data(), as.double(input$a), input$shift, system_names())
    }, res = 96) |>
      bindCache(data(), as.double(input$a), input$shift)
    
  })
}

