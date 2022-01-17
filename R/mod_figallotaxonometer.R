# report_path <- tempfile(fileext = "pdf")
# file.copy("flipbook.pdf", report_path, overwrite = TRUE)

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
          actionButton(ns("save"), "Save"),
          textOutput(ns("print")),
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
        ),
      ),
      div(class = "bottom_sweep",
          actionButton(ns("sweep"), "Generate flipbook")
          ),
      div(class = "small_text", "You like the results? You can download the data:"),br(),
      downloadButton(ns("flipbook"), "Download flipbook"),
    )
  )
}
    
#' figallotaxonometer Server Functions
#'
#' @noRd
#' 
#' @importFrom grid viewport unit grid.layout popViewport grid.newpage pushViewport grid.draw
#' @importFrom grDevices dev.off pdf
mod_figallotaxonometer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    options(warn = -1) # ignore warning

    n <- reactive({
      if (isTruthy(input$upload)) {
        input$upload$name
      } else {
        if (input$example == "Girl baby names") {
          names(elem_girls)
        } else {
          names(elem_boys)
        }
      }
    })

    # Actualize choices according to provided files
    observeEvent(n(), {
      updateSelectInput(inputId = "system1", choices = n()[1:(length(n()) - 1)], selected = n()[1])
      updateSelectInput(inputId = "system2", choices = n()[2:length(n())], selected = n()[2])
    })

    # Keep track of index to filter provided files
    system_idx <- reactive({
      req(input$system1)
      req(input$system2)
      idx_1 <- which(n() == input$system1)
      idx_2 <- which(n() == input$system2)
      c(idx_1, idx_2)
    })


    # Next and prev btns ------------------------------------------------------

    observeEvent(input$next_sys, {
      current_idx <- system_idx()
      len_all_idx <- length(n())
      if ((current_idx[2] + 1) <= len_all_idx) {
        updateSelectizeInput(inputId = "system1", choices = n()[1:(len_all_idx - 1)], selected = n()[current_idx[1] + 1])
        updateSelectizeInput(inputId = "system2", choices = n()[2:len_all_idx], selected = n()[current_idx[2] + 1])
      }
    })

    observeEvent(input$prev_sys, {
      current_idx <- system_idx()
      len_all_idx <- length(n())
      if ((current_idx[1] - 1) >= 1) {
        updateSelectizeInput(inputId = "system1", choices = n()[1:(len_all_idx - 1)], selected = n()[current_idx[1] - 1])
        updateSelectizeInput(inputId = "system2", choices = n()[2:len_all_idx], selected = n()[current_idx[2] - 1])
      }
    })

    # Main function to load and combine distributions -------------------------

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

    data <- reactive({
      if (isTruthy(input$upload)) {
        # checks
        req(input$upload)
        stopifnot(length(input$upload$datapath) >= 2)

        validate(
          need(system_idx()[1] != system_idx()[2], "Please select different systems")
        )

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
      if (isTruthy(input$upload)) {
        files <- input$upload$datapath[1]
      } else {
        files <- names(elem_girls)[1]
      } # boy or girl doesnt matter
      ext <- tools::file_ext(files)
      tmp_fname <- gsub(glue::glue("\\.{ext}"), "", c(input$system1, input$system2))
      gsub("-", " ", tmp_fname)
    }


    # Actual plotting ---------------------------------------------------------


    r <- reactiveValues(i = 1, dp = NULL, sp = NULL)

    output$plot <- renderPlot({
        r$dp <- diamond_plot(data(), as.double(input$a), system_names())
        print(r$dp, vp = viewport(
          angle = 45 * 5,
          width = unit(0.75, "npc"),
          height = unit(0.75, "npc")
        ))
      }, res = 96 ) |> bindCache(data())

    output$shifterator_plot <- renderPlot({
        r$sp <- shifterator_plot(data(), as.double(input$a), input$shift, system_names())
        r$sp
      }, res = 96 ) |> bindCache(data(), input$shift)


    # Saving ------------------------------------------------------------------


    saved <- reactiveValues(plots = list())

    observeEvent(input$save, {
      saved$plots[[r$i]] <- r$dp
      saved$plots[[r$i + 1]] <- r$sp
      r$i <- r$i + 2
    })


    # Generate flipbook -------------------------------------------------------


    observeEvent(input$sweep, {
      len_all_idx <- length(n())
      current_idx <- system_idx()
      names_girl <- names(elem_girls)
      names_boys <- names(elem_boys)

      withProgress(message = "Sweeping through", value = 0, {
        plot_elems <- function(idx_i, idx_j) {
          if (isTruthy(input$upload)) {
            # checks
            req(input$upload)

            e1 <- read_dat(input$upload$datapath[idx_i])
            e2 <- read_dat(input$upload$datapath[idx_j])
            fnames <- c(input$upload$name[[idx_i]], input$upload$name[[idx_j]])
          } else { # load examples
            if (input$example == "Girl baby names") {
              e1 <- elem_girls[[idx_i]]
              e2 <- elem_girls[[idx_j]]
              fnames <- c(names_girl[[idx_i]], names_girl[[idx_j]])
            } else {
              e1 <- elem_boys[[idx_i]]
              e2 <- elem_boys[[idx_j]]
              fnames <- c(names_boys[[idx_i]], names_boys[[idx_j]])
            }
          }

          incProgress(1 / idx_left, detail = paste("Doing pair", fnames[1], "-", fnames[2]))
          mixedelements <- combine_distributions(e1, e2)
          saved$plots[[plot_idx[which(indices_i == idx_i)]]] <- diamond_plot(mixedelements, input$a, fnames)
          saved$plots[[plot_idx[which(indices_i == idx_i)] + 1]] <- shifterator_plot(mixedelements, input$a, input$shift, fnames)
        }

        # Dealing with indices to take into account
        # whatever users is currently looking at.
        indices_j <- current_idx[2]:len_all_idx
        idx_left <- length(indices_j)
        indices_i <- seq(current_idx[1], len_all_idx)[1:idx_left]
        plot_idx <- seq(1, idx_left * 2, 2) # index for plots starts from 1, and increment by 2
        # because we use i+1 to specify secondary plot on same page
        # so lenght(plot_idx) might be =20, but there really are 40 plots

        purrr::map2(.x = indices_i, .y = indices_j, ~ plot_elems(.x, .y))
      })
    })

    output$print <- renderText({
      nb_pages <- length(saved$plots) / 2
      ifelse(test = nb_pages < 2,
        yes = paste0("Flibook has ", nb_pages, " page"),
        no = paste0("Flibook has ", nb_pages, " pages")
      )
    })


    # Download ----------------------------------------------------------------


    output$flipbook <- downloadHandler(
      filename = function() {
        name <- system_names()[1]
        paste0(input$a, "a-", input$shift, "shift", Sys.Date(), ".pdf")
      },
      content = function(file) {
        nb_plots <- length(saved$plots)
        pdf(file, width = 13, height = 8, pointsize = 8)
        withProgress(message = "Writing to file", value = 0, {
          for (i in seq(1, nb_plots, 2)) {
            incProgress(1 / (nb_plots / 2))

            grid.newpage()
            pushViewport(viewport(layout = grid.layout(1, 2)))

            # 1
            pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
            pushViewport(viewport(
              x = unit(3.5, "inches"), angle = 45 * 5,
              width = unit(.9, units = "npc"),
              height = unit(.9, units = "npc")
            ))
            grid.draw(ggplotify::as.grob(saved$plots[[i]]))
            popViewport()

            # 2
            pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 1))
            pushViewport(viewport(
              x = unit(10, "inches"), y = unit(4.1, "inches"),
              width = unit(0.8, units = "npc"),
              height = unit(7.7, "inches")
            ))
            grid.draw(ggplotify::as.grob(saved$plots[[i + 1]]))
            popViewport()
          }
        })
        dev.off()
      }
    )
  })
}