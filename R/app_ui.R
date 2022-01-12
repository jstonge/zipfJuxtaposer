#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    fluidPage(
      theme = bslib::bs_theme(
        bootswatch = "litera",
        base_font = bslib::font_google("Sora")
      ) |> bslib::bs_add_rules(sass::sass_file(app_sys("app/www/style.scss"))),
      div(
        id = "app-title",
        titlePanel("Compare two zipfian systems"),
      ),
      mod_figallotaxonometer_ui("figallotaxonometer_ui_1")
    )
  )
}


#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'zipfJuxtaposer'
    ),
    # Add here other external resources
    
  )
}
