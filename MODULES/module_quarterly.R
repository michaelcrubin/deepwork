## Module Create New Platform user


### UI MODULE -----------------
Quarterly_UI <- function(id, params) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(10, offset = 0,
             shiny::fluidRow(shiny::h2("Quarterly Plan")),
             shiny::fluidRow(shiny::h5("Select Objectives to complete this Quarter"))
      ),
      column(width = 2,
             shiny::fluidRow(shiny::h6("Edit Mode")),
             shiny::fluidRow(switchInput(inputId = ns("edit_mode"), value = TRUE))
      )
    ),
    fluidRow(
      column(width = 12,
             shiny::uiOutput(ns("goal_list"))
      )
    )
  )
}


### SERVER MODULE -----------------
Quarterly_SERVER <- function(id, r_data, r_control, params) {
  moduleServer(id,function(input, output, session) {
    
    ##-------------- APERO----------------
    ns <- session$ns

    ## -----BUCKET RENDERER ----------
    output$goal_list <- renderUI({
      print("rendering")
      bucket_list(
        header = NULL,
        
        group_name = ns("quarterly_bucket"),
        orientation = "horizontal",
        add_rank_list(
          text = h4("Focus this Quarter"),
          input_id = "is_quarterly",
          labels = filter_goals(r_data$goal, "quarterly") %>% mke_goal_box(input$edit_mode, params, ns)
        ),
        add_rank_list(
          text = h4("All Incomplete Goals"),
          input_id = "is_all",
          options = sortable_options(height = "400px"),
          labels = filter_goals(r_data$goal, "all_complete") %>% mke_goal_box(input$edit_mode, params, ns)
        )
      )
    })
    
    ## -----PROGRESS UPDATE ----------
    # listen to update inputs (slider)
    progress_listener_d <- reactive({r_data$goal %>% listen_to(input, "all_complete", "_progress")}) %>% debounce(500)
    
    # observe and Call Update for Progress
    observe({
      input_vec <- progress_listener_d() %>% purrr::discard(is.null)
      req(length(input_vec) > 0)
      r_control$update_reactive <- r_data$goal %>% update_data_set(input_vec, "Progress") %>% 
        store_data_set(r_data$goal)
    })
    
    ## -----STATUS UPDATE ----------
    # listen to update inputs (slider)
    status_listener <- reactive({r_data$goal %>% listen_to(input, "all_complete", "_status")})
    
    # observe and Call Update for Progress
    observe({
      input_vec <- status_listener() %>% purrr::discard(is.null)
      req(length(input_vec) > 0)
      r_control$update_reactive <- r_data$goal %>% update_data_set(input_vec, "Status") %>% 
        store_data_set(r_data$goal)
    })
    
    ## -----SORTING UPDATE ----------

    observeEvent(input$quarterly_bucket, {
      input_vec <- input$quarterly_bucket
      req(length(input_vec) > 0)
      r_control$update_reactive <- r_data$goal %>% update_sorting(input_vec) %>% 
        store_data_set(r_data$goal)
    })
    
    ## -----SHOW AND HIDE DETAILS ----------
    
    # listen to toggle inputs
    toggle_listener <- reactive({r_data$goal %>% listen_to(input, "all_complete", "_toggle")})
    
    # observe to toggle the Detail section
    observe({
      input_vec <- toggle_listener() %>% purrr::discard(is.null)
      req(length(input_vec) > 0)
      input_vec %>% purrr::keep(isFALSE) %>% names() %>% paste0("_body")%>% walk(~shinyjs::hide(.x))
      input_vec %>% purrr::keep(isTRUE) %>% names() %>% paste0("_body")%>% walk(~shinyjs::show(.x))
    })

  })
}
