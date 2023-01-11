## Module Create New Platform user


### UI MODULE -----------------
Weekly_UI <- function(id, params) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(10, offset = 0,
             shiny::fluidRow(shiny::h2("Weekly Focus")),
             shiny::fluidRow(shiny::h5("Select Objectives of actual Week"))
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
Weekly_SERVER <- function(id, r_data, r_control, params) {
  moduleServer(id,function(input, output, session) {
    
    ##-------------- APERO----------------
    ns <- session$ns

    ## -----BUCKET RENDERER ----------
    output$goal_list <- renderUI({
      	#https://community.rstudio.com/t/shiny-sortable-how-to-limit-number-of-items-that-can-be-dropped/69233/2
      print("rendering")

      a <- filter_goals(r_data$goal, "week") %>% mke_goal_box(input$edit_mode, params, ns)
      b <- filter_goals(r_data$goal, "quarter_only") %>% mke_goal_box(input$edit_mode, params, ns)

      x <-bucket_list(
        header = NULL,
        
        group_name = ns("week_bucket"),
        orientation = "horizontal",
        add_rank_list(
          text = mke_bucket_ttl("Focus of Week", "left", ns),
          input_id = "week",
          options = sortable_options(height = "400px"),
          labels = a
        ),
        add_rank_list(
          text = mke_bucket_ttl("Quarterly", "right", ns),
          input_id = "quarter",
          options = sortable_options(height = "400px"),
          labels = b
        )
      )
      
      return(x)

    })
    
    ## -----PROGRESS UPDATE ----------
    # listen to update inputs (slider)
    progress_listener_d <- reactive({r_data$goal %>% listen_to(input, "quarter", "_progress")}) %>% debounce(500)
    
    # observe and Call Update for Progress
    observe({
      input_vec <- progress_listener_d() %>% purrr::discard(is.null)
      req(length(input_vec) > 0)
      r_control$update_reactive <- r_data$goal %>% update_data_set(input_vec, "Progress") %>% 
        store_data_set(r_data$goal)
    })
    
    ## -----STATUS UPDATE ----------
    # listen to update inputs (slider)
    status_listener <- reactive({r_data$goal %>% listen_to(input, "quarter", "_status")})
    
    # observe and Call Update for Progress
    observe({
      input_vec <- status_listener() %>% purrr::discard(is.null)
      req(length(input_vec) > 0)
      r_control$update_reactive <- r_data$goal %>% update_data_set(input_vec, "Status") %>% 
        store_data_set(r_data$goal)
    })
    
    ## -----SORTING UPDATE ----------

    observeEvent(input$week_bucket, {
      input_vec <- input$week_bucket
      req(length(input_vec) > 0)

      r_control$update_reactive <- r_data$goal %>% update_sorting(input_vec) %>% 
        store_data_set(r_data$goal)
    })
    
    
    ## -----SHOW AND HIDE DETAILS ----------
    
    # listen to toggle inputs
    toggle_listener <- reactive({r_data$goal %>% listen_to(input, "quarter", "_toggle")})
    
    # observe to toggle the Detail section
    observe({
      input_vec <- toggle_listener() %>% purrr::discard(is.null)
      req(length(input_vec) > 0)
      input_vec %>% purrr::keep(isFALSE) %>% names() %>% paste0("_body")%>% walk(~shinyjs::hide(.x))
      input_vec %>% purrr::keep(isTRUE) %>% names() %>% paste0("_body")%>% walk(~shinyjs::show(.x))
    })
    
    ## -----RENDERING TITLE TEXT + PLOTS ----------
  
    # left title and plots
    output$left_ttl <- renderUI({
      workload_recommend_badge(r_data$state$week$workload, 
                               r_data$state$week$available_gls)
      })
    
    output$left_plot_1 <- renderPlotly({
      mke_workload_pie(r_data$state$week$workload)
      })
    
    output$left_plot_2 <- renderPlotly({
      mke_percent_pie(r_data$state$week$progress_tot)
      })        
      
    
    # right title and plots
    output$right_ttl <- renderUI({
      workload_recommend_badge(r_data$state$quarter$workload, 
                               r_data$state$quarter$available_gls)
      })
    
    output$right_plot_1 <- renderPlotly({
      mke_workload_pie(r_data$state$quarter$workload)
      })
    
    output$right_plot_2 <- renderPlotly({
      mke_percent_pie(r_data$state$quarter$progress_tot)
    })    

  })
}
