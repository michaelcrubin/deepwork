## Module Create New Platform user


### UI MODULE -----------------
Weekly_UI <- function(id, params) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(10, offset = 0,
             shiny::fluidRow(shiny::h2("Weekly Focus")),
             shiny::fluidRow(shiny::h5("Select Objectives of actual Week"))
      )
      # column(width = 2,
      #        shiny::fluidRow(shiny::h6("Edit Mode")),
      #        shiny::fluidRow(switchInput(inputId = ns("edit_mode"), value = TRUE))
      # )
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
      goal <- isolate(r_data$goal)
      a <- filter_goals(goal, "week") %>% mke_goal_box(input$edit_mode, params, ns)
      b <- filter_goals(goal, "quarter_only") %>% mke_goal_box(input$edit_mode, params, ns)

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
    
    # initial progressbar rendering
    observe({
      print("renders all bars")
      render_progress_bar(isolate(r_data$goal), output)
    })
    
    # initial statusbadge rendering
    observe({
      print("renders all badges")
      render_status_badge(isolate(r_data$goal), output)
    })

    
    ## -----PROGRESS UPDATE ----------
    # listen to update inputs (slider)
    progress_listener_d <- reactive({
      r_data$goal %>% listen_to(input, "quarter", "_progress", "df") %>% 
        mutate(old_Progress = Progress) %>%
        mutate(Progress = as.numeric(value) / 100) %>% 
        mutate(change = Progress != old_Progress) %>% dplyr::filter(change)
      }) %>% debounce(500)
  
    # observe progress slider and update + save Dataset + update bar
    observe({
      input_df <- progress_listener_d()
      req(nrow(input_df) > 0)
      
      # storing the data
      r_control$update_reactive <- r_data$goal %>% save_df_join(update = input_df) %>%
        store_data_set(old = r_data$goal)
      
      # updating the progress bars
      render_progress_bar(input_df, output)
    })

    
    ## -----STATUS UPDATE ----------
    # listen to update inputs (slider)
    status_listener <- reactive({
      r_data$goal %>% listen_to(input, "quarter", "_status", "df") %>% 
        mutate(old_Status = Status) %>%
        mutate(Status = value) %>% 
        mutate(change = Status != old_Status) %>% dplyr::filter(change)
    })
    
    # observe progress slider and update + save Dataset + update bar
    observe({
      input_df <- status_listener()
      req(nrow(input_df) > 0)
      # storing the data
      r_control$update_reactive <- r_data$goal %>% save_df_join(update = input_df) %>%
        store_data_set(old = r_data$goal)
      
      # updating the progress bars
      render_status_badge(input_df, output)
    })
    
    
    ## -----SORTING UPDATE ----------
    # when user rearranges buckets, reacts and stores in DB
    observeEvent(input$week_bucket, {
      input_vec <- input$week_bucket
      req(length(input_vec) > 0)
      r_control$update_reactive <- r_data$goal %>% update_sorting(input_vec) %>% 
        store_data_set(r_data$goal)
    })
    
    
    ## -----SHOW AND HIDE DETAILS ----------
    # listen to toggle inputs
    toggle_listener <- reactive({
      a<- r_data$goal %>% listen_to(input, "quarter", "_toggle") %>% purrr::modify(as.logical)
      })

    
    # observe to toggle the Detail section
    observe({
      input_vec <- toggle_listener()
      req(length(input_vec) > 0)
      print("toggle all edits")
      input_vec %>% iwalk(~toggle_edit_mode(id = .y, flag = .x))
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
