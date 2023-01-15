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
      column(2, actionButton(ns("update"), "Update"))
    ),
    fluidRow(
      column(width = 12,
             shiny::uiOutput(ns("week_bucket"))
      )
    )
  )
}
#https://community.rstudio.com/t/shiny-sortable-how-to-limit-number-of-items-that-can-be-dropped/69233/2


### SERVER MODULE -----------------
Weekly_SERVER <- function(id, r_data, r_control, params) {
  moduleServer(id,function(input, output, session) {
    
    observeEvent(input$update, {
      r_control$rerender_weekly <- OdsUIHelper::reactive_trigger()
    })
    
    ##-------------- APERO----------------
    ns <- session$ns

    
    ## -----BUCKET RENDERER ----------
    
    observe({
      print("rendering weekly")
      r_control$rerender_weekly
      render_bucket_list(goal = isolate(r_data$goal),
                         id = "week_bucket",
                         ttl_left = "Focus of Week",
                         ttl_right = "Quarterly Goals",
                         filter_left = "week",
                         filter_right = "quarter_only",
                         params = params, ns = ns, output = output)
    })
    
    # initial progressbar rendering
    observe({
      print("renders all bars weekly")
      r_control$rerender_weekly
      render_progress_bar(isolate(r_data$goal), output)
    })
    
    # initial statusbadge rendering
    observe({
      print("renders all badges weekly")
      r_control$rerender_weekly
      render_status_badge(isolate(r_data$goal), output)
    })

    observeEvent(r_control$render_status_badge, {

      req(r_control$render_status_badge)
    print("observe week")
      render_status_badge(r_control$render_status_badge, output)
      update_picker_badge(r_control$render_status_badge, session)
      
      r_control$render_weekly <- TRUE
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
      
      # which other buckets to update
      # r_control$rerender_quarterly <- OdsUIHelper::reactive_trigger()
      # r_control$rerender_daily <- OdsUIHelper::reactive_trigger()
    })

    
    ## -----STATUS UPDATE ----------
    # listen to update inputs (slider)
    status_listener <- reactive({

  
      req((r_control$actual_tab) == "weekly_id")

      s<-isolate(r_data$goal) %>% listen_to(input, "quarter", "_status", "df") %>% 
        mutate(old_Status = Status) %>%
        mutate(Status = value) %>% 
        mutate(change = Status != old_Status) %>% dplyr::filter(change)
    })
    
    # observe progress slider and update + save Dataset + update bar
    observe({
      input_df <- status_listener()
      req(nrow(input_df) > 0)
      print("Status week")
      r_control$update_reactive <- r_data$goal %>% save_df_join(update = input_df) %>%
        store_data_set(old = r_data$goal)

      r_control$render_status_badge <- input_df
      # updating the progress bars
      
      
      # which other buckets to update
      # r_control$rerender_quarterly <- OdsUIHelper::reactive_trigger()
      # r_control$rerender_daily <- OdsUIHelper::reactive_trigger()
    })
    

    
    ## -----SORTING UPDATE ----------
    # when user rearranges buckets, reacts and stores in DB
    observeEvent(input$week_bucket, {
      input_vec <- input$week_bucket
      req(length(input_vec) > 0)
      print("Sorting Week")
      r_control$update_reactive <- r_data$goal %>% update_sorting(input_vec) %>% 
        store_data_set(r_data$goal)
      
      # which other buckets to update
      r_control$rerender_daily <- OdsUIHelper::reactive_trigger()
    })
    
    
    ## -----SHOW AND HIDE DETAILS ----------
    # listen to toggle inputs
    toggle_listener <- reactive({
      a<- isolate(r_data$goal) %>% listen_to(input, "quarter", "_toggle") %>% purrr::modify(as.logical)
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
                               r_data$state$week$available_gls,
                               r_data$state$week$n_goal)
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
                               r_data$state$quarter$available_gls,
                               r_data$state$quarter$n_goal)
      })
    
    output$right_plot_1 <- renderPlotly({
      mke_workload_pie(r_data$state$quarter$workload)
      })
    
    output$right_plot_2 <- renderPlotly({
      mke_percent_pie(r_data$state$quarter$progress_tot)
    })    

  })
}
