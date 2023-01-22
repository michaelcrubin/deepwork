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


### SERVER MODULE -----------------
Weekly_SERVER <- function(id, r_data, r_control, params) {
  moduleServer(id,function(input, output, session) {
    
    
    ##-------------- APERO----------------
    ns <- session$ns

    # currently not used
    observeEvent(input$update, {
      req(F)
      r_control$rerender_weekly <- OdsUIHelper::reactive_trigger()
    })
    
    ## -----BUCKET RENDERER ----------
    # main bucket rendering
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

    
    ## -----PROGRESS UPDATE ----------
    # listen to update inputs (slider)
    progress_listener_d <- reactive({
      req(r_control$actual_tab == "weekly_id")
      isolate(r_data$goal) %>% listen_to(input, "quarter", "_progress", "df") %>% 
        mutate(old_Progress = Progress) %>%
        mutate(Progress = as.numeric(value) / 100) %>% 
        mutate(change = Progress != old_Progress) %>% dplyr::filter(change)
      }) #%>% debounce(5000)
  
    # observe progress slider and update + save Dataset + update bar
    observe({
      input_df <- progress_listener_d()
      req(nrow(input_df) > 0)
      
      # storing the data
      r_control$update_reactive <- isolate(r_data$goal) %>% save_df_join(update = input_df) %>%
        store_data_set(old = isolate(r_data$goal))
      
      # updating the progress bars
      r_control$render_progress_bar <- input_df
    })
    
    # rerendering the progress bar in this namespace
    observeEvent(r_control$render_progress_bar, {
      req(r_control$render_progress_bar)
      render_progress_bar(r_control$render_progress_bar, output)
      update_slider_progress(r_control$render_progress_bar, session)
    })
    
    
    ## -----STATUS UPDATE ----------
    # listen to update inputs (slider)
    status_listener <- reactive({
      req(r_control$actual_tab == "weekly_id")
      isolate(r_data$goal) %>% listen_to(input, "quarter", "_status", "df") %>% 
        mutate(old_Status = Status) %>%
        mutate(Status = value) %>% 
        mutate(change = Status != old_Status) %>% dplyr::filter(change)
    })
    
    # observe progress slider and update + save Dataset + update bar
    observe({
      input_df <- status_listener()
      req(nrow(input_df) > 0)
      print("Status week")
      r_control$update_reactive <- isolate(r_data$goal) %>% save_df_join(update = input_df) %>%
        store_data_set(old = isolate(r_data$goal))
      r_control$render_status_badge <- input_df
    })
    
    # rerendering the badge in this namespace
    observeEvent(r_control$render_status_badge, {
      req(r_control$render_status_badge)
      render_status_badge(r_control$render_status_badge, output)
      update_picker_badge(r_control$render_status_badge, session)
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
      r_control$rerender_daily <- OdsUIHelper::reactive_trigger()
      
    })
    
    
    ## -----SHOW AND HIDE DETAILS ----------
    # listen to toggle inputs
    toggle_listener <- reactive({
      req(r_control$actual_tab == "weekly_id")
      isolate(r_data$goal) %>% listen_to(input, "quarter", "_toggle") %>% purrr::modify(as.logical)
      })

    
    # observe to toggle the Detail section
    observe({
      input_vec <- toggle_listener()
      req(length(input_vec) > 0)
      print("toggle all edits")
      input_vec %>% iwalk(~toggle_edit_mode(id = .y, flag = .x))
    })
    
    ## -----SHOW AND HIDE WIP OR SUBHEADER ----------
    
    # listen to day stuff
    isday_listener <- reactive({
      req(r_control$actual_tab == "weekly_id")
      input_vec <- (r_data$goal) %>% mutate(dummy = FALSE) %>% 
        df_to_lst(vals = "dummy", nms = "goal_id")
    })
    
    # observe to toggle the Wip vs Subheader section
    observe({
      input_vec <- isday_listener()
      
      req(length(input_vec) > 0)
      print("toggle wip and subh")
      input_vec %>% iwalk(~toggle_wip_sbh(id = .y, flag = .x))
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
