## Module Create New Platform user






### UI MODULE -----------------
Daily_UI <- function(id, params) {
  ns <- NS(id)
  tagList(
    tags$head(
      # Note the wrapping of the string in HTML()
      tags$style(HTML("

      "))
    ),
    fluidRow(
      column(10, offset = 0,
             shiny::fluidRow(shiny::h2("Day Focus")),
             shiny::fluidRow(shiny::h5("Plan Deep Work Periods"))
      ),
      column(2, actionButton(ns("add_task"), "New Task"))
    ),
    fluidRow(
      column(width = 12,
             shiny::uiOutput(ns("day_bucket"))
      )
    )
  )
}


### SERVER MODULE -----------------
Daily_SERVER <- function(id, r_data, r_control, params) {
  moduleServer(id,function(input, output, session) {
    
    
    ##-------------- APERO----------------
    ns <- session$ns

    # currently not used
    observeEvent(input$add_task, {
     # r_control$rerender_daily <- OdsUIHelper::reactive_trigger()
      r_control$add_task <- OdsUIHelper::reactive_trigger()
    })
    
    ## -----BUCKET RENDERER ----------
    # main bucket rendering
    observe({
      print("rendering daily")
      r_control$rerender_daily
      a<- isolate(r_data$goal)
      render_bucket_list(goal = isolate(r_data$goal),
                         id = "day_bucket",
                         ttl_left = "Focus of Day",
                         ttl_right = "Weekly Goals",
                         filter_left = "day",
                         filter_right = "week_only",
                         params = params, ns = ns, output = output)
    })
    
    # initial progressbar rendering
    observe({
      print("renders all bars daily")
      r_control$rerender_daily
      render_progress_bar(isolate(r_data$goal), output)
    })
    
    # initial statusbadge rendering
    observe({
      print("renders all badges daily")
      r_control$rerender_daily
      render_status_badge(isolate(r_data$goal), output)
    })
    
    # initial wiph rendering
    observe({
      print("renders all wiph initially")
      r_control$rerender_daily
      render_wip_h(isolate(r_data$goal), output)
    })
    
    # initial subh rendering
    observe({
      print("renders all subh initially")
      r_control$rerender_daily
      render_subheader(isolate(r_data$goal), output)
    })

    ## -----PROGRESS UPDATE ----------
    # listen to update inputs (slider)
    progress_listener_d <- reactive({
      req(r_control$actual_tab == "daily_id")
      isolate(r_data$goal) %>% listen_to(input, "week", "_progress", "df") %>% 
        mutate(old_Progress = Progress) %>%
        mutate(Progress = as.numeric(value) / 100) %>% 
        mutate(change = Progress != old_Progress) %>% dplyr::filter(change)
      }) %>% debounce(500)
  
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
      if (r_control$actual_tab %in% c("weekly_id", "quarterly_id")) {
        update_slider_progress(r_control$render_progress_bar, session)
      }
    })
    
    ## -----WIP HOURS UPDATE ----------
    # listen to update inputs (slider)
    wip_h_listener_d <- reactive({
      req(r_control$actual_tab == "daily_id")
     a<- isolate(r_data$goal) %>% listen_to(input, "week", "_wiph", "df") %>% 
        mutate(old_wip_h = wip_h) %>%
        mutate(wip_h = as.numeric(value)) %>% 
        mutate(change = wip_h != old_wip_h) %>% dplyr::filter(change)
    }) %>% debounce(500)
    
    # observe progress slider and update + save Dataset + update bar
    observe({
     # req(F)
      input_df <- wip_h_listener_d()
      req(nrow(input_df) > 0)
      # storing the data
      r_control$update_reactive <- isolate(r_data$goal) %>% save_df_join(update = input_df) %>%
        store_data_set(old = isolate(r_data$goal))
      
      # updating the progress bars
      r_control$render_wip_h <- input_df
    })
    
    # rerendering the progress bar in this namespace
    observeEvent(r_control$render_wip_h, {
      req(r_control$render_wip_h)
      render_wip_h(r_control$render_wip_h, output)
      render_subheader(r_control$render_wip_h, output)
      if (r_control$actual_tab %in% c("weekly_id", "quarterly_id")) {
        update_number_wiph(r_control$render_wip_h, session)
      }
    })
    
    
    ## -----STATUS UPDATE ----------
    # listen to update inputs (slider)
    status_listener <- reactive({
      req(r_control$actual_tab == "daily_id")
      isolate(r_data$goal) %>% listen_to(input, "week", "_status", "df") %>% 
        mutate(old_Status = Status) %>%
        mutate(Status = value) %>% 
        mutate(change = Status != old_Status) %>% dplyr::filter(change)
    })
    
    # observe progress slider and update + save Dataset + update bar
    observe({
      input_df <- status_listener()
      req(nrow(input_df) > 0)
      print("Status Day")
      r_control$update_reactive <- isolate(r_data$goal) %>% save_df_join(update = input_df) %>%
        store_data_set(old = isolate(r_data$goal))
      r_control$render_status_badge <- input_df
    })
    
    # rerendering the badge in this namespace
    observeEvent(r_control$render_status_badge, {
      req(r_control$render_status_badge)
      render_status_badge(r_control$render_status_badge, output)
      if (r_control$actual_tab %in% c("weekly_id", "quarterly_id")) {
        update_picker_badge(r_control$render_status_badge, session)
      }
    })
    
    
    ## -----SORTING UPDATE ----------
    # when user rearranges buckets, reacts and stores in DB
    observeEvent(input$day_bucket, {
      input_vec <- input$day_bucket
      req(length(input_vec) > 0)
      print("Sorting Day")
      r_control$update_reactive <- r_data$goal %>% update_sorting(input_vec) %>% 
        store_data_set(r_data$goal)
      
      # which other buckets to update
      #r_control$rerender_daily <- OdsUIHelper::reactive_trigger()
    })
    
    
    ## -----SHOW AND HIDE DETAILS ----------
    # listen to toggle inputs
    toggle_listener <- reactive({
      req(r_control$actual_tab == "daily_id")
      isolate(r_data$goal) %>% listen_to(input, "week", "_toggle") %>% purrr::modify(as.logical)
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
      req(r_control$actual_tab == "daily_id")
      input_vec <- (r_data$goal) %>% df_to_lst(vals = "day", nms = "goal_id")
    })
    
    # observe to toggle the Wip vs Subheader section
    observe({
      input_vec <- isday_listener()
      req(length(input_vec) > 0)
      print("toggle wip and subh")
      input_vec %>% iwalk(~toggle_wiph_sbh(id = .y, flag = .x))
    })
    
    ## -----RENDERING TITLE TEXT + PLOTS ----------
  
    # left title and plots
    output$left_ttl <- renderUI({
      workload_recommend_badge(r_data$state$day$workload, 
                               r_data$state$day$available_gls,
                               r_data$state$day$n_goal)
      })
    
    output$left_plot_1 <- renderPlotly({
      mke_workload_pie(r_data$state$day$workload)
      })
    
    output$left_plot_2 <- renderPlotly({
      mke_percent_pie(r_data$state$day$progress_tot)
      })        
      
    
    # right title and plots
    output$right_ttl <- renderUI({
      workload_recommend_badge(r_data$state$week$workload, 
                               r_data$state$week$available_gls,
                               r_data$state$week$n_goal)
      })
    
    output$right_plot_1 <- renderPlotly({
      mke_workload_pie(r_data$state$week$workload)
      })
    
    output$right_plot_2 <- renderPlotly({
      mke_percent_pie(r_data$state$week$progress_tot)
    })    

  })
}
