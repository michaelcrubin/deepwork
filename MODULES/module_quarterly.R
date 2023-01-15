## Module Create New Platform user


### UI MODULE -----------------
Quarterly_UI <- function(id, params) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(10, offset = 0,
             shiny::fluidRow(shiny::h2("Quarterly Objectives")),
             shiny::fluidRow(shiny::h5("Select Objectives to Accomplish this Quarter"))
      ),
      column(2, actionButton(ns("update"), "Update"))
    ),
    fluidRow(
      column(width = 12,
             shiny::uiOutput(ns("quarter_bucket"))
      )
    )
  )
}


### SERVER MODULE -----------------
Quarterly_SERVER <- function(id, r_data, r_control, params) {
  moduleServer(id,function(input, output, session) {
    
    observeEvent(input$update, {
      r_control$rerender_quarterly <- OdsUIHelper::reactive_trigger()
    })
    
    ##-------------- APERO----------------
    ns <- session$ns

    
    ## -----BUCKET RENDERER ----------
    
    observe({
      print("rendering quarter")
      r_control$rerender_quarterly
      render_bucket_list(goal = isolate(r_data$goal),
                         id = "quarter_bucket",
                         ttl_left = "Quarterly Goals",
                         ttl_right = "Long-Term Goals",
                         filter_left = "quarter",
                         filter_right = "all_incomplete",
                         params = params, ns = ns, output = output)
    })
    
    # initial progressbar rendering
    observe({
      print("renders all bars quarter")
      r_control$rerender_quarterly
      render_progress_bar(isolate(r_data$goal), output)
    })
    
    #initial statusbadge rendering
    observe({
      print("renders all badges quarter")
      r_control$rerender_quarterly
      render_status_badge(isolate(r_data$goal), output)
    })
    
   
    ## -----PROGRESS UPDATE ----------
    # listen to update inputs (slider)
    progress_listener_d <- reactive({
      r_data$goal %>% listen_to(input, "all", "_progress", "df") %>% 
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
      # r_control$rerender_weekly <- OdsUIHelper::reactive_trigger()
      # r_control$rerender_daily <- OdsUIHelper::reactive_trigger()
    })

    
    ## -----STATUS UPDATE ----------
    # listen to update inputs (slider)
    status_listener <- reactive({

      a<-isolate(r_data$goal) %>% listen_to(input, "all", "_status", "df") %>% 
        mutate(old_Status = Status) %>%
        mutate(Status = value) %>% 
        mutate(change = Status != old_Status) %>% dplyr::filter(change)
    })
    
    # observe progress slider and update + save Dataset + update bar
    observe({

      input_df <- status_listener()
      req(nrow(input_df) > 0)
      # storing the data
      print("Status week")
      r_control$update_reactive <- r_data$goal %>% save_df_join(update = input_df) %>%
        store_data_set(old = r_data$goal)
      
      # updating the progress bars
      r_control$render_status_badge <- input_df
      
      # which other buckets to update
      # r_control$rerender_weekly <- OdsUIHelper::reactive_trigger()
      # r_control$rerender_daily <- OdsUIHelper::reactive_trigger()
    })
    
    # observe({
    # 
    #   req(r_control$render_status_badge)
    #   render_status_badge(r_control$render_status_badge, output)
    #   r_control$render_quarterly <- TRUE
    # })
    # 
    observeEvent(r_control$render_status_badge, {
      browser()
      req(r_control$render_status_badge)
      render_status_badge(r_control$render_status_badge, output)
      update_picker_badge(r_control$render_status_badge, session)
      
      r_control$render_quarterly <- TRUE
    })
    
    ## -----SORTING UPDATE ----------
    # when user rearranges buckets, reacts and stores in DB
    observeEvent(input$quarter_bucket, {
      input_vec <- input$quarter_bucket
      req(length(input_vec) > 0)
      print("Sorting Quarter")

      r_control$update_reactive <- r_data$goal %>% update_sorting(input_vec) %>% 
        store_data_set(r_data$goal)
      
      # which other buckets to update
      r_control$rerender_weekly <- OdsUIHelper::reactive_trigger()
      r_control$rerender_daily <- OdsUIHelper::reactive_trigger()
    })
    
    
    ## -----SHOW AND HIDE DETAILS ----------
    # listen to toggle inputs
    toggle_listener <- reactive({

      a<- isolate(r_data$goal) %>% listen_to(input, "all", "_toggle") %>% purrr::modify(as.logical)
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
      workload_recommend_badge(r_data$state$quarter$workload, 
                               r_data$state$quarter$available_gls,
                               r_data$state$quarter$n_goal)
      })
    
    output$left_plot_1 <- renderPlotly({
      mke_workload_pie(r_data$state$quarter$workload)
      })
    
    output$left_plot_2 <- renderPlotly({
      mke_percent_pie(r_data$state$quarter$progress_tot)
      })        
      
    
    # right title and plots
    output$right_ttl <- renderUI({
      workload_recommend_badge(r_data$state$all$workload, 
                               r_data$state$all$available_gls,
                               r_data$state$all$n_goal)
      })
    
    output$right_plot_1 <- renderPlotly({
      mke_workload_pie(r_data$state$all$workload)
      })
    
    output$right_plot_2 <- renderPlotly({
      mke_percent_pie(r_data$state$all$progress_tot)
    })    

  })
}
