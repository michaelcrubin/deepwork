## Module Create New Platform user


#####-------------------- UI PART ----------------------------------#####

### UI MODULE -----------------
LongTerm_UI <- function(id, params) {
  ns <- NS(id)
  tagList(
    
    # renders all basic UI elements // hides certain elements
    column(12,offset = 0,
           shiny::fluidRow( shiny::h2("Long-Term Goals")),
           #shiny::fluidRow( shiny::h3("Your long-term Goals define where you want to be in 5 years")),
          # shiny::tags$hr(),
          # shiny::fluidRow( shiny::h4("Dashboard")),
           
           
           # shiny::fluidRow(
           #   shiny::column(10, shiny::h4("")),
           #   shiny::column(2, actionButton(inputId = "add_goal", "Add Goal"))
           # ),
           tabBox(
             width = 12,
             tabPanel(
               title = h4("All Goals"),
               shiny::tags$br(),

               #shiny::tags$hr(style="margin-top: 25px; border-top: 0px;"),
               shiny::fluidRow(
                 shiny::column(12, offset = 0,
                               DT::dataTableOutput(ns("long_term_tbl"))
                 )
               )
             )
           )
        )
    
  )
}



#####-------------------- SERVER PART ----------------------------------#####

## SERVER FUNCTIONS: Data Handling-----------------

subset_lgtm_data <- function(X){
  X %>% dplyr::select(-any_of(c("Description","Success Criteria","Comment")))
  
}

### SERVER MODULE -----------------
LongTerm_SERVER <- function(id, r_data, r_control, params) {
  moduleServer(id,function(input, output, session) {
    
    ##-------------- APERO----------------
    ns <- session$ns
    # creating new Temp Var for transfer inside module // SOME OR ALL WILL COME FROM CENTRALIZED R_ PART
    
    #-----NEW
    
    output$long_term_tbl <- renderDT({
      r_data$long_term_goal %>% subset_lgtm_data() %>%
        make_DT(params, table_id = "long_term_tbl")
    })



  })
}
