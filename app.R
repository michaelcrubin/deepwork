


#-----Loading User and Environnment-----------------------------
## Setting odapes user. Will later come from env var
proxy_user <- "michael.rubin.sp@gmail.com"


Sys.setenv("odapes_user" = proxy_user)
Sys.setenv("cognito_active" = FALSE)

## loading all Dependencies
library(here)
source(here("global.R"), local = FALSE)


# SOURCING FOR DEVELOPMENT -----------
# Define which modules to source // ONLY CHANGE TRUE/FALSE. DON'T TOUCH REST
list(
  ods_menu = TRUE
) %>% 
  token_url() %>% purrr::walk(~source(.x, local = F))


# APP-Specific Own Modules. Local sourcing
source(here("APP_MODULES","module_daily.R"), local = F)
source(here("APP_MODULES","module_weekly.R"), local = F)
source(here("APP_MODULES","module_quarterly.R"), local = F)
source(here("APP_MODULES","module_long_term.R"), local = F)
source(here("APP_MODULES","helpers.R"), local = F)

# SHARED MODULES// depend on ODS system for state change
source(here("SHARED_MODULES","MODULE_LTGOAL.R"), local = F)
source(here("SHARED_MODULES","new_helpers.R"), local = F)


## PREPARE: LOADING THE PARAMS LIST --------------------
user <- OdsDataLoad::import_user_data()
sys_params <- get_system_parameters(user)

params <- list(
  ods_params = get_ods_params(sys_params),
  sys_params = sys_params,
  profile = get_user_profile(user),
  text = import_language(sys_params),
  account_settings = user,
  ui_data = get_ui_data(sys_params)
)

#renv::snah()
# here IF I WORK FOR DEV USE "goal_dev.csv" FOR PRPDUCTION USE "goal_original.csv"
save_mode <- "goal_dev.csv"
Sys.setenv("save_mode" = save_mode)
Sys.getenv("save_mode")


# ADDS APP SPECIFIC PARAMETERS ----
get_params <- function(params){
  params$sys_params <- params$sys_params %>% append(list(domains = c("day", "week_only", "week", "quarter_only", "quarter","all_incomplete","all")))
  params$ui_data <- params$ui_data %>% append(list(category_style = read_csv(here("data", "category_style.csv"))))
  return(params)
}

get_r_control<-function(){

  r_control <- reactiveValues(
    goto_tab = NULL,
    update_reactive = NULL,
    rerender_daily = NULL,
    rerender_weekly = NULL,
    rerender_quarterly = NULL,
    to_rerender = NULL
    
    

  )
  return(r_control)
}

get_r_data <- function(params){
 
  goal = read_csv(here("data", Sys.getenv("save_mode"))) %>% mutate(open_h = (1 - Progress) * work_h)
  state <- get_all_states(goal, params$sys_params$domains)
  r_data <- reactiveValues(
    goal = goal,
    state = state
  )

  return(r_data)
}

params <- get_params(params)

#
# App Start -----------------------------------------------------------------

shinyApp(
  ui = dashboardPage(
    title = "Basic Dashboard",
    fullscreen = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "Michael's App",
        color = "primary",
       # href = "https://www.google.fr",
        image = "img/mike.jpeg",
      ),
      skin = "light",
      status = "white",
      border = FALSE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE,
      leftUi = tagList(
        dropdownMenu(
          badgeStatus = "info",
          type = "notifications",
          notificationItem(
            inputId = "triggerAction2",
            text = "Error!",
            status = "danger"
          )
        ),
        dropdownMenu(
          badgeStatus = "info",
          type = "tasks",
          taskItem(
            inputId = "triggerAction3",
            text = "My progress",
            color = "orange",
            value = 10
          )
        )
      ),
      rightUi = dropdownMenu(
        badgeStatus = "danger",
        type = "messages",
        messageItem(
          inputId = "triggerAction1",
          message = "message 1",
          from = "Divad Nojnarg",
          image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
          time = "today",
          color = "lime"
        )
      )
    ),
    sidebar = dashboardSidebar(
      skin = "light",
      status = "primary",
      elevation = 3,
      sidebarUserPanel(
       # image = "https://image.flaticon.com/icons/svg/1149/1149168.svg",
        name = "Be Focused"
      ),
      sidebarMenu(id = "tab_menu",
        sidebarHeader(h4("Goals")),
        menuItem(
          "Daily",
          tabName = "daily_id",
          icon = icon("calendar-day")
        ),
        menuItem(
          "Weekly",
          tabName = "weekly_id",
          icon = icon("calendar-week")
        ),
        menuItem(
          "Quarterly",
          tabName = "quarterly_id",
          icon = icon("calendar-days")
        ),
        menuItem(
          "Long-Term Goals",
          tabName = "longtermgoal_id",
          icon = icon("bars")
        )
      )
    ),
    # controlbar = dashboardControlbar(
    #   skin = "light",
    #   pinned = TRUE,
    #   collapsed = FALSE,
    #   overlay = FALSE,
    #   controlbarMenu(
    #     id = "controlbarmenu",
    #     controlbarItem(
    #       title = "Item 1",
    #       sliderInput(
    #         inputId = "obs",
    #         label = "Number of observations:",
    #         min = 0,
    #         max = 1000,
    #         value = 500
    #       ),
    #       column(
    #         width = 12,
    #         align = "center",
    #         radioButtons(
    #           inputId = "dist",
    #           label = "Distribution type:",
    #           c(
    #             "Normal" = "norm",
    #             "Uniform" = "unif",
    #             "Log-normal" = "lnorm",
    #             "Exponential" = "exp"
    #           )
    #         )
    #       )
    #     ),
    #     controlbarItem(
    #       "Item 2",
    #       "Simple text"
    #     )
    #   )
    # ),
    # footer = dashboardFooter(
    #   left = a(
    #     href = "https://twitter.com/divadnojnarg",
    #     target = "_blank", "@DivadNojnarg"
    #   ),
    #   right = "2018"
    # ),

# App Body -----------------------------------------------------------------
    
    body = dashboardBody(
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "CSS/prov_css.css"),
                tags$link(rel = "stylesheet", type = "text/css", href = "CSS/ruler.css")
                ),
      useShinyjs(),
      
      tabItems(
        #
        tabItem(tabName =  "daily_id", Daily_UI("daily_id", params)),
        
        tabItem(tabName = "weekly_id", Weekly_UI("weekly_id", params)),
        
        tabItem(tabName = "quarterly_id", Quarterly_UI("quarterly_id", params)),

        tabItem(tabName = "longtermgoal_id", LTGOAL_UI("longtermgoal_id", params))
      )
    )
  ),

### SERVER PART-----------------------
server = function(input, output, session) {
    
    r_control <- get_r_control()
    r_data <- get_r_data(params)

    # observes and stores the active (open) tab
    observe({
      req(input$tab_menu)
      r_control$actual_tab <-  input$tab_menu
    })

    
    # Daily server logic
    Daily_SERVER("daily_id", r_data, r_control, params)

    # Weekly server logic
    Weekly_SERVER("weekly_id", r_data, r_control, params)

    # Weekly server logic
    Quarterly_SERVER("quarterly_id", r_data, r_control, params)

    # long term server logic
    LTGOAL_SERVER("long_term_id", r_data, r_control, params)
    
    
    # Modal Handling (later to own module)
    # calling the modal
    observeEvent(r_control$add_task, {
      # r_control$rerender_daily <- OdsUIHelper::reactive_trigger()
      ns <- session$ns
      ms_menu <- params$ui_data$ms_menu %>% dplyr::filter(menu_id == "NEW_TASK")
      new_goal_modal(ms_menu = ms_menu,
                     ns = ns,
                     data = params)
      
    })
    

    # listens to the modal window inputs
    modal_listener <- reactive({
      params$ui_data$ms_menu %>% dplyr::filter(menu_id == "NEW_TASK") %>%
        dplyr::mutate(value = map(id, ~input[[.x]])) %>% OdsDataHelper::df_to_lst(vals = "value", nms = "id")
    })
    

    # adds single vars to DF of type goals
    add_task_goals <- function(txt_activity = NA, txt_project = NA, txt_category = NA, num_workh = NA, date_plan = NA, txt_desc = NA, txt_criteria = NA, num_wip = NA, ... ){
      Y <- tibble(
        goal_id = paste0("gl", sample(1000000:9999999, 1)),
        quarter = FALSE,
        week = FALSE,
        day = FALSE,
        Activity = txt_activity,
        Project = txt_project,
        Category = txt_category,
        Status = "Not started",
        Continous = NA,
        Progress = 0,
        work_h = num_workh,
        Planned = date_plan,
        Concluded = NA,
        Description = txt_desc,
        Criteria = txt_criteria,
        Comment = NA,
        open_h = num_workh,
        wip_h = num_wip
      )
    }
    
    # joins two df and makes sure cols and types are aligned
    save_df_join <- function(X, add){
      add <- add %>% dplyr::select(any_of(colnames(X)))
      colnms <- names(add)[names(add) %in% names(X)]
      add[colnms] <- lapply(colnms, function(x) {
        match.fun(paste0("as.", class(X[[x]])[1]))(add[[x]])
      })
      X %>% tibble::add_case(add)
    }
    
    observeEvent(input$ok_add_task, {
      
      browser()
      removeModal()
      
      input_vec <- modal_listener()
      req(length(input_vec) > 0)
      
      r_control$update_reactive <- do.call(what = add_task_goals, args = input_vec) %>% 
        save_df_join(r_data$goal, .) %>% 
        store_data_set(r_data$goal)
      
    })
    
    # update the goals values + recalc state // later from DB
    observeEvent(r_control$update_reactive, {
      req(r_control$update_reactive)
      print("reimporting")
      # Later more fine graned
      r_data$goal <- read_csv(here("data", Sys.getenv("save_mode"))) %>% mutate(open_h = (1 - Progress) * work_h)
      r_data$state <- get_all_states(r_data$goal, params$sys_params$domains)
    })


  }
)
