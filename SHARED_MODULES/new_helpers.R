# these are new helpers which will be transfered to the packages

## FOR UI PACKAGE  --------------------------------------------------

## FOR ALGEBRA STUFF --------------------------------------------------

# turns a text into a function (parse eval) // if function doesnt exist returns NA
text_2_function <- function(txt, ...) {
  
  if (grepl("::", txt)){
    pkg <- sub("::.*", "", txt) %>% paste0("package:", .)
    fnc <- sub(".*::", "", txt)
    if (exists(fnc, where = pkg,   mode="function")){
      eval(parse(text = txt))
    } else {
      NA
    }
  } else {
    if (exists(txt, mode="function")){
      eval(parse(text = txt))
    } else {
      NA
    }
  }
}


# returns the names of the inputs of a function
get_func_input <- function(f){
  f %>% args %>% as.list %>% purrr::discard(~length(.x) == 0) %>% names
}

ods_evaluate_expression <- function(exp, data, var_id = NULL, ...){
  eval(exp, envir = data)#%>%as.list()%>% setNames(var_id)
}

ods_parse_expression <- function(txt){
  parse(text = txt)
}

ods_expression_variable <- function(exp){
  all.vars(exp)
}

needed_vars <- function(X, data = NULL) {
  X <- X %>%
    mutate(exp = map(txt_exp, ~ods_parse_expression(.x)),
           vars = map(exp, ~ods_expression_variable(.x)))
  
  avail <- c(names(data), X$var_id)
  needed <- X$vars %>% unlist() %>% unique()
  list(
    all_required = needed,
    req_from_data = needed %>% magrittr::extract(!needed %in% X$var_id),
    missing = needed %>% magrittr::extract(!needed %in% avail)
  )
}


# applies a function to a list of values, which can be unused
apply_fnc2 <- function(.f, vals){
  x <- tryCatch({
    do.call(what = .f, args = vals)
  }, error = function(e) {NA})
}

# Recursively applies a list of vlues to a list functions, all of which are names with the var id. 
# As a var is calculated, cuts of in functon list and adds to var list.
# is good for dependent series of formulas
recursive_apply <- function(f_lst, val_lst){
  if (length(f_lst) > 0){
    tryCatch({
      do.call(what = f_lst[[1]], args = val_lst)
    }, error = function(e) {NA}) %>% as.list() %>% setNames(names(f_lst[1])) %>%
      modifyList(val_lst) %>% recursive_apply(f_lst[-1], .)
  } else {
    return(val_lst)
  }
}

recursive_formula_apply <- function(X, data, n_old = 9999){
  browser()
  n <- nrow(X)
  if (n > 0 & n < n_old){
    
    X <- X %>% mutate(in_input = map_lgl(vars, ~(all(.x %in% names(data)))))
    Z <- X %>% filter(!in_input)
    Y <- X %>% filter(in_input)
    
    Y %>% pmap(., ods_evaluate_expression, data = data) %>% setNames(Y$var_id) %>%
      modifyList(data)%>% recursive_formula_apply(Z, ., n_old = n)
  } else {
    if (n == 0){
      list(
        data = data,
        success = TRUE,
        missing_var = NULL,
        not_calc = NULL
      )
    } else {
      list(
        data = data,
        success = FALSE,
        missing_var = needed_vars(X) %>% magrittr::extract2("missing"),
        not_calc = X$var_id
      )
    }
  }
  
}

## total i have 423 entries/ i group by 3 elements and 4 plans = 35, which is exactly the length of the X entry here. seems to work
## if var_id is already in X (entry), it will NOT overwrite. it only completes the ones which are not in the list
## but this is not good because if 1 arbitrary value changed, all downstream calcs are incorrect. 
## I need to cut the down to the necessary and restart all calcus each time i run the algo

# Applies a list of functions to subset of a df and returns the full DF in the same structure. is used for group
apply_function_to_group <- function(X, Y, f_lst){

 # browser()
  vrs <- names(f_lst)
  X1 <- X %>% dplyr::filter(!(var_id %in% vrs))
  val_lst <- X1 %>% df_to_lst("value", "var_id") %>% 
    recursive_apply(f_lst, .) %>% as.tibble()%>%
    pivot_longer(everything(), names_to = "var_id", values_to = "value")
}



## FOR WHICH IS THIS USED???
# cuts off the xxx_1 number
del_str_nmb <- function(x){
  gsub("(_*[0-9])", "", x)
}

# adds word boundries to a string regex expression
regex_bound <- function(x){
  paste0("(\\b",x,"\\b)")
}



# function creates button family in blue frame
menu_btn_group <- function(btns, params){

  btns_elem <- OdsUIHelper::ods_MS_btn(btns = btns, params = params, force_cls = "btn_menu")
  
  # if number of buttons is divisible by 4 I make center alignment, ow left alignment
  if (length(btns_elem) == 0){
    return(NULL)
  }
  else if (length(btns_elem) %% 4 == 0){
    cls<-"blue-btn-frame-4"
  } 
  else {
    cls <- "blue-btn-frame"    
  }
  column(width = 12, align="left", class = cls, btns_elem)## End blue Boarder
}


## MAPPING LIST UI STUFF 
small_checkbox <- function(id, name, value = "TRUE", status = "positive", cls = NULL, ...){
  
  list(inputId = paste0("ms_menu_id-", id),
       label = name,
       value = value,
       status = status
       ) %>%
    OdsUIHelper::UIwalk(.f = shinyWidgets::prettyCheckbox) %>% htmltools::tags$div(class = cls)
}


make_toggle <- function(name, ...){
  tagList(
    shiny::column(width = 1, style = "padding-right: 0; padding-left: 0;", OdsUIHelper::make_MS_btn(...)),
    shiny::column(width = 11, style = "padding-right: 10px; padding-left: 10px;", tags$p(name, class = "flymenu_txt"))
  )
}


create_menu_cell <- function(action, ... ){
  
  switch(action,
         
         "zoom" = {shiny::column(width = 1, style = "padding-right: 0; padding-left: 0;",
                                 OdsUIHelper::make_MS_btn(...))},
         
         "show" = {shiny::column(width = 11, style = "padding-right: 10px; padding-left: 10px;", 
                                 small_checkbox(...))},
         
         "toggle" = {make_toggle(...)}
         
         )
  
}

# makes one line
make_list_col <- function(X,  params, ...){
  X %>% purrr::pmap(., .f = create_menu_cell) %>% 
    fluidRow(class = "flymenu_row")
}

old_or_default <- function(id, type, shows, old, ...){
  if (id %in% names(old)){
    old[[id]]
  } else if (type %in% shows) {
    TRUE
  } else {
    FALSE
  }
}

update_check_id <- function(old, new, what, params){

  vec <- element_or_map(names(old), params, what)
  # leave the rest
  remain <- old %>% magrittr::extract(!vec) # are the not concerned ids
  
  # these ids are changed
  to_change <- old %>% magrittr::extract(vec) # are the not concerned ids
  # discard missing
  x <- to_change %>% purrr::keep(names(to_change) %in% names(new))
  # add news with default value
  y <- new %>% purrr::discard(names(new) %in% names(to_change))
  
  # join all
  z <- x %>% append(y) %>% append(remain)
  
  return(z)
}

# CAN BE TRANSFERED AS SOON AS I HAVE CLEAR HOW THE SHOWS ARE DEFINED.
menu_list1 <- function(X, table_id, params, ns, old){

  idcol <- OdsDataHelper::find_column(X, what = "id")
  nmcol <- OdsDataHelper::find_column(X, what = "name")
  tp_col <- X %>% dplyr::select(any_of(c("data_type", "element_type")), everything()) %>% OdsDataHelper::find_column(what = "type")
  shows <- c("border", "AM", "WS")
  renders <- c(params$profile$available_element_types, params$profile$available_map_types)
  #old_clean <- old %>% setNames(which_row(names(.)))


  # element %>% mutate(is_in = element_id %in% names(x),
  #                    is_TRUE = element_id %in% eles_to_delete) %>%
  #   dplyr::filter(is_in & is_TRUE)
  # 
  #browser()
  
  X <- X %>% dplyr::select(-any_of(c("icn"))) ## name conflict with icon from table_ui // not sure of this is the smartest way
  
  #params$profile$available_element_types
  Y <- params$ui_data$table_ui %>% dplyr::filter(table_id == !!table_id & inline) %>%
    left_join(params$ui_data$ui_element, by = "ui_id")  %>% mutate(action = which_action(ui_id)) %>% 
    arrange(match(action, c("toggle", "zoom","show" ))) %>%
    tidyr::expand_grid(X) %>% 
    dplyr::filter(.data[[tp_col]] %in% renders) %>% 
    dplyr::mutate(
      row_id = .data[[idcol]],
      name = .data[[nmcol]],
      status = css,
      ttl = row_id,
      type = .data[[tp_col]],
      id =  paste(ui_id, table_id, row_id, sep = "-")) %>% 
    dplyr::mutate(
      value = purrr::pmap_lgl(., .f = old_or_default,  shows = shows, old = old)
    )
    
  what <- switch(idcol,
                 "geo_id" = "map",
                 "element_id" = "element")
  
  vec <- element_or_map(names(old), params, what = what)
  remain <- old %>% magrittr::extract(!vec) # are the not concerned ids
  id <- Y %>% dplyr::filter(grepl("show", ui_id)) %>% df_to_lst(vals = "value", nms = "id") %>% append(remain)
    
  #   remain %>% append(Y$id)
 
  # 
  # id <- #
  menu <- Y %>% dplyr::group_by(row_id) %>% dplyr::group_map(.f = make_list_col)
  list(
    menu = menu,
    id = id
  )

}

# list_line <- function(id, ttl, value, ...){
#   ods_checkbox <- function(ui_data, id, params, ui_defs, ui_subtype, label){
#     list(inputId = id) %>%
#       utils::modifyList(checkbx_data(ui_data, ui_defs, params)) %>%
#       OdsUIHelper::UIwalk(.f = shinyWidgets::prettyCheckbox)%>% OdsUIHelper::UIstyleAppend(ui_defs)
#   }
# }


# 
# ## THIS GOES TO TABLE BECAUSE ITS ACTUALLY A TABLE
# # creates a simple list element with radio btn only
# simple_list<-function(ns, id_2, txt){
#   
#   
#   elem<-column(width = 12, align="left", class = "blue-menu-frame", #style="border: solid 1px #2C5890; margin-right:0px; width: 100%; margin-left: 0px; margin-bottom:30px; padding: 10px 5px 0px 5px;",
# 
#                
#                fluidRow(
#                  column(width = 11, align="left",
#                         prettyRadioButtons(
#                           inputId = ns(id_2),
#                           label = NULL,
#                           choices = NA,
#                           status = "primary",
#                           shape = "curve",#c("round", "square", "curve"),
#                           icon = icon("map-marker"),
#                         )
#                  )
#                )
#   ) ## End blue Boarder
#   return(elem)
# }
#  
# 
# 
# # function creates complete List with Radiobtn + checkbox group
# complete_list<-function(ns, id_1, id_2, params){
#   
#   elem<-column(width = 12, align="left", class = "blue-menu-frame", #style="border: solid 1px #2C5890; margin-right:0px; width: 100%; margin-left: 0px; margin-bottom:30px; padding: 10px 5px 0px 5px;",
#                
#                h5(span(icon("eye"), params$text$t("Show on Map"))),
#                tags$hr(style="margin:10px;"),
#                
#                
#                fluidRow(
#                  column(width = 1, align="left",
#                         radioGroupButtons(
#                           inputId = ns(id_2),
#                           label = NULL,
#                           choiceNames = list(icon("map-marker")),
#                           choiceValues = c("A"),
#                           status = "primary",
#                           direction = "horizontal",
#                           size = "xs",
#                           justified = FALSE,
#                           individual = TRUE,
#                         )
#                  ),
#                  column(width = 10, align="left",style="margin-top:5px",
#                         prettyCheckboxGroup(inputId = ns(id_1), status = "success", 
#                                             label = NULL,
#                                             choiceNames = character(0),
#                                             choiceValues = character(0),
#                         )
#                  )
#                )
#   )
#   return(elem)         
#   
# }
# 
# 

