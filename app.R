#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(pool)
library(dplyr)
library(DBI)
library(RPostgreSQL)
library(DT)
library(purrr)
library(yaml)
library(stringr)
library(httr)
library(jsonlite)
library(openssl)


options(shiny.port = 5000)

GET_URL <- function(domain, endpoint, params) {
  paste0(
    domain,
    '/',
    endpoint,
    '?',
    paste(
      mapply(
        paste,
        names(params),
        params,
        MoreArgs = list(sep = '=')
      ),
      collapse = '&'
    )
  )
}

state_code <- stringi::stri_rand_strings(1, 64)

source('db_module.R')
source('ui_module.R')

## UI Definition --------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    useShinyjs()
  ),
   
   # Application title
   titlePanel("Amy's Gradebook"),
  
  fluidRow(
    column(
      width = 1,
      actionButton('login', 'Log In')
    )
  ),
   
   # Main application nav
   navlistPanel(
     id = 'main_nav',
     widths = c(2, 10),
     tabPanel(
       'Course Management',
       fluidPage(
         # tabPanel content
         fluidRow(
           
           # column for course controls
           column(
             width = 6,
             
             ## Course Controls -------------------------------------------------
             control_module('Courses', 'course'),
             
             ## Assignment Categories Controls ----------------------------------
             control_module('Assignment Categories', 'asscat')
           ),
           
           # column for student and assignment controls
           column(
             width = 6,
             
             ## Student Controls ------------------------------------------------
             control_module('Students', 'student'),
             
             ## Assignments Controls --------------------------------------------
             control_module('Assignments', 'assn')
           )
         )
       )
     ),
     tabPanel(
       'Grade Explorer',
       fluidPage(
         fluidRow(
           height = '50%',
           column(
             width = 12,
             tabsetPanel(
               id = 'grade_explorer',
               tabPanel(
                 'Grades by Student',
                 uiOutput('grades_by_student')
               ),
               tabPanel(
                 'Grades by Course'
               )
             )
           )
         )
         #DT::dataTableOutput('grade_display_table')
       )
     )
   )
)

## +++++ ----------------------------------------------------------------------

## Server Function ------------------------------------------------------------
server <- function(input, output, session) {
  ## Initial Setup ------------------------------------------------------------
  # create a connection pool
  # save the password that we can "hide" it as best as we can by collapsing it
 db_info <- yaml.load_file('config/db.yaml')
  
  pool <- dbPool(
    drv = dbDriver("PostgreSQL"),
    dbname = db_info$db,
    host = db_info$host, 
    port = db_info$port,
    user = db_info$user, 
    password = db_info$pw
  )
  
  rm(db_info) # removes the db info
  
  reload_courses <- reactiveVal(value = 0)
  reload_asscats <- reactiveVal(value = 0)
  reload_assns <- reactiveVal(value = 0)
  reload_students <- reactiveVal(value = 0)
  reload_grades <- reactiveVal(value = 0)
  
  user_id <- reactiveVal(value = NULL)
  auth0 <- yaml.load_file('config/auth0_variables.yaml')
  
  ## Login Functionality ------------------------------------------------------
  #If the query string contains the 'code' value, leverage the code value
  #against the auth0 oauth/token endpoint to retrieve a JWT containing
  #the user id, then extract the user id from the JWT
  observe({
    queryString <- getQueryString()
    if(exists('code', where = queryString)) {
      authCode <- queryString$code
      returned_state <- queryString$state
      if(returned_state == state_code) {
        access_token <- POST(
          paste0(
            auth0$AUTH0_DOMAIN,
            '/oauth/token'
          ),
          encode = 'json',
          body = list(
            'grant_type' = 'authorization_code',
            'client_id' = auth0$AUTH0_CLIENT_ID,
            'client_secret' = auth0$AUTH0_CLIENT_SECRET,
            'code' = authCode,
            'redirect_uri' = auth0$AUTH0_CALLBACK_URL
          )
        )
        
        access_token %>%
          content() %>%
          .$id_token %>%
          str_split(., pattern = '\\.', simplify = T) %>%
          .[1, 2] %>%
          base64_decode() %>%
          rawToChar() %>%
          {
            if(!str_detect(., '\\}$')) {
              paste0(., '}')
            } else {
              .
            }
          } %>%
          fromJSON() %>%
          .$sub %>%
          user_id()
      }
    }
    if(exists('authCode')) {
      if(!is.null(authCode)){
        updateQueryString('')
      }
    }
  })
  
  #Redirect user to the sign-in page when they click the login button
  observeEvent(
    input$login,
    {
      runjs(
        paste0(
          'window.location.href=\'',
          GET_URL(
            auth0$AUTH0_DOMAIN,
            'authorize',
            list(
              'response_type' = 'code',
              'client_id' = auth0$AUTH0_CLIENT_ID,
              'redirect_uri' = auth0$AUTH0_CALLBACK_URL,
              'state' = state_code,
              'scope' = 'openid'
            )
          ),
          '\';'
        )
      )
    }
  )
  
  ## Maintain Course Elements Lists -------------------------------------------
  
  courses <- eventReactive(
    reload_courses(),
    courses_named_list(pool, user_id()),
    ignoreNULL = F
  )
  
  ass_cats <- eventReactive(
    {
      reload_asscats()
      input$select_course
    },
    course_asscats_named_list(pool, input$select_course),
    ignoreNULL = T
  )
  
  assns <- eventReactive(
    {
      reload_assns()
      input$select_asscat
    },
    course_assn_named_list(pool, input$select_course, input$select_asscat),
    ignoreNULL = T
  )
  
  students <- eventReactive(
    {
      reload_students()
      input$select_course
    },
    course_students_named_list(pool, input$select_course),
    ignoreNULL = T
  )
  
  grades <- eventReactive(
    {
      reload_grades()
      input$select_assn
      input$select_student
    },
    course_grades_named_list(
      pool, 
      input$select_course,
      input$select_assn,
      input$select_student
    ),
    ignoreNULL = T
  )
  
  ## Render list groups -------------------------------------------------------
  ## Render Courses List
  output$course_list <- renderUI({
    prev_selection <- input$select_course
    
    radioGroupButtons(
      'select_course',
      choices = courses(),
      direction = 'vertical',
      justified = FALSE,
      checkIcon = list(yes = icon("ok", lib = "glyphicon")),
      selected = prev_selection
    )
  })
  
  ## Render Assignment Categories List
  output$asscat_list <- renderUI({
    prev_selection <- input$select_asscat
    
    if (!is.null(input$select_course)) {
      if (!is.null(ass_cats())) {
        radioGroupButtons(
          'select_asscat',
          choices = ass_cats(),
          direction = 'vertical',
          justified = FALSE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon")),
          selected = prev_selection
        )
      } else {
        h4('No assignment categories for this course.')
      }
    }
  })
  
  ## Render Assignment List
  output$assn_list <- renderUI({
    prev_selection <- input$select_assn
      
    if (!is.null(assns())) {
      radioGroupButtons(
        'select_assn',
        choices = assns(),
        direction = 'vertical',
        justified = FALSE,
        checkIcon = list(yes = icon("ok", lib = "glyphicon")),
        selected = prev_selection
      )
    } else {
      h4('No assignments of this category for this course.')
    }
  })
  
  ## Render Student List
  output$student_list <- renderUI({
    prev_selection <- input$select_student
      
    if (!is.null(students())) {
      radioGroupButtons(
        'select_student',
        choices = students(),
        direction = 'vertical',
        justified = FALSE,
        checkIcon = list(yes = icon("ok", lib = "glyphicon")),
        selected = prev_selection
      )
    } else {
      h4('No students for this course.')
    }
  })
  
  ## Render Grade List
  output$grade_list <- renderUI({
    prev_selection <- input$select_grade
    
    if (!is.null(grades())) {
      radioGroupButtons(
        'select_grade',
        choices = grades(),
        direction = 'vertical',
        justified = FALSE,
        checkIcon = list(yes = icon("ok", lib = "glyphicon")),
        selected = prev_selection
      )
    } else {
      h4('No grades for this combination of course, assignment, and student.')
    }
  })
  
  ## Add Button Listeners -----------------------------------------------------
  ## Add Course
  observeEvent(input$add_course, {
    if(is.null(user_id)) {
      showModal(login_modal)
    } else {
      showModal(add_course_modal('add', pool, input))
    }
  })
  
  observeEvent(input$add_course_ok, {
    new_course <- data.frame(
      'course_name' = as.character(input$add_course_name),
      'course_semester' = as.character(input$add_course_semester),
      'course_year' = as.integer(input$add_course_year),
      'course_section' = as.character(input$add_course_section),
      'course_description' = as.character(input$add_course_description),
      'user_auth_id' = as.character(user_id()),
      stringsAsFactors = F
    )
    
    write_to_db(new_course, pool, 'courses')
    
    removeModal()
  })
  
  ## Add Assignment Category
  observeEvent(input$add_asscat, {
    if(is.null(user_id)) {
      showModal(login_modal)
    } else {
      showModal(add_asscat_modal('add', pool, input))
    }
  })

  observeEvent(input$add_asscat_ok, {
  
    new_asscat <- data.frame(
      'course_id' = as.integer(input$select_course),
      'asscat_name' = as.character(input$add_asscat_name),
      'asscat_weight' = as.integer(input$add_asscat_weight),
      stringsAsFactors = F
    )

    add_asscat_to_course(pool, new_asscat)

    removeModal()
  })
  
  ## Add Assignment
  observeEvent(input$add_assn, {
    if(is.null(user_id)) {
      showModal(login_modal)
    } else {
      showModal(add_assn_modal('add', pool, input))
    }
  })
  
  observeEvent(input$add_assn_ok, {
    
    new_assn <- data.frame(
      'course_id' = as.integer(input$select_course),
      'asscat_id' = as.integer(input$select_asscat),
      'assn_name' = as.character(input$add_assn_name),
      'assn_due' = as.Date(input$add_assn_due),
      'assn_notes' = as.character(input$add_assn_notes),
      stringsAsFactors = F
    )
    
    add_assn_to_course(pool, new_assn)
    
    removeModal()
  })
  
  ## Add Student
  observeEvent(input$add_student, {
    if(is.null(user_id)) {
      showModal(login_modal)
    } else {
      showModal(add_student_modal('add', pool, input))
    }
  })
  
  observeEvent(input$add_student_ok, {
    
    new_student <- data.frame(
      'course_id' = as.integer(input$select_course),
      'student_name' = as.character(input$add_student_name),
      stringsAsFactors = F
    )
    
    add_student_to_course(pool, new_student)
    
    removeModal()
  })
  
  ## Add Grade
  observeEvent(input$add_grade, {
    if(is.null(user_id)) {
      showModal(login_modal)
    } else {
      showModal(add_grade_modal('add', pool, input))
    }
  })
  
  observeEvent(input$add_grade_ok, {
    
    new_grade <- data.frame(
      'course_id' = as.integer(input$select_course),
      'assn_id' = as.integer(input$select_assn),
      'student_id' = as.integer(input$select_student),
      'grade_score' = as.integer(input$add_grade_score),
      'grade_notes' = as.character(input$add_grade_notes),
      stringsAsFactors = F
    )
    
    add_grade(pool, new_grade)
    
    removeModal()
  })
  
  ## Modify Button Listeners --------------------------------------------------
  ## Modify Course
  observeEvent(input$mod_course, {
    if(is.null(user_id)) {
      showModal(login_modal)
    } else {
      showModal(add_course_modal('mod', pool, input))
    }
  })
  
  observeEvent(input$mod_course_ok, {
    mod_course <- data.frame(
      'course_id' = as.numeric(input$select_course),
      'course_name' = as.character(input$add_course_name),
      'course_semester' = as.character(input$add_course_semester),
      'course_year' = as.integer(input$add_course_year),
      'course_section' = as.character(input$add_course_section),
      'course_description' = as.character(input$add_course_description),
      'user_auth_id' = as.character(user_id()),
      stringsAsFactors = F
    )
    
    update_course(pool, mod_course)
    
    reload_courses(reload_courses() + 1)
    
    removeModal()
  })
  
  ## Modify Assignment Category
  observeEvent(input$mod_asscat, {
    if(is.null(user_id)) {
      showModal(login_modal)
    } else {
      showModal(add_asscat_modal('mod', pool, input))
    }
  })
  
  observeEvent(input$mod_asscat_ok, {
    
    mod_asscat <- data.frame(
      'course_id' = as.integer(input$select_course),
      'asscat_id' = as.integer(input$select_asscat),
      'asscat_name' = as.character(input$add_asscat_name),
      'asscat_weight' = as.integer(input$add_asscat_weight),
      stringsAsFactors = F
    )
    
    update_asscat(pool, mod_asscat)
    
    reload_asscats(reload_asscats() + 1)
    
    removeModal()
  })
  
  ## Modify Assignment
  observeEvent(input$mod_assn, {
    if(is.null(user_id)) {
      showModal(login_modal)
    } else {
      showModal(add_assn_modal('mod', pool, input))
    }
  })
  
  observeEvent(input$mod_assn_ok, {
    
    mod_assn <- data.frame(
      'course_id' = as.integer(input$select_course),
      'asscat_id' = as.integer(input$select_asscat),
      'assn_id' = as.integer(input$select_assn),
      'assn_name' = as.character(input$add_assn_name),
      'assn_due' = as.Date(input$add_assn_due),
      'assn_notes' = as.character(input$add_assn_notes),
      stringsAsFactors = F
    )
    
    update_assn(pool, mod_assn)
    
    reload_assns(reload_assns() + 1)
    
    removeModal()
  })
  
  ## Modify Student
  observeEvent(input$mod_student, {
    if(is.null(user_id)) {
      showModal(login_modal)
    } else {
      showModal(add_student_modal('mod', pool, input))
    }
  })
  
  observeEvent(input$mod_student_ok, {
    
    mod_student <- data.frame(
      'course_id' = as.integer(input$select_course),
      'student_id' = as.integer(input$select_student),
      'student_name' = as.character(input$add_student_name),
      stringsAsFactors = F
    )
    
    update_student(pool, mod_student)
    
    reload_students(reload_students() + 1)
    
    removeModal()
  })
  
  ## Modify Grade
  observeEvent(input$mod_grade, {
    if(is.null(user_id)) {
      showModal(login_modal)
    } else {
      showModal(add_grade_modal('mod', pool, input))
    }
  })
  
  observeEvent(input$mod_grade_ok, {
    
    mod_grade <- data.frame(
      'course_id' = as.integer(input$select_course),
      'assn_id' = as.integer(input$select_assn),
      'student_id' = as.integer(input$select_student),
      'grade_score' = as.integer(input$add_grade_score),
      'grade_notes' = as.character(input$add_grade_notes),
      stringsAsFactors = F
    )
    
    update_grade(pool, mod_grade)
    
    reload_grades(reload_grades() + 1)
    
    removeModal()
  })
  
  ## Render Grade Displays ----------------------------------------------------
  output$grades_by_student <- renderUI({
    display_source <- grade_table(pool)
    
    student_accordian_list(display_source, input$select_course)
  })
  
  ## Pool Cleanup -------------------------------------------------------------
  session$onSessionEnded(function() {
    poolClose(pool)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

