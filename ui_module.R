# MODALS ----------------------------------------------------------------------
## add_course_modal -----------------------------------------------------------
add_course_modal <-  function(type = c('add', 'mod'), pool, input) {
  
  if (type == 'mod') {
    selected_course <- get_filtered_table(
      pool, 
      'courses',
      list(names = c('id'), values = c(input$select_course))
    )
  }
  
  modalDialog(
    fluidRow(
      column(
        width = 6,
        textInput(
          'add_course_name',
          'Course Name',
          value = if (type == 'mod') selected_course$course_name else ''
        )
      ),
      column(
        width = 6,
        radioButtons(
          'add_course_semester',
          'Semester',
          choices = c('Fall', 'Spring', 'Summer'),
          selected = if (type == 'mod') selected_course$course_semester else NULL
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        textInput(
          'add_course_section',
          'Section',
          value = if (type == 'mod') selected_course$course_section else ''
        )
      ),
      column(
        width = 6,
        textInput(
          'add_course_year',
          'Year',
          value = if (type == 'mod') selected_course$course_year else ''
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        textAreaInput(
          'add_course_description',
          'Description',
          value = if (type == 'mod') selected_course$course_description else ''
        )
      )
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      {
        if (type == 'mod') {
          actionButton("mod_course_ok", "OK")
        } else {
          actionButton('add_course_ok', 'OK')
        }
      }
    )
  )
}

## add_asscat_modal -----------------------------------------------------------
add_asscat_modal <- function(type = c('add', 'mod'), pool, input) {
  if (type == 'mod') {
    selected_asscat <- get_filtered_table(
      pool, 
      'course_asscats',
      list(
        names = c('course_id', 'asscat_id'), 
        values = c(input$select_course, input$select_asscat)
      )
    )
  }
  
  modalDialog(
    fluidRow(
      column(
        width = 12,
        textInput(
          'add_asscat_name',
          'Assignment Category Name',
          value = if (type == 'mod') selected_asscat$asscat_name else ''
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        sliderInput(
          'add_asscat_weight',
          'Weight',
          min = 0, max = 100, 
          value = if (type == 'mod') selected_asscat$asscat_weight else 15, 
          step = 5, round = T
        )
      )
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      {
        if (type == 'mod') {
          actionButton("mod_asscat_ok", "OK")
        } else {
          actionButton('add_asscat_ok', 'OK')
        }
      }
    )
  )
}  

## add_assn_modal -------------------------------------------------------------
add_assn_modal <- function(type = c('add', 'mod'), pool, input) {
  
  if (type == 'mod') {
    selected_assn <- get_filtered_table(
      pool, 
      'course_assn',
      list(
        names = c('course_id', 'asscat_id', 'assn_id'), 
        values = c(input$select_course, input$select_asscat, input$select_assn)
      )
    )
  }

  modalDialog(
    fluidRow(
      column(
        width = 6,
        h3('Course'),
        p(input$select_course)
      ),
      column(
        width = 6,
        h3('Category'),
        p(input$select_asscat)
      )
    ),
    fluidRow(
      column(
        width = 6,
        textInput(
          'add_assn_name',
          'Assignment Name',
          value = if (type == 'mod') selected_assn$assn_name else ''
        )
      ),
      column(
        width = 6,
        dateInput(
          'add_assn_due',
          'Due Date',
          value = if (type == 'mod') selected_assn$assn_due else NULL
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        textAreaInput(
          'add_assn_notes',
          'Assignment Notes',
          value = if (type == 'mod') selected_assn$assn_notes else '' 
        )
      )
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      {
        if (type == 'mod') {
          actionButton("mod_assn_ok", "OK")
        } else {
          actionButton('add_assn_ok', 'OK')
        }
      }
    )
  )
} 

## add_grade_modal -------------------------------------------------------------
add_grade_modal <- function(pool, input) {
  
  modalDialog(
    fluidRow(
      column(
        width = 4,
        h3('Course'),
        p(input$select_course)
      ),
      column(
        width = 4,
        h3('Assignment'),
        p(input$select_assn)
      ),
      column(
        width = 4,
        h3('Student'),
        p(input$select_student)
      )
    ),
    fluidRow(
      column(
        width = 12,
        sliderInput(
          'add_grade_score',
          'Grade',
          min = 0, max = 100, step = 1, value = 100
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        textAreaInput(
          'add_grade_notes',
          'Grade Notes'
        )
      )
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("add_grade_ok", "OK")
    )
  )
} 

## add_student_modal ----------------------------------------------------------
add_student_modal <- function(type = c('add', 'mod'), pool, input) {
  
  if (type == 'mod') {
    selected_student <- get_filtered_table(
      pool, 
      'course_students',
      list(
        names = c('course_id', 'student_id'), 
        values = c(input$select_course, input$select_student)
      )
    )
  }
  
  modalDialog(
    fluidRow(
      column(
        width = 12,
        textInput(
          'add_student_name',
          'Student Name',
          value = if (type == 'mod') selected_student$student_name else ''
        )
      )
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      {
        if (type == 'mod') {
          actionButton("mod_student_ok", "OK")
        } else {
          actionButton('add_student_ok', 'OK')
        }
      }
    )
  )
} 

# CONTROLS --------------------------------------------------------------------
control_module <- function(title, type) {
  fluidRow(
    fluidRow(
      column(
        width = 12,
        h3(title)
      )
    ),
    column(
      width = 3,
      actionButton(
        paste0('add_', type), 
        '', 
        icon = icon('plus'), 
        class = 'btn btn-primary btn-lg', 
        style = 'margin: 5px 5px;'
      ),
      actionButton(
        paste0('mod_', type), 
        '', 
        icon = icon('pencil'), 
        class = 'btn btn-success btn-lg', 
        style = 'margin: 5px 5px;'
      ),
      actionButton(
        paste0('del_', type), 
        '', 
        icon = icon('trash'), 
        class = 'btn btn-danger btn-lg', 
        style = 'margin: 5px 5px;'
      )
    ),
    column(
      width = 9,
      uiOutput(paste0(type, '_list'))
    )
  )
}

# ACCORDIAN LISTS -------------------------------------------------------------
student_accordian_list <- function(display_source, course_id){
  ## Grades by Student, Student Loop
  students <- list()
  for (
    sid in display_source$`Student ID`[
      display_source$`Course ID` == course_id
      ]
  ) {
    students[[sid]] <- div(
      class = 'panel panel-default',
      div(
        class = 'panel-heading',
        h4(
          class = 'panel-title',
          a(
            `data-toggle` = 'collapse',
            `data-parent` = '#grades_by_student_accordian',
            href = paste0('#grades_by_student_collapse_student', sid),
            paste0(
              sid, '. ',
              display_source$`Student`[
                display_source$`Course ID` == course_id &
                  display_source$`Student ID` == sid
                ],
              ', Course Average: ',
              display_source$`Course Average`[
                display_source$`Course ID` == course_id &
                  display_source$`Student ID` == sid
                ], '%'
            )
          )
        )
      ),
      div(
        id = paste0('grades_by_student_collapse_student', sid),
        class = 'panel-collapse collapse',
        {
          student_category_accordian_list(display_source, course_id, sid)
        }
      )
    )
  }
  
  students[['class']] <- 'panel-group'
  students[['id']] <- 'grades_by_student_accordian'
  
  do.call(div, students)
}

student_category_accordian_list <- function(display_source, course_id, sid) {
  ## Grades by Student, Category List
  categories <- list()
  for (
    catid in display_source$`Category ID`[
      display_source$`Course ID` == course_id & 
      display_source$`Student ID` == sid
      ]
  ) {
    categories[[catid]] <- div(
      class = 'panel panel-default',
      style = 'margin: 10px;',
      div(
        class = 'panel-heading',
        h5(
          class = 'panel-title',
          a(
            `data-toggle` = 'collapse',
            `data-parent` = '#grades_by_student_cat_accordion',
            href = paste0('#grades_by_student_collapse_', sid, '_', catid),
            paste0(
              catid, '. ',
              display_source$`Assignment Category`[
                display_source$`Course ID` == course_id &
                  display_source$`Student ID` == sid &
                  display_source$`Category ID` == catid
                ],
              ', Category Average: ',
              display_source$`Category Average`[
                display_source$`Course ID` == course_id &
                  display_source$`Student ID` == sid &
                  display_source$`Category ID` == catid
                ], '%, Category Weight: ',
              display_source$`Category Weight`[
                display_source$`Course ID` == course_id &
                  display_source$`Student ID` == sid &
                  display_source$`Category ID` == catid
                ], '%'
            )
          )
        )
      ),
      div(
        id = paste0('grades_by_student_collapse_', sid, '_', catid),
        class = 'panel-collapse collapse',
        {
          student_category_assn_accordian_list(display_source, course_id, sid, catid)
        }
      )
    )
  }
  
  categories[['class']] <- 'panel-group'
  categories[['id']] <- 'grades_by_student_cat_accordian'
  
  do.call(div, categories)
}

student_category_assn_accordian_list <- function(
  display_source, 
  course_id, 
  sid, 
  catid
) {
  ## Grades by Student, Assignment List 
  assignments <- list()
  for (
    asid in display_source$`Assignment ID`[
      display_source$`Course ID` == course_id & 
        display_source$`Student ID` == sid &
        display_source$`Category ID` == catid
      ]
  ) {
    assignments[[asid]] <- div(
      class = 'panel panel-default',
      style = 'margin: 10px;',
      div(
        class = 'panel-heading',
        h6(
          class = 'panel-title',
          a(
            `data-toggle` = 'collapse',
            `data-parent` = '#grades_by_student_assn_accordion',
            href = paste0(
              '#grades_by_student_collapse_', sid, '_', catid, '_', asid
            ),
            paste0(
              asid, '. ',
              display_source$`Assignment`[
                display_source$`Course ID` == course_id &
                  display_source$`Student ID` == sid &
                  display_source$`Category ID` == catid &
                  display_source$`Assignment ID` == asid
                ],
              ', Due: ',
              display_source$`Due Date`[
                display_source$`Course ID` == course_id &
                  display_source$`Student ID` == sid &
                  display_source$`Category ID` == catid &
                  display_source$`Assignment ID` == asid
                ],
              ', Grade: ',
              display_source$`Grade`[
                display_source$`Course ID` == course_id &
                  display_source$`Student ID` == sid &
                  display_source$`Category ID` == catid &
                  display_source$`Assignment ID` == asid
                ]
            )
          )
        )
      ),
      div(
        id = paste0('grades_by_student_collapse_', sid, '_', catid, '_', asid),
        class = 'panel-collapse collapse',
        div(
          style = 'padding: 10px;',
          fluidRow(
            column(
              width = 6,
              p(
                paste(
                  'Scored at:',
                  display_source$`Scored At`[
                    display_source$`Course ID` == course_id &
                      display_source$`Student ID` == sid &
                      display_source$`Category ID` == catid &
                      display_source$`Assignment ID` == asid
                    ]
                )
              ),
              sliderInput(
                'accordian_update_grade_score',
                'Grade',
                min = 0, 
                max = 100, 
                step = 1, 
                value = display_source$`Grade`[
                  display_source$`Course ID` == course_id &
                    display_source$`Student ID` == sid &
                    display_source$`Category ID` == catid &
                    display_source$`Assignment ID` == asid
                  ]
              )
            ),
            column(
              width = 6,
              textAreaInput(
                'accordian_update_grade_notes',
                'Notes',
                width = '100%',
                height = '100%',
                display_source$`Notes`[
                  display_source$`Course ID` == course_id &
                    display_source$`Student ID` == sid &
                    display_source$`Category ID` == catid &
                    display_source$`Assignment ID` == asid
                  ]
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              actionButton(
                'accordian_update_grade',
                'Update',
                class = 'btn btn-primary'
              ),
              actionButton(
                'accordian_delete_grade',
                'Delete Score',
                class = 'btn btn-danger'
              )
            )
          )
        )
      )
    )
  }
  
  assignments[['class']] <- 'panel-group'
  assignments[['id']] <- 'grades_by_student_assn_accordian'
  
  do.call(div, assignments)
}