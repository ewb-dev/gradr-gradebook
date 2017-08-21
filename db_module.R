## Utility Functions ----------------------------------------------------------
get_max_id <- function(pool, table) {
  max_id <- dbGetQuery(
    pool, 
    paste(
      "SELECT MAX(id) from",
      table
    )
  ) + 1
  
  max_id[[1]]
}

write_to_db <- function(data, pool, table) {
  
  text_cols <- sapply(data, is.character)
  data[, text_cols] <- paste0('\'', data[, text_cols], '\'')
  
  dbExecute(
    pool,
    paste(
      'insert into ',
      table,
      paste0(
        '(',
        paste(names(data), collapse = ', '),
        ')'
      ),
      'values',
      paste0(
        '(',
        do.call(paste, c(data, sep = ", ")),
        ')',
        collapse = ', '
      )
    )
  )
}

get_all_table <- function(pool, table) {
  dbGetQuery(
    pool,
    paste(
      'SELECT * from',
      table
    )
  )
}

get_filtered_table <- function(pool, table, filter_list) {
  filter_string <- purrr::map2_chr(
    filter_list$names, 
    filter_list$values, function(x,y) {
      paste(
        x,
        '=',
        {
          if (is.character(y)) {
            paste0('\'', y, '\'')
          } else {
            y
          }
        }
      )
    }
  ) %>%
    paste(collapse = ' and ')
  
  dbGetQuery(
    pool,
    paste(
      'select * from',
      table,
      'where',
      filter_string
    )
  )
}

## Named List Functions -------------------------------------------------------
courses_named_list <- function(pool) {
  courses <- get_all_table(pool, 'courses')
  if (nrow(courses) > 0) {
    course_choices <- setNames(
      courses$id, 
      paste0(
        courses$course_name, 
        ' (', 
        courses$course_semester, 
        ' ', 
        courses$course_year, 
        ', Section ', 
        courses$course_section, 
        ')'
      )
    )
  }
}

course_asscats_named_list <- function(pool, course_id) {
  asscats <- get_filtered_table(
    pool,
    'course_asscats',
    list(
      names = list('course_id'),
      values = list(course_id)
    )
  )
  
  
  if (nrow(asscats) > 0) {
    asscat_choices <- setNames(
      asscats$asscat_id,
      paste0(
        asscats$asscat_name,
        ' - ',
        asscats$asscat_weight,
        '%'
      )
    )
  }
}

course_assn_named_list <- function(pool, course_id, asscat_id) {
  assns <- get_filtered_table(
    pool,
    'course_assn',
    list(
      names = list('course_id', 'asscat_id'),
      values = list(course_id, asscat_id)
    )
  )
  
  
  if (nrow(assns) > 0) {
    assn_choices <- setNames(
      assns$assn_id,
      paste0(
        assns$assn_name,
        ' - ',
        format(assns$assn_due, '%m/%d/%Y')
      )
    )
  }
}

course_students_named_list <- function(pool, course_id) {
  students <- get_filtered_table(
    pool,
    'course_students',
    list(
      names = list('course_id'),
      values = list(course_id)
    )
  )
  
  
  if (nrow(students) > 0) {
    student_choices <- setNames(
      students$student_id,
      students$student_name
    )
  }
}

course_grades_named_list <- function(pool, course_id, assn_id, student_id) {
  grades <- get_filtered_table(
    pool,
    'grades',
    list(
      names = list('course_id', 'assn_id', 'student_id'),
      values = list(course_id, assn_id, student_id)
    )
  )
  
  
  if (nrow(grades) > 0) {
    grades$grade_score
  }
}

## Add Element Functions ------------------------------------------------------
add_asscat_to_course <- function(pool, data) {
  dbExecute(
    pool,
    paste(
      'insert into course_asscats (course_id, asscat_id, asscat_name, asscat_weight)
      values (
        ', data$course_id, ',
        coalesce((select max(asscat_id) + 1 from course_asscats where course_id = ', data$course_id, '), 1),
        \'', data$asscat_name, '\',
        ', data$asscat_weight, '
      )'
    )
  )
}

add_assn_to_course <- function(pool, data) {
  dbExecute(
    pool,
    paste(
      'insert into course_assn (course_id, asscat_id, assn_id, assn_name, assn_due, assn_notes)
      values (
        ', data$course_id, ',
        ', data$asscat_id, ',
        coalesce((select max(assn_id) + 1 from course_assn where course_id = ', data$course_id, '), 1),
        \'', data$assn_name, '\',
        \'', data$assn_due, '\',
        \'', data$assn_notes, '\'
      )'
    )
  )
}

add_student_to_course <- function(pool, data) {
  dbExecute(
    pool,
    paste(
      'insert into course_students (course_id, student_id, student_name)
      values (
        ', data$course_id, ',
        coalesce((select max(student_id) + 1 from course_students where course_id = ', data$course_id, '), 1),
        \'', data$student_name, '\'
      )'
    )
  )
}

add_grade <- function(pool, data) {
  dbExecute(
    pool,
    paste(
      'insert into grades (course_id, assn_id, student_id, grade_score, grade_notes)
      values (
        ', data$course_id, ',
        ', data$assn_id, ',
        ', data$student_id, ',
        \'', data$grade_score, '\',
        \'', data$grade_notes, '\'
      )'
    )
  )
}

## Modify Element Functions ---------------------------------------------------
update_course <- function(pool, updated_record) {
  dbExecute(
    pool,
    paste0(
      'update courses
       set course_name = \'', updated_record$course_name, '\',
        course_semester = \'', updated_record$course_semester, '\',
        course_year = \'', updated_record$course_year, '\',
        course_section = \'', updated_record$course_section, '\',
        course_description = \'', updated_record$course_description, '\',
        created_at = now()
       where id = ', updated_record$course_id
    )
  )
}

update_asscat <- function(pool, updated_record) {
  dbExecute(
    pool,
    paste0(
      'update course_asscats
       set asscat_name = \'', updated_record$asscat_name, '\',
        asscat_weight = \'', updated_record$asscat_weight, '\',
        created_at = now()
       where course_id = ', updated_record$course_id, '
        and asscat_id = ', updated_record$asscat_id
    )
  )
}

update_assn <- function(pool, updated_record) {
  dbExecute(
    pool,
    paste0(
      'update course_assn
       set assn_name = \'', updated_record$assn_name, '\',
        assn_due = \'', updated_record$assn_due, '\',
        assn_notes = \'', updated_record$assn_notes, '\',
        created_at = now()
       where course_id = ', updated_record$course_id, '
        and asscat_id = ', updated_record$asscat_id, '
        and assn_id = ', updated_record$assn_id
    )
  )
}

update_student <- function(pool, updated_record) {
  dbExecute(
    pool,
    paste0(
      'update course_students
       set student_name = \'', updated_record$student_name, '\',
        created_at = now()
       where course_id = ', updated_record$course_id, '
        and student_id = ', updated_record$student_id
    )
  )
}

update_grade <- function(pool, updated_record) {
  dbExecute(
    pool,
    paste0(
      'update course_grade
       set grade_score = \'', updated_record$grade_score, '\',
        grade_notes = \'', updated_record$grade_notes, '\',
        created_at = now()
       where course_id = ', updated_record$course_id, '
        and assn_id = ', updated_record$assn_id, '
        and student_id = ', updated_record$student_id
    )
  )
}

## Specific Use Queries -------------------------------------------------------
grade_table <- function(pool) {
  dbGetQuery(
    pool, 
    paste(
      'select * from categorized_grades'
    )
  ) %>%
    group_by(Course, Student, `Assignment Category`) %>%
    mutate(
      `Category Average` = round(mean(Grade), 2),
      `Average Weight` = `Category Average` * `Category Weight`
    ) %>%
    group_by(Course, Student) %>%
    mutate(
      `Course Average` = round(sum(`Average Weight`)/sum(`Category Weight`), 2)
    ) %>%
    ungroup() %>%
    mutate(
      `Due Date` = format.Date(`Due Date`, '%m/%d/%Y')
    )
}