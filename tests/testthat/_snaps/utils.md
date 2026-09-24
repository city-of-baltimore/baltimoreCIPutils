# filter_program_data requires a single version unless multiple = TRUE

    Code
      filter_program_data(data, c("Adopted", "Planning"))
    Condition
      Error in `filter_program_data()`:
      ! `program_version` must be a single string, not a character vector.

# filter_program_data errors informatively

    Code
      filter_program_data(data, "Plannng")
    Condition
      Error in `filter_program_data()`:
      ! `program_version` must be one of "Planning", "Adopted", or "Draft", not "Plannng".
      i Did you mean "Planning"?
    Code
      filter_program_data(data, c("Planning", "Adopted"))
    Condition
      Error in `filter_program_data()`:
      ! `program_version` must be a single string, not a character vector.
    Code
      filter_program_data(data.frame(x = 1), "Planning")
    Condition
      Error in `filter_program_data()`:
      ! `program_data` must have the name "ProgramVersion".

