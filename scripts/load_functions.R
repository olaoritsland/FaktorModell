load_functions <- function() {
  
  f_folder <- "R/"
  functions <- list.files(f_folder)
  
  purrr::map(.x = functions, .f = ~source(paste0(f_folder, .)))
  
}

load_functions()
