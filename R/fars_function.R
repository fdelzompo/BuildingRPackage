#' fars_read
#' A funtion that checks if the file exists and returns the content as tbl_df.
#' No need to be exported as it's mainly used inside the package
#'
#' @param filename A character containing the filename built with the \code{make_filename} function
#'
#' @return returns a dataframe (more exactly a tbl_df from dplyr) with the information from
#' the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System.
#' If the filename doesn't exists it returs an error.
#'
#' @examples
#' #' \dontrun{
#' fars_read('accident_2015.csv.bz2')
#' fars_read(make_filename(2015))
#'}
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' make_filename
#' A function that returns the filename for the FARS database given a certain year.
#' No need to be exported as it's mainly used inside the package
#'
#' @param year A character or number (it will be coerced to integer in the function) containing the year
#'
#' @return returns a character with the correct file name to be searched in the FARS database.
#' It will return an error if input is incorrect (ie '2015c')
#'
#' @examples
#' #' \dontrun{
#' make_filename(2015)
#' make_filename('2015')
#'}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#' A funtion that create a list of dataset combining the  \code{make_filename}  \code{fars_read} functions.
#'
#' @param years A character or numeric vector containing the years to import.
#'
#' @return returns a list of dataframes (more exactly a tbl_df from dplyr) with the information from
#' the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System.
#' If the filename with the reference year doesn't exists it returs a warning and a NULL element in the list.
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2014,2015))
#' fars_read_years(c('2013','2014'))
#' }
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter %>%
#' @importFrom GlobalEnv make_filename
#' @importFrom GlobalEnv fars_read
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years
#' A funtion that summarise how many accident per year and month are registered in the FARS dataset.
#'
#' @param years A character or numeric vector containing the years to summirise.
#'
#' @return returns a dataframe (more exactly a tbl_df from dplyr) with the number of accident per Year/Month
#' in the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System.
#' It will return a warning if the years are not presented in the dataset.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2014,2015))
#' fars_summarize_years(c('2013','2014'))
#'}
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr filter %>%
#' @importFrom tidyr spread
#' @importFrom GlobalEnv fars_read_years
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state
#' A funtion that maps the location of the accident during a specific year in one or more states.
#' Returns error if state number is not present or year is incorrect.
#' If there are no accidents (hopefully) it returns a message 'No accident to plot'
#'
#' @param state.num A character or numeric containing the number state to  map (coerced to integer in the function).
#' @param year A character or numeric element containing the year to summirise.
#'
#' @return returns map with the accident as points.
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2014)
#' fars_map_state('20','2014')
#'}
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom graphics points
#' @importFrom dplyr filter
#' @importFrom dplyr filter %>%
#' @importFrom maps map
#' @importFrom GlobalEnv make_filename
#' @importFrom GlobalEnv fars_read
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}


