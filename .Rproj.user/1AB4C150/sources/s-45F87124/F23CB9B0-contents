#' Read filename
#'
#' This function reads a csv file.
#'
#' @param filename A character string giving the name of the file to read.
#'
#' @return This function returns a dataframe with the data contained in the specified file.
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
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

#' Make filename
#'
#' This function returns the name of a filename.
#'
#' @param year Year of the data that the file will contain.
#'
#' @return This function returns a string with the name of the file.
#'
#' @examples
#' \dontrun{
#' make_filename(2017)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Months in data
#'
#' This function returns the month of each observation for the selected years.
#'
#' @param years A vector containing the years to read.
#'
#' @return This function returns a list, in which each element contains the month
#' for each observation of an specific year.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' }
#'
#' @import magrittr
#' @importFrom dplyr mutate select
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

#' Summarize monthly observations
#'
#' This function summarizes the number of observations for each month of the selected years.
#'
#' @param years A vector containing the years to read.
#'
#' @return This function returns a dataframe. The first column indicates the month, and each
#' subsequent column shows the number of observations in that month for a specific year.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2015)
#' }
#'
#' @import magrittr
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot state accidents
#'
#' This function plots the accidents that occured within the selected years in the selected states.
#'
#' @param state.num A vector indicating the number of the states to plot.
#' @param year A vector indicating the years of the accidents to plot.
#'
#' @return This function returns a dataframe. The first column indicates the month, and each
#' subsequent column shows the number of observations in that month for a specific year.
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2014)
#' fars_map_state(5:10, 2013:2015)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
