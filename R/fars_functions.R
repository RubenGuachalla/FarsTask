###########################################
#                                         #
#  Author : Ruben Guachalla Condorena     #
#  Project: Building R Packages - Week 2  #
#                                         #
###########################################

#' Title fars_read
#'
#' This function accepts a csv file and returns a tbl of the data
#'
#' @param filename a csv file in the workspace
#'
#' @return a dplyr tbl_df, a container for the data that comes from the csv file.
#'    If the file does not exist, an error message will be displayed.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
#'
#' @examples
#' \dontrun{x1 <- fars_read('somefile_1.csv')}
#' \dontrun{x2 <- fars_read("somefile_2.csv")}

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Title make_filename
#'
#' This helper function allows the user to create accident data files by year.
#' bz2 files are compressed files, usually on UNIX, for machines that don't support
#' the tar/tarball format
#'
#' @param year  A year as a string
#'
#' @return a filename in the format 'accident_year.csv.bz2'. further, the filename is showed by the function.
#'
#' @source extdate/accident_year.csv.bz2
#'
#' @export
#'
#' @examples newfile <- make_filename('2017')
#' \dontrun{system.file("extdata", "accident_year.csv.bz2", package = "packFars")}
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Title fars_read_years
#'
#'
#'this function accepts as input one or more years as an object of type list, calls
#'the function make_filename () and uses those years.
#'
#' if the year is not valid the function will stop and show an error message
#'
#' Uses make_filename(year)
#'      fars_read(file)
#'
#' @param years one or more years as an atomic value or a list
#'
#' @return Creates one or more datasets based on year number.  Returns NULL if there is an error
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
#'
#' @examples
#' \dontrun{
#'      fars_read_years(1999)
#'      fars_read_years(1999:2016)
#' }
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

#' Title fars_summarize_years
#'
#'
#' It takes a list of years and passes it to fars_read_years (), receives data from
#' the month and year variable and uses the dplyr environment to count the respective
#' count per month for a year.
#'
#' Uses fars_read_years(years)
#'
#' @param One or more years, no error checking
#'
#' @return A wide data frame of counts by month and year,
#'
#' @importFrom  dplyr bind_rows
#' @importFrom  dplyr group_by
#' @importFrom  dplyr summarize
#' @importFrom  tidyr spread
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#'      fars_summarize_years(1999)
#'      fars_summarize_years(1999:2016)
#' }
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Title fars_map_state
#'
#' enter the state number and year of the user.
#' using the year and the make_filename function makes the name.
#' gets a data frame from fars_read().
#'
#' check that the status number exists
#' if that is the way it uses charts and maps based on coordinates from the data file.
#'
#' Uses make_filename(year)
#'      fars_read(filename)
#'
#' @param state.num Number of a state
#' @param year The year in question
#'
#' @return A plot or set of plots based on latitude and longitude from the data file
#'
#' @export

#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
#'
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
