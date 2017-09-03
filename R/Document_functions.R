require(testthat)

#' "fars_read" 
#'
#' This is a function that loads a csv file (specified using the \code{filename} argument) and returns it as tibble.
#'
#' @param filename a character vector of length one which specifies the data path and/or filename which should be loaded.
#' 
#' @return This function returns a tibble with the contents of the csv. file 
#'
#' @details This function is a wrapper for the \code{\link[readr]{read_csv}} and \code{\link[dplyr]{tbl_df}} functions. Therefore
#'          the dplyr and readr package has to be installed and loaded.
#'
#' @note If the \code{filename} argument does not represent a file an error occurs.
#' 
#' @examples
#' \dontrun{ mydata <- fars_read("accident_2013.csv")}
#' \dontrun{myData <- fars_read("accident_2014.csv")}
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' "make_filename" 
#'
#' This is a function that creates form the \code{year} argument a filename the corresponding accident csv.
#'
#' @param year a numerical vector, which contains the years for which a filename should be generated.
#' 
#' @return This function returns a character vector containing the filenames corresponding to the  \code{year} argument.
#'
#' @details This function operates on strings.
#'
#' @note If the \code{year} argument is not a numerical vector the generated filename probobly do not exist.
#' 
#' @examples
#' \dontrun{ make_filename(2013)}
#' \dontrun{make_filename(2014)}
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' "fars_read_years" 
#'
#' This is a function that loads the columns MONTH of the accident data for all specified years (specified using the \code{years} argument) and returns a list.
#'
#' @param years a numeric vector, representing the years for which the accident_data should be loaded.
#' 
#' @return This function returns list for all the years specified in the \code{years} argument. Every entry of the list is a data.frame with the columns year and MONTH.
#'
#' @details This function combines the fars_read and make_filename function. The external functions \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{tbl_df}} and \code{\link[readr]{read_csv}} are included.
#'          Therefore the dplyr and readr package has to be installed and loaded.
#' 
#' @note If for an element of the \code{years} argument no file is found a warning occurs.
#' 
#' @examples
#' \dontrun{myData <- fars_read_years(c(2013,2014))}
#' \dontrun{myData <- fars_read_years(2014)}
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

#' "" 
#'
#' This is a function summarizes the number of accidents for every month of all selected years (using the \code{years} argument) and returns tibble with the columns MONTH and for every year in \code{years} a column with the number of accidents.
#'
#' @param years a numeric vector, representing the years for which the accident_data should be summarized.
#' fars_summarize_years
#' @return This function returns a tibble which contains in the first collumn the month and in the following columns for every year in \code{years} the number of accidents. for all the years specified in the \code{years} argument. Every entry of the list is a data.frame with the columns year and MONTH.
#'
#' @details This function is a wrapper for the fars_read_years function. The external functions \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{bind_rows}},
#'           \code{\link[dplyr]{summarize}}, \code{\link[tidyr]{spread}} \code{\link[dplyr]{tbl_df}} and \code{\link[readr]{read_csv}} are included.
#'          Therefore the dplyr , tidyr and readr package has to be installed and loaded.
#' 
#' @note If for an element of the \code{years} argument no corresponding file is found a warning occurs.
#' 
#' @examples
#' \dontrun{fars_summarize_years(2013)}
#' \dontrun{fars_summarize_years(c(2013,2014))}
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Print "fars_map_state" 
#'
#' This function plot a map where all accidents for the specified states (\code{state.num})  and the specified \code{year} are mapped, dependent on a csv file where these data are stored.
#'
#' @param state.num a numeric vector, specifying the state which should be plotted.
#' @param year a numeric vector of length one, specifying for which year the accidents should be plotted.
#' 
#' @return This function has no return value. It generates a plot with all accidents of the corresponding year and state.
#'
#' @details This function incluede the \code{\link[readr]{read_csv}}, \code{\link[dplyr]{tbl_df}}, \code{\link[dplyr]{filter}} and \code{\link[maps]{map}}  functions.
#'          Therefore the dplyr, maps and readr package has to be installed and loaded.
#'
#' @note If for the year no file exists an error occurs. If the state specifyed with \code{state.num} does not exist in the data an error occors. 
#'       If no coordinates exists for any of the accidents an error occurs.
#' 
#' @examples
#' \dontrun{fars_map_state(01 , 2013)}
#' \dontrun{fars_map_state(01 , 2014)}
#' \dontrun{fars_map_state(02 , 2014)}
#' @import dplyr
#' @import readr
#' @import tidyr
#' @import maps
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
