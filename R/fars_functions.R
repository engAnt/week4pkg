
#' Read a FARS data file
#'
#' @param string filename path to data file
#' @return tibble (data frame) holding observations of accidents.
#' Error if file not found.
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' @importFrom dplyr tbl_df 
#' @importFrom readr read_csv
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Generate a specific filename
#'
#' @param integer year Year
#' @return string filename
#' @examples
#' make_filename(2013)
#' make_filename(2015)
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Retrieve all months for which an observation exists
#'
#' @param integer years Vector of years.
#' @return list of tibbles (data frames) containing month and year variables.
#' NULL if data file not found for a particular year.
#' @examples
#' fars_read_years(2013:2017)
#' @importFrom dplyr mutate select 
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

#' Summarize observations by month across years
#'
#' @param integer years Vector of years.
#' @return tibble count of observations for all the months across years.
#' @examples
#' fars_summarize_years(2013:2014)
#' @importFrom dplyr group_by summarize bind_rows
#' @importFrom tidyr spread 
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map the locations of all accidents in a state
#'
#' @param integer state.num State number as seen in FARS dataset
#' @param integer year Year
#' @return plot (map) the locations where accidents occurred 
#' in the specified year for the specified state.
#' No plot is made if data does not exist for the specified state and year.
#' Error if the state has no data for the specified year.
#' @examples
#' fars_map_state(39, 2014)
#' @importFrom dplyr filter 
#' @importFrom maps map
#' @importFrom graphics points
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
