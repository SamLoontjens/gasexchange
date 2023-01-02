#' A function that reads a 6800 licorfile.
#'
#' @description
#'   A function that reads a licorfile.
#'   It returns a clean dataframe with only the desired parametes.
#'
#'   No dots in the file path accepted, except one for the file type.
#'   The functions of this package expect the parameter names of a Li-6800, so
#'   change_names = TRUE is recommended.
#'   Li-6800 manual page 226
#'   Li-6400 manual page 21 (81 of 1324)
#' @author Sam Loontjens
#' @param filepath A string with the filepath to read.
#' @param sheetnumber An integer which sheet to read from the file.
#' @param parameters
#'   A list of desired parameters to extract. Default is everything.
#' @param change_names
#'   A boolean that regulates if the names from the Li-6400 files are changed
#'   to the Li-6800 format. Default is TRUE.
#' @export
#' @return Returns a dataframe with only the desired parameters.
#' @examples
#' parameters_to_extract <- c("A, Ci, elapsed, Qin")
#' dataframe <- read_licorfile("20210226 PI 50-100 75RH 400CO2 T.xlsx",
#'                          parameters_to_extract)
#'
read_licorfile_6800 <- function(filepath,
                                sheetnumber = 1,
                                parameters = c(),
                                numeric = TRUE) {

  #print which file to read
  print(paste("Reading:", filepath))

  #get the file type
  filetype <- readxl::format_from_ext(path = filepath)

  #read the licor file dendend on the type
  if (is.na(filetype) | filetype == "txt") {
    print("filetype: NA (.txt)")

    #find where the data starts
    con <- file(filepath, "r")
    index <- 1
    while (TRUE) {
      line = readLines(con, n = 1)
      if (line == "[Data]" ) {
        break
      }
      index = index + 1
    }
    close(con)

    dataframe <- readr::read_tsv(file = filepath,
                                 col_names = FALSE,
                                 skip = index,
                                 show_col_types = FALSE)

    #get header
    header_index <- 2
    header <- dataframe[c(header_index), ]

    #remove header and units
    dataframe <- dataframe[-c(1:(header_index+1)), ]

    #set header
    colnames(dataframe) <- header

  } else if (filetype == "xlsx") {
    print("filetype: .xlsx")
    dataframe <- readxl::read_xlsx(filepath,
                                   sheet = sheetnumber,
                                   col_names = FALSE)
    #find and get header
    header_index <- which(dataframe[1] == 'obs')
    header <- dataframe[c(header_index), ]

    #remove header and units
    dataframe <- dataframe[-c(1:(header_index+1)), ]

    #set header
    colnames(dataframe) <- header

  } else if (filetype == "csv") {
    print("filetype: .csv (why is your licor 6800 file a csv?)")
    dataframe <- read.csv(file = filepath,
                          col.names = FALSE)
    #find and get header
    header_index <- which(dataframe[1] == 'obs')
    header <- dataframe[c(header_index), ]

    #remove header and units
    dataframe <- dataframe[-c(1:(header_index+1)), ]

    #set header
    colnames(dataframe) <- header

  } else {
    #if it is none of the above file types give an error
    stop("Wrong file type: File is not a 6800 licorfile,
         try to save it as an .xlsx file")

  }

  #Select parameters if they are included
  if (length(parameters) >= 1) {
    dataframe <- extract_parameters(dataframe, parameters)
  }

  #change all columns to numeric
  if (numeric) {
    dataframe <- numeric_dataframe(dataframe)
  }

  return(dataframe)
}
