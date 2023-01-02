#' A function that reads a licorfile.
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
#' @param filepath A string with the filepath to read (if reading from excel).
#' @param sheetnumber An integer which sheet to read from the file.
#' @param parameters
#'   A list of desired parameters to extract. Default is everything.
#' @param numeric
#'   A boolean that regulates if the dataframe is changed to numeric.
#'   Default is TRUE (reccomended).
#' @param convert
#'   A boolean that regulates if the names from the Li-6400 files are changed
#'   to the Li-6800 format. Default is TRUE.
#' @export
#' @return Returns a dataframe with only the desired parameters.
#' @examples
#' parameters_to_extract <- c("A, Ci, elapsed, Qin")
#' dataframe <- read_licorfile("20210226 PI 50-100 75RH 400CO2 T.xlsx",
#'                          parameters_to_extract)
#'
read_licorfile <- function(filepath,
                           sheetnumber = 1,
                           parameters = c(),
                           numeric = TRUE,
                           convert = TRUE) {

  #get the file type
  filetype <- readxl::format_from_ext(path = filepath)

  #set the licor name
  licor <- ""

  #read the first line dendend on the type
  if (is.na(filetype) | filetype == "txt") {

    #read the first line
    con <- file(filepath, "r")
    first_line <- readLines(con, n=1)
    close(con)

    #set the licor number
    licor = get_licor_number(first_line)

  } else if (filetype == "xlsx") {

    #read the first line
    first_line <- readxl::read_xlsx(path = filepath,
                                    sheet = sheetnumber,
                                    col_names = FALSE,
                                    n_max = 1)

    #set the licor number
    licor = get_licor_number(first_line)

  } else if (filetype == "xls") {

    #ofthen results in errors, so it is in a try catch
    tryCatch(
      {
        #read the first line
        first_line <- readxl::read_xls(path = filepath,
                                       sheet = sheetnumber,
                                       col_names = FALSE,
                                       n_max = 1)

        #set the licor number
        licor = get_licor_number(first_line)


      },
      error = function(e) {
        stop(paste("read xls gave the following error:", e,
                   " Known issue,
                   look at https://github.com/tidyverse/readxl/issues/598 \n
                   try opening in excel and saving to new format xlsx"))
        return("error")
      }
    )

  } else if (filetype == "csv") {

    #read the first line
    first_line <- read.csv(file = filepath,
                           col_names = FALSE,
                           nrows = 1)

    #set the licor number
    licor = get_licor_number(first_line)

  } else {
    #if it is none of the above file types give an error
    stop("Wrong file type: File is not a 6800 licorfile,
         try to save it as an .xlsx file")
  }

  #call the right reading function dependent on the licor number
  if (licor == "6800") {
    #read the licor 6800 file
    dataframe <- read_licorfile_6800(filepath = filepath,
                                     parameters = parameters,
                                     numeric = numeric)
  } else if (licor == "6400") {
    #read the licor 6400 file
    dataframe <- read_licorfile_6400(filepath = filepath,
                                     parameters = parameters,
                                     convert = convert,
                                     numeric = numeric)
  } else {
    #give an error if the licorfile is not correct
    stop("No licor type found in the first line")
  }

  #return the right dataframe
  return(dataframe)
}

get_licor_number <- function(first_line) {

  #check if it is an Licor 6400 or 6800
  if (grepl("OPEN", first_line)) {
    print("Licor 6400 file")
    licor = "6400"

  } else if (grepl("Header", first_line) | grepl("SysConst", first_line[[1]])) {
    print("Licor 6800 file")
    licor = "6800"

  } else {
    stop("No licor type found in the first line")
  }

  #return the licor number
  return(licor)
}
