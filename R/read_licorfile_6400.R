#' A function that reads a 6400 licorfile.
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
read_licorfile_6400 <- function(filepath,
                                sheetnumber = 1,
                                parameters = c(),
                                convert = TRUE,
                                numeric = TRUE) {

  #print which file to read
  print(paste("Reading:", filepath))

  #get the file type
  filetype <- readxl::format_from_ext(path = filepath)

  #read the licor file
  if (is.na(filetype) | filetype == "txt") {
    print("filetype: NA (.txt)")

    #find the line where the data starts
    con <- file(filepath, "r")
    index <- 1
    while (TRUE) {
      line = readLines(con, n = 1)
      if (line == "$STARTOFDATA$" ) {
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
    header_index <- 1
    header <- dataframe[c(header_index), ]

    #set header
    colnames(dataframe) <- header

  } else if (filetype == "xlsx") {
    print("filetype: .xlsx")
    dataframe <- readxl::read_xlsx(filepath,
                                   sheet = sheetnumber,
                                   col_names = FALSE)

    #find and get header
    header_index <- which(dataframe[1] == 'Obs')
    header <- dataframe[c(header_index), ]

    #set header
    colnames(dataframe) <- header

  } else if (filetype == "xls") {
    print("filetype: .xls")
    dataframe <- readxl::read_xls(path.expand(filepath),
                                  sheet = sheetnumber,
                                  col_names = FALSE)

    #find and get header
    header_index <- which(dataframe[1] == 'Obs')
    header <- dataframe[c(header_index), ]

    #set header
    colnames(dataframe) <- header

  } else if (filetype == "csv") {
    print("filetype: .csv")
    dataframe <- read.csv(file = filepath,
                          col.names = FALSE)

    #find and get header
    header_index <- which(dataframe[1] == 'Obs')
    header <- dataframe[c(header_index), ]

    #set header
    colnames(dataframe) <- header

  } else {
    #if it is none of the above file types give an error
    stop("Wrong file type: File is not a 6400 licorfile,
         try to save it as an .xlsx file")

  }

  #remove all remarks/non data rows and header
  remarklist <- suppressWarnings(which(is.na(as.numeric(dataframe[[1]])) == TRUE))
  print(length(remarklist))
  if (length(remarklist) > 0) {
    dataframe <- dataframe[-remarklist, ]
  }

  #change names to new 6800 format
  if (convert) {
    #change Photo to A
    names(dataframe)[names(dataframe) == 'Photo'] <- 'A'
    #change Trmmol to E
    names(dataframe)[names(dataframe) == 'Trmmol'] <- 'E'
    #change Cond to gsw
    names(dataframe)[names(dataframe) == 'Cond'] <- 'gsw'
    #change Tleaf to TleafCnd
    names(dataframe)[names(dataframe) == 'Tleaf'] <- 'TleafCnd'
    #change CO2S to CO2_s
    names(dataframe)[names(dataframe) == 'CO2S'] <- 'CO2_s'
    #change CO2R to CO2_r
    names(dataframe)[names(dataframe) == 'CO2R'] <- 'CO2_r'
    #change H2OS to H2O_s
    names(dataframe)[names(dataframe) == 'H2OS'] <- 'H2O_s'
    #change H2OR to H2O_r
    names(dataframe)[names(dataframe) == 'H2OR'] <- 'H2O_r'
    #change PARi to Qin
    names(dataframe)[names(dataframe) == 'PARi'] <- 'Qin'
    #change Vpdl to VPDleaf
    names(dataframe)[names(dataframe) == 'VpdL'] <- 'VPDleaf'
    #change Press to Pa
    names(dataframe)[names(dataframe) == 'Press'] <- 'Pa'

    #calculate RHcham
    delta_Pa <- 0
    dataframe$VPcham <- as.numeric(dataframe$H2O_s) * (as.numeric(dataframe$Pa) + delta_Pa) / 1000
    dataframe$SVPcham <- 0.61365 * exp((17.502 * as.numeric(dataframe$Tair)) / (240.97 + as.numeric(dataframe$Tair)))
    dataframe$RHcham <- dataframe$VPcham / dataframe$SVPcham * 100

    #calculate elapsed
    dataframe$elapsed <- (as.numeric(dataframe$FTime) - as.numeric(dataframe$FTime[1]))
  }

  if (length(parameters) >= 1) {
    #Select parameters if they are included
    dataframe <- extract_parameters(dataframe, parameters)
  }

  if (numeric) {
    #change all columns to numeric
    dataframe <- numeric_dataframe(dataframe)
  }

  return(dataframe)
}
