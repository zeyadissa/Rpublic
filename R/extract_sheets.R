#' @title Sheet extraction from online excel workbook URLs
#'
#' @description
#' Extracts specific sheets as data.frames from excel urls based on a regex pattern
#'
#' @param files str: A URL to the .xls/.xlsx file of choice
#' @param pattern str: The regex pattern used to filter for the sheets in the .xlsx/.xls file
#' @return A list of data frames of either/and .csv and .xlsx/.xls sheets matching specified regex patterns in a given Zip Archive URL
#'
#' @examples
#' ## Extract all sheets matching the pattern string
#' ## Make sure you have a decent internet connection!
#' nhsd_url <- 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/'
#' rtt_url <- '2/2024/07/Admitted-Commissioner-Mar24-XLSX-1914K-revised.xlsx'
#' Rpublic::extract_sheets(files = paste0(nhsd_url,rtt_url),pattern='Nation')
#'
#' @export

#Download and read excel
extract_sheets <- function(files,pattern=''){

  #creates temp file to read in the data
  temp <- tempfile()

  utils::download.file(files,temp,mode='wb')

  data <- extract_excel_sheets(filename = temp, pattern = pattern)

  #unlink the temp file, important to do
  unlink(temp,recursive = T)

  return(data)}
