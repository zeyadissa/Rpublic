#' @title File Extraction from Online Zip Archives
#'
#' @description
#' Extracts specific files in from a Zip archive URL based on a set regex pattern
#'
#' @param files str: A URL to the zip archive of choice
#' @param pattern str: The regex pattern used to filter for the files in the Zip Archive
#' @param ignore.case bool: if \strong{FALSE} , the pattern matching is case sensitive and if \strong{FALSE} case is ignored during matching. Default is set to \strong{FALSE}
#' @param xlsx_flag bool: if \strong{FALSE} , the pattern ignores .xlsx/.xls files \strong{TRUE} The pattern matching will also extract Excel Sheets. Default is set to \strong{FALSE}
#' @param xlsx_sheet_pattern str: Regex pattern to match specific sheets in an .xlsx/.xls file. (See extract_excel_sheets) Default is set to return all sheets.
#' @param ... Passes additional arguments to data.table::fread
#' @return A list of data frames of either/and .csv and .xlsx/.xls sheets matching specified regex patterns in a given Zip Archive URL
#'
#' @examples
#' ## Extract all .csvs in
#' ## Make sure you have a decent internet connection!
#' nhsd_url <- 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites'
#' rtt_url <- '/2/2024/07/Full-CSV-data-file-Mar24-ZIP-3832K-revised.zip'
#' Rpublic::extract_zipped(files = paste0(nhsd_url,rtt_url),pattern='Annual-Report-spreadsheet|.xls')
#'
#' @export

#Read all csvs from urls; unz for zips
extract_zipped <- function(files,pattern,ignore.case = F, xlsx_flag=F , xlsx_sheet_pattern = '' , ...){

  #Unzipping specific files is not possible, so file needs to be read in temp
  temp <- tempfile()

  utils::download.file(files,temp,extdir=tempdir(),mode='wb')

  #Extract full list of file names.
  file_names <- utils::unzip(temp,list=T)$Name

  #Apply .xlsx/xls filter
  if(xlsx_flag ==T){
    files_names <- file_names[grepl('.xlsx|.xls|.csv',file_names)]
  } else {
    files_names <- file_names[grepl('.csv',file_names)]
  }

  #Apply pattern filter
  file_names <- file_names[grepl(pattern=pattern,
                                 x=file_names,
                                 ignore.case = ignore.case)]

  #d
  data <- lapply(X = file_names,
                 FUN = function(X){
                   if(tools::file_ext(X) == 'csv'){

                   dirty_data <- utils::unzip(zipfile = temp,
                                       files = X,
                                       exdir=tempdir())

                   clean_files(file=dirty_data,
                              newfile=dirty_data)

                   cleaned_data <- data.table::fread(dirty_data,
                                                     encoding = "UTF-8",...)

                   return(cleaned_data) } else {
                     cleaned_data <- Rpublic::extract_sheets(files = X,
                                             pattern = xlsx_sheet_pattern)
                     return(cleaned_data)
                   }
                 })

  names(data) <- file_names

  #unlink the temp file, important to do
  unlink(temp,recursive = T)

  return(data)}
