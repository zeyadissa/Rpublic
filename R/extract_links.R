#' @title Easy URL Extraction
#'
#' @description
#' Extracts all links in a given URL matching a specific regex pattern as a vector
#' of strings.
#'
#' @param url_name str: A http/s URL that contains the URLs the user wishes to extract .
#' @param pattern str: The regex pattern used to filter for the extracted links.
#'                Default is set to return all URLs
#' @param ignore.case bool: if \strong{TRUE}, the pattern matching is case
#'                    sensitive and if \strong{FALSE} case is ignored during matching.
#'                    Default is set to \strong{FALSE}
#'
#' @return A vector of \emph{n} strings containing all links that match the regex pattern
#'         in the pattern param.
#'
#' @examples
#' ## Extract all .xls files with Annual Report in the name from the RTT wait times data
#' ## Make sure you have a decent internet connection!
#' rtt_url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/'
#' Rpublic::extract_links(url_name = rtt_url,pattern='Annual-Report-spreadsheet|.xls')
#'
#' @export

extract_links <- function(url_name,pattern='',ignore.case=F){

  # Read html info from url
  pg <- rvest::read_html(url_name)
  pg <- (rvest::html_attr(rvest::html_nodes(pg, "a"), "href"))
  # Filter list of files based off of regex pattern
  pg <- pg[grepl(pattern = pattern, x = pg, ignore.case = ignore.case)]
  pg <- unique(pg)

  return(pg)

}
