clean_files <- function(file,newfile){
  writeLines(iconv(readLines(file,skipNul = TRUE)),
             newfile)
}

extract_excel_sheets <- function(filename, pattern, tibble = T) {

  sheets <- readxl::excel_sheets(filename)

  sheets <- sheets[grepl(pattern=pattern,x=sheets)]

  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))

  if(!tibble) x <- lapply(x, as.data.frame)

  names(x) <- sheets

  return(x)

}
