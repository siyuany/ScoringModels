#' preprocess
#'
#' @description
#'
#' @param data_set
#' @param xlsx_file
#' @return res
#'
#' @importFrom xlsx read.xlsx2
#' @importFrom stringr str_to_lower
#' @export

preprocess <- function(data_set, xlsx_file = "data_description",
                       sheetName = "Data Overview") {
    filename <- paste0(xlsx_file, ".xlsx")
    data_desc <- read.xlsx2(filename, sheetName, header = TRUE)
    variables <- data_desc$Var[str_to_lower(data_desc$in_model == 'yes') |
                               str_to_lower(data_desc$in_model) == 'default']
    data_copy <- data_set[variables]

    data_copy
}
