#' data_overview
#'
#' @description \code{data_overview} generates an excel file describe the data's
#' basic information like the dimensions, the type of each column, basic stats
#' and so on. Most importantly, the following precedure will base on the
#' (revised) excel file to perform data cleansing.
#'
#' @param data_set The data set
#' @param type_infer \code{TRUE}(default)/\code{FALSE}. If \code{TRUE}, the type
#' of the column will be inferred based on the pattern of the content. Otherwise,
#' the type will be the storing type of the column.
#' @param out_file The output filename. If \code{NULL} or \code{NA} given, no
#' output file will be generated, and the result will be returned as a
#' \code{data.frame}, used for debugging.
#'
#' @return res If \code{out_file} is \code{NULL} or \code{NA}, a data.frame will
#' be returned. Otherwise, nothing returned but an excel file will be generated.
#'
#' @importFrom xlsx createWorkbook
#' @importFrom xlsx createSheet
#' @importFrom xlsx saveWorkbook
#' @importFrom xlsx write.xlsx2
#' @importFrom timeDate skewness
#' @importFrom timeDate kurtosis
#' @export

data_overview <- function(data_set, type_infer = TRUE,
                          out_file = "data_description") {

    # infer the type of a column by given pattern.
    type_inference <- function(column) {
        if (is.numeric(column)) {
            return("Numeric")
        } else {
            pattern = "^\\s*-{0,1}\\d*(\\.\\d*){0,1}([eE]-{0,1}\\d+){0,1}\\s*$"
            if (all(grepl(pattern, column))) {
                return("Numeric")
            } else {
                return("Character")
            }
        }
    }

    # return the storing type of the column
    type_reflect <- function(column) {
        if (is.numeric(column)) {
            return("Numeric")
        } else {
            return("Character")
        }
    }

    # type
    if (type_infer) {
        types <- sapply(data_set, type_inference)
    } else {
        types <- sapply(data_set, type_reflect)
    }

    # basic stats
    basic_stats <- lapply(data_set, function(x) {
        cnt <- tapply(x, x, length)
        na_cnt <- sum(is.na(x))
        if (na_cnt > 0)
            cnt <- c(cnt, 'na' = na_cnt)
        cnt <- sort(cnt, decreasing = TRUE)
        list(Obs      = length(x),
             Mode_10  = paste0(head(names(cnt), 10), collapse = ', '),
             HRatio   = unname(cnt[1]) / sum(cnt),
             Missing  = na_cnt,
             MissingR = na_cnt / sum(cnt),
             UniValue = ifelse(na_cnt > 0, length(cnt) - 1, length(cnt)))
    })
    basic_stats <- t(do.call(cbind, args = basic_stats))
    basic_stats <- as.data.frame(basic_stats, stringsAsFactors = FALSE)

    # six-point summary (Min., 1st Qu., Median, Mean, 3rd Qu., Max.)
    numeric_columns <- names(types)[types == 'Numeric']
    sps <- lapply(data_set[numeric_columns], function(x) {
        x <- as.numeric(x)
        s <- summary(x)
        list("Min."     = s[1],
             "1st.Qu."  = s[2],
             "Median"   = s[3],
             "3rd.Qu."  = s[5],
             "Max."     = s[6],
             "Mean"     = s[4],
             "Sd"       = sd(x, na.rm = TRUE),
             "Skewness" = skewness(x, na.rm = TRUE),
             "Kurtosis" = kurtosis(x, na.rm = TRUE))
    })
    sps <- as.data.frame(t(do.call(cbind, args = sps)),
                         stringsAsFactors = FALSE)
    sps$Variable <- rownames(sps)
    rownames(sps) <- NULL

    old_options <- options()
    options(stringsAsFactors = FALSE)
    res <- cbind(Variable = names(types),
                 in_model = 'default',
                 Type     = types,
                 basic_stats)
    options(old_options)
    rownames(res) <- NULL

    res <- merge(res, sps, by = "Variable", all.x = TRUE)
    res <- as.data.frame(lapply(res, unlist), stringsAsFactors = FALSE)
    rownames(res) <- NULL
    colnames(res) <- c("Variable", "in_model", "Type", "Obs",
                       "Mode_10", "HRatio", "Missing", "MissingR",
                       "UniValue", "Min.", "1st.Qu.", "Median", "3rd.Qu.",
                       "Max.", "Mean", "Sd", "Skewness", "Kurtosis")

    if (is.null(out_file) | is.na(out_file)) {
        return(res)
    } else {
        filename <- paste0(out_file, ".xlsx")
        wb <- createWorkbook(type = "xlsx")
        intro_sheet <- createSheet(wb, sheetName = "Introduction")
        data_ov_sheet <- createSheet(wb, sheetName = "Data Overview")
        introduction <- t(data.frame(
            "in_model" = "Whether the variable should be included in model: default/yes/no/objective",
            "Type" = "Type of a column: Numeric/Character, can be revised",
            "Obs" = "Number of observations",
            "Mode_10" = "10 values of the highest frequency in descending order",
            "HRatio" = "Ratio of the value with the highest frequency",
            "Missing" = "Number of missing values",
            "MissingR" = "Ratio of missing values",
            "UniValue" = "Number of unique values",
            "Min." = "Minimum value",
            "1st.Qu." = "1st Quartile",
            "Median" = "Median",
            "3rd.Qu." = "3rd Quartile",
            "Max." = "Maximun value",
            "Mean" = "Average with NA removed",
            "Sd" = "Standard deviation with NA removed",
            "Skewness" = "Skewness",
            "Kurtosis" = "Kurtosis",
            stringsAsFactors = FALSE
        ))
        addDataFrame(introduction, sheet = intro_sheet, col.names = FALSE)
        addDataFrame(res, sheet = data_ov_sheet, row.names = FALSE)
        saveWorkbook(wb, file = filename)
        invisible()
    }
}
