#'Calculate widths of data columns given the label column(-s) width(-s).
#'
#'get_col_widths(df, lbl_width = 4, num_data_cols = 4) - this will calculate the widths for each of four data columns (equally), given the label column has 4 in width.
#'
#'
#' @param x Dataframe under analysis.
#' @param lbl_width Width of the label column(s).  Default is c(2,)
#' @param num_data_cols Number of data columns in resulting table. Default is 3
#' @param page_width Expected page width. Default is 11.42 inch
#' @param left_margin Expected page left margin. Default is 0.56 inch
#' @param right_margin Expected page right margin. Default is 1.06
#' @export
#' @return Size of resulting data columns (all equal) in inches.
#'
#' @examples
#' get_col_widths(df, lbl_width = 4, num_data_cols = 4)

get_col_widths <- function(x, lbl_width = 2, num_data_cols = 3, page_width = 11.42, left_margin = 0.56, right_margin = 1.06){
  total_page_width = page_width - left_margin - right_margin
  reserved_width = sum(lbl_width)
  other_col_width = (total_page_width - reserved_width) / (ncol(x) - length(lbl_width))
  other_col_width
}



#' Get row numbers of some page groups.
#'
#'Get row numbers of groups that should be kept on the same page.
#' if we need rows 2-5 to be on the same page, this function will return c(2,3,4)
#' 'keep_with_next' attribute will be applied to those rows in Word.
#' Currently, groups are determined by a vector of possible different beginnings.
#'  If next row has a one of the specified strings in rowlbl,  then it is considered
#'  part of the current group. When a row without tab is encountered,
#' the group starts anew.
#'
#' @param df Dataframe under analysis.
#' @param indents a vector of indentation characters used in data frame
#' @param row_label_no Order number of rowlabel column.
#'
#' @return An integer vector of the row numbers of groups that should be kept on the same page
#' @export
#' @examples
#' t_1_1 <- data.frame(list(c('Age', '\t<18', '\t18<=65', 'Weight', '\t<150', '\t150<=250')))
#' group_starts <- get_groups_from_df(t_1_1, row_label_no=1)
get_groups_from_df <- function(df, indents=c("\t"), row_label_no=1){
    groups_result <- c()

    for(i in 1:(nrow(df)-1)) {
        next_indent <- FALSE
        for(j in 1:length(indents)){
            if(is.na(indents[[j]])){ next_indent <- is.na(df[[i+1,row_label_no]])}
            else next_indent <- grepl(paste0("^", indents[[j]]), df[[i+1,row_label_no]]) | next_indent
        }
        if (next_indent){groups_result <- c(groups_result, i)}
    }
    return (groups_result)
}


#' The function to get all group-starting rows from rowlabel column
#'
#' Row is considered a group start if one of the specified indents is detected
#' at its beginning, and the previous row has a different indent.
#' Currently, group start at first row can not be detected by this function.
#'
#' @param df Dataframe under analysis.
#' @param row_label_no Order number of rowlabel column.
#' @param group_starts a list of unique indents that are used in supplied data frame
#' @return Vector of the strings that are group-starting rows from rowlabel column
#' @export
#' @examples
#' t_1_1 <- data.frame(list(c('Age', '\t<18', '\t18<=65', 'Weight', '\t<150', '\t150<=250')))
#' group_starts <- get_group_starts_from_df(t_1_1, row_label_no=1)
get_group_starts_from_df <- function(df, group_starts=c('\t'), row_label_no=1)
    {

    groups_result <- c()

    detect_indent <- function(x, indent){
        if(is.na(indent)) curr_indent <- is.na(x) else curr_indent <- grepl(paste0("^", indent), x)
        return (curr_indent)
        }

    for(i in 2:(nrow(df)-1)) {
        is_new_group_start <- FALSE
        for(j in 1:length(group_starts)){
            if(detect_indent(df[[i,row_label_no]], group_starts[[j]]) & !detect_indent(df[[i-1,row_label_no]], group_starts[[j]])) {
                is_new_group_start = TRUE
            }
        }
        if (is_new_group_start){groups_result <- c(groups_result, i)}
    }
    return (groups_result)
}

