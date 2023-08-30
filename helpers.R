# The function to calculate widths of data columns given the label column(-s) width(-s).
# Sample call:
# `get_col_width(df, lbl_width = 4, num_data_cols = 4)`
# - this will calculate the widths for each of four data columns (equally), given the label column has 4 in width.
get_col_widths <- function(x, lbl_width = c(2,), num_data_cols = 3, page_width = 11.42, left_margin = 0.56, right_margin = 1.06){
  total_page_width = page_width - left_margin - right_margin
  reserved_width = sum(lbl_width)
  other_col_width = (total_page_width - reserved_width) / (ncol(x) - length(lbl_width))
  other_col_width
}


# The function to get row numbers of groups that should be kept on the same page
# if we need rows 2-5 to be on the same page, this function will return c(2,3,4)
# 'keep_with_next' attribute will be applied to those rows in Word.
# currently, groups are determined by a tab. If next row has a tab in rowlbl,
# then it is considered part of the current group. When a row without tab is encountered,
# the group starts anew.

get_groups_from_df <- function(df, row_label_no=1){
  groups_result <- c()
  group.start <- 1
  
  for(i in 1:nrow(df)) {
    next_indent <- if (i<nrow(df)) grepl("\t", df[[i+1,row_label_no]], fixed=TRUE) else FALSE
    if (next_indent){groups_result <- c(groups_result, i)}
  }
  return (groups_result)
}

get_group_starts_from_df <- function(df, row_label_no=1){
  return (sapply(df[[row_label_no]], 
                 function(x) !grepl("\t", x, fixed=TRUE)) %>% 
            unname() %>% 
            which())
}