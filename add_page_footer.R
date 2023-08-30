# FUNCTION to add page footer row.
add_page_footer <- function(list_of_lists) {
  # Check if all lists have length <=3
  if (any(sapply(list_of_lists, length) > 3)) {
    stop("All sublists must have length <= 3")
  }
  
  # List to hold all data frames
  dfs <- list()
  
  for (i in seq_along(list_of_lists)) {
    elements <- list_of_lists[[i]]
    
    # Create a vector of length 3 filled with the last element
    row <- rep(tail(elements, 1), 3)
    # Replace the first n elements of the vector with the elements of the list
    row[seq_along(elements)] <- elements
    
    # Create a dataframe with 1 row and 3 columns
    df <- data.frame(t(row))
    names(df) <- c("Left", "Center", "Right")
    
    # Add the data frame to the list
    dfs[[i]] <- df
  }
  
  # Combine all data frames into one
  df <- bind_rows(dfs)
  
  # Convert the data frame to a flextable
  ft <- flextable(df)
  
  # Apply the common styling
  ft <- ft %>%
    set_header_labels(Left = "", Center = "", Right = "")
  
  # Apply different styling based on the number of elements
  for (i in seq_along(list_of_lists)) {
    elements <- list_of_lists[[i]]
    if (length(elements) == 1) {
      ft <- ft %>%
        merge_h(i=i, part = "body") %>%
        align(j=1, i=i, align = "left", part = "body")
    } else if (length(elements) == 2) {
      ft <- ft %>%
        merge_h(i=i, part = "body") %>%
        align(j = 1, i=i, align = "left", part = "body") %>%
        align(j = 2, i=i, align = "right", part = "body")
    } else if (length(elements) == 3) {
      ft <- ft %>%
        merge_h(i=i, part = "body") %>%
        align(j = 1, i=i, align = "left", part = "body") %>%
        align(j = 2, i=i, align = "center", part = "body") %>%
        align(j = 3, i=i, align = "right", part = "body")
    }
  }
  
  
  # Return the flextable
  return(ft)
}