
ft <- clintable(mtcars) %>%
  # Add titles here is using new_header_footer to allow flextable functions
  # to customzie the titles block
  clin_add_titles(
    ft = new_title_footnote(
      list(
        c("left aligned", "centered", "right aligned"),
        c("left aligned", "right aligned"),
        c("Single element")
      ),
      "titles"
    ) %>%
      border_remove()
  ) %>%
  # Adding footnotes is just using a list of lists instead to show how it can
  # be automatically converted
  clin_add_footnotes(
    list(
      c("left aligned", "centered", "right aligned"),
      c("left aligned", "right aligned"),
      c("", "", "Single element")
    )
  )

ft
