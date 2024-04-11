library(Tplyr)
library(dplyr)
library(flextable)

t <- tplyr_table(tplyr_adae, TRTA) %>%
    set_pop_data(tplyr_adsl) %>%
    set_pop_treat_var(TRT01A) %>%
    add_layer(
        group_count('All subjects')
    ) %>%
    add_layer(
        group_count(vars(AEBODSYS, AEDECOD)) %>%
            set_nest_count(TRUE)
    )

example_df <- t %>% build() #%>%
# mutate(
#     across(where(is.character), ~ replace_leading_whitespace(.x))
# )
example_df <- example_df[1:4]


# Test table 1

typology <- data.frame(
    col_keys = names(example_df),
    top = c("", "", "Xanomeline", "Xanomeline"),
    bottom = c("", "Placebo\n(N=86)", "High Dose\n(N=84)", "Low Dose\n(N=84)"),
    stringsAsFactors = FALSE
)

t_1 <- flextable(example_df, col_keys = names(example_df))
t_1 <- set_header_df(t_1, mapping = typology, key = "col_keys")
t_1 <- merge_at(t_1, i=1, j=3:4, part = "header")
t_1 <- font(t_1, fontname ="COURIER NEW", part="all")
t_1 <- autofit(fit_to_width(t_1, 10))
t_1 <- align(
    t_1,
    j = 2:4,
    align = "center",
    part = "all"
)
t_1


# Test table 2
example_df_sub <- example_df[1:5, c(1, 3, 4)]

typology2 <- data.frame(
    col_keys = names(example_df),
    top = c("", "", "Xanomeline", "Xanomeline"),
    bottom = c("", "Placebo\n(N=86)", "High Dose\n(N=84)", "Low Dose\n(N=84)"),
    stringsAsFactors = FALSE
)[c(1, 3, 4), ]

t_2 <- flextable(example_df_sub, col_keys = names(example_df_sub))
t_2 <- set_header_df(t_2, mapping = typology2, key = "col_keys")
t_2 <- merge_at(t_2, i=1, j=2:3, part = "header")
t_2 <- font(t_2, fontname ="COURIER NEW", part="all")
t_2 <- autofit(fit_to_width(t_2, 10))
t_2 <- align(
    t_2,
    j = 1:3,
    align = "center",
    part = "all"
)
t_2

test_table <- t_1
base_table <- t_2
comp_table <- slice_clintable(t_1, 1:5, c(1, 3:4))

names(base_table)

testthat::expect_equal(base_table$header, comp_table$header)

