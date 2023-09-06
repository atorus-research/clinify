# -----------------------------------------------------------------------------
# LOAD DEPENDENCIES
# -----------------------------------------------------------------------------

# Here we load the libraries and source functions that are required to produce
# the .docx file.

# These three are used for loading and preparing data.
library('Tplyr')
library('dplyr')
library('haven')

# These are used to add the styling to our table.
library('flextable')
library('officer')

# `add_page_*` used to add page header and page footer. They compose header and
# footer in the format of a table, so we can apply `flextable`'s styling on
# them as well.
#
# Assuming the below two .R files are in the same folder with the table
# code (this code).
source("add_page_header.R")
source("add_page_footer.R")

# Load in the 'helpers' functions which we put into "helpers.R".
# Usage of them is not necessary, but simply can be convenient.
source("helpers.R")

# This .R file contains pre-defined styling. You are free to experiment and
# add new or update existing styles (change fonts, color, borders, etc.)
source("styles.R")

# Read the source data, assuming it is in the same folder as the current file.
adae <- read_xpt("adae.xpt")
adsl <- read_xpt("adsl.xpt")

# -----------------------------------------------------------------------------

# Create an "output created" timestamp
program.timestamp <- as.POSIXlt(Sys.time(), "UTC") %>% strftime("%d%b%y(%H:%M)") %>% toupper()

# These values are hardcoded as we were focusing more on styling. But this
# information can be loaded from the metadata.
program.name <- 't_14_3_1_1.R'
program.output <- 't_14_3_1_1.docx'
program.db.version <- '05MAY2023'
program.cutoff.date <- '01MAY2023'


# -----------------------------------------------------------------------------
# PREPARE DATA 
# -----------------------------------------------------------------------------

# Just using Tplyr to prepare the data for reporting...

# Create the Tplyr Table object.
t <- tplyr_table(adae, TRTA, where = TRTEMFL == "Y") %>%
  
  # Set a population dataset, treatment variable, and do subsetting.
  set_pop_data(adsl) %>% 
  set_pop_treat_var(TRT01A) %>% 
  set_pop_where(SAFFL =="Y") %>% 
  
  # We're going to reuse this count layer format, so we'll set it at the table level
  set_count_layer_formats(
    'n_counts' = f_str('xx (xx.x%)', distinct_n, distinct_pct)
  ) %>%
  
  # Add a count layer for TRTEMFL equals Y
  add_layer(
    
    group_count("Number of patients reporting at least one treatment-emergent adverse event") %>% 
      # Create distinct counts per subject
      set_distinct_by(USUBJID)
    
  )%>%
  
  # Add a nested count layer for AEBODSYS as the outer layer and AEDECOD as the inner layer
  add_layer(
    
    group_count(vars(AEBODSYS, AEDECOD)) %>% 
      set_distinct_by(USUBJID) %>% 
      # Create nested formatting
      set_nest_count(TRUE) %>%
      # Sort nested counts
      set_order_count_method(c("byfactor", "bycount")) %>%
      # Chose the `Drug A` to be the sorting column
      set_ordering_cols("Drug A") %>%
      # Sort by count
      set_result_order_var(distinct_n)
  ) 

# Build the table on the Tplyr `t` object.
dat <- t %>% build()

# Select columns and set the columns order.
df <- select(dat,
             "row_label1",
             "var1_Drug A",
             "var1_Active Control",
             "var1_Placebo")
 
# Replace leading spaces with non-breaking spaces (tabulation).
df[] <- lapply(df, function(x) gsub("  ", "\t", x))



# -----------------------------------------------------------------------------
# Add Page Heading
# -----------------------------------------------------------------------------

# Each sublist you add to this list is a row of the page header.
#
# Whenever you pass only one item to each sublist - it will be center-aligned.
# When two items are passed - the first will be left-aligned and the second will
# be right-aligned.
# When three items are passed - they will be left-, center- and right-aligned
# respectively.
#
# Cells with same contents are merged into one cell.
#
# No more than 3 items are allowed in each sublist (limits to left-, center-,
# and right-alignment), however you can add as many sublists (page header rows)
# as needed.

# Adding six rows for the page header...
page_header <- add_page_heading(
  list(
    # "PROTOCOL: INCB XXXXX-XXX" wil be left-alligned and "Page X of Y" is going
    # to be right-alligned, because we're passing two elements to this sublist.
    c("PROTOCOL: INCB XXXXX-XXX", "Page X of Y"),
    # Same scenario for the following row...
    c("DRUG/INDICATION: INCB054707/Hidradentitis Suppurativa", paste('DATABASE VERSION: ', program.db.version, sep="")),
    # ... and for this one as well.
    c(paste("TLF Version: Dry run 1 (Cutoff Date ", program.cutoff.date, ")", sep=""), paste("TLF Version: Dry run 1 (Cutoff Date ", program.cutoff.date, ")", sep=""), "TASK: Dry Run 1"),
    # Below three rows will have center alignment, because just one item is
    # passed to those sublists.
    c("Table 14.3.1.2"),
    c("Summary of Treatment Emergent Adverse Events (TEAE) by System Organ Class and Preferred Term"),
    c("Safety Set")
  )
) %>%
  # Apply the default page header style from the `styles.R` file.
  header_style_default() %>%
  
  # You can fine-tune particular columns' width if you see an unwanted text
  # wrap. For example without this `width()` call below I mentioned that
  # "INCB054707/Hidradentitis Suppurativa" was wrapped to the next line. That
  # means that the 50/50% width split is not going to work and this first column
  # should be wider. Per the `width()` documentation the 'width' parameter
  # should specify width in inches.
  width(j = 1, width = c(3.23)) %>%
  
  # Add computed Word field for 'Page X of Y'. The computed field is put to the
  # i = 1 and j = 2 location, which means first row and second column (second
  # because first and second columns were merged). First and second columns
  # always merge if you want to put two items to the row (for left- and right-
  # alignment).
  compose(
    part = "body", i = 1, j = 2, # because the top line has two columns (1 and 2 are merged)
    as_paragraph("(Page ", as_word_field(x = "Page", width = .05), 
                 " of ", 
                 as_word_field(x = "NumPages", width = .05), ")")
    )

# -----------------------------------------------------------------------------
# Prepare stuff for body table
# -----------------------------------------------------------------------------

# Table headers

# Here we create a 'mapping' dataframe, specifying how the df column names should be translated
# (for example, translating "var1_Placebo" into "Placebo").
# This is also the place to control the span of the header columns: two neighbor cells will be merged
# if both possess the same contents. Use this to your advantage. The merge occurs both vertically and horizontally.
#
# Example:
# table_header_topology <- data.frame(
#   col_keys = c("row_label1", "var1_TRT 10 mg", "var1_TRT 25 mg", "var1_Total", "var1_Compare 10 mg", "var1_Overall"),
#   header_row_1 = c("System Organ Class\n\tPreferred Term[a]", "TRT", "TRT", "TRT", "Compare TRT", "Overall"),
#   header_row_2 = c("System Organ Class\n\tPreferred Term[a]", "10 mg", "25 mg", "Total", "10 mg", "Overall")
# )
# 
# In the above example two vertical cells having "System Organ Class..." text will be merged into one. Also, three horizontally
# neighbor cells with "TRT" in both will be merged into one cell.
#
# Example result:
#
# System Organ Class                TRT            Compare TRT     Overall
# 	 Preferred Term[a]       10 mg 25 mg Total        10 mg	

table_header_topology <- data.frame(
  col_keys = c("row_label1", "var1_Drug A", "var1_Active Control", "var1_Placebo"),
  medication = c("System Organ Class\n\tPreferred Term[a]", "Drug A", "Active Control", "Placebo")
)

# -----------------------------------------------------------------------------
# Calculate line numbers for `unsplitable` blocks to be used with
# `keep_with_next` function. The behavior would be: the blocks won't be torn
# between two pages. Those are 'helpers' functions and can be found in `helpers.R`
# -----------------------------------------------------------------------------

df_groups <- get_groups_from_df(df)
df_group_starts <- get_group_starts_from_df(df, row_label_no = 1)



# -----------------------------------------------------------------------------
# Table body
# -----------------------------------------------------------------------------

body_ft <- as_flextable(df, max_row = "max") %>%
  # Create table's header (use mapping df we created earlier in the code).
  set_header_df(mapping = table_header_topology, key = "col_keys") %>%
  # Set column widths for the label column...
  width(j = 1, width = 4) %>%
  # ...and for the data columns.
  width(j = 2:ncol(df), width = get_col_widths(df, lbl_width = 4, num_data_cols = ncol(df)-1)) %>%
  
  # Here we can fine tune particular columns' widths like this (width is in inches).
  # It is not necessary, but in case you have one-two data columns that differs
  # from the rest - this is the way to re-adjust the width for them.
  
  # width(j = 5:6, width = c(1.1, 0.9)) %>%
  
  # Apply default table style from the `styles.R`.
  table_style_default() %>%
  
  # Keep groups on the same page. This utilizes the result of `get_groups_from_df`
  # and `get_group_starts_from_df` from the `helpers.R`.
  keep_with_next(
    i = df_groups,
    value = TRUE,
    part = "body") %>%
  
  # Add padding to group starts. This and potentially other fine-tunes can be
  # added as needed. This is optional.
  padding(i=df_group_starts, padding.top=9)



# -----------------------------------------------------------------------------
# Page footnote
# -----------------------------------------------------------------------------

page_footer <- add_page_footer(list(
  c(paste("PROGRAM/OUTPUT: ", toupper(program.name), "/", toupper(program.output), sep=""), paste("DATE(TIME): ", program.timestamp, sep="")),
  c("Note: Percentages are based on the number of subjects in the Safety Set in each column. Subjects are counted once within each system organ class and each preferred term within each column."),
  c("[a] All investigator adverse event terms were coded using MedDRA dictionary version 24.0."),
  c("Reference: Listing 16.2.7.1.")
  )
) %>%
  # Apply default page footer style from `styles.R`.
  footer_style_default()


# -----------------------------------------------------------------------------
# Combine things together and save
# -----------------------------------------------------------------------------

doc <- read_docx() %>% 
  # Add body table
  body_add_flextable(value = body_ft) %>%
  
  # Below part probably can be abstracted as well. Or can be left as is...
  # Here you create a new 'landscape' section to put your table in.
  body_set_default_section(
      prop_section(
        # Specifying page properties: size and margins.
        # Page header and footer should be passed here, so they are placed
        # into the page colontitles.
        page_size = page_size(width=11.7, height = 8.3, orient = "landscape"),
        page_margins = page_mar(top=1, bottom=1, left=0.5, right=1),
        type = "continuous",
        footer_default = block_list(page_footer),
        header_default = block_list(page_header)
      )
  ) %>%
  
  # Save the .docx file to the same directory with this program.
  print(target = paste0(program.output))

