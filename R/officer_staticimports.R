# # MIT License
#
# Copyright (c) 2022 ArData
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

officer_section_fortify <- function(node, x) {
  title_page_tag <- "<w:titlePg xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"/>"

  headers <- Filter(
    function(x) {
      xml_name(x) %in% "headerReference"
    },
    xml_children(node)
  )

  for (i in seq_along(headers)) {
    if (!inherits(xml_child(headers[[i]], "w:hdr"), "xml_missing")) {
      hof_spread_to_file(node = headers[[i]], x = x, type = "header")
    }
    if (
      xml_attr(headers[[i]], "type") %in%
        "first" &&
        inherits(xml_child(node, "w:titlePg"), "xml_missing") &&
        "true" %in% xml_attr(node, "officer")
    ) {
      xml_add_child(node, as_xml_document(title_page_tag))
    }
    if (
      xml_attr(headers[[i]], "type") %in%
        "even" &&
        "true" %in% xml_attr(node, "officer")
    ) {
      x$settings$even_and_odd_headers <- TRUE
    }
  }

  footers <- Filter(
    function(x) {
      xml_name(x) %in% "footerReference"
    },
    xml_children(node)
  )
  for (i in seq_along(footers)) {
    if (!inherits(xml_child(footers[[i]], "w:ftr"), "xml_missing")) {
      hof_spread_to_file(node = footers[[i]], x = x, type = "footer")
    }
    if (
      xml_attr(footers[[i]], "type") %in%
        "first" &&
        inherits(xml_child(node, "w:titlePg"), "xml_missing") &&
        "true" %in% xml_attr(node, "officer")
    ) {
      xml_add_child(node, as_xml_document(title_page_tag))
    }
    if (
      xml_attr(footers[[i]], "type") %in%
        "even" &&
        "true" %in% xml_attr(node, "officer")
    ) {
      x$settings$even_and_odd_headers <- TRUE
    }
  }
  xml_set_attr(node, "w:officer", NULL)
  x
}

hof_next_file <- function(x, type = "header") {
  pattern <- paste0("^(", type, ")([0-9]+)(\\.xml)$")
  files <- list.files(
    path = file.path(x$package_dir, "word"),
    pattern = pattern
  )
  if (length(files) < 1) {
    files <- paste0(type, "0.xml")
  }
  str_id <- gsub(pattern, "\\2", files)
  paste0(type, max(as.integer(str_id)) + 1L, ".xml")
}


hof_spread_to_file <- function(node, x, type = "header") {
  xml_basename <- hof_next_file(x, type = type)
  if ("header" %in% type) {
    selector_drop <- "w:hdr"
  } else {
    selector_drop <- "w:ftr"
  }
  node_str <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    as.character(xml_child(node, 1))
  )
  writeLines(
    node_str,
    file.path(x$package_dir, "word", xml_basename),
    useBytes = TRUE
  )
  relationships <- docx_body_relationship(x)
  rid <- sprintf("rId%.0f", relationships$get_next_id())
  relationships$add(
    id = rid,
    type = paste0(
      "http://schemas.openxmlformats.org/officeDocument/2006/relationships/",
      type
    ),
    target = xml_basename
  )
  xml_remove(xml_child(node, selector_drop))
  xml_attr(node, "r:id") <- rid
  x$content_type$add_override(
    setNames(
      paste0(
        "application/vnd.openxmlformats-officedocument.wordprocessingml.",
        type,
        "+xml"
      ),
      paste0("/word/", xml_basename)
    )
  )
}


#' @importFrom xml2 xml_remove as_xml_document xml_parent xml_child
process_footnotes <- function(x) {
  browser()
  footnotes <- x$footnotes
  doc_obj <- x$doc_obj

  rel <- doc_obj$relationship()

  hl_nodes <- xml_find_all(doc_obj$get(), "//w:footnoteReference[@w:id]")
  which_to_add <- hl_nodes[grepl("^footnote", xml_attr(hl_nodes, "id"))]
  hl_ref <- xml_attr(which_to_add, "id")
  for (i in seq_along(hl_ref)) {
    next_id <- rel$get_next_id()
    rel$add(
      paste0("rId", next_id),
      type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes",
      target = "footnotes.xml"
    )

    index <- length(xml_find_all(footnotes$get(), "w:footnote")) - 1
    xml_attr(which_to_add[[i]], "w:id") <- index

    run <- xml_parent(which_to_add[[i]])

    run_rstyle <- xml_child(run, "w:rPr/w:rStyle")

    styles <- styles_info(x, type = "character")
    style_id <- xml_attr(run_rstyle, "val")
    style_id <- styles$style_id[styles$style_name %in% style_id]

    xml_attr(run_rstyle, "w:val") <- style_id

    footnote <- xml_child(which_to_add[[i]], "w:footnote")
    xml_attr(footnote, "w:id") <- index

    footnote_rstyle <- xml_child(footnote, "w:p/w:r/w:rPr/w:rStyle")
    xml_attr(footnote_rstyle, "w:val") <- style_id

    newfootnote <- as_xml_document(as.character(footnote))
    xml_remove(footnote)

    xml_add_child(footnotes$get(), newfootnote)
  }
}


process_links <- function(doc_obj, type = "wml") {
  rel <- doc_obj$relationship()
  if ("wml" %in% type) {
    hl_nodes <- xml_find_all(doc_obj$get(), "//w:hyperlink[@r:id]")
  } else {
    hl_nodes <- xml_find_all(doc_obj$get(), "//a:hlinkClick[@r:id]")
  }
  which_to_add <- hl_nodes[!grepl("^rId[0-9]+$", xml_attr(hl_nodes, "id"))]
  hl_ref <- unique(xml_attr(which_to_add, "id"))
  for (i in seq_along(hl_ref)) {
    rid <- sprintf("rId%.0f", rel$get_next_id())

    rel$add(
      id = rid,
      type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink",
      target = officer_url_decode(hl_ref[i]),
      target_mode = "External"
    )

    which_match_id <- grepl(
      hl_ref[i],
      xml_attr(which_to_add, "id"),
      fixed = TRUE
    )
    xml_attr(which_to_add[which_match_id], "r:id") <- rep(
      rid,
      sum(which_match_id)
    )
  }
}

process_stylenames <- function(doc_obj, styles) {
  styles_nodes <- xml_find_all(doc_obj$get(), "//*[@w:stlname]")
  if (length(styles_nodes)) {
    stylenames <- xml_attr(styles_nodes, "stlname")
    if (!all(stylenames %in% styles$style_name)) {
      missing_styles <- paste0(
        shQuote(unique(setdiff(stylenames, styles$style_name))),
        collapse = ", "
      )
      stop("Some styles can not be found in the document: ", missing_styles)
    }
    xml_attr(styles_nodes, "w:val") <- styles$style_id[match(
      stylenames,
      styles$style_name
    )]
  }
}

update_hf_list <- function(part_list = list(), type = "header", package_dir) {
  files <- list.files(
    path = file.path(package_dir, "word"),
    pattern = sprintf("^%s[0-9]*.xml$", type)
  )
  files <- files[!basename(files) %in% names(part_list)]
  if (type %in% "header") {
    cursor <- "/w:hdr/*[1]"
    body_xpath <- "/w:hdr"
  } else {
    cursor <- "/w:ftr/*[1]"
    body_xpath <- "/w:ftr"
  }

  new_list <- lapply(files, function(x) {
    docx_part$new(
      path = package_dir,
      main_file = x,
      cursor = cursor,
      body_xpath = body_xpath
    )
  })
  names(new_list) <- basename(files)
  append(part_list, new_list)
}


process_docx_poured <- function(
  doc_obj,
  relationships,
  content_type,
  package_dir,
  media_dir = "word"
) {
  hl_nodes <- xml_find_all(
    doc_obj$get(),
    "//w:altChunk[@r:id]",
    ns = c(
      "w" = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
      "r" = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    )
  )

  which_to_add <- hl_nodes[!grepl("^rId[0-9]+$", xml_attr(hl_nodes, "id"))]
  hl_ref <- unique(xml_attr(which_to_add, "id"))
  for (i in seq_along(hl_ref)) {
    rid <- sprintf("rId%.0f", relationships$get_next_id())

    file.copy(
      from = hl_ref[i],
      to = file.path(package_dir, media_dir, basename(hl_ref[i]))
    )

    relationships$add(
      id = rid,
      type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/aFChunk",
      target = basename(hl_ref[i])
    )
    content_type$add_override(
      setNames(
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml",
        paste0("/", media_dir, "/", basename(hl_ref[i]))
      )
    )

    which_match_id <- grepl(
      hl_ref[i],
      xml_attr(which_to_add, "id"),
      fixed = TRUE
    )
    xml_attr(which_to_add[which_match_id], "r:id") <- rep(
      rid,
      sum(which_match_id)
    )
  }
}


process_images <- function(
  doc_obj,
  relationships,
  package_dir,
  media_dir = "word/media",
  media_rel_dir = "media"
) {
  hl_nodes <- xml_find_all(
    doc_obj$get(),
    "//a:blip[@r:embed]|//asvg:svgBlip[@r:embed]",
    ns = c(
      "a" = "http://schemas.openxmlformats.org/drawingml/2006/main",
      "asvg" = "http://schemas.microsoft.com/office/drawing/2016/SVG/main",
      "r" = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    )
  )
  which_to_add <- hl_nodes[!grepl("^rId[0-9]+$", xml_attr(hl_nodes, "embed"))]
  hl_ref <- unique(xml_attr(which_to_add, "embed"))
  for (i in seq_along(hl_ref)) {
    dest_basename <- fake_newname(hl_ref[i])
    img_path <- file.path(package_dir, media_dir)
    if (!file.exists(file.path(img_path, dest_basename))) {
      dir.create(img_path, recursive = TRUE, showWarnings = FALSE)
      file.copy(from = hl_ref[i], to = file.path(img_path, dest_basename))
    }
    if (
      !file.path(media_rel_dir, dest_basename) %in%
        relationships$get_data()$target
    ) {
      rid <- sprintf("rId%.0f", relationships$get_next_id())
      relationships$add(
        id = rid,
        type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image",
        target = file.path(media_rel_dir, dest_basename)
      )
    } else {
      reldf <- relationships$get_data()
      rid <- reldf$id[basename(reldf$target) %in% dest_basename]
    }
    which_match_id <- grepl(
      dest_basename,
      fake_newname(xml_attr(which_to_add, "embed")),
      fixed = TRUE
    )
    xml_attr(which_to_add[which_match_id], "r:embed") <- rep(
      rid,
      sum(which_match_id)
    )
  }
}


# by capturing the path, we are making 'unique' new image names.
#' @importFrom openssl sha1
fake_newname <- function(filename) {
  which_files <- grepl("\\.[a-zA-Z0-0]+$", filename)
  file_type <- gsub("(.*)(\\.[a-zA-Z0-0]+)$", "\\2", filename[which_files])
  dest_basename <- sapply(filename[which_files], function(z) {
    as.character(sha1(file(z)))
  })
  dest_basename <- paste0(dest_basename, file_type)
  x <- filename
  x[which_files] <- dest_basename
  x
}


#' @export
to_wml.docx_settings <- function(x, add_ns = FALSE, ...) {
  out <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n",
    "<w:settings xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
    sprintf("<w:zoom w:percent=\"%.0f\"/>", x$zoom * 100),
    sprintf("<w:defaultTabStop w:val=\"%.0f\"/>", x$default_tab_stop * 1440),
    if (x$auto_hyphenation) sprintf("<w:autoHyphenation/>"),
    sprintf("<w:hyphenationZone w:val=\"%.0f\"/>", x$hyphenation_zone * 1440),
    sprintf(
      "<w:compat><w:compatSetting w:name=\"compatibilityMode\" w:uri=\"http://schemas.microsoft.com/office/word\" w:val=\"%s\"/></w:compat>",
      x$compatibility_mode
    ),
    sprintf("<w:decimalSymbol w:val=\"%s\"/>", x$decimal_symbol),
    sprintf("<w:listSeparator w:val=\"%s\"/>", x$list_separator),
    if (x$even_and_odd_headers) "<w:evenAndOddHeaders/>",
    "</w:settings>"
  )
  out
}

write_docx_settings <- function(x) {
  str <- to_wml(x$settings)
  file <- file.path(x$package_dir, "word", "settings.xml")
  writeLines(str, file, useBytes = TRUE)
  TRUE
}


write_core_properties <- function(core_matrix, package_dir) {
  ns_ <- core_matrix$ns
  core_matrix <- core_matrix$data
  if (!is.matrix(core_matrix)) {
    stop("core_properties should be stored in a character matrix.")
  }

  ns_ <- paste0('xmlns:', names(ns_), '=\"', ns_, '\"', collapse = " ")
  xml_ <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '\n',
    '<cp:coreProperties ',
    ns_,
    '>'
  )
  properties <- sprintf(
    "<%s:%s%s>%s</%s:%s>",
    core_matrix[, "ns"],
    core_matrix[, "name"],
    core_matrix[, "attrs"],
    htmlEscapeCopy(core_matrix[, "value"]),
    core_matrix[, "ns"],
    core_matrix[, "name"]
  )
  xml_ <- paste0(
    xml_,
    paste0(properties, collapse = ""),
    "</cp:coreProperties>"
  )
  props_dir = file.path(package_dir, "docProps")
  dir.create(props_dir, recursive = TRUE, showWarnings = FALSE)
  filename <- file.path(props_dir, "core.xml")
  writeLines(enc2utf8(xml_), filename, useBytes = TRUE)
  invisible()
}

write_custom_properties <- function(custom_props, package_dir) {
  xml_props <- sprintf(
    "<property fmtid=\"{D5CDD505-2E9C-101B-9397-08002B2CF9AE}\" pid=\"%s\" name=\"%s\"><vt:%s>%s</vt:%s></property>",
    custom_props$data[, 1],
    custom_props$data[, 2],
    custom_props$data[, 3],
    custom_props$data[, 4],
    custom_props$data[, 3]
  )

  xml_ <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    "<Properties ",
    "xmlns=\"http://schemas.openxmlformats.org/officeDocument/2006/custom-properties\" ",
    "xmlns:vt=\"http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes\">",
    xml_props,
    "</Properties>"
  )
  props_dir <- file.path(package_dir, "docProps")
  dir.create(props_dir, recursive = TRUE, showWarnings = FALSE)
  filename <- file.path(props_dir, "custom.xml")
  writeLines(enc2utf8(xml_), filename, useBytes = TRUE)
  invisible()
}

correct_id <- function(doc, int_id) {
  all_uid <- xml_find_all(doc, "//*[@id]")
  for (z in seq_along(all_uid)) {
    if (!grepl("[^0-9]", xml_attr(all_uid[[z]], "id"))) {
      xml_attr(all_uid[[z]], "id") <- int_id
      int_id <- int_id + 1
    }
  }
  int_id
}

is_windows <- function() {
  "windows" %in% .Platform$OS.type
}

is_doc_open <- function(file) {
  # The function checks if the `file` is open (a.k.a. is being edited).
  # This function is valid on Windows operating system only.
  suppressWarnings(file.exists(file) && !file.rename(from = file, to = file))
}


# htmlEscapeCopy ----

htmlEscapeCopy <- local({
  .htmlSpecials <- list(
    `&` = '&amp;',
    `<` = '&lt;',
    `>` = '&gt;'
  )
  .htmlSpecialsPattern <- paste(names(.htmlSpecials), collapse = '|')
  .htmlSpecialsAttrib <- c(
    .htmlSpecials,
    `'` = '&#39;',
    `"` = '&quot;',
    `\r` = '&#13;',
    `\n` = '&#10;'
  )
  .htmlSpecialsPatternAttrib <- paste(
    names(.htmlSpecialsAttrib),
    collapse = '|'
  )
  function(text, attribute = FALSE) {
    pattern <- if (attribute) .htmlSpecialsPatternAttrib else
      .htmlSpecialsPattern
    text <- enc2utf8(as.character(text))
    # Short circuit in the common case that there's nothing to escape
    if (!any(grepl(pattern, text, useBytes = TRUE))) return(text)
    specials <- if (attribute) .htmlSpecialsAttrib else .htmlSpecials
    for (chr in names(specials)) {
      text <- gsub(chr, specials[[chr]], text, fixed = TRUE, useBytes = TRUE)
    }
    Encoding(text) <- "UTF-8"
    return(text)
  }
})
