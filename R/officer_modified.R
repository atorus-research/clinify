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

#' Modified from officer print.rdocx
#'
#' Modifications:
#'   - Not using print functionality so rename and strip out target being null
#'   - Take out process_comments
#' @describeIn read_docx write docx to a file. It returns the path of the result
#' file.
#' @param x an rdocx object
#' @param target path to the docx file to write
#' @param ... unused
#' @noRd
write_rdocx_ <- function(x, target) {
  if (!grepl(x = target, pattern = "\\.(docx)$", ignore.case = TRUE)) {
    stop(target, " should have '.docx' extension.")
  }

  if (is_windows() && is_doc_open(target)) {
    stop(target, " is open. To write to this document, please, close it.")
  }

  x <- process_sections(x)

  # process_footnotes(x) # Not using Word's concept of footnotes
  # process_stylenames(x$doc_obj, x$styles) # Seem to be able to get away without this
  # process_links(x$doc_obj, type = "wml") # No built in links
  process_docx_poured(
    doc_obj = x$doc_obj,
    relationships = x$doc_obj$relationship(),
    content_type = x$content_type,
    package_dir = x$package_dir
  )
  process_images(x$doc_obj, x$doc_obj$relationship(), x$package_dir)
  process_images(x$footnotes, x$footnotes$relationship(), x$package_dir)

  x$headers <- update_hf_list(
    part_list = x$headers,
    type = "header",
    package_dir = x$package_dir
  )
  x$footers <- update_hf_list(
    part_list = x$footers,
    type = "footer",
    package_dir = x$package_dir
  )
  for (header in x$headers) process_links(header, type = "wml")
  for (footer in x$footers) process_links(footer, type = "wml")
  for (header in x$headers)
    process_images(header, header$relationship(), x$package_dir)
  for (footer in x$footers)
    process_images(footer, footer$relationship(), x$package_dir)

  # Opens fine without the IDs
  # int_id <- 1 # unique id identifier

  # # make all id unique for document
  # int_id <- correct_id(x$doc_obj$get(), int_id)
  # # make all id unique for footnote
  # int_id <- correct_id(x$footnotes$get(), int_id)
  # # make all id unique for footnote
  # int_id <- correct_id(x$comments$get(), int_id)
  # # make all id unique for headers
  # for (docpart in x[["headers"]]) {
  #   int_id <- correct_id(docpart$get(), int_id)
  # }
  # # make all id unique for footers
  # for (docpart in x[["footers"]]) {
  #   int_id <- correct_id(docpart$get(), int_id)
  # }

  # body <- xml_find_first(x$doc_obj$get(), "w:body")

  # # If body is not ending with an sectPr, create a continuous one append it
  # if (!xml_name(xml_child(body, search = xml_length(body))) %in% "sectPr") {
  #   str <- paste0(
  #     "<w:sectPr xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\">",
  #     "<w:type w:val=\"continuous\"/></w:sectPr>"
  #   )
  #   xml_add_child(body, as_xml_document(str))
  # }

  for (header in x$headers) {
    header$save()
  }
  for (footer in x$footers) {
    footer$save()
  }
  x$doc_obj$save()
  x$content_type$save()
  x$footnotes$save()
  x$comments$save()

  x$rel$write(file.path(x$package_dir, "_rels", ".rels"))
  write_docx_settings(x)

  # save doc properties
  if (nrow(x$doc_properties$data) > 0) {
    x$doc_properties["modified", "value"] <- format(
      Sys.time(),
      "%Y-%m-%dT%H:%M:%SZ"
    )
    x$doc_properties["lastModifiedBy", "value"] <- Sys.getenv("USER")
    write_core_properties(x$doc_properties, x$package_dir)
  }
  if (nrow(x$doc_properties_custom$data) > 0) {
    write_custom_properties(x$doc_properties_custom, x$package_dir)
  }
  invisible(pack_folder(folder = x$package_dir, target = target))
}

# Modified version of process_sections
# Modifications made:
#  - Avoid looking for sub-sections because that will return null
#  -
process_sections <- function(x) {
  # Not used if no section nodes
  # default_pgMar <- xml_find_first(x$doc_obj$get(), "w:body/w:sectPr/w:pgMar")
  # default_pgSz <- xml_find_first(x$doc_obj$get(), "w:body/w:sectPr/w:pgSz")
  # sect_dim <- section_dimensions(xml_find_first(
  #   x$doc_obj$get(),
  #   "w:body/w:sectPr"
  # ))

  node_def_sec <- xml_find_first(x$doc_obj$get(), "w:body/w:sectPr")

  # if w:type not there, each section is on a new page if not continuous
  if (inherits(xml_child(node_def_sec, "w:type"), "xml_missing")) {
    node_type <- as_xml_document(
      "<w:type xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" w:val=\"continuous\"/>"
    )
    xml_add_child(node_def_sec, node_type)
  }

  x <- officer_section_fortify(node_def_sec, x)

  x
}
