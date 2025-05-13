# # MIT License
#
# Copyright (c) 2022 ArData
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions: |>
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

# openxml_document --------------------------------------------------------
# This class handle basic operations on a openxml document:
# - initialize
openxml_document <- R6Class(
  "openxml_document",
  public = list(
    initialize = function(dir) {
      private$reldir = dir
      private$rels_doc <- relationship$new()
    },

    feed = function(file) {
      private$filename <- file
      private$doc <- read_xml(file)

      private$rels_filename <- file.path(
        dirname(file),
        "_rels",
        paste0(basename(file), ".rels")
      )

      if (file.exists(private$rels_filename))
        private$rels_doc <- relationship$new()$feed_from_xml(
          private$rels_filename
        ) else private$rels_doc <- relationship$new()

      self
    },
    file_name = function() {
      private$filename
    },
    name = function() {
      basename(private$filename)
    },
    get = function() {
      private$doc
    },
    dir_name = function() {
      private$reldir
    },
    save = function() {
      # remove duplicate namespace definitions
      private$doc <- read_xml(as.character(private$doc), options = "NSCLEAN")
      write_xml(private$doc, file = private$filename)
      if (nrow(self$rel_df()) > 0) {
        private$rels_doc$write(private$rels_filename)
      }
      self
    },
    remove = function() {
      unlink(private$filename)
      if (file.exists(private$rels_filename)) unlink(private$rels_filename)
      private$filename
    },
    rel_df = function() {
      private$rels_doc$get_data()
    },
    relationship = function() {
      private$rels_doc
    }
  ),
  private = list(
    filename = NULL,
    rels_filename = NULL,
    doc = NULL,
    rels_doc = NULL,
    reldir = NULL
  )
)


docx_part <- R6Class(
  "docx_part",
  inherit = openxml_document,
  public = list(
    initialize = function(path, main_file, cursor, body_xpath) {
      super$initialize("word")
      private$package_dir <- path
      private$body_xpath <- body_xpath
      super$feed(file.path(private$package_dir, "word", main_file))
      private$cursor <- cursor
    },
    length = function() {
      xml_length(xml_find_first(self$get(), private$body_xpath))
    }
  ),
  private = list(
    package_dir = NULL,
    cursor = NULL,
    body_xpath = NULL
  )
)

#' @importFrom R6 R6Class
relationship <- R6Class(
  "relationship",
  public = list(
    initialize = function(
      id = character(0),
      type = character(0),
      target = character(0)
    ) {
      private$id <- id
      private$type <- type
      private$target <- target
      private$target_mode <- as.character(rep(NA, length(target)))
      private$ext_src <- character(length(id))
    },
    feed_from_xml = function(path) {
      doc <- read_xml(x = path)
      children <- xml_children(doc)
      ns <- xml_ns(doc)

      private$id <- c(private$id, sapply(children, xml_attr, attr = "Id", ns))
      private$type <- c(
        private$type,
        sapply(children, xml_attr, attr = "Type", ns)
      )
      private$target <- c(
        private$target,
        sapply(children, xml_attr, attr = "Target", ns)
      )
      private$target_mode <- c(
        private$target_mode,
        sapply(children, xml_attr, attr = "TargetMode", ns)
      )
      private$ext_src <- c(private$ext_src, character(length(children)))
      self
    },
    write = function(path) {
      if (length(private$id))
        str <- paste0(
          "<Relationship Id=\"",
          private$id,
          "\" Type=\"",
          private$type,
          "\" Target=\"",
          htmlEscapeCopy(private$target),
          ifelse(
            is.na(private$target_mode),
            "",
            "\" TargetMode=\"External"
          ),
          "\"/>"
        ) else str <- character(length = 0)

      str <- c(
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
        "\n<Relationships  xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
        str,
        "</Relationships>"
      )
      dir.create(dirname(path), showWarnings = FALSE, recursive = FALSE)
      writeLines(str, con = path, useBytes = TRUE)
      self
    },
    get_next_id = function() {
      max(c(0, private$get_int_id()), na.rm = TRUE) + 1
    },
    get_data = function() {
      if (length(private$id)) {
        data <- data.frame(
          id = private$id,
          int_id = as.integer(gsub("rId([0-9]+)", "\\1", private$id)),
          type = private$type,
          target = private$target,
          target_mode = private$target_mode,
          ext_src = private$ext_src,
          stringsAsFactors = FALSE
        )
        data[order(data$id), ]
      } else {
        data <- data.frame(
          id = character(0),
          int_id = integer(0),
          type = character(0),
          target = character(0),
          target_mode = character(0),
          ext_src = character(0),
          stringsAsFactors = FALSE
        )
      }
      data
    },
    get_images_path = function() {
      is_img <- basename(private$type) %in% "image"
      targets <- private$target[is_img]
      names(targets) <- private$id[is_img]
      targets
    },
    add_img = function(src, root_target) {
      src <- setdiff(src, private$ext_src)
      if (!length(src)) return(self)

      if (any(grepl(" ", basename(src)))) {
        stop(
          paste(src, collapse = ", "),
          ": images with blanks in their basenames are not supported, please rename the file(s).",
          call. = FALSE
        )
      }

      last_id <- max(c(0, private$get_int_id()), na.rm = TRUE)

      id <- paste0("rId", seq_along(src) + last_id)
      type <- rep(
        "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image",
        length(src)
      )
      target <- file.path(root_target, basename(src))

      private$id <- c(private$id, id)
      private$type <- c(private$type, type)
      private$target <- c(private$target, target)
      private$target_mode <- c(private$target_mode, rep(NA, length(id)))
      private$ext_src <- c(private$ext_src, src)

      self
    },
    add_drawing = function(src, root_target) {
      src <- setdiff(src, private$ext_src)
      if (!length(src)) return(self)
      last_id <- max(c(0, private$get_int_id()), na.rm = TRUE)

      id <- paste0("rId", seq_along(src) + last_id)
      type <- rep(
        "http://schemas.openxmlformats.org/officeDocument/2006/relationships/drawing",
        length(src)
      )
      target <- file.path(root_target, basename(src))

      private$id <- unlist(c(private$id, id))
      private$type <- unlist(c(private$type, type))
      private$target <- unlist(c(private$target, target))
      private$target_mode <- unlist(c(private$target_mode, rep(NA, length(id))))
      private$ext_src <- unlist(c(private$ext_src, rep(NA, length(id))))

      self
    },
    add = function(id, type, target, target_mode = NA) {
      Encoding(target) <- "UTF-8"
      if (!target %in% private$target) {
        private$id <- c(private$id, id)
        private$type <- c(private$type, type)
        private$target <- c(private$target, target)
        private$target_mode <- c(private$target_mode, target_mode)
        private$ext_src <- c(private$ext_src, "")
      }
      self
    },
    remove = function(target) {
      id <- which(basename(private$target) %in% basename(target))
      private$id <- private$id[-id]
      private$type <- private$type[-id]
      private$target <- private$target[-id]
      private$target_mode <- private$target_mode[-id]
      private$ext_src <- private$ext_src[-id]
      self
    },
    show = function() {
      print(self$get_data())
    }
  ),
  private = list(
    id = NA,
    type = NA,
    target = NA,
    target_mode = NA,
    ext_src = NA,
    get_int_id = function() {
      if (length(private$id) > 0)
        as.integer(gsub("rId([0-9]+)", "\\1", private$id)) else integer(0)
    }
  )
)
