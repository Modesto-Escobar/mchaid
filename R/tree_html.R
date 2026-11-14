# Interactive tree as an HTML page (same scheme as the graphs of 'rD3plot': a folder with index.html, the scripts, the styles and
# the data as JSON inside the page).

# folder of the package with the files of the page (D3, scripts, styles and fonts)
.www_dir <- function() system.file("www", package = "mchaid")

# page: folder 'dir' with index.html, styles/ and scripts/. An existing folder is accepted if it is empty or holds a page of this package
# (which is replaced); otherwise nothing is written and there is an error.
.create_html <- function(dir, styles, scripts, json, title) {
  www <- .www_dir()
  if (!nzchar(www)) stop("the package files were not found (is 'mchaid' installed?)")
  indexfile <- file.path(dir, "index.html")
  if (dir.exists(dir) && length(list.files(dir, all.files = TRUE, no.. = TRUE)) > 0L) {          # an empty folder is accepted
    ok <- file.exists(indexfile) && any(grepl("<!--(mchaid|rD3tree) Project-->", readLines(indexfile, warn = FALSE, encoding = "UTF-8")))
    if (!ok) stop("directory '", dir, "' already exists and is not empty nor a page of 'mchaid': use another folder or empty it")
    unlink(dir, recursive = TRUE)
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir.create(file.path(dir, "styles"))
  dir.create(file.path(dir, "scripts"))
  file.copy(file.path(www, styles), file.path(dir, "styles"))
  file.copy(file.path(www, c("Roboto-Regular-latin.woff2", "Roboto-Regular-latin-ext.woff2")), file.path(dir, "styles"))
  file.copy(file.path(www, scripts), file.path(dir, "scripts"))
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  html <- c("<!DOCTYPE html>", "<html>", "<head>", "<meta charset=\"UTF-8\">",
            "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">", "<!--mchaid Project-->",
            paste0("<title>", title, "</title>"),
            paste0("<link rel=\"stylesheet\" type=\"text/css\" href=\"styles/", styles, "\">"),
            "</head>", "<body>",
            paste0("<script type=\"application/json\" id=\"data\">", json, "</script>"),
            paste0("<script src=\"scripts/", scripts, "\"></script>"), "</body>", "</html>")
  con <- file(indexfile, "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(html, con)
  invisible(indexfile)
}

# Interactive tree of an mchaid() or maid() fit. Arguments as in plot() of 'mchaid'.
#   treestyle: "boxes" (default) every node is a box with the plot of its distribution inside (as plot(treestyle = "boxes")); "classic" as the
#              classic graph of plot(): small boxes with the category of the branch and the size, the splitting variable and its test on the
#              branch, and the bars (mchaid) or the deviations of the mean (maid) only under the terminal nodes. It ignores nodeplot, center,
#              whisker, shade, nodehead and maxdots (with a warning); quartiles (median and quartiles on the bars) only works in this style
#   collapse: depth from which the branches start collapsed (NULL: the whole tree is shown)
#   nodehead: "category" (default) the heading of each box is the category (or interval) of the splitting variable that forms the node
#             the number of the node goes in a small square on the top left corner and the labels of the branches disappear from the
#             connectors; "number": the classic heading "Node N" with the labels on the branches
#   digits: with maid, decimals of the mean, median, SD, their differences and the scale (default 1; 0 to 6)
#   maxdots: with maid and nodeplot = "dots", most points drawn in a node (default 200): all those outside the whiskers and a sample of the
#            rest; the values of the response of the points go inside the page
#   rootlabel: heading of the root node with nodehead = "category": "total" (default; "Total", translated), "depvar" (the name of the
#              response variable) or any text
tree_html <- function(x, main = NULL, note = NULL, treestyle = c("boxes", "classic"), nodeplot = NULL, center = NULL, whisker = "p5", shade = TRUE, pval = TRUE, pcut = NULL,
                     language = "en", col = NULL, palette = "default", ramp = FALSE, collapse = NULL, pct = TRUE, quartiles = NULL,
                     nodehead = "category", rootlabel = "total", digits = 1, maxdots = 200, dir = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("package 'jsonlite' is needed for the interactive tree: install.packages(\"jsonlite\")")
  treestyle <- .match_opt(treestyle, c("boxes", "classic"), "treestyle")
  if (treestyle == "classic") {                                  # arguments of the style of boxes that the classic style ignores
    ign <- c(if (!is.null(nodeplot)) "nodeplot", if (!is.null(center)) "center", if (!missing(nodehead)) "nodehead", if (!missing(whisker)) "whisker",
             if (!missing(shade)) "shade", if (!missing(maxdots)) "maxdots")
    if (length(ign)) warning(paste(sQuote(ign, FALSE), collapse = ", "), if (length(ign) > 1L) " are" else " is", " ignored with treestyle = \"classic\": use treestyle = \"boxes\"", call. = FALSE)
  } else if (!is.null(quartiles)) warning("'quartiles' is ignored with treestyle = \"boxes\": use treestyle = \"classic\"", call. = FALSE)
  dat <- .tree_data(x, nodeplot = nodeplot, center = center, whisker = whisker, shade = shade, pval = pval, pcut = pcut,
                   language = language, col = col, palette = palette, ramp = ramp, collapse = collapse, pct = pct, nodehead = nodehead, rootlabel = rootlabel, digits = digits, maxdots = maxdots,
                   treestyle = treestyle, quartiles = quartiles)
  dat$options$main <- if (is.null(main)) "" else main
  dat$options$note <- if (is.null(note)) "" else paste(note, collapse = "\n")
  if (!is.null(dat$options$forcednote)) dat$options$note <- paste(c(if (nzchar(dat$options$note)) dat$options$note, dat$options$forcednote), collapse = "\n")
  if (is.null(dir)) dir <- file.path(tempdir(), paste0("mchaid_", format(Sys.time(), "%H%M%S")))
  json <- jsonlite::toJSON(dat, auto_unbox = TRUE, na = "null", null = "null", digits = 8, force = TRUE)
  .create_html(dir, styles = "tree.css", scripts = c("d3.min.js", "tree.js"), json = as.character(json),
               title = if (nzchar(dat$options$main)) dat$options$main else paste(dat$options$kind, "tree"))
  structure(list(dir = dir, file = file.path(dir, "index.html"), data = dat), class = "tree_html")
}

print.tree_html <- function(x, ...) {
  cat("Interactive tree: ", x$file, "\n", sep = "")
  if (interactive()) utils::browseURL(x$file)
  invisible(x)
}

plot.tree_html <- function(x, ...) print.tree_html(x, ...)
