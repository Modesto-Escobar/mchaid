\name{tree_html}
\alias{tree_html}
\alias{print.tree_html}
\alias{plot.tree_html}
\title{Interactive tree of a CHAID or AID fit as an HTML page}
\description{
Writes a folder with a self-contained HTML page (\code{index.html}, the scripts and the data) that shows the tree of an
object returned by \code{\link{mchaid}} or \code{\link{maid}} with the same boxes as \code{plot(x, treestyle = "boxes")}, or in
the layout of the classic graph (\code{treestyle = "classic"}), but interactive. Open the page in any browser; no server is needed.
}
\usage{
tree_html(x, main = NULL, note = NULL, treestyle = c("boxes", "classic"),
     nodeplot = NULL, center = NULL, whisker = "p5", shade = TRUE, pval = TRUE,
     pcut = NULL, language = "en", col = NULL, palette = "default", ramp = FALSE,
     collapse = NULL, pct = TRUE, quartiles = NULL, nodehead = "category",
     rootlabel = "total", digits = 1, maxdots = 200, dir = NULL)

\method{print}{tree_html}(x, ...)
\method{plot}{tree_html}(x, ...)
}
\arguments{
  \item{x}{an object returned by \code{\link{mchaid}} or \code{\link{maid}} (for \code{print} and \code{plot}, the object
    returned by \code{tree_html}).}
  \item{main}{title written at the top of the page.}
  \item{note}{text (source of the data...) written at the bottom left of the drawing, so it is also in the exported files;
    a new line (\code{"\\n"}) starts a new paragraph.}
  \item{treestyle}{\code{"boxes"} (default): every node is a box with the plot of its distribution inside, as in
    \code{plot(x, treestyle = "boxes")}. \code{"classic"}: the layout of the classic graph of \code{\link{plot.mchaid}} and
    \code{\link{plot.maid}}: every node is a small box with the category (or group of categories) of its branch as heading and its
    size, the splitting variable and its test (\code{chi2=434; p<0.001}) go over the branch under the node, and only the terminal
    nodes have a plot under them (the stacked bar of the categories in \code{mchaid}; the deviation of the mean from the overall
    one, with the mean and the standard deviation, in \code{maid}); the legend, the risk estimate with Cramer's V (Eta squared in
    \code{maid}) and the note of the bars are written at the foot. The classic style ignores \code{nodeplot}, \code{center},
    \code{whisker}, \code{shade}, \code{nodehead} and \code{maxdots} (with a warning).}
  \item{quartiles}{only with \code{treestyle = "classic"}: \code{"auto"} (or \code{TRUE}) or \code{"all"}, to draw on the bars
    the median (solid green line) and the quartiles (dashed), as in \code{\link{plot.mchaid}} (with an ordered response, see
    \code{dvordered} in \code{\link{mchaid}}) and \code{\link{plot.maid}}.}
  \item{nodeplot, center, whisker, shade, pval, pcut, language, col, palette, ramp, pct, nodehead, rootlabel}{as in
    \code{\link{plot.mchaid}} and \code{\link{plot.maid}}: what each box shows, the statistic
    that is compared with the overall one, the whiskers, the shades, the p-values, the language of the texts, the colours,
    the percentages inside the bars, the heading of the boxes and the label of the root node.}
  \item{digits}{whole number between 0 and 6 (default 1): decimals of the mean, the median and the standard deviation of the
    nodes, of their differences with the overall one and of the numbers of the legend and the scale (the percentages, the tests
    and the p-values do not change).}
  \item{maxdots}{with \code{maid} and \code{nodeplot = "dots"}: most points drawn in a node (default 200; between 20 and 5000),
    as in \code{\link{plot.maid}}. The values of the response variable of the points drawn are written inside the page (the
    rest of the data are not), which is a consideration if the page is to be shared.}
  \item{collapse}{depth from which the branches start collapsed (the default, \code{NULL}, shows the whole tree).}
  \item{dir}{folder where the page is written; by default a new folder in the temporary directory of the session. An existing
    folder is accepted if it is empty or holds a page written by this package (which is replaced); if it holds anything else the
    page is not written and there is an error.}
  \item{...}{not used.}
}
\details{
In a tree grown with \code{forcevars} the variable of every forced split is followed by an asterisk (by a dagger in the classic
style of \code{mchaid}, as in the static graph) and the note that explains it is added to the \code{note} of the page.

The page shows, in a box for every node, the same numbers as the static graph. On the page, passing the mouse over a node shows
its definition, its size and its distribution, and highlights the path from the root; the button next to the name of the splitting
variable closes and opens the branch (with animation), as the buttons \emph{Expand all} and \emph{Collapse all}; the tree can be
moved and zoomed with the mouse; passing the mouse over a category of the legend (\code{mchaid}) fades the others in all the
bars; and the buttons \emph{SVG} and \emph{PNG} download the tree as it is seen, with its legend, scale and note.

The package \pkg{jsonlite} is needed (it is in \code{Suggests}). The page uses a subset of \pkg{D3} (version 5, BSD license) and the font
Roboto, which are distributed with the package.
}
\value{
An object of class \code{"tree_html"}, a list with the folder (\code{dir}), the page (\code{file}) and the data (\code{data}).
Printing it (and \code{plot}) shows the path of the page and, in an interactive session, opens it in the browser.
}
\seealso{\code{\link{plot.mchaid}}, \code{\link{plot.maid}}, \code{\link{asnetCoin}}}
\examples{
if (requireNamespace("jsonlite", quietly = TRUE)) {
  fit <- maid(mpg ~ cyl + hp + wt, data = mtcars, ordered = c("cyl", "hp", "wt"),
              xtile = c(hp = 4, wt = 4), minnode = 5, minsplit = 10)
  page <- tree_html(fit, main = "mtcars: mpg", dir = file.path(tempdir(), "tree_mtcars"))
  page$file          # the page: open it in a browser
}
}
