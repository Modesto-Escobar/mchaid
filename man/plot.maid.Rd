\name{plot.maid}
\alias{plot.maid}
\title{Plot a MAID tree}
\description{
Draws the tree as \code{\link[=plot.mchaid]{plot.mchaid}} does: by default (\code{treestyle = "boxes"}) every node is a box with
the category of its branch as its heading, its size, its mean and standard deviation and a plot of the distribution of the response
(see \code{nodeplot}); with \code{treestyle = "classic"} every node is a small box with the category of its branch and its number
of cases, the splitting variable and its test, \code{F=19.7; p<0.001}, go over the branch under the node, and each terminal node has
a bar that shows the deviation of the mean of the node from the overall mean (the dashed line): blue above it, orange below it, with
the mean and the standard deviation written under it. The last line of the classic graph gives the risk estimate
(variance of the response around the mean of its node; in parentheses, the variance around the overall mean) and Eta squared
(the R-squared of the nodes).

With \code{dir} the same tree is also written as an interactive HTML page (see \code{\link{tree_html}}).
}
\usage{
\method{plot}{maid}(x, main = NULL, note = NULL, pval = TRUE, pcut = NULL, language = "en",
     quartiles = NULL, treestyle = c("boxes", "classic"), nodeplot = NULL,
     center = NULL, whisker = c("p5", "minmax", "tukey"), shade = TRUE,
     col = NULL, cex = 1, nodehead = c("category", "number"), rootlabel = "total",
     digits = 1, maxdots = 200, collapse = NULL, dir = NULL, ...)
}
\arguments{
  \item{x}{an object returned by \code{\link{maid}}.}
  \item{main}{title; by default, in the classic graph, the name of the response variable with a text in the chosen language (the
    graph of the boxes and the page have no title unless \code{main} is given).}
  \item{note}{text (source of the data...) written at the bottom left of the graph, under the legend; a new line (\code{"\\n"})
    starts a new line of the note, and a vector of texts is one line each. It is also in the interactive page written with \code{dir}.}
  \item{pval}{logical: write the p-value of each split.}
  \item{pcut}{if a number between 0 and 1, the p-values smaller than it are written \code{p<}\emph{pcut}.}
  \item{language}{language of the texts: \code{"en"} (default), \code{"es"}, \code{"fr"}, \code{"de"},
    \code{"it"}, \code{"pt"}, \code{"ca"}, \code{"gl"}, \code{"nl"} or \code{"eu"}.}
  \item{quartiles}{only with \code{treestyle = "classic"} (with the boxes there is a warning, unless it is \code{FALSE}):
    \code{"auto"} (or \code{TRUE}) or \code{"all"}: draw over each bar the median (solid green line)
    and the quartiles (dashed green lines), at their value on the same scale as the bars. With \code{"auto"}
    only the median is drawn in the nodes where a quartile is too close to it to be told apart.}
  \item{treestyle}{\code{"boxes"} (default) or \code{"classic"}, the two graphs described above. In the boxes every node of the
    tree, the root and the intermediate nodes included, is a box with its heading, its size (n and \% of the cases), its
    mean (or median, see \code{center}) with, in parentheses, the difference with the overall one, the standard
    deviation and a plot of the distribution of the response (see \code{nodeplot}); under a node that is split go the
    splitting variable and its test (\code{F = 19.7; p < 0.001}) and on each branch the categories. The nodes are
    numbered as in SPSS (the root is node 0, level by level, from left to right), so the numbers of the terminal
    nodes are not those of \code{print} or \code{summary}. The numbers are written with the separators of the
    language, there is no title unless \code{main} is given, and \code{quartiles} is ignored. The classic graph was the default until
    version 0.9.4.}
  \item{nodeplot}{with \code{treestyle = "boxes"}: \code{"bar"}, the deviation of the mean (or median) of the node
    from the overall one as a bar from a line that sticks out of it (dark grey for the overall mean, green for the overall median), on the scale of the largest deviation;
    \code{"box"} (default), a box plot on a scale common to all the nodes (box from the first to the third quartile
    with the median in it as a black line, and whiskers), a thin green line at the overall median that sticks out of
    the boxes, and under the tree a scale with its caption; \code{"both"}, the box plot and, under it, a bar from
    the overall mean (or median) to that of the node on the same scale, with the line of the overall mean in the
    colour of the bar; \code{"dots"}, the box plot and, under it, the observations of the node as points (the scale
    holds all of them, also the outliers and the extremes that the whiskers leave out; they are drawn in grey, with
    some transparency, spread vertically in a fixed way, so the graph is always the same). The colour says whether
    the node is above (blue) or below (orange) the reference. With frequency weights the observations count
    as many times as their weight, except those outside the whiskers, which are drawn once.}
  \item{maxdots}{with \code{nodeplot = "dots"}: most points drawn in a node (default 200; a whole number from 20 to
    5000). All the observations outside the whiskers are always drawn; of those between them only as many as leave the node at
    \code{maxdots} (never fewer than a tenth of it) are drawn, a systematic sample of the sorted values that keeps the
    shape of the distribution. The legend says it when a node has been sampled.}
  \item{center}{with \code{treestyle = "boxes"}: \code{"mean"} (default) or \code{"median"}, the statistic of each
    node that is written, compared with the overall one, painted and used as reference of the bars. The green line
    of the box plots is always the overall median.}
  \item{whisker}{with \code{nodeplot = "box"}, \code{"both"} or \code{"dots"}: \code{"p5"} (default), the 5th and 95th percentiles;
    \code{"minmax"}, the minimum and the maximum; \code{"tukey"}, the most extreme values within 1.5 times the
    interquartile range of the quartiles (the outliers are not drawn).}
  \item{shade}{with \code{treestyle = "boxes"}: if \code{TRUE} (default) the blue and the orange are stronger the more
    overall standard deviations the node is away from the reference (one or more, the strongest); if \code{FALSE}
    they have one single intensity.}
  \item{col}{two colours (any colour name or code of R) for the nodes above and below the reference; by default
    blue and orange. They can be named, \code{c(above = "darkgreen", below = "purple")}, in any order, or given as a list
    with a single element as in \code{netCoin::netCoin()}. In the boxes style the shades of the nodes are those of each colour mixed with white.}
  \item{cex}{factor that multiplies the size of all the texts as they are seen on the device (default 1; between 0.5 and
    3), in both tree styles. The layout does not change, so with a wide tree a large value makes the texts overlap or
    overflow the boxes; the size of the texts depends on the size of the device, and the function returns invisibly the
    natural size of the tree in inches (\code{width}, \code{height}): a device of that size gives texts of about 8-12 points.}
  \item{nodehead}{with \code{treestyle = "boxes"}: the heading of each box. \code{"category"} (default) is the category
    (or the interval) of the splitting variable that forms the node, the one that \code{"number"} writes on the branches;
    the number of the node goes in a small square on the top left corner of the box (dark, with the number in white, when the node is a terminal one) and the branches have no labels. A long
    heading is written with a smaller letter, in two lines or cut with an ellipsis. \code{"number"} is the heading
    "Node N" with the labels on the branches (the heading of a terminal node is dark, with the text in white).}
  \item{rootlabel}{the heading of the root node (in the boxes style, only with \code{nodehead = "category"}): \code{"total"}
    (default; "Total", translated), \code{"depvar"} (the name of the response variable) or any text.}
  \item{digits}{whole number between 0 and 6 (default 1): decimals of the mean, the median and the standard deviation of the
    nodes, of their differences with the overall one and of the numbers of the legend and the scale (the percentages, the tests
    and the p-values do not change).}
  \item{collapse}{only with \code{dir}: depth from which the branches of the interactive page start collapsed (the default,
    \code{NULL}, shows the whole tree). See \code{\link{tree_html}}.}
  \item{dir}{if given, the graph is drawn and, after it, the same tree is written as an interactive HTML page in this folder, with
    \code{\link{tree_html}} and the same arguments (\code{cex} does not apply to the page). The path of the page is written and, in an
    interactive session, the page is opened in the browser. An existing folder is accepted if it is empty or holds a page written by
    this package (which is replaced); the package \pkg{jsonlite} is needed.}
  \item{...}{not used.}
}
\details{
In a tree grown with \code{forcevars} the variable of every forced split is followed by an asterisk, and a note at the bottom of the
graph, in the language of the graph, explains it.
}
\value{
Invisibly, a named vector with the \code{width} and \code{height} in inches of the layout (with \code{dir}, the path of the page is in
its attribute \code{"html"}); \code{NULL} if the tree has a single node (then no page is written either).
}
\examples{
set.seed(1)
fit <- maid(mpg ~ cyl + gear + carb, data = mtcars, unordered = "carb", minnode = 5, minsplit = 10)
plot(fit, treestyle = "classic", quartiles = TRUE)
plot(fit, nodeplot = "both", language = "es")
plot(fit, nodeplot = "dots", note = "Source: mtcars (R datasets)")
## the graph and, after it, the interactive page of the same tree (needs jsonlite)
if (requireNamespace("jsonlite", quietly = TRUE))
  plot(fit, dir = file.path(tempdir(), "tree_mtcars"))
}
