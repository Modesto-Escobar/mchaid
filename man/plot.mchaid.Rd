\name{plot.mchaid}
\alias{plot.mchaid}
\title{Plot a CHAID tree}
\description{
Draws the tree. Every node is a box with the category (or group of categories, with the value label if the variable has one)
of the branch that leads to it, as its heading, and its number of cases (the root says "Total"). Over the branch under a node that is
split go the splitting variable and, under it, the likelihood-ratio chi-square and the Bonferroni-adjusted p-value of the split
(\code{chi2=434; p<0.001}). Each terminal node has at the bottom a stacked bar with the distribution (\%) of the response
variable (the modal category is marked with an asterisk). The last line gives the risk estimate (proportion misclassified when every case is assigned
the modal category of its node; the same for the root node in parentheses) and Cramer's V.
}
\usage{
\method{plot}{mchaid}(x, main = NULL, pct = TRUE, pval = TRUE, pcut = NULL,
     language = "en", quartiles = NULL, col = NULL, palette = "default", ramp = FALSE,
     treestyle = c("classic", "boxes"), nodeplot = NULL,
     whisker = c("p5", "minmax", "tukey"), shade = TRUE, cex = 1,
     nodehead = c("category", "number"), rootlabel = "total", ...)
}
\arguments{
  \item{x}{an object returned by \code{\link{mchaid}}.}
  \item{main}{title; by default the name of the response variable with a text in the chosen language.}
  \item{pct}{logical: write the percentages inside the bars.}
  \item{pval}{logical: write the p-value of each split.}
  \item{pcut}{if a number between 0 and 1, the p-values smaller than it are written
    \code{p<}\emph{pcut}.}
  \item{language}{language of the texts of the graph: \code{"en"} (default), \code{"es"}, \code{"fr"},
    \code{"de"}, \code{"it"}, \code{"pt"}, \code{"ca"}, \code{"gl"}, \code{"nl"} or \code{"eu"}.}
  \item{quartiles}{with an ordered response, \code{"auto"} (or \code{TRUE}) or \code{"all"}: draw the
    median and the quartiles (see Details). With \code{"auto"} only the median is drawn in the nodes
    where a quartile is too close to it to be told apart; with \code{"all"} all of them are always
    drawn.}
  \item{col}{optional colours (any colour name or code of R) for the categories of the response; it takes precedence over
    \code{palette}. As in the \code{color} argument of \code{netCoin::netCoin()}, it can be a vector \emph{named by category},
    \code{c(Yes = "forestgreen", No = "firebrick")}, in any order (the categories without colour take that of the palette, with
    a warning, and the names that are not categories are ignored, also with a warning); a list with a single named vector,
    \code{list(Survived = c(Yes = "forestgreen", No = "firebrick"))}; or a vector \emph{without names}, with the colours of the
    categories in their order (at least as many as categories; an error otherwise, and a warning if there are more). The
    numbers inside the bars are written in black or white, whichever contrasts more with the colour.}
  \item{ramp}{\code{FALSE} (default), \code{TRUE} or \code{"reverse"}. With \code{TRUE}, \code{col} is one single colour and the
    categories get its shades, from light (the first category) to the colour itself (the last one), for example
    \code{col = "forestgreen", ramp = TRUE} for an ordered response; \code{"reverse"} goes from dark to light. The shades are the
    colour mixed with white, from 20\% to 100\% of it.}
  \item{palette}{palette of the categories of the response when \code{col} is not given: \code{"default"} (the blues from
    light to dark if the response is ordered, otherwise the colour-blind safe palette of Okabe and Ito, with a lighter orange
    and purple), \code{"okabe"} (that colour-blind safe palette also for an ordered response), \code{"viridis"},
    \code{"grey"} (greys from light to dark, to print in black and white), \code{"blues"}, or any name of
    \code{grDevices::palette.pals()} (e.g. \code{"Set 2"}, \code{"R4"}; it must have as many colours as categories) or of
    \code{grDevices::hcl.pals()} (e.g. \code{"Dark 3"}, \code{"Zissou 1"}); case does not matter.}
  \item{treestyle}{\code{"classic"} (default), the graph described above, or \code{"boxes"}: every node of the
    tree, the root and the intermediate nodes included, is a box with its name, its size (n and \% of the cases)
    and a plot of the distribution of the response variable; under a node that is split go the splitting variable
    and its test (\code{chi2 = 434; p < 0.001}), on each branch the categories, and under each terminal node the
    predicted (modal) category. The nodes are numbered as in SPSS (the root is node 0, level by level, from left to
    right), so the numbers of the terminal nodes are not those of \code{print} or \code{summary}. The numbers are
    written with the separators of the language, there is no title unless \code{main} is given, and
    \code{quartiles} is ignored.}
  \item{nodeplot}{with \code{treestyle = "boxes"}: \code{"bar"} (default), the stacked bar of the percentages of the
    categories; \code{"box"}, only with an ordered response, a box plot on the scale of the categories (category
    j occupies the band from j - 1 to j and the percentiles are interpolated inside the category where the
    cumulative percentage reaches them): a box from the first to the third quartile with the median in it (black
    line) and whiskers, a thin green line at the overall median, which sticks out of the boxes, and a colour that
    says whether the median of the node is above (blue) or below (orange) it.}
  \item{whisker}{with \code{nodeplot = "box"}: \code{"p5"} (default), the 5th and 95th percentiles; \code{"minmax"},
    the first and the last occupied categories; \code{"tukey"}, the most extreme values within 1.5 times the
    interquartile range of the quartiles.}
  \item{shade}{with \code{nodeplot = "box"}: if \code{TRUE} (default) the blue and the orange are stronger the more
    standard deviations of the scale of the categories the node is away from the overall median (one or more, the
    strongest); if \code{FALSE} they have one single intensity.}
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
  \item{...}{not used.}
}
\details{
\strong{Forced splits.} In a tree grown with \code{forcevars} the variable of every split that the user chose is followed by an
asterisk (by a dagger in the classic graph of \code{mchaid}, which already uses the asterisk for the modal category), and a note at the
bottom of the graph explains it in the language of the graph. The splits below the forced levels, and those of the nodes where the
forced split was not possible, carry no mark. Trees without forced splits are drawn as before.

The layout is in inches (about 1.15 inches for each terminal node), so a large tree needs a large
device. The width and the height that the graph asks for are returned invisibly, for example to open
\code{png(width = w, height = h, units = "in", res = 150)}; the text is scaled with the device.

With an ordered response (\code{dvordered}) the colours are sequential, and \code{quartiles} draws over
the bar of each terminal node a green line at the median (the second quartile) and, if the response has
five categories or more, dashed lines at the first and third quartiles. The height of a line is the
value of the percentile on the scale of the categories (the bar is divided in as many equal bands as
categories, and the percentile is interpolated within the category where the cumulative percentage
reaches 25, 50 or 75), so the lines can be compared between nodes.
}
\value{
Invisibly, a named vector with the \code{width} and \code{height} in inches of the layout; \code{NULL}
if the tree has a single node.
}
\examples{
tt <- as.data.frame(Titanic)
fit <- mchaid(Survived ~ Class + Sex + Age, tt, weights = Freq, minnode = 30, minsplit = 60)
plot(fit, language = "es")
plot(fit, treestyle = "boxes", language = "es")
size <- plot(fit)
## colours by the name of the category, in the order of the categories, a palette
## and shades of one colour
plot(fit, treestyle = "boxes", col = c(Yes = "forestgreen", No = "firebrick"))
plot(fit, treestyle = "boxes", col = c("firebrick", "forestgreen"))
plot(fit, treestyle = "boxes", palette = "Set 2")
plot(fit, treestyle = "boxes", col = "forestgreen", ramp = TRUE)
}
