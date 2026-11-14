\name{asnetCoin}
\alias{asnetCoin}
\title{A CHAID or MAID tree as a network (netCoin)}
\description{
A tree is a directed network: the root, the nodes that are split and the terminal nodes are its nodes, and
the branches are its links. \code{asnetCoin} builds the tables of nodes and links, and the coordinates
of the tree, in the format of the \pkg{netCoin} package, and creates the \code{netCoin} object when that
package is installed (it is not needed to fit or plot a tree, and the tables are made with base R). It works
with the trees of \code{\link{mchaid}} (categorical response) and of \code{\link{maid}} (quantitative response).
}
\usage{
asnetCoin(x, label = "label", size = "n", labsize = NULL, color = "auto",
          shape = "type", image = NULL, legend = NULL, group = NULL,
          border = NULL, lwidth = "n", lweight = NULL,
          lcolor = NULL, ltext = "label", build = TRUE, node = "Node", digits = 1,
          col = NULL, palette = "default", ramp = FALSE, ...)
}
\arguments{
  \item{x}{an object returned by \code{\link{mchaid}} or \code{\link{maid}} with at least one split.}
  \item{label, size, labsize, color, shape, image, legend, group, border}{names of the columns of the
    table of nodes (see Details) that give the label, size, size of the label, colour, shape, image,
    legend, group and border of the nodes; they are passed to the arguments \code{label}, \code{size},
    \code{labelSize}, \code{color}, \code{shape}, \code{image}, \code{legend}, \code{group} and
    \code{border} of \code{netCoin::netCoin()}. \code{NULL} omits the attribute. The defaults are the
    label of the node, its number of cases, the colour and the shape by type of node. By default there is
    no \code{group}.

    \code{color = "auto"} is, for a tree of \code{mchaid}, \code{"modal"}: the colours of the categories of the
    response (the same as in \code{\link[=plot.mchaid]{plot}}) according to the modal category of every node; the
    nodes that are split take the colour of their modal category before the split. For a tree of \code{maid} it is
    \code{"mean"}: the colour reflects the mean of the response in the node (also in the nodes that are split);
    use \code{color = "dev"} with the argument \code{nodeBipolar = TRUE} of \pkg{netCoin} to colour the deviation
    of the mean from the overall mean with two colours.

    \code{shape = "type"} stands for a triangle for the root, a circle for the nodes that are split and a square
    for the terminal nodes. A list like \code{list(modal = c(No = "red", Yes = "blue"))} (a column and a named
    vector of values) is also accepted, as in \pkg{netCoin}.}
  \item{lwidth, lweight, lcolor, ltext}{names of the columns of the table of links that give the width,
    weight, colour and text of the links (the same arguments as in \code{netCoin::netCoin()}). The defaults are
    the number of cases of the child node (\code{"n"}) for the width and the categories on the branch
    (\code{"label"}) for the text.}
  \item{build}{logical. If \code{TRUE} (default) and \pkg{netCoin} is installed, return the result of
    \code{netCoin::netCoin()}; otherwise (or if it is not installed) return a list with the tables.}
  \item{node}{word used to name the terminal nodes (\code{"Node 1"}, \code{"Node 2"}...).}
  \item{digits}{decimals of the percentages (trees of \code{mchaid}).}
  \item{col, palette, ramp}{colours of the categories of the response with \code{color = "modal"} (trees of \code{mchaid}), as in
    \code{\link{plot.mchaid}}: a vector named by category, a list with a single named vector, a vector without names or a palette.}
  \item{...}{further arguments for \code{netCoin::netCoin()}, for example \code{dir = "tree"} to write the
    web page, \code{language = "es"}, \code{main}, or \code{layout = "re"} (Reingold-Tilford) to replace the
    coordinates of the tree. The tree is drawn with \code{showCoordinates = TRUE}, so that netCoin spreads it
    over the whole plotting area instead of fitting it in a square, and the levels are closer to each other;
    use \code{showCoordinates = FALSE} to get the square back. \code{cex = 1.3} makes the text larger. The page offers only the network (\code{mode = "network"}),
    without the heatmap switch, which tells nothing about a tree; \code{mode = c("network", "heatmap")} brings it back.}
}
\details{
\strong{Nodes.} Columns common to both kinds of trees: \code{name} (unique: \code{"root"}, \code{"split2"}...,
\code{"Node 3"}), \code{label} (the splitting variable, or the name of the terminal node), \code{type}
(\code{"root"}, \code{"split"} or \code{"node"}), \code{depth}, \code{n} (cases, weighted), \code{variable}
and \code{p} (splitting variable and adjusted p-value of the split, of the nodes that are split),
\code{forced} (logical: the variable of the split was chosen with \code{forcevars}),
\code{definition} (conditions from the root), \code{ntext} (HTML text for the tooltip) and \code{node} (number
of the terminal node, as in \code{predict(x)}).

For \code{mchaid}: \code{modal} (modal category of the node, also for the root and the nodes that are split) and
\code{pct_<category>} (percentage of each category of the response in the node, also in the nodes that are
split). For \code{maid}: \code{mean}, \code{sd}, \code{min}, \code{p25}, \code{median}, \code{p75}, \code{max}
of the response in the node, also in the nodes that are split, and \code{dev} (mean of the node minus the overall
mean).

\strong{Links.} \code{Source} and \code{Target} (names of the nodes), \code{label} (categories of the
splitting variable on the branch, merged categories together), \code{n} (cases of the child), \code{p} and
\code{variable} (of the split), and \code{pct_of_parent}.

\strong{Layout.} A matrix with the columns \code{x} and \code{y} and the nodes as rows, in the order of the
nodes table: the terminal nodes in a row at the bottom, in the order of the tree, and each split above the
middle of the nodes below it.
}
\value{
A \code{netCoin} object, or (\code{build = FALSE}, or \pkg{netCoin} not installed) a list with
\code{nodes}, \code{links} and \code{layout}, ready for \code{netCoin::netCoin(nodes, links, layout = layout)}.
}
\examples{
tt <- as.data.frame(Titanic)
fit <- mchaid(Survived ~ Class + Sex + Age, tt, weights = Freq, minnode = 30, minsplit = 60)
net <- asnetCoin(fit, build = FALSE)
net$nodes[, c("name", "label", "type", "n", "modal")]
net$links

set.seed(1)
fit2 <- maid(mpg ~ cyl + gear + carb, data = mtcars, unordered = "carb", minnode = 5, minsplit = 10)
asnetCoin(fit2, build = FALSE)$nodes[, c("name", "label", "type", "n", "mean", "sd")]
\dontrun{
asnetCoin(fit, dir = "tree_netcoin")                 # needs the netCoin package
asnetCoin(fit, size = "depth", lwidth = "pct_of_parent", ltext = "p")
asnetCoin(fit2, color = "dev", nodeBipolar = TRUE)
}
}
