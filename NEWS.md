# mchaid 0.9.5

* `plot.mchaid()` and `plot.maid()` draw by default the style of boxes (`treestyle = "boxes"`, as `tree_html()` already
  did); the classic graph is `treestyle = "classic"`. **This changes the default graph.** In the boxes style
  `quartiles` has no effect and now says so with a warning (unless it is `FALSE`); the classic graph needs
  `treestyle = "classic"` for it, as in `plot(fm, treestyle = "classic", quartiles = TRUE)`.
* New argument `dir` in `plot.mchaid()` and `plot.maid()`: if it is given, the graph is drawn and, after it, the same
  tree is written as the interactive page of `tree_html()` in that folder, with the same arguments (the path is
  written, the page is opened in the browser in an interactive session, and the path is also in the attribute
  `"html"` of the value). `collapse`, which only the page uses, is also an argument of `plot()`. A tree without
  splits writes no page.
* New argument `note` in `plot.mchaid()` and `plot.maid()`, as in `tree_html()`: text at the bottom left of the graph,
  in the two styles (a new line, `"\n"`, starts a new line of the note). It is also written in the page with `dir`.
* `center`, `digits` and `maxdots` stay only in `plot.maid()` and `tree_html()`: with `mchaid` there is no mean, no
  decimals and no points to draw, so they would have no effect.

# mchaid 0.9.4

* Repository metadata: `URL` and `BugReports` in `DESCRIPTION`, and the credit to Joseph N. Luchman's Stata
  command `chaid` (on which the Stata commands `mchaid` and `maid` are based) moved from the author list to the
  package description. No change in the code.

# mchaid 0.9.3

* `asnetCoin()` fixes `mode = "network"` in the netCoin page: the "Heatmap" switch, which says nothing about a
  tree, disappears. `mode = c("network", "heatmap")` brings it back.

# mchaid 0.9.2

* The netCoin page of `asnetCoin()` is drawn with `showCoordinates = TRUE`. netCoin used to fit the fixed
  coordinates of the tree in a square (of side `min(width, height)/1.2`), so the levels were far apart and the
  root could be cut off in short windows; now the tree uses the whole available rectangle and the levels are
  closer. `showCoordinates = FALSE` brings the square back. The table of nodes gets the columns `x` and `y`.
  Larger labels need only `cex`, which reaches `netCoin()` through `...`, for example
  `asnetCoin(fit, cex = 1.3)`.

# mchaid 0.9.1

* In the interactive classic tree (`tree_html(treestyle = "classic")`) a collapsed node shows its own bar (the
  distribution of the response in `mchaid`; the deviation from the mean with its mean and standard deviation in
  `maid`, and the median and quartiles if requested) under its box, at the height of the bars of the terminal
  nodes, as if it were a terminal node. The open/close button moves next to the box while it is collapsed, and
  the number of terminal nodes it hides is shown in the tooltip. Expanding the node restores the previous drawing.

# mchaid 0.9.0

* `tree_html(treestyle = "classic")`: the interactive tree in the layout of the classic graph of `plot.mchaid`
  and `plot.maid` (the new one of 0.8.0). Every node is a small box with the category of its branch and its
  size, the splitting variable with its statistic (`chi2=434; p<0.001`, `F=59.3; p<0.001`) goes on the branch,
  and only the terminal nodes carry a plot below: the stacked bar of the categories (`mchaid`) or the deviation
  from the overall mean with its mean and standard deviation (`maid`). The legend, the risk estimate with
  Cramer's V (Eta-squared in `maid`) and the note of the bars go at the bottom. The tooltip, the highlighted
  path, the folding of branches, the zoom and the SVG and PNG download are kept.
* New arguments: `treestyle = c("boxes", "classic")` (the default is still `"boxes"`) and `quartiles` (median
  and quartiles over the bars, classic style only). With `treestyle = "classic"`, `nodeplot`, `center`,
  `whisker`, `shade`, `nodehead` and `maxdots` are ignored with a warning. `rootlabel`, `language`, `pval`,
  `pcut`, `pct`, `col`, `palette`, `ramp`, `collapse`, `digits` and `note` work in both styles.

# mchaid 0.8.0

* New format of the classic graph (`treestyle = "classic"`, the default) of `plot.mchaid` and `plot.maid`. All
  the nodes are boxes with the category (or group of categories) of the branch leading to them as heading and
  their size (`n = 24`); the root says "Total" (translated; `rootlabel = "depvar"` or any text changes it, and
  `rootlabel` now also works in the classic style). "Node N" is no longer written and the branches carry no
  labels: the splitting variable (with its `forcevars` mark) and its statistic, `chi2=29.0; p<0.001` or
  `F=3.79; p=0.052`, go on the stem, under the box of the node, in a block with a white background. The bars of
  the leaves, the legend and the footer do not change. A long heading is written in a smaller font, in two lines
  or cut with an ellipsis.
* Labels instead of codes: a numeric variable with value labels (attribute `labels`, as given by `haven` when
  reading a .dta file) is written with its labels in all the texts (headings, tables, node labels); the codes
  used by `predict` for new data do not change.
* **Behaviour change:** the classic graph is no longer drawn as before. The interactive tree (`tree_html`) and
  the style of boxes do not change.

# mchaid 0.7.0

* `nodeplot = "dots"` in `plot.maid` and `tree_html` (`maid`): the box plot and, under it, the observations of
  the node as points; `maxdots` sets the most points drawn in a node.

# mchaid 0.6.1

* Trees grown with `forcevars` mark the forced splits. In the graphs of boxes, in the interactive tree
  (`tree_html`) and in the classic graph of `maid`, the variable of every forced split is followed by an
  asterisk, and a note at the bottom of the graph (translated into the 10 languages of `language`) explains it;
  in the classic graph of `mchaid`, where the asterisk already marks the modal category in the bars, a dagger is
  used and the note is added to that of the bars. Deeper splits, and those of nodes where the forced split was
  not possible, carry no mark. `asnetCoin()` adds the logical column `forced` to the table of nodes. Trees
  without forced splits are drawn as before.

# mchaid 0.6.0

* `forcevars` in `mchaid()` and `maid()`: names of variables that choose the first splits of the tree (the
  first variable splits the first level, the second the second level, and so on; any number of variables),
  instead of those the algorithm would choose. Below them the tree grows as usual. In every node the categories
  of the forced variable are merged as in the algorithm and the node is split even if the adjusted p is not
  smaller than `spltalpha`; if it is not, merging goes on until two groups are left (binary split).
  `minnode` is respected: if the sample cannot be split by the first variable there is an error, and in a
  deeper node where the forced split is not possible (the variable has fewer than two categories or a node
  would be too small) the node grows freely, with a warning. `maxbranch` cannot be smaller than the number of
  forced variables. The result stores `forcevars` and `force_fallback`; `print` shows the forced variables and
  `steps = TRUE` explains every decision.

# mchaid 0.5.0

* Colours of the categories in the style of `netCoin`. In `plot.mchaid`, `tree_html` and `asnetCoin()` (with
  `color = "modal"`), `col` accepts (1) a named vector by category, in any order,
  `col = c(Yes = "forestgreen", No = "firebrick")`; (2) a list with a single named vector,
  `list(Survived = c(Yes = ..., No = ...))`, like the `color` of `netCoin`; (3) an unnamed vector, with the
  colours in the order of the categories (error if colours are missing, warning if there are too many).
  Categories without a colour take that of the palette, with a warning; names that are not categories are
  ignored, also with a warning; invalid colours are an error. `palette` also accepts `"blues"` and any name
  from `grDevices::palette.pals()` (with R >= 4.0) or `grDevices::hcl.pals()`, ignoring case. `col` used to be
  recycled silently. In `plot.maid`, `col` can be `c(above = ..., below = ...)` in any order.

# mchaid 0.4.1

* Argument `digits` (0 to 6, default 1) in `plot.maid` and `tree_html` (`maid`): decimals of the mean, the
  median and the standard deviation of the nodes, of their differences with the overall ones and of the numbers
  of the legend and the scale (they used to be 4 significant digits, with different decimals in each node).
  Percentages, tests and p-values do not change.

# mchaid 0.4.0

* Interactive tree built into the package. `tree_html(fit, ...)` writes a folder with a self-contained HTML
  page (`index.html`, scripts, styles and data in JSON) with the same boxes as
  `plot(fit, treestyle = "boxes")` and the same arguments (`nodeplot`, `center`, `whisker`, `shade`, `pval`,
  `pcut`, `language`, `col`, `palette`, `pct`, `nodehead`, `rootlabel`), plus `main`, `note`, `collapse` and
  `dir`. The page has a tooltip with the definition and the distribution of the node, the path highlighted
  from the root, animated folding and unfolding of branches, zoom and panning with the mouse, a category
  filter by hovering over the legend and SVG and PNG download. It needs `jsonlite` (in `Suggests`; the rest of
  the package is still base R). The files of the page (D3 5, Roboto, `tree.js`, `tree.css`) are in `inst/www/`
  and weigh about 170 KB.

# mchaid 0.3.1

* The error messages of the graph options say which argument fails (`match.arg()` used to say `'arg'`), and
  `plot()` warns when `nodeplot`, `nodehead`, `rootlabel`, `center` or `whisker` are given with
  `treestyle = "classic"`, which ignores them.

# mchaid 0.3.0

* Heading of the boxes of `treestyle = "boxes"` (`plot.mchaid` and `plot.maid`). With `nodehead = "category"`
  (default) the heading of every box is the category (or interval) of the splitting variable that forms the
  node, the one that used to go on the label of the branch; the number of the node moves to a small square on
  the upper left corner of the box and the branches carry no labels. The root says "Total" (translated) by
  default: `rootlabel = "depvar"` writes the name of the response variable and any other text is used as it is.
  A long heading is written in a smaller font, in two lines or cut with an ellipsis. `nodehead = "number"`
  gives the previous design. **Behaviour change:** the style of boxes is no longer drawn as before by default.

# mchaid 0.2.6

* The median is drawn in green (Okabe-Ito green) instead of red, which was confused with the orange of
  "below": the line of the overall median of the box plots, and the median and quartiles over the bars of
  `quartiles =`. In `plot.maid` with `nodeplot = "bar"` and `center = "mean"`, the line of the overall mean
  turns dark grey.

# mchaid 0.2.5

* Colours. In `plot.mchaid`, `palette = "default" | "okabe" | "viridis" | "grey"` chooses the palette of the
  categories of the response (`"grey"` is for black-and-white printing) and `col` is still a vector of your own
  colours, which takes precedence. In `plot.maid`, `col = c(above, below)` changes the blue and the orange of
  the nodes. The orange and purple of the default unordered palette are lighter (same hue) and the numbers
  inside the bars are written in black or white depending on the real contrast with the background. Needs
  R >= 3.6.0 (`hcl.colors`).

# mchaid 0.2.4

* The width of the lines and of the borders of the boxes follows the scale of the drawing: it used to be fixed,
  and in trees with many nodes, which are shrunk to fit the device, the borders were too thick for the boxes.
  It is independent of `cex`.

# mchaid 0.2.3

* `cex` (between 0.5 and 3, default 1) multiplies the size of the texts as seen on the device, in both styles
  (the classic one used to ignore it and the boxes one cancelled it when readjusting the drawing to the device).
  The layout does not change, so with wide trees a large `cex` overlaps the texts. `plot()` returns the natural
  size of the tree in inches.

# mchaid 0.2.1 - 0.2.2

* In the style of boxes the width of the boxes fits the widest line of text, measured with the real font of the
  graphics device, and the legend is laid out in up to three rows of the width of the tree.

# mchaid 0.2.0

* Style of boxes (`treestyle = "boxes"`): every node of the tree, root and intermediate nodes included, is a
  box with its name, its size and a plot of its distribution; under every node that splits, the variable and
  its test (`chi2 = 434; p < 0.001`, or `F = 19.7; ...` in `maid`); under the terminal nodes, the predicted
  category. Nodes are numbered as in SPSS (the root is 0, level by level and from left to right).
  `nodeplot = "bar"` (default in `mchaid`) draws the bar of percentages; `nodeplot = "box"` (in `mchaid` only
  with an ordered response; default in `maid`) a box plot on the common scale of all the nodes, with a line at
  the overall median; `nodeplot = "both"` (`maid` only) adds under the box a bar of the mean of the node
  against the overall one. Whiskers are the 5th and 95th percentiles (`whisker = "minmax"` or `"tukey"` change
  them).
* The classic style also writes the statistic (`chi2=434; p=...`) before the p-value of every split.

# mchaid 0.1.0

* First version: port to R of the Stata commands `mchaid` (version 1.0.0, categorical response) and `maid`
  (quantitative response), with `print`, `summary`, `plot`, `predict`, `classification`, `importance` and
  `asnetCoin`.
