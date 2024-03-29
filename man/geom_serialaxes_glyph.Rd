% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-serialaxes-glyph.R
\name{geom_serialaxes_glyph}
\alias{geom_serialaxes_glyph}
\title{Add serial axes glyphs on scatter plot}
\usage{
geom_serialaxes_glyph(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  serialaxes.data,
  axes.sequence = character(0L),
  scaling = c("data", "variable", "observation", "none"),
  axes.layout = c("parallel", "radial"),
  andrews = FALSE,
  show.axes = FALSE,
  show.enclosing = FALSE,
  linewidth = 1,
  axescolour = "black",
  bboxcolour = "black",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{serialaxes.data}{a serial axes numerical data set.
If not provided, a point visual (\code{geom_point()}) will be displayed.}

\item{axes.sequence}{A vector to define the axes sequence. In serial axes coordinate, the sequence can be either
determined in \code{mapping} (function \code{aes()}) or by \code{axes.sequence}. The only difference is that
the \code{mapping} aesthetics will omit the duplicated axes (check examples in \code{\link{geom_serialaxes}}).}

\item{scaling}{one of \code{data}, \code{variable}, \code{observation} or
\code{none} (not suggested the layout is the same with \code{data})
to specify how the data is scaled.}

\item{axes.layout}{either "radial" or "parallel"}

\item{andrews}{Logical; Andrew's plot (a Fourier transformation)}

\item{show.axes}{boolean to indicate whether axes should be shown or not}

\item{show.enclosing}{boolean to indicate whether enclosing should be shown or not}

\item{linewidth}{line width of the "glyph" object}

\item{axescolour}{axes color}

\item{bboxcolour}{bounding box color}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
a \code{geom} layer
}
\description{
To visualize high dimensional data on scatterplot.
Each point glyph is surrounded by a serial axes (parallel axes or radial axes) object.
}
\section{Aesthetics}{

geom_..._glyph() understands the following aesthetics (required aesthetics are in bold):
\itemize{
\item{\strong{x}}
\item{\strong{y}}
\item{alpha}
\item{colour}
\item{fill}
\item{group}
\item{size}
\item{linetype}
\item{shape}
\item{stroke}
}

The size unit is \code{cm}

Note that the shape and stroke do not have real meanings unless the essential
argument \code{serialaxes.data} is missing. If so, a point visual will be displayed with
corresponding shape and stroke.
}

\examples{
# serial axes glyph
p <- ggplot(data = iris,
            mapping = aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_serialaxes_glyph(serialaxes.data = iris[, -5],
                        axes.layout = "radial")
p
}
\seealso{
\code{\link{geom_polygon_glyph}}, \code{\link{geom_image_glyph}}
}
