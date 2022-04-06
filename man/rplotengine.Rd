% ---------------------------------------------------------------
% File: /rplotengine/man/rplotengine.Rd
% Una vez instalado el paquete se puede acceder con:
%    > library(rplotengine)
%    > ?rplotengine
% Tambien se puede generar un PDF con:
%    R CMD Rd2pdf rplotengine
% ---------------------------------------------------------------

% Nombre, alias, titulo
\name{rplotengine}
\alias{rplotengine}
\title{R as a plotting engine}

% Descripcion (+ archivo DESCRIPTION)
\description{
   Generate a set of graphs as defined in the file specified.
%\code{save}.
}

% Uso
\usage{
   rplotengine (args_file = "mygraph.arg")
}

% Argumentos
\arguments{
   \item{args_file}{A character string giving the name of the text file containing the parameters for the graph to be plotted. The format of such input file is described in detail below.}
}

% Descripcion detallada
\details{

The \strong{input file} name passed to \pkg{rplotengine} is a plain text file with a list of pairs \code{parameter=value}, one-per-line. 
The meaning of each parameter is explained below:

%\tabular{ll}{
%	Argument     \tab Description \cr
%	Argument     \tab Description
%}

% The % characters must be preceded by \
\describe{
\item{title       }{Main title that can be shown in the graph. It is required to specify \code{show_titles=1}; otherwise the value will be ignored.}
\item{subtitle    }{Subtitle that can be shown in the graph. It is required to specify \code{show_titles=1}; otherwise the value will be ignored.}
\item{x_axis_title}{Title for the X-axis.}
\item{y_axis_title}{Title for the Y-axis.}
\item{col_x_values}{Column number of the values for the X-axis within the input data file (1-based). DEPRECATED: since 1.0-6 the first column (1) is always the values for the X-axis (this argument will be ignored).}
\item{col_y_values}{List with the columns number within the input data file corresponding to the series to be plotted. In case of using confidence intervals, each column within the input data file should be followed by another column containing its corresponding confidence interval values, but that column is not specified here, and this is why a typical value for this argument is something like: 2,4,6,8 (being 3,5,7,9 the corresponding columns for the confidence intervals). DEPRECATED: since 1.0-6 all columns in the data file will be used (this argument will be ignored).}
\item{series_names}{Name for the series to be plotted as should be shown in the legend. The number of names specified should match with the number of columns specified in the argument col_y_values. In case of showing an overall serie (total_serie greater than 0), an additional name for it should be specified too.}
\item{x_factor    }{Value which the X-axis values will be multiplied by (usually 1).}
\item{y_factor    }{Value which the Y-axis values will be multiplied by (usually 1).}
\item{total_serie }{Value ranged from 0 to 4, to indicate if an additional summary serie should be shown. The values mean: 0-none; 1-sum of all the series; 2-average of all the series; 3-constant value (specified by \code{total_value}); 4-proportional to the x value (proportion specified by \code{total_value}). In case of a total serie be shown (argument total_serie greater than 0), an additional name need to be specified in the list of names (argumento series_names).}
\item{total_value }{Value used for the argument \code{total_serie} in certain cases (3 and 4); otherwise, it is ignored.}
\item{x_min       }{Minimum value for the X-axis (usually 0). Specify 'automatic' (without the quotes) in order to consider the minimum value from the data.}
\item{x_max       }{Maximum value for the X-axis. It is recommended to adjust automatically the graph depending on the data by assigning the value 'automatic' (without the quotes).}
\item{y_min       }{Minimum value for the Y-axis (usually 0). Specify 'automatic' (without the quotes) in order to consider the minimum value from the data. When using logarithmic scale in the y-axis (y_log=1), this value cannot be 0.0; the script try to fix this automatically.}
\item{y_max       }{Maximum value for the Y-axis (usually -1). It is recommended to adjust automatically the graph depending on the data by assigning the value 'automatic' (without the quotes).}
\item{y_log       }{Value 0/1 to indicate if the Y-axis should be shown in logarithm scale (usually 0).}
\item{smooth_data          }{Smooth data; 0.0 (data as is) to 1.0 (maximum).}
\item{show_titles          }{Value 0/1 to indicate if title and subtitle are shown.}
\item{show_grid            }{Value 0/1 to indicate if a grid are shown in the background.}
\item{show_confint         }{Value 0/1 to indicate if confidence interval are shown.}
\item{confint_as_percentage}{Value 0/1 to indicate if the values for the confidence interval within the input data file are expressed as absolute values (0) or as a \% value (1, the margin need to be calculated).}
\item{text_size_title      }{Font size for the title. The value specified will be a proportion to the normal text (1.0): for a greater font size specify values > 1.0; for smaller font size specify values < 1.0.}
\item{text_size_subtitle   }{Font size for the subtitle. The value specified will be a proportion to the normal text (1.0), as explained before.}
\item{text_size_axis_ticks }{Font size for the X-axis and Y-axis numbers. The value specified will be a proportion to the normal text (1.0), as explained before.}
\item{text_size_axis_titles}{Font size for the X-axis and Y-axis titles. The value specified will be a proportion to the normal text (1.0), as explained before.}
\item{text_size_legend     }{Font size for the series names in the legend. The value specified will be a proportion to the normal text (1.0), as explained before.}
\item{pos_legend           }{Place for the legend in the graph, within a 3x3 grid with cells numbered from 1 to 9: 1-top left corner, 2-top middle; 3-top right corner; ... ; 9-bottom right corner. Specify a 0 value if a legend is not required.}
\item{graph_type           }{Type of graph to generate (0:lines; 1:stacked bars).}
\item{lines_width          }{Width of lines for series (default: 1.0); total series will be shown x3.}
\item{hotspots_size        }{Size of the hotspots (default: 1.0).}
\item{width_factor         }{Graph width. The value specified will be a proportion to the normal size (1.0): for a greater font size specify values > 1.0; for smaller font size specify values < 1.0.}
\item{height_factor        }{Graph height. The value specified will be a proportion to the normal size (1.0): for a greater font size specify values > 1.0; for smaller font size specify values < 1.0.}
\item{dpi                  }{Dots per inch; by default, dpi=72. This parameter affects to the width and height of the chart.}
\item{latex_digits         }{Decimal digits for tables exported to latex (requires the 'xtable' R package).}
\item{verbose              }{Value 0/1 to indicate if debug messages are printed on the screen.}
\item{data_filename        }{Path and name for the input data file. Path may be absolute or relative to the current directory.}
\item{graph_filename       }{Path and name for the graphs to be generated (without suffix). Path may be absolute or relative to the current directory.}
\item{graph_fileext_seq    }{List of suffixes for each output format to generate. For example: png,eps. The final graph file names generated will be the specified with \code{graph_filename} joined to each of these suffixes.}
}

The following is an \strong{example of input file} called \file{mygraph.arg}.

% The % characters must be preceded by \
\preformatted{
# -------------------------------------------------------
# File: mygraph.arg
# -------------------------------------------------------
title=QoE - PSNR (AI mode; all tiles; varying BGT)
subtitle=Varying Background Traffic
x_axis_title=Background packets per second
y_axis_title=PSNR (dB)
col_x_values=1
col_y_values=2,4,6,8
series_names=Serie 1, Serie 2, Serie 3, Serie 4, Maximum, Acceptable
x_factor=1.0
y_factor=1.0
total_serie=3, 3
total_value=36.14, 27.0
x_min=automatic
x_max=automatic
y_min=14.5
y_max=36.5
y_log=0
smooth_data=0
show_titles=0
show_grid=1
show_confint=0
confint_as_percentage=0
text_size_title=1.0
text_size_subtitle=1.2
text_size_axis_ticks=1.6
text_size_axis_titles=1.8
text_size_legend=1.6
pos_legend=7
graph_type=2
lines_width=2
hotspots_size=3
width_factor=1.0
height_factor=1.0
dpi=72
latex_digits=3
verbose=1
data_filename=mydata.txt
graph_filename=mygraph
graph_fileext_seq=png,eps
}

In the previous file, a data file called \file{mydata.txt} is specified with the \code{data_filename} parameter. The following is an \strong{example of input data file} (slighly modified for clarity), which includes columns for the 95\% confidence intervals for each statistic:

% The % characters must be preceded by \
% From 1.0-6 the data file should not use header.

%\preformatted{
%% x  Stat1      95\%CI   Stat2      95\%CI   Stat3      95\%CI  Stat4      95\%CI
%1  2.7021E+06 121023  1.9424E+06 76146   1.1396E+06 72200  1.1434E+06 60750
%2  1.9207E+06 189865  1.5882E+06 80444   1.0464E+06 60387  0.9824E+06 80031
%3  2.3384E+06 120934  1.8543E+06 116278  1.1471E+06 76487  1.3576E+06 84200
%}

% Example from 1.0.8 (April, 2022)
\preformatted{
0	36.141	36.142	36.139	36.139
25	29.271	30.083	31.751	31.839
50	22.076	23.907	26.977	27.684
75	15.383	19.44	22.346	23.899
}

As we can see, column #1 is the X-axis values, as set with the \code{col_x_values} parameter. On the other hand, four series are going to be shown, corresponding to the columns specified by \code{col_y_values} parameter: 2,4,6,8. The rest of columns (3,5,7,9) are the confidence intervals computed for each serie, respectively.

As a result of executing the command \code{rplotengine("mygraph.arg")} within the R console, two graphs files will be generated: \file{mygraph.png} and \file{mygraph.eps}. In addition, a LaTeX file \file{mygraph.tex} will be written with the same values as in the input data file but formatted as a table ready for be imported from a LaTeX document. \strong{NOTE:} the package 'xtable' should be installed and loaded before: \code{install.packages('xtable');library('xtable')}.

The same can be achieved from the system console with the command:

\code{Rscript -e "library('xtable');source('rplotengine.R');rplotengine('mygraph.arg')"}.


The following graph \file{mygraph.png} is the result of the previous command with the parameter file \file{mygraph.arg} and data file \file{mydata.txt} shown:

% https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Figures
% Las imagenes se guardan en el subdrectorio man/figures y se referencian
% con \figure relativas a ese directorio
%\if{latex}{\figure{mygraph.png}{options: width="60\%" alt="File: mygraph.png"}}
%\if{latex}{\figure{mygraph.eps}{options: width=8cm}

\if{html}{\figure{mygraph.png}{options: width=100 alt="File: mygraph.png"}}
\if{latex}{\figure{mygraph.png}{options: width=8cm}}

%\Sexpr[stage=render,results=rd,echo=FALSE]{
%library(xtable)
%library(rplotengine)
%rplotengine('mygraph.arg')
%}

}

% Valor retornado por la funcion
\value{ 
   This function return a logical value indicating if the operation ended successfully (TRUE) or not (FALSE).
}

% Ver tambien
%\seealso{
%\code{\link{save}}.
%}

% Palabra clave
%\keyword{file}

% Autor
\author{
   {Pedro-Pablo Garrido Abenza} (\email{pgarrido@umh.es}) \cr
   \href{http://www.umh.es}{Miguel Hernandez University} (Elche, Spain)
}

% Ejemplos de uso
\examples{
## Generate the graphs as defined in the 'mygraph.arg' file.
rplotengine ()
## Generate the graphs as defined in the 'mygraph.arg' file.
rplotengine ("mygraph.arg")
}



