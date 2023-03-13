# =========================================================================
# R package: rplotengine.R
# =========================================================================

rplotengine = function (args_file = "mygraph.arg") {

# -----------------------------------------------------------------------
# Import external packages: xtable
# Search in paths specified in the R_LIBS_USER environment variable
# -----------------------------------------------------------------------
#library(xtable)
#require(xtable)      # Export data to LaTeX
#do.call(library, list("xtable"))

# -------------------------------------------------------------------------
# Load external files with additional functions (use file.path for multiplataform absolute paths)
# -------------------------------------------------------------------------
#source(...)

# ------------------------------------------------------------------------
# Arguments used for this script (arguments for defining a graph)
# ------------------------------------------------------------------------

# List of R variables for storing the arguments values
R_VARS = c( "title", "subtitle", "x_axis_title", "y_axis_title",
    "col_x_values", "col_y_values", "series_names",
    "x_factor", "y_factor", "total_serie", "total_value",
    "x_min", "x_max", "y_min", "y_max", "y_log", "smooth_data",   # "smooth_lines",
    "show_titles", "show_grid", #"show_hotspots",
    "show_confint", "confint_as_percentage",
    "text_size_title", "text_size_subtitle",
    "text_size_axis_ticks", "text_size_axis_titles", "text_size_legend",
    "pos_legend", "graph_type", "lines_width", "hotspots_size",
    "width_factor", "height_factor", "dpi", "latex_digits", "verbose",
    "data_filename", "graph_filename", "graph_fileext_seq" )

NUM_ARGS = length(R_VARS)

# Initialize the argument values for avoiding the following warning messages when
# checking the R package:
#    rplotengine: no visible binding for global variable 'xxx'
title = ""
subtitle = ""
title = ""
subtitle = ""
x_axis_title = ""
y_axis_title = ""
col_x_values = ""
col_y_values = ""
series_names = ""
x_factor = ""
y_factor = ""
total_serie = ""
total_value = ""
x_min = ""
x_max = ""
y_min = ""
y_max = ""
y_log = ""
smooth_data = "0"
#smooth_lines = "0"
show_titles = ""
show_grid = ""
#show_hotspots = ""
show_confint = ""
confint_as_percentage = ""
text_size_title = ""
text_size_subtitle = ""
text_size_axis_ticks = ""
text_size_axis_titles = ""
text_size_legend = ""
pos_legend = ""
graph_type = ""
lines_width = "1"
hotspots_size = "1"
width_factor = ""
height_factor = ""
dpi = "72"
latex_digits = ""
verbose = "1"
data_filename = ""
graph_filename = ""
graph_fileext_seq = ""

# ------------------------------------------------------------------------
# Load the graph parameters from the input file (e.g. mygraph.arg)
# ------------------------------------------------------------------------

# Firstly read in the arguments listed at the command-line
args  = commandArgs()
nargs = length(args)

# Input/Output directory (by default, the current working directory)
inputpath  = "."  # getwd()
outputpath = inputpath

# Check that the specified input file exists
if (!file.exists(args_file)) {
   full_args_file = paste(inputpath, .Platform$file.sep, args_file, sep="")
   if (!file.exists(full_args_file)) {
      # The following is needed when checking the R package (examples)
      full_args_file = system.file(args_file, package="rplotengine")
      #if (length(full_args_file) == 0) {
      if (!file.exists(full_args_file)) {
         # When the package is checked the current directory is .../rplotengine.Rcheck
         #    print(getwd())
         # and it exists in .../rplotengine.Rcheck/rplotengine (copied from /rplotengine/inst).
         if (verbose == "1") {
            writeLines (paste("Graph parameters file '", args_file, "' not found.", sep=""))
         }
         # End
         quit(status=1)  # return (FALSE)
      } else {
          # When checking the example of the package the output directory should not be in the
          # package directory "xxx.Rcheck", it should be the tempdir directory.
          outputpath = tempdir()
      }
   }
} else {
   full_args_file = args_file
}
writeLines ("Graph parameters file ...")
writeLines (paste("   File: ", full_args_file, sep=""))

# Load the file (one line for each parameter)
args_lines = readLines (full_args_file)
args_rows = length (args_lines)

# ----------------------------------------------------------
# Process parameters loaded
# ----------------------------------------------------------

var = 1
for (l in 1:args_rows) {
   args_lines[l] = trim(args_lines[l])
   # Ignore blank empty lines and comments (from '#')
   if (args_lines[l] == "") next
   if (substr (args_lines[l],1,1) == '#') next
   arg = unlist(strsplit(args_lines[l],"\\="))
   if (length(arg) == 2) {
      # This is the most common case 2 (nombre=valor)
      arg_name  = arg[1]
      arg_value = arg[2]
   } else if (length(arg) == 1) {
      # In case of don't specify value (e.g. subtitle, ...)
      arg_name  = arg[1]
      arg_value = ""
   } else {
      # Error
      writeLines ( paste("Syntax error in input file, line: ", l, sep="") )
      writeLines ( args_lines[l] )
      quit(status=1)  # return (FALSE)
   }
   # Get the index for this parameter
   if (arg_name == R_VARS[var]) {
      # When the parameters come in the correct order within the file, that is,
      # in the same order as defined above in R_VARS: fast
      i = var
      if (var < NUM_ARGS) var = var+1
   } else {
      # Otherwise, we need to search in the list in order to get the index: slower
      i = NUM_ARGS+1
      for (a in 1:NUM_ARGS) {
         if (arg_name == R_VARS[a]) {
            i=a
            break
         }
      }
   }
   if (i > NUM_ARGS) {
      # Error: the argument is unknown
      writeLines ( paste("Unknown argument in input file, line: ", l, sep="") )
      writeLines ( args_lines[l] )
      quit(status=1)  # return (FALSE)
   }
   val_name = R_VARS[i]
   line = paste (val_name, "='", arg_value, "'", sep="")  
   eval (parse(text=line))
}

# ------------------------------------------------------------------------
# Load the data file
#
# Format:
#    - Without row header
#    - Columns separated by tabulator
#    - Floating values with dot point '.'
#    - Comments allowed with '#' character.
#
# Example:
#    0	0.104	0.0	    13.825   # Comment 0
#    1	4.158	16.84	14.865   # Comment 1
#    2	6.757	22.453	25.884   # Comment 2
#    3	7.484	27.755	18.607   # Comment 3
# ------------------------------------------------------------------------

# Check that the specified input data file exists
if (!file.exists(data_filename)) {
  full_data_filename = paste(inputpath, .Platform$file.sep, data_filename, sep="")
  if (!file.exists(data_filename)) {
    # The following is needed when checking the R package (examples)
    full_data_filename = system.file(data_filename, package="rplotengine")
    #if (length(data_filename) == 0) {
    if (!file.exists(full_data_filename)) {
      # When the package is checked the current directory is .../rplotengine.Rcheck
      #    print(getwd())
      # y it exists in .../rplotengine.Rcheck/rplotengine (copied from /rplotengine/inst).
      if (verbose == "1") {
         writeLines (paste("Data file '", data_filename, "' not found.", sep="")) # stop
      }
      quit(status=1)  # return (FALSE)
    }
  }
} else {
  full_data_filename = data_filename
}

# Load the data file (the '#' symbol is used as comment with read.table)
if (verbose == "1") {
   writeLines ("Loading data file ...")
   writeLines (paste("   File: ", full_data_filename, sep=""))
}
data = read.table (full_data_filename, sep="\t", header=FALSE, fill=TRUE, comment.char="#")
data_cols = NCOL(data)
data_rows = NROW(data)

# Debug: show the data
if (verbose == "1") {
   writeLines (paste("   cols: ", NCOL(data), ifelse(data_cols==0, " --> ERROR", ""), sep=""))
   writeLines (paste("   rows: ", NROW(data), ifelse(data_rows==0, " --> ERROR", ""), sep=""))
   print (data)
   writeLines (paste("   CI: ", show_confint, sep=""))
}

if ((data_cols==0) | (data_rows==0)) {
    # End: file empty
    quit(status=1)  # return (FALSE)
}

# -------------------------------------------------------------------------
# Convert some arguments to numeric values or list of numeric values
# -------------------------------------------------------------------------

# Columnas correspondientes a los valores del eje X y series del eje Y
#    Older versions (v1.0.6 or earlier): by specifying a list of columns with a file
#col_x_values  = as.numeric (col_x_values)
#col_y_values  = as.numeric (unlist (strsplit (col_y_values,",")))
#
#    From 1.0.7 and older: ignoring that arguments; it is considered the while file
#       - X-axis values: first column (1)
#       - Y-axis values: if Confidence Interval (CI) is used, every 2 columns (2, 4, 6, 8, ...);
#                        otherwise all of them (2, 3, 4, 5, ...)

col_x_values  = 1
if ((show_confint == "0") || (NCOL(data)<=2)) {
    # 2, 3, 4, 5, ...
  #col_y_values = array(seq(1, (data_cols-1), by=1), dim=cols-1)
  col_y_values = seq(2, data_cols, by=1)
} else {
    # 2, 4, 6, 8, ...
  #col_y_values = array(seq(1, (data_cols-1)*2, by=2), dim=cols-1)
  col_y_values = seq(2, (data_cols-1), by=2)
}

series_names_lst = unlist(strsplit(series_names,","))
x_factor    = as.double(x_factor)
y_factor    = as.double(y_factor)

# Process total serie(s)
#total_serie = as.numeric(total_serie)
#total_value = as.double(total_value)
total_serie_lst = unlist(strsplit(total_serie,","))
total_value_lst = unlist(strsplit(total_value,","))
total_serie_len = NROW(total_serie_lst)
total_value_len = NROW(total_value_lst)
if ((total_serie_len==1) && (total_serie_lst=="0")) {
   # If only '0' is specified then there is none total serie
   total_serie_len = 0
} else {
   # Otherwise, there are 1 or more total series
   if (total_serie_len != total_value_len) {
      # These values should match; otherwise, the minimum is considered
      total_serie_len = min(total_serie_len, total_value_len)
      total_value_len = total_serie_len
   }
   total_serie = array(0, c(total_serie_len))
   total_value = array(0, c(total_value_len))
   for (t in 1:total_serie_len) {
      total_serie[t] = as.numeric(total_serie_lst[t])
      total_value[t] = as.double (total_value_lst[t])
   }
}

#y_log         = ifelse((y_log=="0"), FALSE, TRUE)
#show_titles   = ifelse((show_titles=="0"), FALSE, TRUE)
#show_grid     = ifelse((show_grid=="0"), FALSE, TRUE)
#show_hotspots = ifelse((show_hotspots=="0"), FALSE, TRUE)
#show_confint  = ifelse((show_confint=="0"), FALSE, TRUE)
#confint_as_percentage = ifelse((confint_as_percentage=="0"), FALSE, TRUE)

text_size_title       = as.double (text_size_title)
text_size_subtitle    = as.double (text_size_subtitle)
text_size_axis_ticks  = as.double (text_size_axis_ticks)
text_size_axis_titles = as.double (text_size_axis_titles)
text_size_legend      = as.double (text_size_legend)
pos_legend    = as.numeric (pos_legend)
graph_type    = as.numeric (graph_type)
lines_width   = as.double  (lines_width)
hotspots_size = as.double  (hotspots_size)
width_factor  = as.numeric (width_factor)
height_factor = as.numeric (height_factor)
dpi           = as.numeric (dpi)
latex_digits  = as.numeric (latex_digits)
graph_fileext_seq = unlist (strsplit(graph_fileext_seq,","))

# ------------------------------------------------------------------------
# Process the loaded data and compute the real data used for the graph
# ------------------------------------------------------------------------

if (verbose == "1") {
   writeLines ("Processing data ...")
}

# Number of values in X-axis and Y-axis
col_x_values_len = NROW(data)
col_y_values_len = NROW(col_y_values)

# Number of lines to draw
series_names_len = NROW(series_names_lst)

# Create data array to be used for the graph and LaTeX file
num_series        = col_y_values_len
num_series_legend = col_y_values_len  # The legend dos not include averages

# Total series?
if (total_serie_len > 0) {    # 1-TOTAL o 2-AVERAGE o 3-CONSTANT o 4-X_PROPORTION
   num_series        = num_series        + total_serie_len;
   num_series_legend = num_series_legend + total_serie_len;
}
# Create data arrays with the computed length depending on the data loaded,
# the number of total series and if CI is used;
# 'num_series' is necessary for drawing CI -> plot(x[,1]... y lines(x[,1]...
x = array (0, c(col_x_values_len,num_series))
y = array (0, c(col_x_values_len,num_series))

# The label of each serie is specified in a parameter
if (series_names_len >= num_series_legend) {
   # In case of specify more labels than data loaded, only the necessary will be used.
   series_names_lst = trim(series_names_lst[1:num_series_legend])
} else {
   # In case of specify less labels, the header will be considered.
   # For total series, a generic name will be assigned (total, average, etc.).
   series_names_aux = array (0, c(num_series_legend))
   for (serie in 1:col_y_values_len) {
      if (serie <= series_names_len) {
         series_names_aux[serie] = trim(series_names_lst[serie])
      } else {
         col = col_y_values[serie]
         series_names_aux[serie] = trim(names(data)[col])
      }
   }
   # Total series
   if (total_serie_len > 0) {
     # serie = col_y_values_len
     for (t in 1:total_serie_len) {
         serie = col_y_values_len + t;
         if (serie <= series_names_len) {
           series_names_aux[serie] = trim(series_names_lst[num_series_legend])
         } else if (total_serie[t] == 1) {  # 1-TOTAL
           series_names_aux[serie] = "Total"
         } else if (total_serie[t] == 2) {  # 2-AVERAGE
           series_names_aux[serie] = "Average"
         } else if (total_serie[t] == 3) {  # 3-CONSTANT
           series_names_aux[serie] = "Constant value"
         } else if (total_serie[t] == 4) {  # 4-X_PROPORTION
           series_names_aux[serie] = "Proportional to x"
         }
      }
   }
   series_names_lst = trim(series_names_aux)
}

# Debug: print series names
#if (verbose == "1") {
#   for (serie in 1:num_series) {
#      writeLines (paste("   Serie #", serie, ": ", series_names_lst[serie], " (col=", col_y_values[serie], ")", sep=""))
#   }
#}

# For each value of the X-axis (row in the data file)
for (x_idx in 1:col_x_values_len) {
   # Get the value for the X-axis, which will be the same for all the series (applying a factor)
   x_value = data[x_idx,col_x_values] * x_factor 	# row.names(data)[x_idx] * x_factor
   # Get the value for the Y-axis para all the series (applying a factor)
   for (serie in 1:col_y_values_len) {
       col = col_y_values[serie]
       y_value = data[x_idx,col] * y_factor
       # For security check the range of columns
       if (col>NCOL(data)) {
           # Error: out of range
          writeLines (paste("   Column ", col, " for serie #", serie, " is out of range [0..", NCOL(data), "]", sep=""));
          quit(status=1)  # return (FALSE)
       }
       x[x_idx,serie] = x_value		# Needed only for computing CI
       y[x_idx,serie] = y_value
   }
   # Total series
   if (total_serie_len > 0) {
       # serie = col_y_values_len  #num_series
       x[x_idx,serie] = x_value
       for (t in 1:total_serie_len) {
           serie = col_y_values_len + t;
           if (total_serie[t] == 1) {         # 1-TOTAL
              y[x_idx,serie] = sum(y[x_idx,], na.rm=TRUE)
           } else if (total_serie[t] == 2) {  # 2-AVERAGE
              y[x_idx,serie] = sum(y[x_idx,], na.rm=TRUE) / col_y_values_len  #series_names_len??
           } else if (total_serie[t] == 3) {  # 3-CONSTANT
              y[x_idx,serie] = total_value[t] * y_factor
           } else if (total_serie[t] == 4) {  # 4-X_PROPORTION
              y[x_idx,serie] = total_value[t] * data[x_idx,col_x_values] * y_factor   # total_value * x_value
           }
       }
   }
}

#if (verbose == "1") {
#    writeLines (paste("X:", sep=""))
#    print(x);
#    writeLines (paste("Y:", sep=""))
#    print(y);
#}

# ------------------------------------------------------------------------
# Compute the min/max values for both axis in case of using "automatic" as value
# ------------------------------------------------------------------------
x_min = ifelse((x_min=="automatic"), min(x,na.rm=TRUE),                as.double(x_min))
x_max = ifelse((x_max=="automatic"), max(x,na.rm=TRUE),                as.double(x_max))
y_min = ifelse((y_min=="automatic"), min(y[,1:num_series],na.rm=TRUE), as.double(y_min))
y_max = ifelse((y_max=="automatic"), max(y[,1:num_series],na.rm=TRUE), as.double(y_max))

# ------------------------------------------------------------------------
# Check the y_min value for the Y-axis: particular case.
# If the logarithm scale is used for Y-axis, the minimum value cannot be 0.0 (error).
# ------------------------------------------------------------------------
if (y_log == "0") {
    #y_min = 0
   y_log_str = ""
} else {
   if (y_min == 0.0) {
      y_min = 0.01
      writeLines ("   WARNING: parameter y_min cannot be equals to 0.0 with logarithmic Y-axis; forced to 0.01 !")
      flush.console()
   }
   y_log_str = "y"
}
# Smooth parameter for data (0: none, 1: maximum)
smooth_data = as.double(smooth_data)
if ((smooth_data < 0.0) || (smooth_data > 1.0)) smooth_data = 0.0

# ------------------------------------------------------------------------
# Export data to LaTeX (archivo .tex): same filename as the graph with the .tex suffix.
# The number of decimals digits used is specified in the "latex_digits" parameter.
# This is done once the graphs have been generated, just is case the required "xtable"
# R package is not installed.
# ------------------------------------------------------------------------
col_names = c(format(round(x[,1], latex_digits), nsmall=latex_digits))
export_data_to_latex (title, outputpath, graph_filename, series_names_lst,
	col_names, y, col_x_values_len, latex_digits, verbose)


# -------------------------------------------------------------------------
# Generate the graphs
#
# There are some type of graphs supported:
#    0-lines
#    1-points
#    2-lines & points
#    3-lines & points superpuestos
#    4-histogram (vertical lines)
#    5-Stacked bars (experimental)
#
#  Each graph will be generated with several formats specified in the
#  'graph_fileext_seq' parameter (e.g., graph_fileext_seq="png,eps")
# -------------------------------------------------------------------------

if (verbose == "1") {
   writeLines ("Generating graphs ...")
}

if ((graph_type >= 0) && (graph_type <= 4)) {
   plot_lines (title, subtitle, x_axis_title, y_axis_title,
               col_x_values, col_y_values, col_y_values_len, series_names_lst,
               x_factor, y_factor, total_serie_len, total_serie,
               x_min, x_max, y_min, y_max, y_log, smooth_data,
               show_titles, show_grid, #show_hotspots,
               show_confint, confint_as_percentage, 
               text_size_title, text_size_subtitle, text_size_axis_ticks,
               text_size_axis_titles, text_size_legend,
               pos_legend, graph_type, lines_width, hotspots_size,
               width_factor, height_factor, dpi,
               outputpath, graph_filename, graph_fileext_seq,
               x, y, data, num_series, num_series_legend, verbose)
} else if (graph_type == 5) {
   # 5-Stacked bars (experimental)
   plot_bars  (title, subtitle, x_axis_title, y_axis_title,
               col_x_values, col_y_values, series_names_lst,
               x_min, x_max, y_log, show_titles, show_grid,
               text_size_legend, pos_legend, width_factor, height_factor, dpi,
               outputpath, graph_filename, graph_fileext_seq, x, y, data, verbose)
}

if (verbose == "1") {
   writeLines ("Ok\n")
}
flush.console()

quit(status=0)  # return (TRUE)

}

# =========================================================================

# =========================================================================
# plot_lines: graphs with one line (or points) for each serie.
# =========================================================================

plot_lines = function ( title, subtitle, x_axis_title, y_axis_title,
      col_x_values, col_y_values, col_y_values_len, series_names_lst,
      x_factor, y_factor, total_serie_len, total_serie,
      x_min, x_max, y_min, y_max, y_log, smooth_data,
      show_titles, show_grid, #show_hotspots,
	  show_confint, confint_as_percentage, 
      text_size_title, text_size_subtitle, text_size_axis_ticks,
      text_size_axis_titles, text_size_legend,
      pos_legend, graph_type=0, lines_width=1.0, hotspots_size=1.0,
      width_factor=1.0, height_factor=1.0, dpi=72,
      outputpath, graph_filename, graph_fileext_seq,
      x, y, data, num_series, num_series_legend, verbose ) {

# Number of values in X-axis and Y-axis
col_x_values_len = NROW(data)
col_y_values_len = NROW(col_y_values)
series_names_len = NROW(series_names_lst)

# List of extensions (one or more)
graph_fileext_len = NROW(graph_fileext_seq)
if (graph_fileext_len < 1) {
   stop ("ERROR: graph parameter 'graph_fileext_seq' is empty.")
}

# ------------------------------------------------------------------------
# Minimum/Maximum values for the Y-axis depending on if logarithm scale
# is used for Y-axis or not.
# ------------------------------------------------------------------------
if (y_log == "0") {
   y_log_str = ""
} else {
   y_log_str = "y"
}

# Smooth data: require(graphics)
if (smooth_data > 0.0) {
    #   #heft <- vector()
    #   #dim(heft) <- c(1,col_x_values_len)
    #   #heft <- c(0.01,0.01,0.01,1,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,...)
    #   heft <- matrix(0.01, 1, col_x_values_len)

    #smooth = array(0, c(col_x_values_len,col_y_values_len))
    for (serie in 1:col_y_values_len) {
        smooth <- smooth.spline(x[,1], y[,serie], spar=smooth_data)  # w=0.45  # w=heft
        for (x_idx in 1:col_x_values_len) {
            y[x_idx,serie] = smooth$y[x_idx];
        }
    }
}
   
# Generate graphs for the formats specified (png, eps, etc.)
for (ext_idx in 1:graph_fileext_len) {
   # ------------------------------------------------------------------------
   # Open the graph: png(...), postscript(...), X11(), ...
   # An error could be got if we are using the text mode R console in order to generate png().
   # ------------------------------------------------------------------------
   # Filename for the specific extension (.png, .eps, etc.)
   ext = graph_fileext_seq[ext_idx]
   graph_filenameext = paste (outputpath, .Platform$file.sep, graph_filename, ".", ext, sep="")
   if ( open_graph (graph_filenameext, ext, width_factor, height_factor, dpi) == FALSE ) {
      writeLines (paste("   WARNING: suffix '", ext, "' unsupported.", sep=""))
      next
   }
   if (verbose == "1") {
      writeLines (paste("   File: ", graph_filenameext, sep=""))
   }

   # ------------------------------------------------------------------------
   # Compute graph margins taking into account if title/subtitle is shown ('show_titles')
   #
   # mar = A numerical vector of the form c(bottom, left, top, right) which
   #       gives the number of lines of margin to be specified on the four
   #       sides of the plot.
   #       The default is c(5, 4, 4, 2) + 0.1. 
   # mgp = The margin line (in mex units) for the axis title, axis labels and axis line.
   #       The default is c(3, 1, 0). 
   # las = Orientacion texto valores de los ejes:
   #       [0]-Paralelo al eje, 1-Horizontal, 2-Perpendicular, 3-Vertical
   #
   # cex.sub  = Tamano subtitulos     (respecto 'cex')
   # cex.axis = Tamano texto ejes     (respecto 'cex')
   # cex.lab  = Tamano etiquetas ejes (respecto 'cex')
   # yaxp     = Posicion de los valores en el eje y (ver 'at' en 'barplot').
   # ------------------------------------------------------------------------
   if (show_titles == "0") {
      title    = ""
      subtitle = ""
      if (text_size_axis_titles <= 1.2) {
        par(mar=c(3.2, 2.9, 0.1, 0.1) + 0.1)
      } else {
        par(mar=c(3.2, 3.3, 0.1, 0.1) + 0.1)
      }
      #par(mar=c(3.2, 2.9, 0.1, 0.1) + 0.1)
      par(mgp=c(2.0,0.7,0))
      #legend_cex = 0.8
   } else {
      #legend_cex = 0.6
   }

   # --------------------------------------------------------------
   # Draw an empty graph with any data (titles, size, ...): type='n'.
   # --------------------------------------------------------------
   # Font sizes: cex, cex.main, cex.sub, cex.axis, cex.lab (relative to cex)
   plot (x[,1], y[,1], main=title, sub=subtitle, type='n',
      xlab=x_axis_title, ylab=y_axis_title, cex=1.0, cex.main=text_size_title,
      cex.sub=text_size_subtitle, cex.axis=text_size_axis_ticks,
      cex.lab=text_size_axis_titles,
	  xlim=c(x_min,x_max), ylim=c(y_min,y_max), lty=1, log=y_log_str)

   # Smooth data: require(graphics)
   #smooth_lines = "0"
   #if ((smooth_lines == "1") && (smooth_data > 0.0)) {
   #  lines(smooth, col="red")
   #}

   # --------------------------------------------------------------
   # Draw 'ticks' in X-axis and Y-axis
   # - side: 1=below, 2=left, 3=above and 4=right
   # --------------------------------------------------------------
   #axis (side=3, at=seq(x_min, x_max, by=5), tck=0.01, labels=FALSE)
   #axis (side=4, at=seq(y_min, y_max, by=5), tck=0.01, labels=FALSE)

   # --------------------------------------------------------------
   # Draw the grid at the begin ('show_grid')
   # --------------------------------------------------------------
   if (show_grid == "1") {
      # - If nx,ny=NULL -> default value (ticks in the axis), if NA -> without line in that axis
      # - col = line colors
      # - lty = type of lines
      # - lwd = width of lines
      # - equilogs = equidistants lines (TRUE when nx,ny=NULL)
      grid(nx=NULL, ny=NULL, col="lightgray", lty="dotted", lwd=par("lwd"), equilogs=TRUE)
   }

   # --------------------------------------------------------------
   # Draw Confidence Interval (CI)
   # --------------------------------------------------------------
   if (show_confint == "1") {
      # Arrays for CI
      confint_top    = array(0, c(col_x_values_len,col_y_values_len))
      confint_bottom = array(0, c(col_x_values_len,col_y_values_len))
      # Compute top and bottom margins for CI of each serie
      for (x_idx in 1:col_x_values_len) {          
          for (serie in 1:col_y_values_len) {
              col = col_y_values[serie]
              # Compute the range of the interval.
              # The CI is in col+1, and can be expressed:
              # - In absolute value (confint_as_percentage='0')
              # - In % or absolute value (confint_as_percentage='1')
              if (confint_as_percentage == "1") {
                 # The CI is expressed in %: compute the margin
                 margin = (data[x_idx,col] * data[x_idx,col+1]) / 100.0
              } else {
                 # The CI is expressed in absolute value: as is
                 margin = data[x_idx,col+1] / 100.0   # <--- ???
              }
              confint_bottom[x_idx,serie] = (data[x_idx,col] - margin) * y_factor   # y[x_idx,serie]
              confint_top   [x_idx,serie] = (data[x_idx,col] + margin) * y_factor
          }
      }
      
      # Debug: show CI values
      #writeLines ("   Confidence Interval")
      #writeLines (paste("   Shown in %? = ", confint_as_percentage, sep=""))
      #writeLines (paste("   [", confint_top, " .. ", confint_bottom, "]", sep=""))

      # Draw CI
      for (serie in 1:col_y_values_len) {
          # Draw vertical lines
          segments (x[,serie], confint_top[,serie],
                    x[,serie], confint_bottom[,serie],
                    lwd=1.5, cex=0.5, col="grey65", lty=1);
          # Compute final dimensions of the graph: vector with x-min, x-max, y-min, y-max
          plotDim = par("usr")
          plot_x_min = plotDim[1];
          plot_x_max = plotDim[2];
          plot_width = plot_x_max-plot_x_min
          # Draw top/bottom segments (with = 1% of the graph)
          width_whiskers = plot_width/100
          segments (x[,serie]-width_whiskers, confint_top[,serie],
                    x[,serie]+width_whiskers, confint_top[,serie],
                    lwd=1.5, cex=0.5, col="grey65", lty=1);
          segments (x[,serie]-width_whiskers, confint_bottom[,serie],
                    x[,serie]+width_whiskers, confint_bottom[,serie],
                    lwd=1.5, cex=0.5, col="grey65", lty=1);
      }
   }

   # --------------------------------------------------------------
   # Draw the series
   # --------------------------------------------------------------
   # Line types and colors to use; the default colors in R are defined in colors()
   #linetypes  = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
   colores = c("blue", "darkgreen", "darkviolet", "darkorange1", "chocolate4",
               "cornsilk4", "darkolivegreen", "darkgoldenrod1",
               "darkred", "deepskyblue4", "greenyellow", "indianred4", "khaki4",
               "midnightblue", "navyblue", "aquamarine4", "dodgerblue4")
   coloresTotal = c("red", "gold4", "dodgerblue4", "aquamarine4", "navyblue", "midnightblue",
               "khaki4", "indianred4", "greenyellow", "deepskyblue4", "darkred",
               "darkgoldenrod1", "darkolivegreen", "cornsilk4", "chocolate4",
               "darkorange1", "darkviolet", "darkgreen")

   # Type of graph:
   #    0-lines (l), 1-points (p), 2-lines & points splitted (b),
   #    3-lines & points overlapped (o), or 4-histogram (h)
   tipo = "l"
   if (graph_type == 0) {          # Lines
      tipo = "l"     
   } else if (graph_type == 1) {   # Points
      tipo = "p"
   } else if (graph_type == 2) {   # Lines & points splitted
      tipo = "b"
   } else if (graph_type == 3) {   # Lines & points overlapped
      tipo = "o"
   } else if (graph_type == 4) {   # Histogram
      tipo = "h"              
   } else {
      tipo = "o"                   # Lines & points (superpuestos) (default)
   }
   
   # Type and width of lines, type and size of points, and color
   # - lty : line type
   # - lwd : line width
   # - pch : point type
   # - cex : the size of pch symbols
   # - col : color
   lty = array (""         , c(num_series_legend))  # line type
   lwd = array (1.0        , c(num_series_legend))  # line width
   pch = array (NA_integer_, c(num_series_legend))  # point type
   cex = array (1.0        , c(num_series_legend))  # Size of pch symbols
   col = array (""         , c(num_series_legend))  # Color
   
   # Data series
   for (serie in col_y_values_len:1) {
       lty[serie] = "solid"  # linetypes[serie+2] o serie (error in lty=lty[serie], but with lty=serie is ok)
       #if ( show_hotspots=="1" ) {   # Deprecated
       if ((graph_type >= 1) && (graph_type <= 3)) {
          pch[serie] = serie
       }
       lwd[serie] = lines_width
       cex[serie] = hotspots_size
       col[serie] = colores[serie]
       lines(x[,1], y[,serie], type=tipo, lty=lty[serie], lwd=lwd[serie],
             pch=pch[serie], cex=cex[serie], col=col[serie])
       #if ( show_hotspots=="1" ) {  # Deprecated
       #   points(x[,1], y[,serie], pch=serie)  # Si quisieramos mostrar solo puntos
       #}
   }
   # Total series with different look: color, dotted, without points (only line)
   if (total_serie_len > 0) {
       # serie = num_series_legend
       for (t in 1:total_serie_len) {
          serie = col_y_values_len + t;
          lty[serie] = "dotted"
          lwd[serie] = 3 * lines_width
          cex[serie] = hotspots_size
          pch[serie] = NA_integer_   # or "" (without points)
          col[serie] = coloresTotal[t]
          lines(x[,1],  y[,serie], type="l", lty=lty[serie], lwd=lwd[serie],
                pch=pch[serie], cex=cex[serie], col=col[serie])
       }
   }

   # --------------------------------------------------------------
   # Draw the legend
   # --------------------------------------------------------------
   if (pos_legend > 0) {
      # Legend position in R from the specified value 1..9
      xLegend = pos_legend_index2string (pos_legend)

      # Legend parameters:
      # - x,y = top-left coordinates
      # - legend = text for each serie
      # - cex = font size
      # - lty = type of line
      # - lwd = width of line
      # - pch = symbol of points
      # - box.lty = borderline of the legend (0:none)
      # - bty  = background with color ("o") or transparent ("n")
      # - fill = background color (NULL none, by default)
      # - xjust = alignment (0:left, by default, 0.5:centered, 1:right)
      # - title = title of legend (NULL none, by default)
      # - inset = inset distance(s) from the margins as a fraction of the
      #           plot region when legend is placed by keyword
      legend (x=xLegend, legend=series_names_lst, cex=text_size_legend,
              lty=lty, lwd=lwd, pch=pch, col=col,
              box.lty=0, xjust=0, inset=c(0.01,0.01)) # y=yLegend,
   }
   # Close the graph
   #graphics.off()  # Error in grDevices::dev.off(): cannot shut down device 1 (the null device)
   dev.off()
   
  # if ((ext == "ps") || (ext == "eps")) {
  #    # R does not embed the font(s) used in the PostScript output (requires Ghostscript)
  #    graph_filenameext_fonts = paste (outputpath, .Platform$file.sep, graph_filename, "_withfonts.", ext, sep="")
  #    embed_fonts(graph_filenameext_fonts, output=graph_filenameext_fonts)
  # }
} # Extension

}

# =========================================================================

# =========================================================================
# plot_bars: graphs for stacket bars (%) - Experimental!
# =========================================================================

plot_bars = function ( title, subtitle, x_axis_title, y_axis_title,
    col_x_values, col_y_values, series_names_lst, x_min, x_max, y_log,
    show_titles, show_grid, text_size_legend, pos_legend,
    width_factor=1.0, height_factor=1.0, dpi=72,
    outputpath, graph_filename, graph_fileext_seq, x, y, data, verbose ) {

# ------------------------------------------------------------------------
# Minimum/Maximum values for the Y-axis depending on if logarithm scale
# is used for Y-axis or not.
# ------------------------------------------------------------------------
if (y_log == "0") {
   y_min = 0
   y_log_str = ""
} else {
   y_log_str = "y"
}
y_max = 100.1   # Should be > 100.0; With = 100.0, the 100 value does not appears in the Y-axis

# Number of values in X-axis and Y-axis
col_x_values_len = NROW(data)
col_y_values_len = NROW(col_y_values)
series_names_len = NROW(series_names_lst)

# List of extensions (one or more)
graph_fileext_len = NROW(graph_fileext_seq)
if (graph_fileext_len < 1) {
   stop ("ERROR: graph parameter 'graph_fileext_seq' is empty.")
}

# Generate graphs for the formats specified (png, eps, etc.)
for (ext_idx in 1:graph_fileext_len) {
    # ------------------------------------------------------------------------
    # Open the graph: png(...), postscript(...), X11(), ...
    # An error could be got if we are using the text mode R console in order to generate png().
    # ------------------------------------------------------------------------
    # Filename for the specific extension (.png, .eps, etc.)
    ext = graph_fileext_seq[ext_idx]
    graph_filenameext = paste (outputpath, .Platform$file.sep, graph_filename, ".", ext, sep="")
    if ( open_graph (graph_filenameext, ext, width_factor, height_factor, dpi) == FALSE ) {
        writeLines (paste("   WARNING: suffix '", ext, "' unsupported.", sep=""))
        next
    }
    if (verbose == "1") {
       writeLines (paste("   File: '", graph_filenameext, "' ...", sep=""))
    }

    # ------------------------------------------------------------------------
    # Compute graph margins taking into account if title/subtitle is shown ('show_titles')
    #
    # mar = A numerical vector of the form c(bottom, left, top, right) which
    #       gives the number of lines of margin to be specified on the four
    #       sides of the plot.
    #       The default is c(5, 4, 4, 2) + 0.1.
    # mgp = The margin line (in mex units) for the axis title, axis labels and axis line.
    #       The default is c(3, 1, 0).
    # las = Orientacion texto valores de los ejes:
    #       [0]-Paralelo al eje, 1-Horizontal, 2-Perpendicular, 3-Vertical
    #
    # cex.sub  = Tamano subtitulos     (respecto 'cex')
    # cex.axis = Tamano texto ejes     (respecto 'cex')
    # cex.lab  = Tamano etiquetas ejes (respecto 'cex')
    # yaxp     = Posicion de los valores en el eje y (ver 'at' en 'barplot').
    # ------------------------------------------------------------------------
    if (show_titles == "0") {
        title    = ""
        subtitle = ""
        #if (text_size_axis_titles <= 1.2) {
        #    par(mar=c(3.2, 2.9, 0.1, 0.1) + 0.1)
        #} else {
        #    par(mar=c(3.2, 3.3, 0.1, 0.1) + 0.1)
        #}
        par(mar=c(3.2, 2.9, 0.1, 0.1) + 0.1)
        par(mgp=c(2.0,0.7,0))
        #legend_cex = 0.8
    } else {
        #legend_cex = 0.6
    }
    par(cex.sub=0.7, cex.axis=1.0, cex.lab=1.0, las=0)  #, yaxt="n")

   # --------------------------------------------------------------
   # Number of decimal points
   # --------------------------------------------------------------
   # Change the default number of decimal points (options() or getOption('digits')).
   #  digits: Controls the number of digits to print when printing numeric values.
   #          It is a suggestion only. Valid values are 1...22 with default 7.
   #  scipen: A penalty to be applied when deciding to print numeric values in
   #          fixed or exponential notation. Positive values bias towards fixed
   #          and negative towards scientific notation: fixed notation will be
   #          preferred unless it is more than scipen digits wider. 
   options('digits'=3)
   options('scipen'=0)  # For >= 1 decimal points will be shown

   # --------------------------------------------------------------
   # Draw the series
   # --------------------------------------------------------------

   # Reverse order of data series as in the stacked bars it begin from bottom to top.
   bandwidth_share = array(0, c(series_names_len,col_x_values_len))
   for (x_idx in 1:col_x_values_len) {
      #bandwidth_share[,x_idx] = rev(y[x_idx,])

      # Cumulative total (suming all the series) -> for LaTeX table
      total = sum (y[x_idx,1:series_names_len], na.rm=TRUE)
      for (serie in 1:series_names_len) {
         bandwidth_share[series_names_len-serie+1,x_idx] = (100.0 * y[x_idx,serie]) / total
      } # serie
   }

   # Color and label of series in columns
   seriesbarcolors = grey(0.5 + 1:series_names_len/12)    # c("grey90", "grey70"))
   namesbar = x[,1]  # c(1:col_x_values_len)  # "1", "2", "3", ...
      
   # Draw bars (axes=TRUE to show Y-axis)
   midpts = barplot (bandwidth_share, main=title, sub=subtitle,
         xlab=x_axis_title, ylab=y_axis_title, ylim=c(y_min,y_max), axes=TRUE,
         col=seriesbarcolors, log=y_log_str, names.arg=namesbar)
         #yaxt="n")    # Sin eje Y (se pondra despues manualmente)
         #legend=seriesnames[series_names_len:1])    # O bien esta leyenda, o separada (ver abajo)   

   # X-Axis (avoiding floating-point values)
   # Habria que poner yaxt="n" en el barplot, y descomentar esto.
   # side: 1=below, 2=left, 3=above and 4=right
   # las = Orientacion texto valores de los ejes:
   #       [0]-Paralelo al eje, 1-Horizontal, 2-Perpendicular, 3-Vertical
   #axis (side=2, yaxp=c(1, 10, 20, 50, 75, 100))
   #axis (side=2, at=seq(0,25,50,75,100), labels=seq(0,25,50,75,100))
   #axis (side=2, at=seq(0,25,50,75,100), labels=TRUE, las=1)
   #axis (side=2, at=seq(0,100, by = 25), labels=TRUE, las=1)
   #if ((y_log=="y") || (y_log=="xy")) {
   #   # Si el eje es logaritmico, y_min>0. Para que aparezca el 0.
   #   text (x=0, y=100, labels="0", pos=4)
   #}

   # If labels are too long, uncomment the following instead of names.arg in barplot, and
   #    names=rep("", series_names_len)
   # Replace blank space with new line ('\n').
   #mtext (sub(" ", "\n", colnames(bandwidth_share)), at=midpts, side=1, line=0.5, cex=0.5)

   # Compute middle point and draw a numeric value
   midpts_x = rep (midpts, each=series_names_len)
   midpts_y = apply (bandwidth_share, 2, cumsum) - bandwidth_share/2  # Apply "cumsum" (cumulative sum) to columns
   seriestextcolors = rep (c("white", "black"), times=2:2, cex=0.8)  
   bandwidth_share_rounded = round (bandwidth_share, digits=1)
   text (midpts_x, midpts_y, bandwidth_share_rounded, col=seriestextcolors, cex=0.8)

   # --------------------------------------------------------------
   # Draw the grid ('show_grid'); in this case, after the barplot
   # --------------------------------------------------------------
   if (show_grid == "1") {
      grid (nx=NULL, ny=NULL, col="lightgray", lty="dotted", lwd=par("lwd"), equilogs=TRUE)
   }

   # --------------------------------------------------------------
   # Draw the legend
   # --------------------------------------------------------------
   if (pos_legend > 0) {
       # Legend position in R from the specified value 1..9
      xLegend = pos_legend_index2string (pos_legend)
      legend (x=xLegend, legend=series_names_lst, cex=text_size_legend,
              box.lty=0, bty="o", fill = rev(seriesbarcolors), bg="white",
              xjust=0, inset=c(0.01,0.00))
   }
   # Close the graph
   #graphics.off()  # Error in grDevices::dev.off(): cannot shut down device 1 (the null device)
   dev.off()
} # Extension

}

return

# =========================================================================


# =========================================================================
# Trim blank spaces on the left abd right of the string
# =========================================================================
trim<-function (str) {
   str <- gsub ("(^ +)|( +$)","",str)
   return(str)
}


# =========================================================================
# Trim blank spaces on the left of the string
# =========================================================================
ltrim<-function (str) {
   str <- sub ("^ +","",str)
   return (str)
}


# =========================================================================
# Trim blank spaces on the right of the string
# =========================================================================
rtrim<-function (str) {
   str <- sub (" +$","",str)
   return (str);
}


# =========================================================================
# import_data: load a data file and summarize them for the graphs
# =========================================================================

import_data = function (data_path, data_filename, data_fileext, verbose) {
   # Build the input filename (data) and output (graph)
   filename  = paste (data_filename, ".", data_fileext, sep="")
   filename = file.path (data_path, filename, fsep = .Platform$file.sep)
   # Load the data, considering the first line as header
   if (verbose == "1") {
      writeLines (paste("Loading data file '", filename, "' ...", sep=""))
   }
   data = read.table (filename, header=TRUE)
   return (data)
}


# =========================================================================
# export_data: write a text file with the data
# =========================================================================

export_data = function (data_path, data_filename, data_fileext, x, y, verbose) {
   # Data frame combining x and y vectors
   data = data.frame(y)  # cbind(x, y)
   data.row.names = x
   # Build the output filename
   filename  = paste (data_filename, ".", data_fileext, sep="")
   filename = file.path (data_path, filename, fsep = .Platform$file.sep)
   # Write the data
   if (verbose == "1") {
      writeLines (paste("Writting data file '", filename, "' ...", sep=""))
   }
   write.table (data, filename, row.names=TRUE, col.names=TRUE)
}


# =========================================================================
# export_data_to_latex: write a LaTeX file with the data
# =========================================================================

export_data_to_latex = function (title, outputpath, filename, row_names, col_names,
                                 data, col_x_values_len, latex_digits, verbose) {
   # Filename (the same as the graph filename with the .tex extension)
   filename_tex = paste (outputpath, .Platform$file.sep, filename, ".tex", sep="")
   if (verbose == "1") {
      writeLines (paste("Exporting data to LaTeX (", latex_digits, " decimals) ...", sep=""))
      writeLines (paste("   File: ", filename_tex, sep=""))
   }

   # Latex table (the first row and column are headers): row_names and col_names, respctively.

   # To avoid problems, an escape character '\' is added before underscores '_'.
   #new_names = sapply (strsplit(row_names,"_"), paste, collapse="\_")
   # But with "\_", an error is launched when loading this file in R:
   #    Error: '\_' is an unrecognized escape in character string starting "\_"
   #
   # With two escape characters "\\_", this is replaced with "$\backslash$\_"
   #    in the latex table.
   #new_names = sapply (strsplit(row_names,"_"), paste, collapse="\\_")
   #
   # With recent versions of "xtable" this is unnecessary.
   new_names = row_names

   table_names = list (new_names, col_names)
   table_data  = array (0, c(NROW(row_names), NROW(col_names)), dimnames=table_names)
   for (row in 1:NROW(row_names)) {
      for (col in 1:NROW(col_names)) {
         table_data[row,col] = data[col,row]
      }
   }
   # Decimal places in each column (1 value -> repeated)
   table_digits  = array (latex_digits, c(col_x_values_len+1))
   # Caption of the table: title
   table_caption = paste (title, " data", sep="")
   # In the title, insert a '\' before the '%' (if it is there)
   #table_caption = sapply (strsplit(table_caption,"%"), paste, collapse="\\%")
   table_caption = gsub("%", "\\%", table_caption, fixed=TRUE)  # fixed=TRUE to avoid using an regular expression
   #table_label  = paste (simulset, ":data:", title, sep="")
   table_label   = paste ("data:", basename(filename_tex), sep="")
   tabla_latex   = xtable (table_data, digits=table_digits, caption=table_caption, label=table_label)

   # Package: xtable - Version: 1.4-3
   #print.xtable (tabla_latex, file=filename_tex, table.placement="h!", size="scriptsize")
   # Package: xtable - Version: 1.5-2
   print (tabla_latex, file=filename_tex, table.placement="!ht", size="scriptsize")
}


# =========================================================================
# open_graph: initialize a graph.
#
# WARNING: png() require a graphical environment (e.g. X11 or Windows),
#          an error will be launched if a text mode console is used.
# Solutions for Mac OS X:
#    1. Use bitmap() for generating .png
#    2. Use Xvfb (fake X server)
#    http://www.X.org
#    http://xorg.freedesktop.org
#    http://lists.freedesktop.org/mailman/listinfo/xorg
# =========================================================================

open_graph = function (filename, fileext, width_factor=1.0, height_factor=1.0, dpi=72) {
   if ((fileext == "ps") || (fileext == "eps")) {
      # File .ps o .eps
      # R does not embed the font(s) used in the PostScript output;
      # is is required Ghostscript at the end, or export to pdf with
      # cairo_pdf() (see below)
      # For true EPS: onefile=FALSE and paper="special"
      try (postscript (filename, onefile=FALSE, horizontal=FALSE, pointsize=12,
                       width=6*width_factor, height=6*height_factor), silent=FALSE)
   } else if (fileext == "pdf") {
      # New in v1.0-9
      # pdf() does not embed fonts; cairo_pdf() embeds fonts.
      try (cairo_pdf (filename, onefile=TRUE, pointsize=12,
           width=6*width_factor, height=6*height_factor), silent=FALSE)
   } else {
      w = 480 * width_factor
      h = 480 * height_factor
      if (dpi != 72) {
          # It is necessary to take into account the resolution (dpi) to compute the width and height
          res_factor = dpi/72
          w = w * res_factor
          h = h * res_factor
      }
      if (fileext == "png") {
         # File .png
         # OJO: Para generar los .png, en windows se puede utilizar png(),
         # pero en Linux solo se puede utilizar png desde X11, no desde la consola.
         # Para evitar problemas, se utiliza bitmap(), que no requiere X11.
         if ( capabilities("png") ) {
            #writeLines ("    Modo grafico: generando .png con png()")
            try (png (filename, units="px", width=w, height=h, res=dpi), silent=FALSE)
         } else {
            #writeLines ("    Modo no grafico: generando .png con bitmap()")
            try (bitmap (filename, type="pnggray", units="px", width=w, height=h, res=dpi), silent=FALSE)
         }
      } else if (fileext == "tiff") {
         try (tiff (filename, units="px", width=w, height=h, res=dpi), silent=FALSE)
      } else {
         # Unsupported format
         #try (X11(), silent=FALSE)   # Preview
         return (FALSE)
      }
   }
   return (TRUE)
}


# =========================================================================
# pos_legend_index2string: obtener la posicion de la leyenda en R a partir del
#                      valor 1..9, para usar en la llamada a 'legend'.
# Retorna: el parametro 'xlegend' en forma de cadena.
# =========================================================================

pos_legend_index2string = function (posLegend) {
      if (posLegend==1) {            # Esquina superior izquierda
         xLegend = "topleft"
      } else if (posLegend==2) {     # Centro superior
         xLegend = "top"
      } else if (posLegend==3) {     # Esquina superior derecha
         xLegend = "topright"
      } else if (posLegend==4) {     # Centro izquierda
         xLegend = "left"
      } else if (posLegend==5) {     # Centro Centrado
         xLegend = "center"
      } else if (posLegend==6) {     # Centro derecha
         xLegend = "right"
      } else if (posLegend==7) {     # Esquina inferior izquierda
         xLegend = "bottomleft"
      } else if (posLegend==8) {     # Centro inferior
         xLegend = "bottom"
      } else if (posLegend==9) {     # Esquina inferior derecha
         xLegend = "bottomright"
      } else {
         # Otras posibles posiciones asignando a x alguno de los valores:
         # "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center". 
         xLegend = "bottomright"
         #yLegend = 0.0  # No importa el valor
      }
      return (xLegend)
}

# =========================================================================
