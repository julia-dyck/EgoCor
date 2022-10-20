#' Spatial Data Coordinate Plot
#'
#' Plot of the Cartesian coordinates of study participant with color and shape coded
#' indication whether the variable of interest is observed at a specific location.
#'
#' @param data A data frame or matrix containing
#'    the x-coordinates in meters in the first column,
#'    the y-coordinates in meters in the second column,
#'    and the values of the attribute of interest in the third column.
#'    Additional columns are ignored.
#' @param pch Determines the point shape used in the plot.
#' @param pch If set to TRUE the points are colored according to their values.
#'
#'
#' @return
#' The function returns a plot showing the points based on Cartesian coordinates. A black circle indicates that the
#'         variable of interest is observed at that location. A red cross flags a missing value.
#'
#'
#'
#' @examples
#' ## Example 1
#' xcoords = rnorm(10, mean = 0, sd = 20)
#' ycoords = rnorm(10, mean = 0, sd = 20)
#' value = c(22, 31, 10, NA, NA, 18, 9, NA, 1, 34)
#' dataset = cbind(xcoords, ycoords, value)
#' coords.plot(dataset)
#'
#' ## Example 2
#' coords.plot(birth)
#'
#' @export


coords.plot <- function(data, pch = 16, col = F){
  #### necessary packages
  # graphics

  ### message about required data format
  if(ncol(data)>3){warning('Data matrix contains more than 3 columns. Are the columns in correct order?\n')}
  message(paste('Message:',
                'Input data interpretation:',
                '    column 1: Cartesian x-coordinates in meters',
                '    column 2: Cartesian y-coordinates in meters',
                '    column 3: outcome variable \n \n',sep="\n"))

  ### delete rows with incomplete coordinates
  if(sum(is.na(data[,1:2])) > 0){
    ind.missing.x = which(is.na(data[,1]))
    ind.missing.y = which(is.na(data[,2]))
    ind.incompl.coords = unique(c(ind.missing.x, ind.missing.y))
    warning(paste("Data contains",
                  length(ind.incompl.coords),
                  "rows with missing coordinates. Rows with incomplete coordinates are ignored."))
    data = data[-ind.incompl.coords,]
  }
  ### visualization of the coordinates
  x.range = c(min(data[,1]), max(data[,1]))
  y.range = c(min(data[,2]), max(data[,2]))
  # splitting up the data set in NA and non-NA
  data_na = data[which(is.na(data[,3])), 1:2]
  data_no_na = data[which(!is.na(data[,3])), 1:3]

  if (col == T){
    rbPal = grDevices::colorRampPalette(c("yellow", "red"))
    color = rbPal(nrow(data_no_na))[as.numeric(cut(data_no_na[,3], breaks = nrow(data_no_na)))]
  }
  else{color = 1}

  plot(data_no_na[,1:2], main = "Coordinate plot",
       xlim = x.range, ylim = y.range, col = color, pch = pch)
  graphics::points(data_na[,1], data_na[,2], pch = 4, col = "red")
  graphics::legend("topright", title="outcome observed?", legend=c("yes  ", "no"),
         pch = c(1,4), col = c(1,2), ncol = 2, cex = 0.8)

}

