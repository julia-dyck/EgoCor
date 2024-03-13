#'  Scatterplot of spatial coordinates
#'
#' Plot of the Cartesian coordinates of study participant with color and shape coded
#' indication whether the variable of interest is observed at a specific location.
#'
#' @param data A data frame or matrix containing
#'    the x-coordinates in meters in the first column,
#'    the y-coordinates in meters in the second column,
#'    and the values of the attribute of interest in the third column.
#'    Additional columns are ignored.
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


coords.plot <- function(data,...){
  #### necessary packages
  # graphics

  ### message about required data format
  if(ncol(data)>3){warning('Data matrix contains more than 3 columns. Are the columns in correct order?\n')}
  message(paste('Message:',
                'Input data interpretation:',
                '    column 1: Cartesian x-coordinates in meters',
                '    column 2: Cartesian y-coordinates in meters',
                '    column 3: outcome variable \n \n',sep="\n"))

  ### look for rows with missing values in coordinates
  comp.row = stats::complete.cases(data[,1:2])

  if(sum(comp.row == F) > 0){
    warning(paste("Coordinates contain",
                  sum(comp.row == F),
                  "rows with missing data. Rows with incomplete coordinates are ignored."))
    data = data[comp.row,]
  }

  ### look for rows with missing values in outcome variable
  comp.row = stats::complete.cases(data[,3])
  group = ifelse(comp.row == T, "yes", "no")
  group.f = factor(group, levels = c("yes", "no"))


  ### visualization of the coordinates
  x.range = range(data[,1])
  y.range = range(data[,2])
  group.col = ifelse(group == "yes", 1, 2)
  group.pch = ifelse(group == "yes", 1, 4)

  plot(data[,1:2], main = "Coordinate plot",
       xlim = x.range, ylim = y.range,
       col = group.col, pch = group.pch,
       lwd = 2,
       ...)

  graphics::legend("topright",
         title = "outcome observed?", legend = levels(group.f),
         col = c(1,2), pch = c(1,4), lty = c(NA, NA), ncol = 2,
         lwd = 2, cex = 0.8)

}

