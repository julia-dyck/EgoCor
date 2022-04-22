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
#'
#' @return
#' The function returns a plot showing the points based on Cartesian coordinates. A black circle indicates that the
#'         variable of interest is observed at that location and a red cross flags a missing value.
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


coords.plot <- function(data){
  #### necessary packages
  #geoR
  #graphics
  if(ncol(data)>3){warning('Data matrix contains more than 3 columns. Are the columns in correct order?\n')}
  message(paste('Message:',
                'Input data interpretation:',
                '    column 1: Cartesian x-coordinates in meters',
                '    column 2: Cartesian y-coordinates in meters',
                '    column 3: outcome variable \n \n',sep="\n"))

  # formatting of the data
  data <- data
  data <- as.data.frame(data.frame(geoR::jitterDupCoords(data[,1:2],max=0.01),data[,3]))
  data.ge <- geoR::as.geodata(data, coords.col = 1:2, data.col = 3, na.action = "ifany")
  #-> list containing [[1]]coordinates, [[2]]variable

  ### visualization of the coordinates
  x.range = c(min(data[,1]), max(data[,1]))
  y.range = c(min(data[,2]), max(data[,2]))
  plot(data[which(is.na(data[,3])),1:2], main = "Coordinate plot",
       xlim = x.range, ylim = y.range, col = 2, pch = 4)
  graphics::points(data.ge[[1]])
  graphics::legend("topright", title="outcome observed?", legend=c("yes  ", "no"),
         pch = c(1,4), col = c(1,2), ncol = 2, cex = 0.8)

}
