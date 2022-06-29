#' Pairwise Distances of Spatial Coordinates
#'
#' A range of descriptive statistics on the distribution of the pairwise distances between observations.
#'
#' @param data A data frame or matrix containing the x-coordinates in the first column
#'                    and the y-coordinates in the second column.
#'                    Additional columns are ignored.
#'
#' @return A list containing:
#'
#' \item{distmatrix}{A matrix containing the pairwise Euclidean distances between
#'       all given locations.}
#' \item{distset}{A vector containing the pairwise Euclidean distances.
#'       The elements equal the upper (or lower) triangle minus the diagonal
#'       of the distance matrix. If the input dataset has n rows, \code{distset} contains \eqn{n(n-1)/2} distances.}
#' \item{distsummary}{A summary containing the minimum, 1st quartile, median, mean,
#'       3rd quartile and maximum of the \code{distset}.}
#' \item{maxdist}{The maximal distance.}
#'
#' Moreover, the function prints a histogram of the pairwise distances saved in the list entry \code{distset}.
#'
#' @examples
#' ## Example 1
#' x = c(1,3,7,10,15)
#' y = c(5,19,8,3,11)
#' z = rnorm(5, mean = 20, sd = 40)
#' dataset = as.data.frame(cbind(x,y,z))
#' distance.info(data = dataset)
#'
#' ## Example 2
#' distance.info(birth)
#'
#'
#' @export

distance.info = function(data){
  ### provides information on the euclidean distances of the data coordinates

  #### necessary packages
  #SpatialTools
  #stats
  #graphics

  #### data input: formatting
  if(ncol(data)>3){warning('Data matrix contains more than 3 columns. Are the columns in correct order?\n')}
  message(paste('Message:',
                'Input data interpretation:',
                '    column 1: Cartesian x-coordinates in meters',
                '    column 2: Cartesian y-coordinates in meters',
                '    column 3: outcome variable \n \n',sep="\n"))

  if(sum(is.na(data[,1:2])) > 0){
    ind.missing.x = which(is.na(data[,1]))
    ind.missing.y = which(is.na(data[,2]))
    ind.incompl.coords = unique(c(ind.missing.x, ind.missing.y))
    warning(paste("Data contains",
                  length(ind.incompl.coords),
                  "rows with missing coordinates. Rows with incomplete coordinates are ignored."))
    data = data[-ind.incompl.coords,]
  }

  coordinates = as.matrix(data[,1:2])
  distmat = SpatialTools::dist1(coordinates)
  distset = as.vector(stats::dist(coordinates))

  graphics::hist(distset, main= "Histogram of distances", xlab = "distances")
  #graphics::lines(density(distset), lwd = 2)
  cat('\n Summary of distance set: \n')
  print(summary(distset))
  invisible(list(distmatrix = distmat,
              distset = distset,
              distsummary = summary(distset),
              maxdist = max(distset)))
  # output$...
  # ## distmatrix: symmetric matrix containing the euclidean distances of each data pair
  # ## distset: vector containing the upper (= lower) triangle of the distmatrix
  # ## distsummary: min, 1st quartile, median, mean, 3rd quartile and max of the set of distance values
  # ## maxdist: maximal distance observed -> as orientation for  max.dist in function vario.mod

  # ##histogram of the distances
}
