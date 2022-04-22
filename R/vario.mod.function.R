#' Semi-variogram modeling function
#'
#' Based on the \code{geoR} functions \code{variog} and \code{variofit}, the function fits one
#' or multiple exponential empirical semi-variograms based on provided maximal distances and number of bins.
#' All estimated parameter values are saved in a single table.
#' Graphics of all models can be observed in a shiny application output (default)
#' or in several plot windows, one for each empirical semi-variogram.
#' Additionally, a pdf file including all the figures can be
#' saved in a specified working directory.
#'
#'
#' @param data A data frame or matrix containing the x-coordinates  in the first column,
#'        the y-coordinates  in the second column (by default in meters)
#'        and the data values in the third column.
#'        The data set may contain more attributes in further columns. In this case, a warning is provided.
#'        All columns beyond the third one are ignored.
#' @param max.dist An optional numeric argument; the default is the following maximal distance vector \code{c(2000,1500,1000,750,500,250)}.
#'        Either a scalar or vector containing the maximal distances can be inserted. If a vector is
#'        provided, the \code{ nbins} argument must be either a scalar or a vector of the same length.
#' @param nbins  An optional argument; the default is 13 bins for all empirical semi-variograms to be estimated.
#'        Either a scalar or vector containing the number of bins considered. If a vector is
#'        provided, the \code{max.dist} argument must be either a scalar or a vector of the same length.
#' @param shinyresults A logical argument; by default TRUE. If \code{shinyresults = T},
#'        the information table and graphics of
#'        all estimated semi-variogram models can be observed in an automatically generated
#'        shiny application.
#' @param windowplots A logical argument; by default FALSE. If \code{windowplots = T}, all graphics are
#'        opened in new windows. They can be observed and saved manually in a wished format.
#' @param pdf A logical argument; by default FALSE. If \code{pdf = T}, all graphics are saved in a pdf file.
#'        The file path and the name of the pdf file can be specified by the following two arguments
#'        \code{pdf.directory} and \code{pdf.name}.
#' @param pdf.directory A character argument to specify the folder in which the pdf file is saved.
#'        If no file path is given, the pdf file is saved in the current working directory identified
#'        by \code{getwd()}.
#' @param pdf.name A character argument to specify the name of the pdf file. If no name is provided, the
#'        file is saved as 'Semivariograms.pdf'.
#'
#' @return A list containing the following arguments:
#' \item{infotable}{A table containing the statistics of all estimated exponential semi-variogram models.
#'       Each row corresponds to one model.
#'       Shown are the prespecified \code{max.dist} and \code{nbins} values, the parameter estimates for the nugget effect, partial
#'       sill and shape, the resulting estimated practical range,
#'       the relative structured variability (RSV) and the relative bias.}
#' \item{variog.list}{A list: each list entry contains the \code{variog} output with further information
#'      on the estimated empirical semi-variogram.}
#' \item{vmod.list}{A list: each list entry contains the \code{variofit} output with further information
#'      on the fitted parametric semi-variogram model.}
#' \item{input.arguments}{A list containing the evaluated input arguments, namely the \code{$data} used to fit the
#'       exponential semi-variogram, the \code{$max.dist} and \code{$nbins} specifications and the
#'       specifications for the pdf-output, \code{$pdf}, \code{$pdf.directory} and \code{$pdf.name}.}
#' \item{call}{Contains the call of the function.}
#'
#' The models are visualized in an automatically opened shiny application if \code{shinyresults = T}. It is to be noticed,
#' that in this case, the output of the \code{vario.mod} function is not saved in the environment, even with a variable name assigned.
#' If the argument \code{windowplots = T}, one or multiple graphics of the estimated
#' empirical semi-variograms and semi-variogram models are plotted in the R environment.
#' If the argument \code{pdf = T}, a pdf file containing the same figures is saved in the manually
#' specified or current working directory.
#'
#' @details \strong{Prespecification and Interpretation of max.dist and nbins arguments:}
#'
#'          \code{max.dist}:
#'          only data pairs with a separation smaller than the prespecified maximal distance are included in the semi-variogram
#'          estimation. Data pairs that are separated by a higher distance are excluded.
#'
#'          \code{nbins}: the interval (0, \code{max.dist}] is separated into \code{nbins} equidistant
#'          lag bins or intervals, respectively. Each pairwise distance can then be assigned to one of the
#'          bins. The point pair subsets \eqn{N(h_k) := \{(\mathbf{s_i}, \mathbf{s_j}) \in D |\;\; ||\mathbf{s_i}-\mathbf{s_j}|| \in Bin_k\}}
#'          are defined
#'          and a point estimate of the semi-variogram is estimated for each \eqn{Bin_k} for \eqn{k =1,...}\code{nbins}.
#'
#'
#'          \strong{Empirical semi-variogram estimator:}
#'
#'          Using the \code{geoR} functions \code{variog} an empirical semi-variogram according to
#'          Matheron's semi-variogram estimator \insertCite{matheron1962traite}{EgoCor}
#'          \deqn{\hat{\gamma}(h) = \frac{1}{2\cdot|N(h)|} \sum_{(\mathbf{s_i},
#'          \mathbf{s_j}) \in N(h)}\{Z(\mathbf{s_i})- Z(\mathbf{s_j})\}^2}
#'          with \eqn{N(h)} defined as above is obtained.
#'
#'
#'          \strong{Exponential semi-variogram model:}
#'
#'          Based on the empirical semi-variogram an exponential semi-variogram model of the form
#'          \insertCite{Cressie.1993}{EgoCor}
#'          \deqn{\gamma_{exp}(h) =  c_0 + \sigma_0^2 \Big\{1 - \exp\big(- \frac{h}{\phi}\big)\Big\}}
#'          for \eqn{h > 0}
#'          is fitted using the \code{variofit} function from package \code{geoR} via
#'          weighted least squares estimation.
#'          For the numerical optimization, starting values for the model parameters have to be provided.
#'          The initial value for the partial sill \eqn{\sigma_0^2} equals the empirical variance of
#'          the observations. The starting value for the nugget effect \eqn{c_0} is set to zero.
#'          The initial value for the shape parameter \eqn{\phi} is set as \code{max.dist} divided by 3.
#'
#'
#'          \strong{Result statistics:}
#'
#'          The results for all models are automatically printed when running the function and can be
#'          found under \code{function.output$infotable}. Part of the table contains a repetition of the
#'          specified \code{max.dist} and \code{nbins} parameters as well as the estimated model parameters.
#'          The additional statistics within the infotable output are the following:
#'
#'          Practical range: In case of the exponential semi-variogram model, the
#'          sill \eqn{\sigma^2 = c_0 + \sigma_0^2} is only reached asymptotically. The distance \eqn{H}
#'          at which \eqn{\gamma(h^* = 0.95 \cdot \sigma^2)} is called the practical range.
#'          Formally, the practical range is
#'          defined as
#'          \deqn{prac.range = \frac{1}{\phi} log\Big( \frac{\sigma_0^2}{0.05(c_0 + \sigma_0^2)} \Big).}
#'
#'          Relative Structural Variability (RSV): The relative structural variability is a measure of
#'          the proportion of the total variance with a spatial structure and
#'          defined as
#'          \deqn{RSV = \frac{\sigma_0^2}{c_0 + \sigma_0^2}.}
#'
#'          Relative Bias: The relative bias describes the proportion of the total variance according
#'          to the semi-variogram model to the true total variance. It is estimated as
#'          \deqn{rel. bias = \frac{c_0 + \sigma_0^2}{\widehat{Var(Z)}},}
#'          where \eqn{\widehat{Var(Z)}} is the sample variance or empirical variance of the attribute of
#'          interest of the dataset at hand.
#'          A relative bias of 1 indicates equality of
#'          sample variance and variance according to the semi-variogram model.
#'
#'          For more details, see \insertCite{schabenberger2017statistical;textual}{EgoCor}.
#'
#'
#'
#'
#'
#' @examples
#' if(interactive()){
#'
#' ## Example 1
#' # Default options:
#' vario.mod(data = birth)
#'
#' # This is equal to
#' vario.mod(data = birth, max.dist = c(2000,1500,1000,750,500,250), nbins = 13,
#'           shinyresults = TRUE, windowplots = FALSE,
#'           pdf = FALSE, pdf.directory = getwd(), pdf.name = "Semivariograms")
#'
#' ## Example 2
#' # Open graphics in regular windows and not in shiny application:
#' vario.mod(data = birth, max.dist = c(2000,1500,1000,750,500,250), nbins = 15:10,
#'           shinyresults = FALSE, windowplots = TRUE)
#'
#' ## Example 3
#' # Generate a pdf with the following command:
#' vario.mod(data = birth, shinyresults = FALSE, windowplots = FALSE,
#'           pdf = TRUE, pdf.directory = getwd())
#' # You find a pdf file in your current working directory.
#'
#'}
#'
#'
#' @seealso \code{variog} in the \code{geoR} package for further information on the arguments \code{max.dist} and \code{nbins} as well as on
#'          the estimation of the empirical semi-variogram itself;
#'
#'          \code{variofit} in the \code{geoR} package for further information on the default settings
#'          when estimating the exponential semi-variogram model.
#'
#'
#'
#'
#' @references
#' \insertAllCited{}
#'
#' @importFrom Rdpack reprompt
#'
#' @export


vario.mod = function(data, max.dist = c(2000,1500,1000,750,500,250), nbins = 13,
                     shinyresults = TRUE, windowplots = FALSE,
                     pdf = FALSE, pdf.directory = getwd(), pdf.name = "Semivariograms"){
  #### necessary packages
  #geoR
  #SpatialTools
  #stats
  #graphics
  #grDevices
  #shiny

  #### input arguments (for call)
  data.arg = deparse(substitute(data))
  max.dist.arg = deparse(substitute(data))
  nbins.arg = deparse(substitute(data))
  pdf.arg = deparse(substitute(data))
  pdf.directory.arg = deparse(substitute(data))
  pdf.name.arg = deparse(substitute(data))

  fct.call = as.call(str2lang(paste("vario.mod(data =", data.arg, ", max.dist = ", max.dist.arg, ", nbins = ", nbins.arg, ", pdf = ", pdf.arg, ", pdf.directory = ", pdf.directory.arg, ", pdf.name = ", pdf.name.arg, ")")))

  #### data input: formatting
  if(ncol(data)>3){warning('Data matrix contains more than 3 columns. Are the columns in correct order?\n')}
  message(paste('Message:',
            'Input data interpretation:',
            '    column 1: Cartesian x-coordinates in meters',
            '    column 2: Cartesian y-coordinates in meters',
            '    column 3: outcome variable \n \n',sep="\n"))

  data <- cbind(data[,1], data[,2], data[,3])
  data <- as.data.frame(data.frame(geoR::jitterDupCoords(data[,1:2],max=0.01),data[,3]))
  data.ge <- geoR::as.geodata(data, coords.col = 1:2, data.col = 3, na.action = "ifany")
  #-> list containing [[1]]coordinates, [[2]]variable
  sample.var = stats::var(data.ge[[2]])

  #### estimate variogram
  variog.dist.bin.dep = function(max.dist.nbins.cbinded){
    # variogram calculator for fixed data, only variable is the maximal distance
    # purpose: make variog function usable in lapply()
    max.dist = max.dist.nbins.cbinded[1]
    nbins = max.dist.nbins.cbinded[2]
    est.variog = geoR::variog(data.ge,estimator.type="classical",
                              max.dist = max.dist, uvec = nbins, messages = F)
    return(est.variog)}

  if(is.atomic(max.dist) && length(max.dist) == 1 && is.atomic(nbins) && length(nbins) == 1){# max.dist and nbins both scalar
    max.dist.vect = max.dist
    nbins.vect = nbins
  }
  else if(is.vector(max.dist) && is.atomic(nbins) && length(nbins) == 1){# max.dist vector and nbins scalar
    max.dist.vect = max.dist
    nbins.vect = rep(nbins, length(max.dist.vect))
  }
  else if(is.atomic(max.dist) && length(max.dist) == 1 && is.vector(nbins)){# max.dist scalar and nbins vector
    nbins.vect = nbins
    max.dist.vect = rep(max.dist, length(nbins))
  }
  else if (is.vector(max.dist) && is.vector(nbins)){
    max.dist.vect = max.dist
    nbins.vect = nbins
    if(length(max.dist.vect) != length(nbins.vect)){
      stop("If vectors for both input parameters max.dist and nbins are specified, they must have the same length.")
    }
  }
  else{stop("Input parameters max.dist and nbins have to be either \n     both a scalar, \n     max.dist a scalar and nbins a vector, \n     max.dist a vector and nbins a scalar, \n     both vectors of the same length.")}

  max.dist.nbins.matrix = cbind(max.dist.vect,nbins.vect)
  variog.list = list()
  variog.list = apply(max.dist.nbins.matrix, 1, variog.dist.bin.dep)

  nbins.used = sapply(variog.list, function(x) length(x$uvec))

  #### estimate exponential variogram model
  variofit.less.arg = function(vario){
    # variogram modelling function with parameter structure, st. lapply can be used
    ini.partial.sill <- sample.var # partial sill parameter of the exp. model (also called sigmasq)
    ini.shape <- vario$max.dist/3 # oder /4; shape parameter of the exp. model (also called phi)
    ini.values <- c(ini.partial.sill, ini.shape)
    exp.variogram.mod <- geoR::variofit(vario, ini.cov.pars = ini.values, cov.model = "exponential", messages = F)
  }

  vmod.list = lapply(variog.list, variofit.less.arg)

  par.extraction = function(vmod){
    est.pars = summary(vmod)$estimated.pars
    #loss.fct.value = summary(vmod)$sum.of.squares
    return(est.pars)
  }
  estimated.pars = t(sapply(vmod.list, par.extraction))
  colnames(estimated.pars) = c("nugget","partial.sill","shape")
  prac.range = sapply(estimated.pars[,3], geoR::practicalRange, cov.model="exp")
  est.total.var = estimated.pars[,1] + estimated.pars[,2]
  RSV = estimated.pars[,2]/est.total.var # relative structured variability
  rel.bias = est.total.var/sample.var # relative bias

  infotable = data.frame(max.dist = max.dist.vect, nbins = as.integer(nbins.vect),
                         nbins.used = nbins.used,
                         estimated.pars, prac.range, RSV, rel.bias)

  #### Save and restore graphical parameter settings ###
  originalpar = graphics::par(no.readonly = TRUE) # save par settings before adjusting here for graphical output
  on.exit(graphics::par(originalpar), add = TRUE) # restore par() settings before exiting function

  #### Visualization and pdf extraction ###
  if(pdf == T){
    pdf.name = paste0("/",pdf.name, ".pdf")
    grDevices::pdf(file = paste0(pdf.directory,pdf.name), width = 8.3, height = 11.7) # width and height of DinA4 in inch rounded
    graphics::par(mfrow = c(4,2))
    graphics::par(mar = c(4.0, 3.1, 3.1, 2.1))
    graphics::par(mgp = c(1.5, 0.5, 0.0))
    graphics::par(omi = c(1.0, 1, 1.5, 1.0))
    for (d in 1:length(max.dist.vect)){
      plt = plot(variog.list[[d]], xaxt = "n", yaxt = "n")
      plt
      graphics::axis(1, cex.axis = 0.8)
      graphics::axis(2, cex.axis = 0.8)
      # graphics::mtext(bquote(.("Maximal distance of ") ~ bold(.(paste(max.dist.vect[[d]])))),side=3, adj=0, line=1.2, cex=1, font=1, outer = F)
      graphics::title(paste("Maximal distance:",max.dist.vect[d],
                            "\nNumber of bins:",nbins.used[d] , sep=" "),
                      adj = 0,
                      cex.main = 1,
                      font = 1)
      graphics::lines(vmod.list[[d]])
      pars = round(infotable[d,c(4,5,7,8,9)], digits = 2)

      if(pars[3] > (10*max.dist.vect[[d]]) | pars[3] < 0){
        leg = paste(c("nugget effect","partial sill","prac. range","RSV","rel. bias"),":"
                    ,pars,c("","","*","",""))
        text.col.vect = c("black", "black", "red3", "black", "black")
        graphics::legend("bottomright", legend = leg, ncol = 1, cex = 0.8, x.intersp = -0.3, text.col = text.col.vect)
        #graphics::abline(v = 0.1*max.dist.vect[[d]], col="red", lwd=2)
        graphics::legend("bottomleft", inset = 0.1, c("*Consider changing", " the number of bins."), cex = 0.7, x.intersp = -0.3,
                         box.col = "red3", text.col = "red3", bg = "white")
      }
      else{leg = paste(c("nugget effect","partial sill","prac. range","RSV","rel. bias"),":",pars)
      graphics::legend("bottomright", legend = leg, ncol = 1, cex = 0.8, x.intersp = -0.3)}
      if(d == 1){graphics::mtext("Semivariograms", side = 3, adj = 0.09, line = 3, cex=2, font = 2, outer=TRUE)}
      if((d%%8) == 1){
        graphics::mtext(paste0(((d-1)/8)+1), side = 1, line = 4, outer = TRUE)
        graphics::mtext(paste0(pdf.directory,pdf.name), side = 3, adj = 0.95, line = 5.2, outer = TRUE, cex = 0.7)
        graphics::mtext(Sys.time(), side = 3, adj = 0.95, line = 4, outer = TRUE, cex = 0.7)
      }
    }
    grDevices::dev.off()
  }

  #### Visualization in RWindow
  if(windowplots == T){
    for (d in 1:length(max.dist.vect)){
      grDevices::x11() # open a new window for each plot
      # esp. to prevent overwriting plots in basic R GUI
      plt = plot(variog.list[[d]], xaxt = "n", yaxt = "n")
      graphics::title(paste("Maximal distance:",max.dist.vect[d],
                            "\nNumber of bins:",nbins.used[d] , sep=" "),
                      adj = 0,
                      cex.main = 0.8)
      plt
      graphics::axis(1, cex.axis = 0.8)
      graphics::axis(2, cex.axis = 0.8)
      graphics::lines(vmod.list[[d]])
      pars = round(infotable[d,c(4,5,7,8,9)], digits = 2)

      if(pars[3] > (10*max.dist.vect[[d]]) | pars[3] < 0 ){
        leg = paste(c("nugget effect","partial sill","prac. range","RSV","rel. bias"),":"
                    ,pars,c("","","*","",""))
        text.col.vect = c("black", "black", "red3", "black", "black")
        graphics::legend("bottomright", legend = leg, ncol = 1, cex = 0.8, x.intersp = -0.3, text.col = text.col.vect)
        #graphics::abline(v = 0.1*max.dist.vect[[d]], col="red", lwd=2)
        graphics::legend("bottomleft", inset = 0.1, c("*Consider changing", " the number of bins."), cex = 0.7, x.intersp = -0.3,
                         box.col = "red3", text.col = "red3", bg = "white")
      }
      else{leg = paste(c("nugget effect","partial sill","prac. range","RSV","rel. bias"),":",pars)
      graphics::legend("bottomright", legend = leg, ncol = 1, cex = 0.8, x.intersp = -0.3)}

      #readline(prompt="Press [enter] to see next graphic")
    }
  }


  #### Output formatting
  cat("\nParameter estimates:\n")
  print(infotable)
  cat(paste('\nParameter Legend:',
            'Each row contains the estimated parameters of the exponential semi-variogram model with the stated maximal distance.',
            '   - Index = model number,',
            '   - max.dist = maximal distance considered in the empirical semi-variogram estimation,',
            '   - nbins = number of bins specified for the empirical semi-variogram estimation,',
            '   - nbins.used = number of bins used for the empirical semi-variogram estimation (can differ from nbins in case of colocatted data points),',
            '   - nugget = the estimated nugget effect,',
            '   - partial.sill = the estimated partial sill, ',
            '   - shape = the estimated shape parameter,',
            '   - prac.range = the practical range of the exponential semi-variogram model,',
            '   - RSV = the relative structured variability, ',
            '   - rel.bias = the relative bias between sample variance and estimated variance according to the model.\n \n',sep="\n"))

  # specified arguments in the input:
  # for information purposes and for the parameter uncertainty estimation
  input.arguments = list(data = data, max.dist = max.dist,
                         nbins = nbins,
                         pdf = pdf, pdf.directory = pdf.directory,
                         pdf.name = pdf.name)

  #### Visualization with shiny
  if(shinyresults == T){
    uiv = shiny::fluidPage(shiny::tags$h1("Semi-variogram models"),
                           shiny::fluidRow(
                             shiny::column(width = 5,
                                    shiny::radioButtons(inputId = "modID",
                                                        label = "Choose a maximal distance and nbins combination.",
                                                        choices = paste0("Model ", as.numeric(rownames(infotable))," with ",
                                                                         "max. distance of ", max.dist.vect, " and ",
                                                                         nbins.used, " bins"))),
                             shiny::column(width = 7, offset = 0,
                                    shiny::plotOutput(outputId = "pl"))
                           ),

                           shiny::tags$h4("Info table"),
                           shiny::tableOutput(outputId = "infotable"),

                           shiny::tags$h4("Info table legend"),
                           shiny::tags$div(
                             shiny::tags$ul(
                               shiny::tags$li(shiny::tags$b("Index:"), " model number,"),
                               shiny::tags$li(shiny::tags$b("max.dist:"),  "maximal distance considered in the empirical semi-variogram estimation,"),
                               shiny::tags$li(shiny::tags$b("nbins:"), " number of bins specified for the empirical semi-variogram estimation,"),
                               shiny::tags$li(shiny::tags$b("nbins.used:"), " number of bins used for the empirical semi-variogram estimation (can differ from nbins in case of colocatted data points),"),
                               shiny::tags$li(shiny::tags$b("nugget:"), " the estimated nugget effect,"),
                               shiny::tags$li(shiny::tags$b("partial.sill:"), " the estimated partial sill,"),
                               shiny::tags$li(shiny::tags$b("shape:"), " the estimated shape parameter,"),
                               shiny::tags$li(shiny::tags$b("prac.range:"), " the practical range of the exponential semi-variogram model,"),
                               shiny::tags$li(shiny::tags$b("RSV:"), " the relative structured variability,"),
                               shiny::tags$li(shiny::tags$b("rel.bias:"), " the relative bias between sample variance and estimated variance according to the model.")
                             )
                           )
    )

    serverv = function(input, output){
      # inputs can be toggled by the user
      # outputs: things that the user sees
      output$infotable = shiny::renderTable(
        {
          infotable
        },
        rownames = TRUE
      )
      output$pl = shiny::renderPlot({
        expr = paste0("Model ", as.numeric(rownames(infotable))," with ",
                      "max. distance of ", max.dist.vect, " and ",
                      nbins.used, " bins")
        d = as.numeric(which(expr == input$modID))
        plt = plot(variog.list[[d]], xaxt = "n", yaxt = "n")
        graphics::title(paste("Maximal distance:",max.dist.vect[d],
                              "\nNumber of bins:",nbins.used[d] , sep=" "),
                        adj = 0,
                        cex.main = 0.8)
        plt
        graphics::axis(1, cex.axis = 0.8)
        graphics::axis(2, cex.axis = 0.8)
        graphics::lines(vmod.list[[d]])
        pars = round(infotable[d,c(3,4,6,7,8)], digits = 2)
      })

    }
    print(shiny::shinyApp(ui = uiv, server = serverv))
  }

  output = list(infotable = infotable,
                variog.list = variog.list,
                vmod.list = vmod.list,
                input.arguments = input.arguments,
                call = fct.call)
  class(output) <- "vario.mod.output"

  return(output)
}
