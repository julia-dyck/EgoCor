
# needed packages
# ## stats



backtr = function(scores, nscore, tails='none', draw=TRUE) {
  # Given a vector of normal scores and a normal score object
  # (from nscore), the function returns a vector of back-transformed
  # values. One major issue is how to extrapolate to the tails. Options
  # other than none may result in dramatically incorrect tail estimates!
  # tails options:
  # 'none' : No extrapolation; more extreme score values will revert
  # to the original min and max values.
  # 'equal' : Calculate magnitude in std deviations of the scores about
  # initial data mean. Extrapolation is linear to these deviations.
  # will be based upon deviations from the mean of the original
  # hard data - possibly quite dangerous!
  # 'separate' :  This calculates a separate sd for values
  # above and below the mean.

  if(tails=='separate') {
    mean.x <- mean(nscore$trn.table$x)
    small.x <- nscore$trn.table$x < mean.x
    large.x <- nscore$trn.table$x > mean.x
    small.sd <- sqrt(sum((nscore$trn.table$x[small.x]-mean.x)^2)/
                       (length(nscore$trn.table$x[small.x])-1))
    large.sd <- sqrt(sum((nscore$trn.table$x[large.x]-mean.x)^2)/
                       (length(nscore$trn.table$x[large.x])-1))
    min.x <- mean(nscore$trn.table$x) + (min(scores) * small.sd)
    max.x <- mean(nscore$trn.table$x) + (max(scores) * large.sd)
    # check to see if these values are LESS extreme than the
    # initial data - if so, use the initial data.
    #print(paste('lg.sd is:',large.sd,'max.x is:',max.x,'max nsc.x is:',max(nscore$trn.table$x)))
    if(min.x > min(nscore$trn.table$x)) {min.x <- min(nscore$trn.table$x)}
    if(max.x < max(nscore$trn.table$x)) {max.x <- max(nscore$trn.table$x)}
  }
  if(tails=='equal') { # assumes symmetric distribution around the mean
    mean.x <- mean(nscore$trn.table$x)
    sd.x <- stats::sd(nscore$trn.table$x)
    min.x <- mean(nscore$trn.table$x) + (min(scores) * sd.x)
    max.x <- mean(nscore$trn.table$x) + (max(scores) * sd.x)
    # check to see if these values are LESS extreme than the
    # initial data - if so, use the initial data.
    if(min.x > min(nscore$trn.table$x)) {min.x <- min(nscore$trn.table$x)}
    if(max.x < max(nscore$trn.table$x)) {max.x <- max(nscore$trn.table$x)}
  }
  if(tails=='none') {   # No extrapolation
    min.x <- min(nscore$trn.table$x)
    max.x <- max(nscore$trn.table$x)
  }
  min.sc <- min(scores)
  max.sc <- max(scores)
  x <- c(min.x, nscore$trn.table$x, max.x)
  nsc <- c(min.sc, nscore$trn.table$nscore, max.sc)

  if(draw) {plot(nsc,x, main='Transform Function')}
  back.xf <- stats::approxfun(nsc,x) # Develop the back transform function
  val <- back.xf(scores)

  return(val)
}
