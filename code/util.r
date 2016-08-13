pal = function (n, h = c(-127, -316), c. = c(100, 100), l = c(80, 90
), power = c(1.47692307692308, 3), fixup = TRUE, gamma = NULL, 
    alpha = 1, ...) 
{
    if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
        return(character(0L))
    h <- rep(h, length.out = 2L)
    c <- rep(c., length.out = 2L)
    l <- rep(l, length.out = 2L)
    power <- rep(power, length.out = 2L)
    rval <- seq(1, 0, length = n)
    rval <- hex(polarLUV(L = l[2L] - diff(l) * rval^power[2L], 
        C = c[2L] - diff(c) * rval^power[1L], H = h[2L] - diff(h) * 
            rval), fixup = fixup, ...)
    if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
            width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
}

no.axes = theme(axis.line=element_blank(), axis.text.x=element_blank(),
  axis.text.y=element_blank(), axis.ticks=element_blank(),
  axis.title.x=element_blank(), axis.title.y=element_blank())

rad = function(angle) 2*pi*angle/360

cv.ranger = function(formula, data, mtry = floor(ncol(data)^(1:4/4)),
  node.size = c(100, 30, 5, 1), ...) {

  error = matrix(NA, length(mtry), length(node.size))
  fit = list()
  for (m in 1:length(mtry)) {
    fit[[m]] = list()
    for (n in 1:length(node.size)) {
      fit[[m]][[n]] = ranger(formula, as.data.frame(data), mtry = mtry[m],
        min.node.size = node.size[n], ...)
      error[m, n] = fit[[m]][[n]]$prediction.error
    }
  }
  n = which.min(apply(error, 2, min))
  m = which.min(error[, n])
  fit[[m]][[n]]
}

