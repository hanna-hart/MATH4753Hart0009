#' Afunction for plotting a quadratic
#'
#' @param x
#'
#' @return Estimated Height
#' @export
#'
#' @examples
myplot=function(x){
  quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
}
