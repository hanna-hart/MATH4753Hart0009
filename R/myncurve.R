
myncurve = function(mu, sigma, p1, p2){

  mu = mu
  sigma = sigma
  p1 = p1
  p2 = p2

  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve = seq(p1, p2, length = 1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(p1, xcurve, p2), c(0, ycurve, 0), col = "Red")

  area = pnorm(p2, mu, sigma) - pnorm(p1,mu,sigma)
  area = round(area, 4)
  text(2, 0.5*dnorm(2,mu,sigma), paste0("Area: ", area))

}
