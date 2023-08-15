
#install.packages("dsa2a", type = "source", repos = NULL)

library(deSolve)
library(scatterplot3d)

Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- a * X + Y * Z
    dY <- b * (Y - Z)
    dZ <- -X * Y + c * Y - Z
    list(c(dX, dY, dZ))
  })
}

parameters <- c(a = -8/3, b = -10, c =  28)
state <- c(X = 1, Y = 1, Z = 1)
times <- seq(0, 100, by = 0.01)

out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
plot(out)
scatterplot3d(out[,-1], type="l")

out <- ode(state, times, func = "derivs", parms = parameters,
           dllname = "dsa2a", initfunc = "initmod")
plot(out)
scatterplot3d(out[,-1], type="l")

out <- ode(state, times, func = "derivs_a2a", parms = parameters,
           dllname = "dsa2a", initfunc = "initmod_a2a")
plot(out)
scatterplot3d(out[,-1], type="l")

microbenchmark::microbenchmark(
  out <- ode(y = state, times = times, func = Lorenz, parms = parameters),
  out <- ode(state, times, func = "derivs", parms = parameters, dllname = "dsa2a", initfunc = "initmod"),
  out <- ode(state, times, func = "derivs_a2a", parms = parameters, dllname = "dsa2a", initfunc = "initmod_a2a"))




Lorenz <- function(t, y, ydot) {
    a_db <- -8/3;
    b_db <- -10
    c_db <- 28
	y[1] <- a_db* y[1] + y[2]*y[3]
	y[2] <- b_db * (y[2] - y[3])
	y[3] <- - y[1]*y[2] + c_db * y[2] - y[3]
}

LorenzCpp <- ast2ast::translate(Lorenz, output = "XPtr", types_of_args = c("double", "sexp", "sexp"), 
								return_type = "void", reference = TRUE, getsource = TRUE)
cat(LorenzCpp)
