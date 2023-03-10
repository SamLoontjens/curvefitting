% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_any_curve.R
\name{fit_any_curve}
\alias{fit_any_curve}
\title{A function that fits any curve with nls.}
\usage{
fit_any_curve(
  x,
  y,
  formula = as.formula(y ~ a * x + b),
  variable_name = "x",
  list_of_start_parameters = list(a = 0, b = 5),
  list_of_static_parameters = list(),
  title = "random fit",
  subtitle = "no filename",
  manual_check = TRUE,
  save_plot = FALSE,
  save_path = "output_directory_licorfiles/plots/",
  lower_bounds = NULL,
  upper_bounds = NULL
)
}
\arguments{
\item{x}{The variable values.}

\item{y}{The dependent values.}

\item{formula}{The formula to use for fitting the data, default is linear.}

\item{variable_name}{A string of the name of the variable, default is x.}

\item{list_of_start_parameters}{The list of starting parameters.}

\item{title}{The title of the plot.}

\item{subtitle}{The subtitle of the plot.
Filename is reccomended. Used for the saved plot name.}

\item{manual_check}{The boolean if the plots are manually checked or not.}

\item{save_plot}{The boolean to check if the plots have to be saved.}

\item{save_path}{The path sting to save the plot if save_plot is TRUE.}
}
\value{
A list of parameters that are fitted.
}
\description{
A function that fits any curve with nls.
}
\examples{
parameters <- fit_any_curve(x = 1:10,
                            y = 1:10 +5 + rnorm(10),
                            formula = as.formula(banaan ~ k * x + p),
                            list_of_start_parameters = list(k = 2, p = 3))
or
x <- 1:10
y <- 0.5 * x^2 + rnorm(10) + 10
parameters <- fit_any_curve(x = x,
                            y = y,
                            formula = as.formula(stuff ~ a1 * t^2 + b1),
                            variable_name = "t",
                            list_of_start_parameters = list(a1 = 2, b1 = 3),
                            title = "quadratic fit",
                            subtitle = "from no file",
                            manual_check = FALSE,
                            save_plot = TRUE)

}
\author{
Sam Loontjens
}
