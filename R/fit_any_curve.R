#' A function that fits any curve with nls.
#'
#' @description
#'   A function that fits any curve with nls.
#' @author Sam Loontjens
#' @param x The variable values.
#' @param y The dependent values.
#' @param formula The formula to use for fitting the data, default is linear.
#' @param variable_name A string of the name of the variable, default is x.
#' @param list_of_start_parameters The list of starting parameters.
#' @param title The title of the plot.
#' @param subtitle The subtitle of the plot.
#'                 Filename is reccomended. Used for the saved plot name.
#' @param manual_check The boolean if the plots are manually checked or not.
#' @param save_plot The boolean to check if the plots have to be saved.
#' @param save_path The path sting to save the plot if save_plot is TRUE.
#' @export
#' @return A list of parameters that are fitted.
#' @examples
#' parameters <- fit_any_curve(x = 1:10,
#'                             y = 1:10 +5 + rnorm(10),
#'                             formula = as.formula(banaan ~ k * x + p),
#'                             list_of_start_parameters = list(k = 2, p = 3))
#' or
#' x <- 1:10
#' y <- 0.5 * x^2 + rnorm(10) + 10
#' parameters <- fit_any_curve(x = x,
#'                             y = y,
#'                             formula = as.formula(stuff ~ a1 * t^2 + b1),
#'                             variable_name = "t",
#'                             list_of_start_parameters = list(a1 = 2, b1 = 3),
#'                             title = "quadratic fit",
#'                             subtitle = "from no file",
#'                             manual_check = FALSE,
#'                             save_plot = TRUE)
#'
fit_any_curve <- function(x,
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
                          upper_bounds = NULL) {

  #get the formula strings
  left_hand_side <- as.character(formula[2])
  right_hand_side <- as.character(formula[3])

  #assign x values
  assign(variable_name, as.numeric(x))

  #assign y values
  assign(left_hand_side, as.numeric(y))

  #set parameters for output
  fit_parameters <- list()
  state_string <- paste(left_hand_side, "fit", sep = "_")

  #get and assign the start and static parameters from the list
  for (i in 1:(length(list_of_start_parameters))) {
    assign(names(list_of_start_parameters[i]), list_of_start_parameters[[i]])
  }
  if (length(list_of_static_parameters) > 0) {
    for (i in 1:(length(list_of_static_parameters))) {
      assign(names(list_of_static_parameters[i]), list_of_static_parameters[[i]])
    }
  }

  #calculate the initial y values for the guessed line
  y_initial <- eval(parse(text = right_hand_side))

  if (manual_check | save_plot) {

    #create empty ggplot
    current_plot <- ggplot()

    #create title
    current_plot <- current_plot + labs(title = title,
                                        x = variable_name,
                                        y = left_hand_side,
                                        subtitle = subtitle,
                                        caption = paste(left_hand_side, "~",
                                                        right_hand_side,
                                                        "\n", 'no statistics')) +
      scale_shape_manual("Data", values = c("raw_data" = 1)) +
      scale_linetype_manual("Fits", values = c("try" = "dashed", "fitted" = "solid")) +
      guides(linetype = guide_legend(override.aes = list(size = 0.5)))


    #plot raw data points
    current_plot <- current_plot + geom_point(mapping = aes(x, y,
                                                            shape = "raw_data"))

    #make a guessed line
    current_plot <- current_plot + geom_line(mapping = aes(x = x,
                                                           y = y_initial,
                                                           linetype = "try"),
                                             size = 1, alpha = 0.5)
  }

  #try the model
  model <- tryCatch(
    {
      print(formula)
      print(names(list_of_start_parameters))

      #use the nonlinear least squares function
      nls(formula = formula,
          data = environment(),
          start = list_of_start_parameters,
          control = nls.control(maxiter  = 1000,
                                warnOnly = TRUE),
          trace = TRUE,
          algorithm = "port",
          lower = lower_bounds,
          upper = upper_bounds)
    },
    error = function(e) {
      print(paste("nls gave the following error:", e))
      return("error")
    },
    warning = function(w){
      print(paste("nls gave the following warning:", w))
      return("warning")
    }
  )

  #if there is an error or warning return a list with NA values
  if (is_single_string(model)) {

    if (manual_check) {

      #show the current plot
      print(current_plot)

      print(paste("Fit gave an", model, "(type 'quit' to stop the loop)"))
      user_input <- readline("(ENTER):")

      #if the user input is quit then stop the loop
      if (user_input == "quit") {
        stop("User quit the loop")
      }
    }

    #fitstate is warning or error
    fit_state <- model

    #parameters are all NA
    statistics_parameters <- list()
    statistics_parameters[paste(left_hand_side, "RSS", sep = "_")] <- NA
    statistics_parameters[paste(left_hand_side, "TSS", sep = "_")] <- NA
    statistics_parameters[paste(left_hand_side, "Rsq", sep = "_")] <- NA
    statistics_summary <- ""
    list_of_static_parameters <- replace(list_of_static_parameters, values = NA)
    new_parameters <- replace(list_of_start_parameters, values = NA)


  } else {

    #if there is no error

    #calculate some statistics
    RSS <- sum(residuals(model)^2)   # Residual sum of squares
    TSS <- sum((y - mean(y))^2)      # Total sum of squares
    Rsq <- 1 - (RSS/TSS)             # R-squared measure

    #make a list of the statistics
    statistics_parameters <- list()
    statistics_parameters[paste(left_hand_side, "RSS", sep = "_")] <- RSS
    statistics_parameters[paste(left_hand_side, "TSS", sep = "_")] <- TSS
    statistics_parameters[paste(left_hand_side, "Rsq", sep = "_")] <- Rsq

    statistics_summary <- paste("RRS:", RSS, "TSS:", TSS, "Rsq", Rsq)

    if (manual_check | save_plot) {
      #add the statistics to the caption of the plot
      current_plot <- current_plot + labs(caption = paste(left_hand_side, "~",
                                                          right_hand_side, "\n",
                                                          statistics_summary))

      #make fitted line with the fitted parameters
      current_plot <- current_plot + geom_line(mapping = aes(x = x,
                                                             y = predict(model),
                                                             linetype = "fitted"),
                                               size = 1.3, alpha = 0.7)
    }

    if (manual_check) {

      #show the current plot
      print(current_plot)

      #ask user to check if the model is a good fit
      print("Is is a good fit? (type 'quit' to stop the loop)")
      user_input <- readline("([Y]es/[N]o):")

      #if the user input is quit then stop the loop
      if (user_input == "quit") {
        stop("User quit the loop")
      }

      if (user_input == "Y" | user_input == "y") {

        #if the fit is accepted
        print("Fit accepted")
        fit_state <- "accepted"
        new_parameters <- split(unname(coef(model)),names(coef(model)))

      } else {

        #if the fit is rejected
        print("Fit rejected")
        fit_state <- "rejected"
        list_of_static_parameters <- replace(list_of_static_parameters, values = NA)
        new_parameters <- replace(list_of_start_parameters, values = NA)
      }
    } else {

      #if the fit is not manually checked
      fit_state <- "not checked"
      new_parameters <- split(unname(coef(model)),names(coef(model)))
    }
  }

  if (save_plot) {
    #add the fit state to the plot
    current_plot <- current_plot + labs(caption = paste(left_hand_side, " ~ ",
                                                        right_hand_side, "\n",
                                                        fit_state,
                                                        ", ",
                                                        statistics_summary,
                                                        sep = ""))
    #save the plot, the name is the title
    png_filepath <- paste(save_path, subtitle, ".png", sep = "")
    ggsave(filename = png_filepath)
  }

  #add the fit state to the list
  fit_parameters[state_string] <- fit_state

  #add all parameters together
  fit_parameters <- c(fit_parameters,
                      statistics_parameters,
                      list_of_static_parameters,
                      new_parameters)

  #return a list of all the fitted parameters
  return(fit_parameters)
}
