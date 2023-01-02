#' A function that fits multiple curves with nls.
#'
#' @description
#'   A function that fits multiple curves with nls.
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
#'                             save_plot = TRUE)
#'
fit_multiple_curves <- function(x,
                                y,
                                formula_list = list(as.formula(y ~ a * x + b),
                                                    as.formula(y ~ a * x^2 + b * x + c)),
                                variable_name = "x",
                                list_of_start_parameters_lists = list(list(a = 0, b = 10),
                                                                      list(a = 0, b = 0, c = 12)),
                                list_of_static_parameters_lists = list(list(),list()),
                                title = "random fit",
                                subtitle = "no filename",
                                show_try = FALSE,
                                save_plot = FALSE,
                                save_path = "output_directory_licorfiles/plots/",
                                list_of_lower_bounds = list(NULL, NULL),
                                list_of_upper_bounds = list(NULL, NULL)) {

  #create an empty list for the parameter lists
  plot_dataframe <- data.frame()
  plot_dataframe_try <- data.frame()
  list_of_fit_parameters <- list()

  for (index in 1:length(formula_list)) {
    print(index)

    #loop trough the formula list
    formula <- formula_list[[index]]
    list_of_start_parameters <- list_of_start_parameters_lists[[index]]
    list_of_static_parameters <- list_of_static_parameters_lists[[index]]
    lower_bounds <- list_of_lower_bounds[[index]]
    upper_bounds <- list_of_upper_bounds[[index]]

    #get the formula strings
    left_hand_side <- as.character(formula[2])
    right_hand_side <- as.character(formula[3])

    #assign x values
    assign(variable_name, as.numeric(x))

    #assign y values
    assign(left_hand_side, as.numeric(y))

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

    fit_number <- paste("fit", as.character(index), sep = "")
    if (show_try) {
      #make a guessed line
      current_dataframe <- data.frame(fit = fit_number, x = x, y = y_initial)
      plot_dataframe_try <- rbind(plot_dataframe_try, current_dataframe)
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
      current_dataframe <- data.frame(fit = fit_number, x = x, y = NA)


    } else {

      #if there is no error
      fit_state <- "no error"

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

      #get the new parameters
      new_parameters <- split(unname(coef(model)),names(coef(model)))
      current_dataframe <- data.frame(fit = fit_number, x = x, y = predict(model))
    }

    #set parameters for output
    fit_parameters <- list()
    state_string <- paste(left_hand_side, "fit", sep = "_")

    #add the fit state to the list
    fit_parameters[state_string] <- fit_state

    #add all parameters together
    fit_parameters <- c(fit_parameters,
                        statistics_parameters,
                        list_of_static_parameters,
                        new_parameters)


    plot_dataframe <- rbind(plot_dataframe, current_dataframe)

    #return a list of all the fitted parameters
    list_of_fit_parameters <- c(list_of_fit_parameters, list(fit_parameters))

  }

  #create ggplot, title and raw datapoints
  current_plot <- ggplot(environment = environment()) +
    labs(title = title, x = variable_name, y = left_hand_side, subtitle = subtitle) +
    scale_shape_manual("Data", values = c("raw_data" = 1)) +
    scale_linetype_manual("Type", values = c("try" = "dashed", "fitted" = "solid")) +
    #scale_color_manual("Fits", values = c("fit1" = "blue", "fit2" = "red")) +
    guides(linetype = guide_legend(override.aes = list(size = 0.5))) +
    geom_point(mapping = aes(x, y, shape = "raw_data"))

  if (show_try) {
    #make a guessed line
    current_plot <- current_plot + geom_line(data = plot_dataframe_try,
                                             mapping = aes(x = x,
                                                           y = y,
                                                           linetype = "try",
                                                           color = fit),
                                             size = 1)
    #Note: adding an alpha = 0.5 results in a legend drawing bug on Windows
  }

  #make fitted line with the fitted parameters
  current_plot <- current_plot + geom_line(data = plot_dataframe,
                                           mapping = aes(x = x,
                                                         y = y,
                                                         linetype = "fitted",
                                                         color = fit),
                                           size = 1.3)

  #show the current plot
  plot(current_plot)

  #ask user to check if the model is a good fit
  print(paste("Which is a good fit? (1:", length(formula_list), " or [N]one)", sep = ""))
  user_input <- readline()

  none_vector <- c("N", "n", "None", "none", "error", "enter", "")
  if(user_input %in% none_vector) {
    fit_parameters <- replace(list_of_fit_parameters[[1]], values = NA)

    #add the caption to the plot
    current_plot <- current_plot + labs(caption = paste("chosen fit = ",
                                                        "None",
                                                        ", ",
                                                        "No formula",
                                                        "\n",
                                                        "error",
                                                        ", R2 = ",
                                                        "NA",
                                                        sep = ""))

  } else if ((as.integer(user_input) >= 1) && (as.integer(user_input) <= length(formula_list))) {

    chosen_number <- as.integer(user_input)
    fit_parameters <- list_of_fit_parameters[[chosen_number]]

    #add the caption to the plot
    current_plot <- current_plot + labs(caption = paste("chosen fit = ",
                                                        chosen_number,
                                                        ", ",
                                                        formula_list[chosen_number],
                                                        "\n",
                                                        fit_parameters[1],
                                                        ", R2 = ",
                                                        fit_parameters[4],
                                                        sep = ""))
  } else {
    stop("not a valid input given")
  }

  if (save_plot) {
    #save the plot, the name is the title
    png_filepath <- paste(save_path, subtitle, ".png", sep = "")
    ggsave(filename = png_filepath)
  }


  #return the parameters of the best fit
  return(fit_parameters)
}
