#' A function that analyses a licorfile for visual examination
#'
#' @description
#'   A function that analyses a licorfile for visual examination.
#'   Gives a quick overview of the data.
#' @author Sam Loontjens
#' @param pathname The licor file that will be analysed.
#' @param parameters A vector of the recuired parameters
#' @param x_axis A vector with the string of the x_axis name
#' @param exclude A vector of parameters to exclude from the normalized plot
#' @param calculate_means A boolean if the means should be calculated and returned
#' @param plot_individuals A boolean that regulates if all the individuals are plotted
#' @export
#' @return
#' Returns a list of means if calculate means is true
#' @examples
#'
analyse_licorfile <- function(pathname,
                              parameters = c('elapsed', 'A', 'Ci', 'Qin',
                                             'TleafCnd', "VPDleaf", "RHcham",
                                             "CO2_r", "gsw"),
                              x_axis = c('elapsed'),
                              exclude = c("CO2_r"),
                              calculate_means = FALSE,
                              plot_individuals = TRUE) {

  #read the licor file
  dataframe <- read_licorfile(filepath = pathname, parameters = parameters)

  #make an empty list
  mean_list <- list()

  #for each parameter
  for (i in parameters[!parameters %in% x_axis]) {

    #make a plot
    current_ggplot <- ggplot(data = dataframe, mapping = aes_string(x = "elapsed", y = i)) +
      geom_point()

    if (calculate_means) {
      #calculate the means
      current_mean <- mean(dataframe[[i]])

      #add it to the list
      mean_list[[i]] <- current_mean

      #plot the means
      current_ggplot <- current_ggplot + geom_hline(yintercept = current_mean)
    }

    if (plot_individuals) {
      plot(current_ggplot)

      #ask for an ENTER to continue
      readline(prompt = ("(press ENTER to continue):"))
    }
  }

  #make a normalized dataframe with the right length
  normalized_dataframe <- data.frame("ID" = 1:length(dataframe$elapsed))

  #add all the normalized data
  for (i in names(dataframe[!names(dataframe) %in% x_axis])) {
    column <- dataframe[[i]]
    normalized_dataframe[i] <- normalize_column(column, na.rm = TRUE)
  }

  #remove the excluded
  normalized_dataframe <- normalized_dataframe[!names(dataframe) %in% exclude]

  #add the x_axis
  normalized_dataframe[x_axis] <- dataframe[[x_axis]]

  #remove the ID tags
  normalized_dataframe <- normalized_dataframe[!names(normalized_dataframe) %in% "ID"]

  #plot the data
  normalized_dataframe <- reshape2::melt(normalized_dataframe,  id.vars = 'elapsed', variable.name = 'series')
  plot(ggplot(normalized_dataframe, aes(elapsed, value)) + geom_line(aes(colour = series)))

  #ask for ENTER to continue
  readline(prompt = ("(press ENTER to continue):"))

  return(mean_list)
}
