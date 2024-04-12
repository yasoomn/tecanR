ang = c("en", "de")


cycle_strings = c("Zyklus Nr.", "Cycle Nr.")

find_skip = function(table) {
  match(cycle_strings[1], table[, 1])
}

#' Read the Tecan output
#'
#' This function reads the xlsx file produced from the Tecan plate reader. It assumes
#' a kinetic measurement with more than 1 cycle.
#' @param file The path to the measurement file.
#' @param sheet Select the sheet to be used. Defaults to 1.
#' @param lang Language of the Tecan report. Defaults to English.
#' @export
read.tecan = function(file, sheet = 1, lang = "en") {
  # This function takes the file location of the tecan file and the desired sheet,
  #outputs the data without the header as a data.frame
  sprdx = readxl::read_xlsx(file, skip = 0, sheet = sheet)
  sprdx = readxl::read_xlsx(file, skip = find_skip(data.frame(sprdx)), sheet = sheet)

  # remove last 5 rows, contains more comments
  sprdx = sprdx %>% dplyr::slice_head(n = nrow(sprdx)-5)
  data.frame(sprdx, row.names = 1) # Converts tibble to data.frame
}

#' @export
elongate.tecan = function(data, labels = NULL, sep = ";", labels.names = c()) {

   # Separate rows with temperature and time
  data.info <- data[1:2, ] %>%
    t() %>%
    as.data.frame()
  # Add column with cycle number
  data.info$cycle <- c(1:nrow(data.info))
  # rename columns
  names(data.info) <- c("time", "temp", "cycle")

  # Separate rows with measurements
  data.values <- data %>% dplyr::slice_tail(n = nrow(data) - 2) %>% t() %>% as.data.frame()
  # How many wells were used?
  wellsN = data %>% dplyr::slice_tail(n = nrow(data) - 2) %>% nrow()
  data.values$cycle <- c(1:nrow(data.values))
  data.values.long <- data.values %>% tidyr::pivot_longer(cols = 1:wellsN)

  # Combine both
  data.long <- dplyr::full_join(data.info, data.values.long, by = "cycle")


  names(data.long) <- c("time", "temp", "cycle", "well", "measurement")

  # Create the X and Y positions of the wells  as separate columns
  data.long <- data.long %>%
    dplyr::rowwise() %>%
    dplyr::mutate(well_x = substr(well, 2, 3), well_y = substr(well, 1, 1))

  if (!is.null(labels)) {
    ## TODO: move this code in another function, elongate.tecan should accept a
    ## data.frame instead of a file path

    # option to add custom labes to each well. If more than one label needs to be added, separate it with
    # semicolons.
    plate = read.csv(labels, header = T, row.names = 1, colClasses = "character")
    # create a data.frame with the correct dimensions and a well column and add the labels
    plate_long <- data.frame(well = paste(rep(LETTERS[1:8], each = 12), 1:12, sep = ""),
                             label = tidyr::pivot_longer(plate, values_to = "label", cols = 1:12)$label)
    ## Previous inner_join
    data.long <- dplyr::inner_join(data.long, plate_long, by = "well")
    ## Separate labels if there are multiple variables
    data.long <- data.long %>%
      separate_wider_delim(cols = "label", names = labels.names, delim = sep)
  }
  data.long

}
