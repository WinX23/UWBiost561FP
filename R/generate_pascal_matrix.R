#' Generate Pascal's triangle in matrix form
#'
#' @param n a positive integer, number of rows and columns
#' @param position  either "lower" for an lower-triangular matrix, the default, or "upper" for an upper-triangular matrix
#'
#' @return a nxn Pascal matrix
#' @export
#' @examples
#' matrix <- generate_tri_pascal_matrix(20)
#' matrix
generate_tri_pascal_matrix <- function(n, position = "lower") {

  # check inputs are correct
  stopifnot(n %% 1 == 0, n > 0,
            position == "lower" || position == "upper")

  if (n == 1) {
    m <- matrix(1, nrow = 1, ncol = 1)

  } else if (position == "lower") {

    m <- matrix(0, nrow = n, ncol = n)

    # subdiagonal entry
    for (i in 2:n) {
      m[i, i-1] <- i-1
    }

    # calculate the matrix exponential
    m <- expm::expm(m)
  } else {
    m <- matrix(0, nrow = n, ncol = n)

    # superdiagonal entry
    for (i in 1:n-1) {
      m[i, i+1] <- i
    }

    m <- expm::expm(m)
  }
  m <- round(m)
  return(m)
}


#' Generate a symmetric pascal matrix
#'
#' @param n a positive integer, number of rows and columns
#'
#' @return a nxn symmetric pascal matrix
#' @export
#' @examples
#' matrix <- generate_symm_pascal_matrix(15)
#' matrix
generate_symm_pascal_matrix <- function(n) {
  # check n is correct
  stopifnot(n %% 1 == 0, n > 0)

  if (n == 1) {
    m <- matrix(1, nrow = 1, ncol = 1)

  } else {
    m1 <- matrix(0, nrow = n, ncol = n)
    m2 <- matrix(0, nrow = n, ncol = n)

    # subdiagonal entry
    for (i in 2:n) {
      m1[i, i-1] <- i-1
    }

    # superdiagonal entry
    for (i in 1:n-1) {
      m2[i, i+1] <- i
    }

    m <- expm::expm(m1) %*% expm::expm(m2)
  }
  m <- round(m)
  return(m)
}

#' Plot Sierpiński triangle based on pascal matrix
#'
#' @param m a pascal matrix
#'
#' @return a plot with Sierpiński triangle
#' @export
#' @examples
#' m <- generate_tri_pascal_matrix(10)
#' plot <- pascal_matrix_plot(m)
#' plot
pascal_matrix_plot <- function(m) {

  # assign odds to 1, even to 0
  m_odd_df <- dplyr::as_tibble(m)
  m_odd_df <- ifelse(m_odd_df %% 2 == 1, 1, 0)

  # add row numbers as new column
  m_odd_df <- as.data.frame(m_odd_df)
  m_odd_df <- dplyr::mutate(m_odd_df, Row = dplyr::row_number())

  # convert to long format
  m_odd_long <- tidyr::pivot_longer(m_odd_df,
                                    cols = !Row,
                                    names_to = "Column",
                                    values_to = "Value")

  m_odd_long <- dplyr::mutate(m_odd_long, Column = as.numeric(gsub("V", "", Column)))

  # subset of odds
  m_odd_use <- subset(m_odd_long, Value == 1)

  plot <- ggplot2::ggplot(data = m_odd_use,
                        ggplot2::aes(x = Column, y = Row)) +
    ggplot2::geom_point(color = "darkblue") +
    ggplot2::theme_bw() +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::labs(x = NULL, y = NULL)

  return(plot)

}

