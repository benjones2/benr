#' Dice
#'
#' @param dietype The type of die.
#' @param probvalues The probability of getting a particular number of divots.
#'
#' @return The sum of two dice.
#' @export
#'
#' @examples
#' set.seed(123)
#' roll()
#' roll(1:20)
#'
roll <- function(dietype = 1:6, probvalues = rep(1/length(dietype), length(dietype))){
  if(sum(probvalues) != 1)
    stop("probvalues must add to one")
  dice <- sample(dietype, size = 2, replace = TRUE, prob = probvalues)
  sum(dice)
}
#' Squaring Numbers
#'
#' @param x Must be a numeric object.
#'
#' @return Squares the values in the numeric object.
#' @export
#'
#' @examples
#' sq(c(1,3,5))
#' sq(1:10)
sq <- function(x){
  if(typeof(x) != "double" & typeof(x) != "integer")
    stop("Power arg is not numeric")
  x^2
}
#' Power Function
#'
#' @param x Must be a numeric object.
#' @param power A numeric object called \code{power}.
#'
#' @return Raises values in numeric object \code{x} to value of \code{power}.
#' @export
#'
#' @examples
#' RP(c(1,3,5), 3)
#' RP(x = c(1,3,5), power = 3)
#' RP(power = 2, x = c(1, 3, 5))
RP <- function(x, power){
  if(typeof(power) != "double")
    stop("Power arg is not numeric")
  if(typeof(x) != "double")
    stop("Vector arg is not numeric")
  x^power
}


#' Shuffle Function
#'
#' @param cards vector or array
#'
#' @return cards in a random order
#' @export
#'
#' @examples
#' site <- "https://gist.githubusercontent.com/garrettgman/9629323/raw/ee5dfc039fd581cb467cc69c226ea2524913c3d8/deck.csv"
#' cards <- readr::read_csv(site)
#' shuffle(cards)
shuffle <- function(cards){
  index <- sample(dim(cards)[1], size = dim(cards)[1], replace = FALSE)
  cards[index, ]
}
