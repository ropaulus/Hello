
#' @title
#' Hellos !
#'
#' @description
#' ´hello will say hello in different language.´
#'
#' @usage
#' hello(who, lang = "ENG", LangData = language)
#'
#' @param
#' who a `character` vector of length 1 that specifies the name of the person to whom the message is addressed.
#' @param
#' lang a `character` vector of length 1 that specifies the preferred language. Default to "EN" for English. Other possible values include "FR" for French, "IT"  for Italian, "ES" for Spanish, or "DE" for German. Case is ignored.
#' @param
#' LangData an optional data.frame with two columns each of mode `character`. The first column gives the language codes and the second column gives the corresponding "hello" word. Default to `language`.
#'
#' @return a personalized hello message
#'
#' @examples
#' hello("James")
#'
#'
#' @export
#' Hello World
#'
#' `hello` says _"hello"_ in the user-specified language. The user is asked to give her/his name so that the hello message gets personalized.
#'
#' @param
#' who a `character` vector of length 1 that specifies the name of the person to whom the message is addressed.
#'
#' @param
#' lang a `character` vector of length 1 that specifies the preferred language. Default to "EN" for English. Other possible values include "FR" for French, "IT"  for Italian, "ES" for Spanish, or "DE" for German. Case is ignored.
#'
#' @param
#' LangData an optional data.frame with two columns each of mode `character`. The first column gives the language codes and the second column gives the corresponding "hello" word. Default to `language`.
#'
#' see `?language`
#'
#' @return
#' a `character` vector with a personalized _"hello"_ message.
#'
#' @examples
#' hello("James")
#' hello("Amelia", "Es")
#'
#' @export
#'
hello <- function(who, lang = "EN", LangData = TESTO::language) {
  if (!exists("who", mode = "character") | length(who) > 1) {
    stop("Please enter a valid namea; see ?hello")
  }

  LangData <- data.frame(LangData)

  if (ncol(LangData) > 2) {
    stop("Please enter a valid LangData; see ?hello")
  }

  colnames(LangData) <- c("code", "hello")

  if ((mode(LangData$code) != "character") | (mode(LangData$hello) != "character")) {
    stop("Please enter a valid LangData; see ?hello")
  }

  llang <- tolower(lang)

  hello <- subset(LangData, LangData$code == llang)[[2]]

  ifelse(
    length(hello) == 1,
    paste0(hello, ", ", who, "!", sep = ""),
    paste0("Sorry, ", who, ", ", "your language ", "('", lang, "') ", "is not available!", sep = "")
  ) |> cat()
}
