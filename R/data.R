#' Przykładowe dane z testu matematycznego
#'
#' Zbiór danych zawierający odpowiedzi uczniów na zadania z testu
#' matematycznego. Dane zawierają odpowiedzi binarne (0/1) na 48 zadań
#' oraz informacje identyfikacyjne.
#'
#' @format Ramka danych o 2448 wierszach i 52 kolumnach:
#' \describe{
#'   \item{id_szkoly}{Identyfikator szkoły}
#'   \item{id_ucznia}{Identyfikator ucznia}
#'   \item{pomiar}{Numer pomiaru}
#'   \item{grupa}{Grupa}
#'   \item{mat_1, mat_2, ..., mat_48}{Odpowiedzi na zadania matematyczne
#'     (0 = błędna, 1 = poprawna, NA = brak odpowiedzi)}
#' }
"math_data"
