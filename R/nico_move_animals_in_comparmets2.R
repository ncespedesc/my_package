


#' @title Move animals among compartments
#' @description this function move the cattle pig and small ruminand ccording with the day and  SEIR compartment
#' @param movements dataframe with the movements
#'
#' @param linha day of the movemnts
#'
#' @return
#' @export
#'
#' @examples#'
#' > nico_move_animals_in_comparmets(movements = movements, 1)
#' date event_type        from          to   host number From To pop_S_ori pop_E_ori pop_I_ori pop_R_ori pop_S_move pop_E_move pop_I_move pop_R_move
#' 136    1          1 43015003571 43001002076 Bovine      3  236  1       100       100       100       100          1          0          2          0
nico_move_animals_in_comparmets <- function(movements, linha){

  #select farms with more that 0 animals in all compartments
  aux <-  movements[linha, c("pop_S_ori", "pop_E_ori","pop_I_ori", "pop_R_ori")] %>%
    select_if(~ !is.numeric(.) || sum(.) != 0)

  # gerenates a ranmdon vector with the animals to move from the compartments
  number_of_remo <-  nico_rand_vect(ncol(aux),  movements[linha, "number"], pos.only = TRUE)
  names(number_of_remo) <- names(aux)

  # Suceptibles
  if ("pop_S_ori" %in% names(number_of_remo) ) {
    movements[linha,]$pop_S_move  <- number_of_remo["pop_S_ori"]}
  if (!"pop_S_ori" %in% names(number_of_remo) ) {
    movements[linha,]$pop_S_move <- 0 }
  # Exposed
  if ("pop_E_ori" %in% names(number_of_remo) ) {
    movements[linha,]$pop_E_move  <- number_of_remo["pop_E_ori"]}
  if (!"pop_E_ori" %in% names(number_of_remo) ) {
    movements[linha,]$pop_E_move <- 0 }
  # Infected
  if ("pop_I_ori" %in% names(number_of_remo) ) {
    movements[linha,]$pop_I_move  <- number_of_remo["pop_I_ori"]}
  if (!"pop_I_ori" %in% names(number_of_remo) ) {
    movements[linha,]$pop_I_move <- 0 }
  # Recovered
  if ("pop_R_ori" %in% names(number_of_remo) ) {
    movements[linha,]$pop_R_move  <- number_of_remo["pop_R_ori"]}
  if (!"pop_R_ori" %in% names(number_of_remo) ) {
    movements[linha,]$pop_R_move <- 0 }


  return(movements[linha, ])
}
