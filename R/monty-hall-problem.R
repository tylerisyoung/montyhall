#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export #
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title Contestant door selection 
#' @description Here the contestant chooses 1/3 doors 
#' @details first door selection 
#' @param this door is used to determine which other door should be opened by the host to reveal the goat behind it
#' @return  return a number 1, 2, or 3. 
#' @examples : 2
#' @export #
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title Opening the goat door
#' @description Here the host of the game reveals a door which was not picked by the contestant 
#' @details the door reveals a goat 
#' @param if the contestant has selected the car door, the host can open either of the two remaining doors, if the contestant has initially picked a goat door, the doors will revel the remaining goat
#'
#' @return  return a number 1, 2, or 3. 
#' @examples : 3
#' @export #
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title Change Door?
#' @description Here the contestant has the opportunity to change their pick to the only remaining closed door, or stick with their current and first pick. 
#' @details If the contestant stays, that door is revealed, if they switch, the door they switch to will be revealed in the next step
#' @param Stay=T means the contestant has stayed with their original pick, when stay=F, they switch their pick to the remaining door
#' @return return a number 1, 2, or 3. 
#' @examples : 1
#' @export #
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title Reveal 
#' @description The host reveals the door the contestant has picked if it is a car or a goat
#' @details The car choice is the winning choice, and the goat is the loosing choice
#' @param The initial door saetup reveals which door the contestant has chosen to pick and either stick with or change to the remaining mysterious door
#' @return : WIN/LOSE 
#' @examples : WIN  
#' @export #
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title Whole Game
#' @description This is the whole game wrapped up into a single package 
#' @details  #
#' @param #
#' @return WIN/LOSE
#' @examples #
#' @export #
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Repitition
#' @description This function allows the game to be repeated to see the outcome percentages over time
#' @details n specifies the number of times the game will be simulated, and a table of the results will be provided
#' @param Proportional results will be given as to the strategy that the contestant has picked (stay or switch) 
#' @return Same result of WIN/LOSE
#' @examples WIN
#' @export #
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
