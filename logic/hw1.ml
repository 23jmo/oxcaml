open! Core

type color =
  | Red | Blue | Green | Yellow | Purple | Orange

type player =
  | Player1
  | Player2

type board = color array array
type territory = bool array array

type state = {
  turn : player;
  board : board;
  p1 : territory;
  p2 : territory;
}

type winner =
  | No_winner
  | Winner of player
  | Tie

type move = color

type move_result = {
  new_state : state;
  winner : winner;
}