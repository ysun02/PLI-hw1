(*  COMP 323 Homework 1:  lexical analysis.
*   
*   Token definition.
*
*   N. Danner
*   Spring 2019
*)

structure NumsTokens =
struct

  (*  The type of tokens.
  *)
  datatype token = Num of int | EOF

  (*  toString t = a string representation of t.
  *)
  fun toString(t : token) : string =
    case t of
         Num n => String.concat ["Num(", Int.toString n, ")"]
       | EOF => "EOF"

end
