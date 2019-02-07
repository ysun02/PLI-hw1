(*  COMP 323 Homework 1:  ML-ulex specification.
*
*   N. Danner
*   Spring 2019
*)

(*  The name of the generated structure.
*)
%name NumLex ;

(* Some useful abbreviations.
 *)

%let digit = [0-9] ;
%let digit1 = [1-9] ;
%let zero = 0;
%let comma = "," ;
%let num1 = {digit1}{digit} ;
%let num2 = {digit1}{digit}{digit} ;
%let num3 = {digit}{digit}{digit} ;
%let num4 = {digit1} | {num1} | {num2} ;
%let num5 = {comma}{num3} ;
%let num6 = {num5}* ;
%let num7 = {num4}{num6} ;
%let all = {zero}|{num7};

(*  Definitions for NumLex.  You may add more definitions (and probably
*  should).
*)
%defs (
  structure T = NumsTokens

  (*  The type of the result of a successful lexical analysis.
   *)
  type lex_result = T.token

  (* End-of-file function. Simplest is to return an EOF token, which the
   * parser can deal with.
   *)
  
  fun string2Int x =
    case String.explode(x) of
      [] => 0
    | y::ys =>
        let
          val len = String.size(x)
        in
          if y <> #"," then (valOf (Int.fromString(Char.toString(y)))) * Real.trunc(Math.pow(10.0, Real.fromInt (len-(len div 4)-1))) + string2Int (String.implode(ys))
          else string2Int (String.implode(ys))
        end
  
  fun eof() = T.EOF
) ;

{all} => (T.Num ( string2Int yytext )) ;
(*  Add your rule here.

*)
