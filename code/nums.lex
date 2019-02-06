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
%let zero = "0" ;       ??????
%let comma = "," ;      ??????
%let num1 = {digit1}{digit}
%let num2 = {digit1}{digit}{digit}
%let num3 = {comma}{num2}
%let num4 = {digit1} | {num1} | {num2}
%let num5 = {num4}{num3}
%let all = {zero} | {num5}
%let ws=[\n\t\ ] ;

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
  fun eof() = T.EOF
) ;

(*  Add your rule here.
{all} => (T.Num) ;
{ws}  => ( skip() ) ;
.     => ( raise Fail ( "Unexpected character: " ^ yytext )) ;
*)
