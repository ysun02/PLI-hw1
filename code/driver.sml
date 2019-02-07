(*  COMP 323 Homework 1:  lexical analysis.
*
*   N. Danner
*   Spring 2019
*   
*   Upon building this driver, it must be executed as follows:
*
*       $ ./driver lex e
*
*   where e is a sequence of characters upon which to perform lexical analysis.
*   It is recommended to surround the string with double-quotes; e.g.,
*
*       $ ./driver lex "234,542"
*
*   If e can be lexed, then the token stream that results is printed to the
*   terminal (using NumsTokens.toString).
*
*   N. Danner
*)

structure Main =
struct

  structure Lex = NumLex
  structure Toks = NumsTokens

  type token = Toks.token

  type strm = Lex.strm

  fun lex strm = 
    case Lex.lex (AntlrStreamPos.mkSourcemap()) strm of
         (t, _, s) => (t, s)

  structure T = TextIO

  (*  printnl s = ().
  *
  *   As a side-effect, s will be printed to the terminal followed by a newline.
  *)
  fun printnl(s : string) : unit =
    print (String.concat [s, "\n"])

  (* doLex strm = ()
  *
  * Side-effect:  prints tokens from strm to terminal until EOF reached.
  *)
  fun doLex (strm : Lex.strm) : unit =
  let
    fun printTokens (strm : Lex.strm) : unit =
      case lex strm of
           (Toks.EOF, _) => printnl (Toks.toString Toks.EOF)
         | (t, strm) => print ((Toks.toString t) ^ " ") 
                           before printTokens strm
  in
    printTokens strm
  end

  structure M = SplayMapFn(
    struct type ord_key = string val compare = String.compare end : ORD_KEY)

  exception Usage
  val usage = String.concatWith "\n" [
    "driver cmd e",
    "",
    "Process the string e according to cmd.  Possible values of cmd are:",
    "\tlex:      perform lexical analysis and print the token sequence.",
    "\n"
  ]

  fun main(arg0 : string, argv : string list) : int =
  let

    val expHandlers = [
      ("lex", doLex o #2)
    ]

    val makeHandlerMap =
      foldr (fn ((cmd, hndlr), m) => M.insert(m, cmd, hndlr)) M.empty

    val expHandlerMap = makeHandlerMap expHandlers

    val streamFromString = (Lex.streamifyInstream o TextIO.openString)

    val stream = ref (streamFromString)
    val handlerMap = ref(expHandlerMap)

    val cmd :: optsArgs = argv

    (*  handleOpt : handle a single option by setting stream or parser
    *   appropriately.
    *
    *   Pre-condition:  oa = "--" ^ oa'.
    *)
    fun handleOpt (oa : string) : unit =
    let
    in
      case String.substring(oa, 2, String.size oa - 2) of
           "arg" => stream := streamFromString
         | "expr" => handlerMap := expHandlerMap
         | _ => raise Usage
    end

    (*  handleOpts : handle all options by calling handleOpt o for each option o
    *   on the command line.
    *)
    fun handleOpts (optsargs : string list) : string list =
    let
    in
      case optsargs of
           [] => []
         | oa :: oas =>
             if String.isPrefix "--" oa then (handleOpt oa ; handleOpts oas)
             else oa :: oas
    end

    val [arg] = handleOpts optsArgs

    val hndlr = valOf(M.find(!handlerMap, cmd))

  in
    BackTrace.monitor (fn () => ((hndlr (arg, !stream arg)) ; 0))
  end
  handle 
    (* Usage errors *)
      Usage => (print usage ; 1)
    | Bind => (print usage ; 1)
    | Option => (print usage ; 1)
    (* I/O errors *)
    | e as IO.Io {name=name, function=_, cause=cause} => 
        (printnl (String.concatWith " " [
          "I/O error reading",
          name,
          "(",
          exnMessage cause,
          ")"
        ]) ; 1)

end
