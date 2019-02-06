(*  COMP 323 Homework 1:  ML-Ulex specification tests.
*
*   N. Danner
*   Spring 2019
*)

structure Main =
struct

  structure U = UnitTest
  structure TR = TestRunner

  structure Lex = NumLex
  structure T = NumsTokens

  (*  parse s = n, where n is the integer represented by s.  s is assumed
  *   to be a string of digits written in groups of three separated by 
  *   commas with no leading zeros.
  *
  *   If s does not have the correct form, an error is raised with a message
  *   indicating the initial segment of s that could be lexed.
  *)
  fun parse(s : string) : int =
  let
    val sm = AntlrStreamPos.mkSourcemap()
    val lex = Lex.lex sm

    val strm = Lex.streamifyInstream (TextIO.openString s)

    val (T.Num n, _, strm) = lex strm
    val (tok, _, _) = lex strm

  in

    case tok of
         T.EOF => n
       | _ => 
             let
               val msg = String.concat [
                 "Parsed ",
                 Int.toString n,
                 ", but then had extra tokens."
               ]
             in
               raise (Fail msg)
             end

  end

  (*  testGoodLex(s, exp) is a test that succeeds if parse s = exp.
  *)
  fun testGoodLex(s : string, exp : int) : U.test =
    U.assertEqInt(s,
      fn () => parse(s),
      exp)

  (*  testBadLex s is a test that succeeds if parse s raises Fail or Bind.
  *)
  fun testBadLex(s : string) : U.test =
    U.orTest(s, [
      U.assertExn("Raise Fail (stuck state or extra tokens)",
        fn () => parse s,
        Fail ""),
      U.assertExn("Raise Bind (lex does not yield a Num)",
        fn () => parse s,
        Bind)
      ])

  val tests = [
    ("Good numbers", [
      testGoodLex("0", 0),
      testGoodLex("1", 1),
      testGoodLex("12", 12),
      testGoodLex("123", 123),
      testGoodLex("1,234", 1234),
      testGoodLex("12,345", 12345),
      testGoodLex("123,456", 123456),
      testGoodLex("1,234,567", 1234567),
      testGoodLex("12,345,678", 12345678),
      testGoodLex("123,456,789", 123456789)
    ]),

    ("Bad numbers", [
      testBadLex(""),
      testBadLex("1,2"),
      testBadLex("1234"),
      testBadLex("123,45"),
      testBadLex("123_456"),
      testBadLex("123$456")
    ])
  ]

  (*  main function for the executable. *)
  fun main(arg0 : string, argv : string list) : int =
    TR.runTimedTestSuites (tests, 60, true)

end
