(* ----- Test Cases ----- *)
val test10 = only_capitals ["A","B","C"] = ["A","B","C"]
val test11 = only_capitals ["A","B","C","d"] = ["A","B","C"]
val test12 = only_capitals ["A","C","d","B"] = ["A","C","B"]
val test101 = only_capitals ["a","B","C"] = ["B","C"]
val test102 = only_capitals ["Abc","ABc","abC"] = ["Abc","ABc"]
val test103 = only_capitals ["1AB","?AB","Abc","ABc","abC"] = ["Abc","ABc"]

val test20 = longest_string1 ["A","bc","C"] = "bc"
val test21 = longest_string1 ["A","bc","CBZ"] = "CBZ"
val test22 = longest_string1 [] = ""
val test23 = longest_string1 ["AB","bcd","CBZ"] = "bcd"
val test201 = longest_string1 ["A","bc","C", "de"] = "bc"
val test202 = longest_string1 ["A","bc","C", "def"] = "def"

val test30 = longest_string2 ["A","bc","C"] = "bc"
val test31 = longest_string2 ["A","bc","CBZ"] = "CBZ"
val test32 = longest_string2 [] = ""
val test33 = longest_string2 ["AB","bcd","CBZ"] = "CBZ"
val test301 = longest_string2 ["A","bc","C", "de"] = "de"
val test302 = longest_string2 ["A","bc","C", "def"] = "def"

val test4a0 = longest_string3 ["A","bc","C"] = "bc"
val test4a1 = longest_string3 ["A","bc","CBZ"] = "CBZ"
val test4a2 = longest_string3 [] = ""
val test4a3 = longest_string3 ["AB","bcd","CBZ"] = "bcd"


val test4b0 = longest_string4 ["A","B","C"] = "C"
val test4b1 = longest_string4 ["A","bc","C"] = "bc"
val test4b2 = longest_string4 ["A","bc","CBZ"] = "CBZ"
val test4b3 = longest_string4 [] = ""
val test4b4 = longest_string4 ["AB","bcd","CBZ"] = "CBZ"

val test50 = longest_capitalized ["A","bc","C"] = "A"
val test51 = longest_capitalized [] = ""
val test52 = longest_capitalized ["AB","bc","C"] = "AB"
val test53 = longest_capitalized ["AB","bc","CB"] = "AB"
val test54 = longest_capitalized ["AB","bc","CBA"] = "CBA"
val test501 = longest_capitalized [] = ""
val test502 = longest_capitalized ["ab", "a", "b"] = ""

val test60 = rev_string "abc" = "cba"
val test61 = rev_string "" = ""
val test62 = rev_string "abcdef" = "fedcba"

val test70 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test71 = first_answer (fn x => if x > 2 then SOME x else NONE) [1,2,3,4,5] = 3
val test72 = first_answer (fn x => if x < 2 then SOME x else NONE) [1,2,3,4,5] = 1
val test73 = ((first_answer (fn x => if x > 6 then SOME x else NONE) [1,2,3,4,5] = 1;
			false) handle NoAnswer => true)
val test701 = first_answer (fn x => if x > 3 then SOME x else NONE) [4,2,3,5] = 4
val test702 = (first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3] ; false) handle NoAnswer => true
val test7022 = (first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3] ; false) handle OtherException => true
val test703 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,2] = 4

val test80 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7]
	= NONE
val test81 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [2,3,4,5,6,7]
	= NONE
val test82 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,3,4,5,6,7]
	= NONE
val test83 = all_answers (fn x => if x > 0 then SOME [x] else NONE) [2,3,4,5,6,7]
	= SOME([2,3,4,5,6,7])
val test84 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,4,6]
	= SOME([2,4,6])
val test801 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [3,2,4,5,6,7] = NONE
val test802 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,4,5,6,8] = NONE
val test803 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,4,6,8] = SOME [2,4,6,8]
val test804 = all_answers (fn x => if x mod 2 = 0 then SOME [x, x + 1] else NONE) [2,4,6,8] = SOME [2,3,4,5,6,7,8,9]
val test805 = all_answers (fn x => if x mod 2 = 0 then SOME [] else NONE) [2,4,6,8] = SOME []
val test806 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [] = SOME []

val test9a0 = count_wildcards Wildcard = 1
val test9a1 = count_wildcards (TupleP [Wildcard,Wildcard]) = 2
val test9a2 = count_wildcards (TupleP [Wildcard,(Variable "test")]) = 1
val test9a3 = count_wildcards (TupleP [Wildcard,ConstructorP("c", Wildcard)]) = 2
val test9a4 = count_wildcards (TupleP [UnitP,ConstructorP("c", Wildcard)]) = 1
val test9a5 = count_wildcards (TupleP [ConstP 7,ConstructorP("c", Wildcard)]) = 1
val test9a01 = count_wildcards (Variable "str") = 0
val test9a02 = count_wildcards (TupleP [Wildcard, ConstP 12, Wildcard]) = 2
val test9a03 = count_wildcards (ConstructorP("pattern", (TupleP [Wildcard, ConstP 12, Wildcard]))) = 2

val test9b0 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b1 = count_wild_and_variable_lengths (Variable("ab")) = 2
val test9b2 = count_wild_and_variable_lengths (TupleP [Variable("ab"),Wildcard]) = 3
val test9b01 = count_wild_and_variable_lengths Wildcard = 1
val test9b02 = count_wild_and_variable_lengths (TupleP [Wildcard, ConstP 12, Wildcard]) = 2
val test9b03 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "str", Wildcard]) = 5
val test9b04 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "str", Wildcard, Variable "str2"]) = 9
val test9b05 = count_wild_and_variable_lengths (ConstructorP("pattern", (TupleP [Wildcard, ConstP 12, Wildcard]))) = 2
val test9b06 = count_wild_and_variable_lengths (ConstructorP("pattern", (TupleP [Wildcard, Variable "str", Wildcard]))) = 5

val test9c0 = count_some_var ("x", Variable("x")) = 1
val test9c1 = count_some_var ("x", TupleP [Variable("x")]) = 1
val test9c2 = count_some_var ("x", TupleP [Variable("x"),Variable("x")]) = 2
val test9c3 = count_some_var ("x", TupleP [Variable("x"),ConstructorP("a",
	Variable("x"))]) = 2
val test9c01 = count_some_var ("x", (TupleP [Wildcard, ConstP 12, Wildcard])) = 0
val test9c02 = count_some_var ("x", (TupleP [Wildcard, Variable "str", Wildcard])) = 0
val test9c03 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Wildcard])) = 1
val test9c04 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Wildcard, Variable "x"])) = 2
val test9c05 = count_some_var ("x", (ConstructorP("pattern", (TupleP [Wildcard, Variable "x", Wildcard])))) = 1
val test9c06 = count_some_var ("x", (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard])))) = 1

val test100 = check_pat (Variable("x")) = true
val test1001 = check_pat (TupleP [Wildcard, Variable "x", Wildcard]) = true
val test1002 = check_pat (TupleP [Wildcard, Variable "x", Variable "y"]) = true
val test1003 = check_pat (TupleP [Wildcard, Variable "x", Variable "x"]) = false
val test1004 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard]))) = true
val test1005 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "y")]))) = true
val test1006 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "x")]))) = false
val test1007 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "y"])]))) = true
val test1008 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "z"])]))) = true
val test1009 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "x"])]))) = false
val test1010 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "y"])))) = true
val test1011 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "x"])))) = false
val test1012 = check_pat (TupleP [Wildcard, Variable "x", TupleP [Variable "y"]]) = true
val test1013 = check_pat (TupleP [Variable("x"),Variable("x")]) = false
val test1014 = check_pat (TupleP [Variable("x"),Variable("y")]) = true
val test1015 = check_pat (TupleP []) = true
val test1016 = check_pat (TupleP [ConstructorP("c", Variable("x"))]) = true

val test110 = match (Const(1),UnitP) = NONE
val test111 = match (Const(1),ConstP(2)) = NONE
val test112 = match (Const(1),ConstP(1)) = SOME []
val test113 = match (Const(1),Variable("x")) = SOME [("x",Const(1))]
val test1101 = match (Const(1), ConstP 1) = SOME []
val test1102 = match (Const(1), Variable "s") = SOME [("s", Const(1))]
val test1103 = match (Const(1), TupleP [Wildcard]) = NONE
val test1104 = match (Const(1), TupleP [ConstP 1]) = NONE
val test1105 = match (Tuple [Unit], TupleP [UnitP]) = SOME []
val test1106 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP]]) = SOME []
val test1107 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP, Variable "x"]]) = NONE
val test1108 = match (Tuple [Const(1), Tuple [Unit]], TupleP [ConstP 1, TupleP[UnitP]]) = SOME []
val test1109 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s")]]) = SOME [("s", Const(2))]
val test1110 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 2, TupleP[UnitP, Variable("s")]]) = NONE
val test1111 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s"), Wildcard]]) = NONE
val test1112 = match (Tuple [Unit], TupleP[UnitP, Variable "x"]) = NONE

val test1200 = first_match Unit [UnitP] = SOME []
val test1201 = first_match Unit [Variable ("s")] = SOME [("s", Unit)]
val test1202 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, Variable("s")]])] = SOME [("s", Const(2))]
val test1203 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, ConstP 3]])] = NONE