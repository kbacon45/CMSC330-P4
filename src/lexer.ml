open TokenTypes



let specialToken inputStr = let intType = Re.Perl.compile_pat "^-?[0-9]+$" in 
let idType = Re.Perl.compile_pat "^[a-zA-Z][a-zA-Z0-9]*$" in 
let intCheck = Re.execp intType inputStr in match intCheck with 
| true -> Tok_Int (int_of_string inputStr)
| false -> ( let idCheck = Re.execp idType inputStr in match idCheck with
| true -> Tok_ID inputStr
| false -> EOF
)


let validToken inputStr = match inputStr with
| "(" -> Tok_LParen
| ")" -> Tok_RParen
| "{" -> Tok_LBrace
| "}" -> Tok_RBrace
| "==" -> Tok_Equal
| "!=" -> Tok_NotEqual
| "=" -> Tok_Assign
| ">" -> Tok_Greater
| "<" -> Tok_Less
| "<=" -> Tok_LessEqual
| ">=" -> Tok_GreaterEqual
| "||" -> Tok_Or
| "&&" -> Tok_And
| "!" -> Tok_Not
| ";" -> Tok_Semi
| "int" -> Tok_Int_Type
| "bool" -> Tok_Bool_Type
| "printf" -> Tok_Print
| "main" -> Tok_Main
| "if" -> Tok_If
| "else" -> Tok_Else
| "for" -> Tok_For
| "from" -> Tok_From
| "to" -> Tok_To
| "while" -> Tok_While
| "+" -> Tok_Add
| "-" -> Tok_Sub
| "*" -> Tok_Mult
| "/" -> Tok_Div
| "^" -> Tok_Pow
| "true" -> Tok_Bool true
| "false" -> Tok_Bool false
| _ -> (specialToken inputStr)




let rec crawl input start ender = if (ender > (String.length input)) 
  then input  
  else ( let inputRecur = (crawl input start (ender+1)) in
  if (validToken inputRecur) = EOF
  then String.sub input 0 ((String.length inputRecur) - 1)
else inputRecur 
  )






let rec nextDiff input checkLst switch = if (String.length input > 0)
  then (

  if ( if switch then List.mem input.[0] checkLst else not (List.mem input.[0] checkLst) )
  then input
  else nextDiff (String.sub input 1 ((String.length input) - 1)) checkLst switch

  )

else input





let rec tokenizeHelp (input : string) (lst : token list) : token list =
  if input = "" then 
    lst @ [EOF]
  else 
    match input.[0] with
    | '\t' | '\n' | ' ' -> 
        tokenizeHelp (nextDiff input [' '; '\n'; '\t'] false) lst
    | _ -> 
        let newStr = (crawl input 0 1) in if newStr = "" then raise (InvalidInputException "This is an invalid input!") 
        else let newTok = String.sub input 0 (String.length newStr) in
        tokenizeHelp (String.sub input (String.length newStr) 
        (String.length input - String.length newStr)) (lst @ [validToken newTok])


let tokenize input = tokenizeHelp input []
  
