open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

  let lookahead2 (toks : token list) : token =
    match toks with
    | [] -> EOF
    | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)


let rec findTok tokLst tok idx = match tokLst with
  | [] -> -1
  | curr::rest -> if curr = tok then idx else findTok rest tok (idx + 1)

  let rec findTokExp tokLst tok idx track = match tokLst with
  | [] -> -1
  | curr::rest -> ( match curr with
  | Tok_Semi -> -1
  | Tok_To -> -1
  | Tok_LParen -> findTokExp rest tok (idx + 1) (track+1)
  | Tok_RParen -> if track <= 0 then -1 else findTokExp rest tok (idx + 1) (track-1)
  | _ -> if (curr = tok && track <= 0 ) then idx else findTokExp rest tok (idx + 1) track
  )
    
    
   

let rec splitAtIdx lst accum idx targ = match lst with
  | [] -> ([], [])
  | curr::rest -> 
      if idx = targ then (accum, rest)
      else splitAtIdx rest (accum@[curr]) (idx + 1) targ


let rec parse_expr toks : expr_result = parseOr toks


and parsePrim tokLst = match tokLst with
  | [] -> raise (InvalidInputException "No more tokens")
  | curr::rest -> (match curr with
    | Tok_Bool tf -> (rest, Bool tf)
    | Tok_Int num -> (rest, Int num)
    | Tok_ID id -> (rest, ID id)
    | Tok_LParen -> 
        let (a, b) = parse_expr rest in
        (match_token a Tok_RParen, b)
    | _ -> raise (InvalidInputException "Invalid token encountered")
  )


and parseUn tokLst = 
  let num = findTokExp tokLst Tok_Not 0 0 in
  if num = 0 then 
    let tupSplit = splitAtIdx tokLst [] 0 num in
    match tupSplit with
    | (a, b) -> 
        let resTup = parseUn b in
        match resTup with
        | (c, d) -> (c, Not d)
  else parsePrim tokLst

and parsePower tokLst =
  let num = findTokExp tokLst Tok_Pow 0 0 in
  if num >= 0 then 
    let tupSplit = splitAtIdx tokLst [] 0 num in
    match tupSplit with
    | (a, b) -> 
        let resTup1 = parseUn a in
        let resTup2 = parsePower b in
        match resTup1 with
        | (c, d) -> 
            match resTup2 with
            | (e, f) -> (c @ e, Pow (d, f))
  else parseUn tokLst

and parseMult tokLst =
  let num = findTokExp tokLst Tok_Mult 0 0 in
  if num >= 0 then
    let tupSplit = splitAtIdx tokLst [] 0 num in
    match tupSplit with
    | (a, b) -> 
        let resTup1 = parsePower a in
        let resTup2 = parseMult b in
        match resTup1 with
        | (c, d) -> 
            match resTup2 with
            | (e, f) -> (c @ e, Mult (d, f))
  else 
    let num = findTokExp tokLst Tok_Div 0 0 in
    if num >= 0 then 
      let tupSplit = splitAtIdx tokLst [] 0 num in
      match tupSplit with
      | (a, b) -> 
          let resTup1 = parsePower a in
          let resTup2 = parseMult b in
          match resTup1 with
          | (c, d) -> 
              match resTup2 with
              | (e, f) -> (c @ e, Div (d, f))
    else parsePower tokLst

and parseAdd tokLst =
  let num = findTokExp tokLst Tok_Add 0 0 in
  if num >= 0 then 
    let tupSplit = splitAtIdx tokLst [] 0 num in
    match tupSplit with
    | (a, b) -> 
        let resTup1 = parseMult a in
        let resTup2 = parseAdd b in
        match resTup1 with
        | (c, d) -> 
            match resTup2 with
            | (e, f) -> (c @ e, Add (d, f))
  else 
    let num = findTokExp tokLst Tok_Sub 0 0 in
    if num >= 0 then 
      let tupSplit = splitAtIdx tokLst [] 0 num in
      match tupSplit with
      | (a, b) -> 
          let resTup1 = parseMult a in
          let resTup2 = parseAdd b in
          match resTup1 with
          | (c, d) -> 
              match resTup2 with
              | (e, f) -> (c @ e, Sub (d, f))
    else parseMult tokLst

and parseRelat tokLst =
  let num = findTokExp tokLst Tok_Less 0 0 in
  if num >= 0 then
    let tupSplit = splitAtIdx tokLst [] 0 num in
    match tupSplit with
    | (a, b) -> 
        let resTup1 = parseAdd a in
        let resTup2 = parseRelat b in
        match resTup1 with
        | (c, d) -> 
            match resTup2 with
            | (e, f) -> (c @ e, Less (d, f))
  else 
    let num = findTokExp tokLst Tok_Greater 0 0 in
    if num >= 0 then 
      let tupSplit = splitAtIdx tokLst [] 0 num in
      match tupSplit with
      | (a, b) -> 
          let resTup1 = parseAdd a in
          let resTup2 = parseRelat b in
          match resTup1 with
          | (c, d) -> 
              match resTup2 with
              | (e, f) -> (c @ e, Greater (d, f))
    else 
      let num = findTokExp tokLst Tok_LessEqual 0 0 in
      if num >= 0 then 
        let tupSplit = splitAtIdx tokLst [] 0 num in
        match tupSplit with
        | (a, b) -> 
            let resTup1 = parseAdd a in
            let resTup2 = parseRelat b in
            match resTup1 with
            | (c, d) -> 
                match resTup2 with
                | (e, f) -> (c @ e, LessEqual (d, f))
      else 
        let num = findTokExp tokLst Tok_GreaterEqual 0 0 in
        if num >= 0 then 
          let tupSplit = splitAtIdx tokLst [] 0 num in
          match tupSplit with
          | (a, b) -> 
              let resTup1 = parseAdd a in
              let resTup2 = parseRelat b in
              match resTup1 with
              | (c, d) -> 
                  match resTup2 with
                  | (e, f) -> (c @ e, GreaterEqual (d, f))
        else parseAdd tokLst

and parseEqual tokLst =
  let num = findTokExp tokLst Tok_Equal 0 0 in
  if num >= 0 then 
    let tupSplit = splitAtIdx tokLst [] 0 num in
    match tupSplit with
    | (a, b) -> 
        let resTup1 = parseRelat a in
        let resTup2 = parseEqual b in
        match resTup1 with
        | (c, d) -> 
            match resTup2 with
            | (e, f) -> (c @ e, Equal (d, f))
  else 
    let num = findTokExp tokLst Tok_NotEqual 0 0 in
    if num >= 0 then 
      let tupSplit = splitAtIdx tokLst [] 0 num in
      match tupSplit with
      | (a, b) -> 
          let resTup1 = parseRelat a in
          let resTup2 = parseEqual b in
          match resTup1 with
          | (c, d) -> 
              match resTup2 with
              | (e, f) -> (c @ e, NotEqual (d, f))
    else parseRelat tokLst

and parseAnd tokLst =
  let num = findTokExp tokLst Tok_And 0 0 in
  if num >= 0 then 
    let tupSplit = splitAtIdx tokLst [] 0 num in
    match tupSplit with
    | (a, b) -> 
        let resTup1 = parseEqual a in
        let resTup2 = parseAnd b in
        match resTup1 with
        | (c, d) -> 
            match resTup2 with
            | (e, f) -> (c @ e, And (d, f))
  else parseEqual tokLst

and parseOr tokLst =
  let num = findTokExp tokLst Tok_Or 0 0 in
  if num >= 0 then 
    let tupSplit = splitAtIdx tokLst [] 0 num in
    match tupSplit with
    | (a, b) -> 
        let resTup1 = parseAnd a in
        let resTup2 = parseOr b in
        match resTup1 with
        | (c, d) -> 
            match resTup2 with
            | (e, f) -> (c @ e, Or (d, f))
  else parseAnd tokLst





  let rec parse_stmt toks : stmt_result = 
    let eofCheck = lookahead2 toks in 
    match eofCheck with
    | EOF -> ([EOF], NoOp)
    | Tok_RBrace -> (toks, NoOp)
    | _ -> 
        let (a, b) = parseOptions toks in  
        if b = NoOp then 
          ([], NoOp) 
        else 
          let nextStmt = parse_stmt a in
          (match nextStmt with 
          | (c, d) -> (c, Seq(b, d))
          )
  
  
  
  and parseDec tokLst = 
    let num = findTok tokLst Tok_Int_Type 0 in
    if num >= 0 then
      let tupSplit = splitAtIdx tokLst [] 0 num in
      (match tupSplit with
      | (a, b) -> 
          (match b with 
          | [] -> raise (InvalidInputException "Invalid token encountered 2")
          | curr::rest -> 
              (match curr with 
              | Tok_ID id -> 
                  let ret = match_token rest Tok_Semi in 
                  (ret, Declare(Int_Type, id))
              | _ -> raise (InvalidInputException "Invalid token encountered 3")
              )
          )
      )
    else 
      let num = findTok tokLst Tok_Bool_Type 0 in
      if num >= 0 then
        let tupSplit = splitAtIdx tokLst [] 0 num in
        (match tupSplit with
        | (a, b) -> 
            (match b with 
            | [] -> raise (InvalidInputException "Invalid token encountered 4")
            | curr::rest -> 
                (match curr with 
                | Tok_ID id -> 
                    let ret = match_token rest Tok_Semi in 
                    (ret, Declare(Bool_Type, id))
                | _ -> raise (InvalidInputException "Invalid token encountered 5")
                )
            )
        )
      else 
        (tokLst, NoOp)
  
  
  and parseAss tokLst = 
    let num = findTok tokLst Tok_Assign 0 in
    if num >= 0 then
      let tupSplit = splitAtIdx tokLst [] 0 num in
      (match tupSplit with
      | (a, b) -> 
          let ar = List.rev a in
          let first = lookahead ar in
          (match first with
          | Tok_ID id -> 
              let exp = parse_expr b in
              (match exp with 
              | (c, d) -> 
                  let ret2 = match_token c Tok_Semi in
                  (ret2, Assign(id, d))
              )
          | _ -> raise (InvalidInputException "Invalid token encountered 6")
          )
      )
    else 
      (tokLst, NoOp)
  
  
  and parsePrint tokLst =
    let num = findTok tokLst Tok_Print 0 in
    if num >= 0 then (
      let tupSplit = splitAtIdx tokLst [] 0 num in
      (match tupSplit with
      | (a, b) -> 
          let checkLP = match_token b Tok_LParen in
          let exp = parse_expr checkLP in
          (match exp with 
          | (c, d) -> 
              let ret2 = match_token c Tok_RParen in 
              let ret3 = match_token ret2 Tok_Semi in
              (ret3, Print d)
          )
      )
    ) else 
      (tokLst, NoOp)
  
  
  and parseElse tokLst =
    let num = findTok tokLst Tok_Else 0 in
    if num >= 0 then (
      let tupSplit = splitAtIdx tokLst [] 0 num in
      (match tupSplit with
      | (a, b) -> 
          let checkLP = match_token b Tok_LBrace in
          let stat = parse_stmt checkLP in
          (match stat with 
          | (c, d) -> 
              let ret2 = match_token c Tok_RBrace in 
              (ret2, d)
          )
      )
    ) else 
      (tokLst, NoOp)
  
  
  and parseIf tokLst =
    let num = findTok tokLst Tok_If 0 in
    if num >= 0 then (
      let tupSplit = splitAtIdx tokLst [] 0 num in
      (match tupSplit with
      | (a, b) -> 
          let checkLP = match_token b Tok_LParen in
          let exp = parse_expr checkLP in
          (match exp with 
          | (c, d) -> 
              let ret2 = match_token c Tok_RParen in 
              let ret3 = match_token ret2 Tok_LBrace in
              let statement = parse_stmt ret3 in
              (match statement with 
              | (e, f) ->
                  let ret4 = match_token e Tok_RBrace in
                  let stat2 = parseElse ret4 in
                  (match stat2 with 
                  | (g, h) -> 
                      (g, If(d, f, h))
                  )
              )
          )
      )
    ) else 
      (tokLst, NoOp)
  
  
  and parseFor tokLst =
    let num = findTok tokLst Tok_For 0 in
    if num >= 0 then (
      let tupSplit = splitAtIdx tokLst [] 0 num in
      (match tupSplit with
      | (a, b) -> 
           let lpCheck = (match_token b Tok_LParen) in (
           match lpCheck with
           | [] -> raise (InvalidInputException "Invalid token encountered 7")
           | curr::rest ->  
              (match curr with 
              | Tok_ID id -> 
                  let ret = match_token rest Tok_From in 
                  let ret2 = parse_expr ret in
                  (match ret2 with 
                  | (c, d) ->
                      let ret3 = match_token c Tok_To in 
                      let ret4 = parse_expr ret3 in
                      (match ret4 with 
                      | (e, f) ->
                          let retter = match_token e Tok_RParen in
                          let ret5 = match_token retter Tok_LBrace in 
                          let ret6 = parse_stmt ret5 in
                          (match ret6 with 
                          | (g, h) ->
                              let ret7 = match_token g Tok_RBrace in 
                              (ret7, For(id, d, f, h))
                          )
                      )
                  )
              | _ -> raise (InvalidInputException "Invalid token encountered 8")
              )
          )
          )
      
    ) else 
      (tokLst, NoOp)
  
  
  and parseWhile tokLst =
    let num = findTok tokLst Tok_While 0 in
    if num >= 0 then (
      let tupSplit = splitAtIdx tokLst [] 0 num in
      (match tupSplit with
      | (a, b) -> 
          let checkLP = match_token b Tok_LParen in
          let exp = parse_expr checkLP in
          (match exp with 
          | (c, d) -> 
              let ret2 = match_token c Tok_RParen in 
              let ret3 = match_token ret2 Tok_LBrace in
              let statement = parse_stmt ret3 in
              (match statement with 
              | (e, f) ->
                  let ret4 = match_token e Tok_RBrace in
                  (ret4, While(d, f))
              )
          )
      )
    ) else 
      (tokLst, NoOp)
  
    and parseOptions tokLst = 
      let checkAhead = (lookahead tokLst) in
      match checkAhead with
      | Tok_Int_Type -> parseDec tokLst
      | Tok_Bool_Type -> parseDec tokLst
      | Tok_ID id -> parseAss tokLst
      | Tok_Print -> parsePrint tokLst
      | Tok_If -> parseIf tokLst
      | Tok_For -> parseFor tokLst
      | Tok_While -> parseWhile tokLst
      | EOF -> ([],NoOp)
      |_ -> raise (InvalidInputException "Invalid token encountered 9")




  

let parse_main toks : stmt = 
  let intCheck = (match_token toks Tok_Int_Type) in
  let mainCheck = (match_token intCheck Tok_Main) in
  let lpCheck = (match_token mainCheck Tok_LParen) in
  let rpCheck = (match_token lpCheck Tok_RParen) in
  let lbCheck = (match_token rpCheck Tok_LBrace) in
  let statement = (parse_stmt lbCheck) in
   match statement with (a,b) ->
  let rbCheck = (match_token a Tok_RBrace) in
  let endCheck = (lookahead rbCheck) in
  if endCheck = EOF
  then b
  else raise (InvalidInputException "Invalid token encountered 10")
  
  
