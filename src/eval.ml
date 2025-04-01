open SmallCTypes
open Utils
open TokenTypes

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec findName lst targ = match lst with
| [] -> false
| curr::rest -> match curr with
(a,b) -> if a = targ then true else (findName rest targ)

let declareVar env dt name = match dt with
| Int_Type -> (name,Int_Val(0))::env
| Bool_Type -> (name,Bool_Val(false))::env

let rec assignVar lst targ newVal = match lst with
| [] -> []
| curr::rest -> match curr with
(a,b) -> if a = targ then (a,newVal)::rest else curr::(assignVar rest targ newVal)

let rec findPow value power count =
if power = 0 then 1 else
if power < 0 then 1/(findPow value (-1 * power) (-1 * count))
else if count > 1 then
value * (findPow value power (count-1))
else value


let rec eval_expr env t = match t with
| Int value -> Int_Val value
| Bool tf -> Bool_Val tf
| ID name -> if (findName env name) then (List.assoc name env) else raise (DeclareError "Can not find var")
| Add(exp1,exp2) ->( 
let eval1 = (eval_expr env exp1) in 
let eval2 = (eval_expr env exp2) in 
match eval1,eval2 with
| Int_Val value,Int_Val value2 -> Int_Val(value+value2)
| _,_ -> raise (TypeError "non integer used in add operation")
)
|  Sub(exp1,exp2) ->( 
    let eval1 = (eval_expr env exp1) in 
    let eval2 = (eval_expr env exp2) in 
    match eval1,eval2 with
    | Int_Val value,Int_Val value2 -> Int_Val(value-value2)
    | _,_ -> raise (TypeError "non integer used in sub operation")
    )
|  Mult(exp1,exp2) ->( 
    let eval1 = (eval_expr env exp1) in 
    let eval2 = (eval_expr env exp2) in 
    match eval1,eval2 with
    | Int_Val value,Int_Val value2 -> Int_Val(value*value2)
    | _,_ -> raise (TypeError "non integer used in mult operation")
    )
|  Div(exp1,exp2) ->( 
    let eval1 = (eval_expr env exp1) in 
    let eval2 = (eval_expr env exp2) in 
    match eval1,eval2 with
    | Int_Val value,Int_Val value2 -> if (value2 <> 0) then Int_Val(value/value2) else raise (DivByZeroError)
    | _,_ -> raise (TypeError "non integer used in div operation")
    )
    |  Pow(exp1,exp2) ->( 
        let eval1 = (eval_expr env exp1) in 
        let eval2 = (eval_expr env exp2) in 
        match eval1,eval2 with
        | Int_Val value,Int_Val value2 -> Int_Val((findPow value value2 value2))
        | _,_ -> raise (TypeError "non integer used in pow operation")
        )
    |  Greater(exp1,exp2) ->( 
            let eval1 = (eval_expr env exp1) in 
            let eval2 = (eval_expr env exp2) in 
            match eval1,eval2 with
            | Int_Val value,Int_Val value2 -> Bool_Val(value>value2)
            | _,_ -> raise (TypeError "non integer used in > operation")
            )
    | GreaterEqual(exp1,exp2) ->( 
        let eval1 = (eval_expr env exp1) in 
        let eval2 = (eval_expr env exp2) in 
        match eval1,eval2 with
        | Int_Val value,Int_Val value2 -> Bool_Val(value>=value2)
        | _,_ -> raise (TypeError "non integer used in >= operation")
        )
    | Less(exp1,exp2) ->( 
        let eval1 = (eval_expr env exp1) in 
        let eval2 = (eval_expr env exp2) in 
        match eval1,eval2 with
        | Int_Val value,Int_Val value2 -> Bool_Val(value<value2)
        | _,_ -> raise (TypeError "non integer used in < operation")
        )
    | LessEqual(exp1,exp2) ->( 
        let eval1 = (eval_expr env exp1) in 
        let eval2 = (eval_expr env exp2) in 
        match eval1,eval2 with
        | Int_Val value,Int_Val value2 -> Bool_Val(value<=value2)
        | _,_ -> raise (TypeError "non integer used in <= operation")
        )
    |  And(exp1,exp2) ->( 
        let eval1 = (eval_expr env exp1) in 
        let eval2 = (eval_expr env exp2) in 
        match eval1,eval2 with
        | Bool_Val value,Bool_Val value2 -> Bool_Val(value&&value2)
        | _,_ -> raise (TypeError "non bool used in and operation")
        )
    |  Or(exp1,exp2) ->( 
        let eval1 = (eval_expr env exp1) in 
        let eval2 = (eval_expr env exp2) in 
        match eval1,eval2 with
        | Bool_Val value,Bool_Val value2 -> Bool_Val(value || value2)
        | _,_ -> raise (TypeError "non bool used in or operation")
        )
    |  Not exp ->( 
        let eval = (eval_expr env exp) in 
        match eval with
        | Bool_Val value -> Bool_Val(not value)
        | _ -> raise (TypeError "non bool used in not operation")
        )
    |  Equal(exp1,exp2) ->( 
        let eval1 = (eval_expr env exp1) in 
        let eval2 = (eval_expr env exp2) in 
        match eval1,eval2 with
        | Bool_Val value,Bool_Val value2 -> Bool_Val(value = value2)
        | Int_Val value,Int_Val value2 -> Bool_Val(value = value2)
        | _,_ -> raise (TypeError "bool and int mismatch in =")
        )
    | NotEqual(exp1,exp2) ->( 
        let eval1 = (eval_expr env exp1) in 
        let eval2 = (eval_expr env exp2) in 
        match eval1,eval2 with
        | Bool_Val value,Bool_Val value2 -> Bool_Val(value <> value2)
        | Int_Val value,Int_Val value2 -> Bool_Val(value <> value2)
        | _,_ -> raise (TypeError "bool and int mismatch in !=")
        )

let rec eval_stmt env s = match s with
| NoOp -> env
| Seq(state1, state2) -> let eval1 = eval_stmt env state1 in eval_stmt eval1 state2
| Declare(dataType,name) -> if not (findName env name) then (declareVar env dataType name) else raise (DeclareError "Var already exists")
| Assign(name, exp) -> 
    if findName env name then 
        let typ = List.assoc name env in
        let expRes = eval_expr env exp in
        match typ, expRes with
        | Int_Val _, Int_Val _ -> assignVar env name expRes
        | Bool_Val _, Bool_Val _ -> assignVar env name expRes
        | Int_Val _, Bool_Val _ | Bool_Val _, Int_Val _ -> 
            raise (TypeError "int and bool mismatch")
    else 
        raise (DeclareError "Var does not exist")

| If(guard,ifbranch,elsebranch) -> ( let exp = (eval_expr env guard) in 
    match exp with 
    | Bool_Val b -> if b then (eval_stmt env ifbranch) else (eval_stmt env elsebranch)
    | Int_Val i -> raise (TypeError "invalid if guard")
    )
    | For(name, startexp, endexp, body) -> 
        let start = eval_expr env startexp in
        let ender = eval_expr env endexp in
        (match start, ender with
        | Int_Val v1, Int_Val v2 -> 
            let newEnv = eval_stmt env (Assign(name, Int v1)) in
            if v1 <= v2 then (
                let bodyEnv = eval_stmt newEnv body in
                match List.assoc name bodyEnv with
                | Int_Val v -> 
                    let incEnv = eval_stmt bodyEnv (Assign(name, Int (v + 1))) in
                    eval_stmt incEnv (For(name, Int (v + 1), endexp, body))
                | Bool_Val _ -> raise (DeclareError "down Bad")
            ) else newEnv
        | _, _ -> raise (TypeError "non integer in for loop guard")
        )
| While(guard,body) -> ( let exp = (eval_expr env guard) in 
 match exp with
 | Int_Val v -> raise (TypeError "Int value found in while loop guard")
 | Bool_Val b -> if b 
    then let newEnv = (eval_stmt env body) in (eval_stmt newEnv (While(guard,body)))
    else env
)
| Print message -> let res = (eval_expr env message) in 
match res with
| Int_Val v -> let printer = print_output_int v in let _ = print_output_newline printer in env
| Bool_Val b -> let printer = print_output_bool b in let _ = print_output_newline printer in env



