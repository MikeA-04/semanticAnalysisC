//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// Mike Apreza
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

  //
  // empty 
  //
  let private empty tokens =
    match tokens with
    | head::tail when head = "}" && tail = ["$"] -> tokens // empty
    | head::tail when head = ";" -> matchToken ";" tokens // empty with ";"
    | _ -> tokens

  //
  // var-decl
  //
  let private vardecl tokens =
    let T1 = matchToken "int" tokens // Remove the int
    let next_token = List.head T1 // Get the next token
    // Check if the next token starts with...
    if next_token.StartsWith("identifier:") then 
      let T2 = List.tail T1
      let T3 = matchToken ";" T2
      T3
    else 
      failwith ("expecting identifier or literal, but found " + next_token)

  //
  // input
  // 
  let private input tokens =
    let T1 = matchToken "cin" tokens
    let T2 = matchToken ">>" T1
    let next_token = List.head T2
    // Check if the next token starts with...
    if next_token.StartsWith("identifier:") then 
      let T3 = List.tail T2
      let T4 = matchToken ";" T3 // make sure there's a ";"
      T4
    else 
      failwith ("expecting identifier, but found " + next_token)

  //
  // exprvalue
  //
  let private exprvalue tokens =
    let next_token = List.head tokens // Get the head token
    if (next_token = "true" || next_token = "false") then  
      List.tail tokens
    elif (next_token.StartsWith("identifier:")) then
      List.tail tokens
    elif (next_token.StartsWith("int_literal:") || next_token.StartsWith("str_literal:")) then
      List.tail tokens
    else
      failwith ("expecting identifier or literal, but found " + next_token)

  //
  // outputvalue
  //
  let private outputvalue tokens =
    match tokens with
    | head::tail when head = "endl" -> tail
    | _ -> exprvalue tokens

  //
  // output
  // 
  let private output tokens =
    let T1 = matchToken "cout" tokens
    let T2 = matchToken "<<" T1
    let T3 = outputvalue T2
    let T4 = matchToken ";" T3
    T4

  //
  // exprop
  //
  let private exprop tokens = 
    let ops = ["+"; "-"; "*"; "/"; "^"; "<"; "<="; ">"; ">="; "=="; "!="]
    let next_token = List.head tokens // Get the head token
    if (List.exists (fun elem -> elem = next_token) ops) then  
      List.tail tokens
    else
      failwith ("expecting expression operator, but found " + next_token)

  //
  // expr
  //
  let private expr tokens =
    let T1 = exprvalue tokens
    let ops = ["+"; "-"; "*"; "/"; "^"; "<"; "<="; ">"; ">="; "=="; "!="]
    let next_token = List.head T1 // Get the head token
    if (List.exists (fun elem -> elem = next_token) ops) then  
      let T2 = exprop T1
      let T3 = exprvalue T2
      T3
    else
      T1

  //
  // condition
  //
  let private condition tokens =
    let T1 = expr tokens
    T1

  //
  // assignment
  // 
  let private assignment (tokens:string list) =
    let T1 = List.head tokens
    if T1.StartsWith("identifier:") then
      let T2 = List.tail tokens
      let T3 = matchToken "=" T2
      let T4 = expr T3
      let T5 = matchToken ";" T4
      T5
    else
      failwith ("expecting identifier or literal, but found " + T1)

  //
  // morestmts
  //
  let rec private morestmts tokens = 
    match tokens with
    | head::tail when head = "}" && tail = ["$"] -> tokens // empty
    | _ -> let T1 = stmt tokens
           let T2 = morestmts T1
           T2
  and private stmts tokens = 
    let T1 = stmt tokens
    let T2 = morestmts T1
    T2
  and private then_part tokens =
    let T1 = stmt tokens
    T1
  and private else_part tokens =
    match tokens with
    | head::tail when head = "else" -> stmt tail
    | _ -> empty tokens
  and private ifstmt tokens =
    let T1 = matchToken "if" tokens
    let T2 = matchToken "(" T1
    let T3 = condition T2
    let T4 = matchToken ")" T3
    let T5 = then_part T4
    let T6 = else_part T5
    T6
  and private stmt (tokens:string list) =
    match tokens with
    | head::tail when head = ";" -> empty tokens
    | head::tail when head = "int" -> vardecl tokens
    | head::tail when head = "cin" -> input tokens
    | head::tail when head = "cout" -> output tokens
    | head::tail when head.StartsWith("identifier:") -> assignment tokens
    | head::tail when head = "if" -> ifstmt tokens
    | _ -> failwith ("expecting statement, but found " + (List.head tokens))

  //
  // simpleC
  //
  let private simpleC tokens =
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8 // EOF symbol
    T9 // Return T9

  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message
