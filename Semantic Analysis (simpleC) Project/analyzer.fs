//
// Analyzer for simple C programs.  This component performs
// semantic analysis, in particular collecting variable
// names and their types. The analysis also checks to ensure
// variable names are unique --- no duplicates.
//
// If all is well, a "symbol table" is built and returned,
// containing all variables and their types. A symbol table
// is a list of tuples of the form (name, type).  Example:
//
//   [("x", "int"); ("y", "int"); ("z", "real")]
//
// Modified by:
//   Mike Apreza
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//


namespace compiler

module analyzer =
  //
  // NOTE: all functions in the module must be indented.
  //

  let private matchToken expected_token (tokens: string list) =
    // let next_token = List.head tokens
    // //
    // if expected_token = "identifier" && next_token.StartsWith("identifier") then
    //   List.tail tokens
    // elif expected_token = "int_literal" && next_token.StartsWith("int_literal") then
    //   List.tail tokens
    // elif expected_token = "real_literal" && next_token.StartsWith("real_literal") then
    //   List.tail tokens
    // elif expected_token = "str_literal" && next_token.StartsWith("str_literal") then
    //   List.tail tokens
    // elif expected_token = next_token then  
    //   List.tail tokens
    // else
      List.tail tokens


  let rec private expr_value tokens =
    // let next_token = List.head tokens
    // //
    // if next_token = "false" then
    //   let T2 = matchToken "false" tokens
    //   T2
    // elif next_token = "true" then
    //   let T2 = matchToken "true" tokens
    //   T2
    // elif next_token.StartsWith("identifier") then
    //   let T2 = matchToken "identifier" tokens
    //   T2
    // elif next_token.StartsWith("int_literal") then
    //   let T2 = matchToken "int_literal" tokens
    //   if ((List.head T2).StartsWith("real_literal")) then
    //     let T3 = matchToken "real_literal" T2
    //     T3
    //   else
    //     T2
    // elif next_token.StartsWith("real_literal") then
    //   let T2 = matchToken "real_literal" tokens
    //   T2
    // elif next_token.StartsWith("str_literal") then
    //   let T2 = matchToken "str_literal" tokens
    //   T2
    // else
      List.tail tokens

  let rec private expr_op tokens = 
    let next_token = List.head tokens
    //
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      //
      let T2 = matchToken next_token tokens
      T2
    else
      List.tail tokens

  let rec private expr tokens = 
    let T2 = expr_value tokens
    let next_token = List.head T2
    //
    if next_token = "+"  ||
       next_token = "-"  ||
       next_token = "*"  ||
       next_token = "/"  ||
       next_token = "^"  ||
       next_token = "<"  ||
       next_token = "<=" ||
       next_token = ">"  ||
       next_token = ">=" ||
       next_token = "==" ||
       next_token = "!=" then
      //
      let T3 = expr_op T2
      let T4 = expr_value T3
      T4
    else
      T2

  let rec private empty tokens = 
    let T2 = matchToken ";" tokens
    T2

  // This gets the name of the variable after "identifier:"
  let getVarName (str : string) (name : string) =
    let newName = name + str.[11..]
    newName

  // Check if there's a duplicate variable declaration
  let private checkDup symbTable name =
    List.iter (fun (n, t) -> if (n = name) then 
                                failwith("redefinition of variable '" + name + "'")
              ) symbTable

  // This will help with making the symbol table
  let rec private vardecl tokens symbT = 
    if (List.head tokens = "int") then
      let T2 = matchToken "int" tokens
      let next_token = List.head T2 // This has the name
      let T3 = matchToken "identifier" T2
      let T4 = matchToken ";" T3
      // Edit the token
      let varName = getVarName next_token ""
      checkDup symbT varName
      let tuple = (varName, "int")
      (T4, tuple::symbT)
    else
      let T2 = matchToken "real" tokens
      let next_token = List.head T2 // This has the name
      let T3 = matchToken "identifier" T2
      let T4 = matchToken ";" T3
      // Edit the token
      let varName = getVarName next_token ""
      checkDup symbT varName
      let tuple = (varName, "real")
      (T4, tuple::symbT)

  let rec private input tokens = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let T4 = matchToken "identifier" T3
    let T5 = matchToken ";" T4
    T5

  let rec private output_value tokens = 
    let next_token = List.head tokens
    //
    if next_token = "endl" then
      let T2 = matchToken "endl" tokens
      T2
    else
      let T2 = expr_value tokens
      T2

  let rec private output tokens = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3
    let T5 = matchToken ";" T4
    T5

  let rec private assignment tokens = 
    let T2 = matchToken "identifier" tokens
    let T3 = matchToken "=" T2
    let T4 = expr T3
    let T5 = matchToken ";" T4
    T5

  let rec private stmt tokens symbT = 
    let next_token = List.head tokens
    //
    if next_token = ";" then
      let T2 = empty tokens
      (T2, symbT)
    elif next_token = "int" then
      let (T2, symbT2) = vardecl tokens symbT
      (T2, symbT2)
    elif next_token = "real" then
      let (T2, symbT2) = vardecl tokens symbT
      (T2, symbT2)
    elif next_token = "cin" then
      let T2 = input tokens
      (T2, symbT)
    elif next_token = "cout" then
      let T2 = output tokens
      (T2, symbT)
    elif next_token.StartsWith("identifier") then
      let T2 = assignment tokens
      (T2, symbT)
    elif next_token = "if" then
      let (T2, table) = ifstmt tokens symbT
      (T2, table)
    else
      let tl = List.tail tokens
      (tl, symbT)
  //
  and private ifstmt tokens symbTable = 
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let (T4, table) = condition T3
    let T5 = matchToken ")" T4
    let (T6, newTable) = then_part T5 symbTable
    let (T7, newT) = else_part T6 newTable
    (T7, newT)
  //
  and private condition tokens = 
    let T2 = expr tokens 
    (T2, [])
  //
  and private then_part (tokens : string list) symbTable = 
    let (T2, table) = stmt tokens symbTable
    (T2, table)
  //
  and private else_part (tokens : string list) symbTable = 
    let next_token = List.head tokens
    //
    if next_token = "else" then
      let T2 = matchToken "else" tokens
      let (T3, table) = stmt T2 symbTable
      (T3, table)
    else
      (tokens, symbTable)

  let rec private morestmts tokens symbT = 
    let next_token = List.head tokens
    //
    if next_token = ";"    ||
       next_token = "int"  ||
       next_token = "real" || 
       next_token = "cin"  ||
       next_token = "cout" ||
       next_token.StartsWith("identifier") ||
       next_token = "if" then
      //
      let (T2, symbT2)  = stmt tokens symbT
      let (T3, symbTable) = morestmts T2 symbT2
      (T3, symbTable)
    else 
      (tokens, symbT)

  let rec private stmts tokens symbT = 
    let (T2, symbT2) = stmt tokens symbT
    let (T3, symbTable) = morestmts T2 symbT2
    (T3, symbTable)

  let private simpleC tokens = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7, symbTable) = stmts T6 []
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8
    (T9, symbTable)


  //
  // build_symboltable tokens
  //
  // Given a list of tokens, analyzes the program by looking
  // at variable declarations and collecting them into a
  // list. This list is known as a symbol table. Returns
  // a tuple (result, symboltable), where result is a string 
  // denoting "success" if valid, otherwise a string of the 
  // form "semantic_error:...".
  //
  // On success, the symboltable is a list of tuples of the
  // form (name, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  //
  let build_symboltable tokens = 
    try
      let (T2, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])
