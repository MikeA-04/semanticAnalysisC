//
// Analyzer for simple C programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid simple C program.
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

module checker =
  //
  // NOTE: all functions in the module must be indented.
  //

  let private matchToken (expected_token : string) (tokens: string list) =
    let next_token = List.head tokens
    //
    if expected_token = "identifier" && next_token.StartsWith("identifier") then
      List.tail tokens
    elif expected_token = "int_literal" && next_token.StartsWith("int_literal") then
      List.tail tokens
    elif expected_token = "real_literal" && next_token.StartsWith("real_literal") then
      List.tail tokens
    elif expected_token = "str_literal" && next_token.StartsWith("str_literal") then
      List.tail tokens
    elif expected_token = next_token then  
      List.tail tokens
    else
      List.tail tokens


  // This gets the name of the variable after "identifier:"
  let private getVarName (name : string) =
    let newName = name.[11..]
    newName


  // Checks if the variable name exists in the symbol table 
  let private check symbTable name =
    let exists = List.exists (fun (n, t) -> n = name) symbTable
    if exists = false then
      failwith("variable '" + name + "' undefined")


  // This find the data type of the variable "name" to return it
  let private returnType name symbT =
    let tupleList = List.filter (fun (n, t) -> n = name) symbT
    let (n, ty) = List.head tupleList
    ty

  //
  // expr-value
  //               
  let rec private expr_value tokens symbTable =
    let next_token = List.head tokens
    //
    if next_token = "false" then
      let T2 = matchToken "false" tokens
      (T2, "bool")
    elif next_token = "true" then
      let T2 = matchToken "true" tokens
      (T2, "bool")
    elif next_token.StartsWith("identifier") then
      let name = getVarName next_token // Get variable name
      check symbTable name // Check if identifier exists
      let typ = returnType name symbTable // Check the indentifier type
      let T2 = matchToken "identifier" tokens
      (T2, typ)
    elif next_token.StartsWith("int_literal") then
      let T2 = matchToken "int_literal" tokens
      (T2, "int")
    elif next_token.StartsWith("real_literal") then
      let T2 = matchToken "real_literal" tokens
      (T2, "real")
    elif next_token.StartsWith("str_literal") then
      let T2 = matchToken "str_literal" tokens
      (T2, "str")
    else
      let T2 = List.tail tokens
      (T2, "")


  //
  // expr-op
  //
  let rec private expr_op tokens = 
    let next_token = List.head tokens
    if next_token = "+"  || next_token = "-"  || next_token = "*" || next_token = "/"  || next_token = "^"  || next_token = "<" || next_token = "<=" || next_token = ">"  || next_token = ">=" || next_token = "==" || next_token = "!=" then
      //
      let T2 = matchToken next_token tokens
      T2
    else
      List.tail tokens


  // This function is executed if one operand is a literal
  let private checkOperation varType (literal : string ) op symbT intIdent =
    // Check to see if the type is valid
    if not(varType.Equals("int") || varType.Equals("real")) then
      failwith("operator " + op + " must involve 'int' or 'real'")
    // Check to see if the literal is valid
    if not(literal.Equals("int") || literal.Equals("real")) then
      failwith("operator " + op + " must involve 'int' or 'real'")
    if literal.Equals("int") && varType <> "int" && intIdent = 1 then
      failwith("type mismatch '" + varType + "' " + op + " 'int'")
    if literal.Equals("int") && varType <> "int" && intIdent = 2 then
      failwith("type mismatch 'int' " + op + " '" + varType + "'")
    if literal.Equals("real") && varType <> "real" && intIdent = 1 then
      failwith("type mismatch '" + varType + "' " + op + " 'real'")
    if literal.Equals("real") && varType <> "real" && intIdent = 2 then
      failwith("type mismatch 'real' " + op + " '" + varType + "'")


  // Checks if BOTH identifiers are valid
  let private validIdentifiers (ident1 : string) (type1 : string) (ident2 : string) (type2 : string) (op : string) =
    if not(type1.Equals("int") || type1.Equals("real")) || not(type2.Equals("int") || type2.Equals("real")) then
        failwith("operator " + op + " must involve 'int' or 'real'")
    if not(type1.Equals(type2)) then
      failwith("type mismatch '" + type1 + "' " + op + " '" + type2 + "'")


  // Checks if identifiers are valid
  let private validArithOp (ident1 : string) (type1 : string) (ident2 : string) (type2 : string) (op : string) symbolTable =
    // If both are identifiers
    if (ident1.StartsWith("identifier") && ident2.StartsWith("identifier")) then
      validIdentifiers ident1 type1 ident2 type2 op
    // If only first is identifier
    elif ident1.StartsWith("identifier") then
      checkOperation type1 type2 op symbolTable 1
    // If only second is identifier
    elif ident2.StartsWith("identifier") then
      checkOperation type2 type1 op symbolTable 2
    elif type1.Equals("str") || type2.Equals("str") || type1.Equals("bool") || type2.Equals("bool") then
      failwith("operator " + op + " must involve 'int' or 'real'")


  let private warning t1 t2 op =
    if t1.Equals(t2) && t1 = "real" && op = "==" then
      printfn "warning: comparing real numbers with == may never be true"


  //
  // expr
  //
  let rec private expr tokens symbolTable = 
    let (T2, type1) = expr_value tokens symbolTable // Type of first operand
    let ident1 = List.head tokens // Get the value on the left
    let next_token = List.head T2 // operator
    // ARITHMETIC OPERATORS
    if (next_token = "+" || next_token = "-" || next_token = "*" || 
        next_token = "/" || next_token = "^") then
        let T3 = expr_op T2
        let ident2 = List.head T3 // Get the value on the right
        let (T4, type2) = expr_value T3 symbolTable // Type of second operand
        //
        validArithOp ident1 type1 ident2 type2 next_token symbolTable
        if type1.Equals(type2) then
          (T4, type1)
        else
          (T4, "either")
    // LOGICAL OPERATORS
    elif (next_token = "<"  || next_token = "<=" || next_token = ">"  || 
          next_token = ">=" || next_token = "==" || next_token = "!=") then
        let T3 = expr_op T2
        let ident2 = List.head T3 // Get the value on the right
        let (T4, type2) = expr_value T3 symbolTable // Type of second operand
        //
        if type1.Equals(type2) && (type1 = "bool" || type1 = "int" || type1 = "real" || type1 = "str") then
          warning type1 type2 next_token
          (T4, "bool")
        else
          failwith("type mismatch '" + type1 + "' " + next_token + " '" + type2 + "'")
    else
      (T2, type1)


  //
  // <empty> -> ;
  //
  let rec private empty tokens = 
    let T2 = matchToken ";" tokens
    T2


  //
  // <vardecl> -> (int | real) identifier ;
  //
  let rec private vardecl tokens = 
    if (List.head tokens = "int") then
      let T2 = matchToken "int" tokens
      let T3 = matchToken "identifier" T2
      let T4 = matchToken ";" T3
      T4
    elif (List.head tokens = "real") then
      let T2 = matchToken "real" tokens
      let T3 = matchToken "identifier" T2
      let T4 = matchToken ";" T3
      T4
    else
      failwith ("redefinition of variable '"+ (List.head tokens) + "'")


  //
  // <input> -> cin >> identifier ;
  //
  let rec private input tokens symbolTable = 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2
    let name = getVarName (List.head T3) // Get the name of the identifier
    check symbolTable name // Check if identifier exists
    let T4 = matchToken "identifier" T3
    let T5 = matchToken ";" T4
    T5


  //
  // <output-value> -> <expr-value>
  //                 | endl
  //
  let rec private output_value tokens symbolTable = 
    let next_token = List.head tokens
    if next_token = "endl" then
      let T2 = matchToken "endl" tokens
      T2
    else
      let (T2, t) = expr_value tokens symbolTable
      T2


  //
  // <output> -> cout << <output-value> ;
  //
  let rec private output tokens symbolTable = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2
    let T4 = output_value T3 symbolTable
    let T5 = matchToken ";" T4
    T5


  //
  // <assignment> -> identifier = <expr> ;
  //
  let rec private assignment tokens symbolTable = 
    let name = getVarName (List.head tokens) // Get name of identifier
    check symbolTable name // Check if identifier exists
    let leftType = returnType name symbolTable // Get the identifier type
    let T2 = matchToken "identifier" tokens
    let T3 = matchToken "=" T2
    let (T4, rightType) = expr T3 symbolTable
    //printfn "%A %A" leftType rightType
    // Check if the types are valid
    if (leftType <> "real" || rightType <> "int") && (leftType <> rightType) then
      failwith("cannot assign '" + rightType + "' to variable of type '" + leftType + "'")
    let T5 = matchToken ";" T4
    T5


  //
  // <stmt> -> <empty>
  //         | <vardecl>
  //         | <input>
  //         | <output>
  //         | <assignment>
  //         | <ifstmt>
  //
  let rec private stmt tokens symbolTable = 
    let next_token = List.head tokens
    // Use the next token to determine which rule to call;
    // if none match then it's a syntax error:
    if next_token = ";" then
      let T2 = empty tokens
      T2
    elif next_token = "int" then
      let T2 = vardecl tokens
      T2
    elif next_token = "real" then // add real as a valid next token
      let T2 = vardecl tokens
      T2
    elif next_token = "cin" then
      let T2 = input tokens symbolTable
      T2
    elif next_token = "cout" then
      let T2 = output tokens symbolTable
      T2
    elif next_token.StartsWith("identifier") then
      let T2 = assignment tokens symbolTable
      T2
    elif next_token = "if" then
      let T2 = ifstmt tokens symbolTable
      T2
    else
      failwith ("expecting statement, but found " + next_token)
  //
  // <ifstmt> -> if ( <condition> ) <then-part> <else-part>
  //
  and private ifstmt tokens symbolTable = 
    let T2 = matchToken "if" tokens
    let T3 = matchToken "(" T2
    let (T4, t) = condition T3 symbolTable
    if not(t.Equals("bool")) then
      failwith("if condition must be 'bool', but found '" + t + "'")
    let T5 = matchToken ")" T4
    let T6 = then_part T5 symbolTable
    let T7 = else_part T6 symbolTable
    T7
  //
  // <condition> -> <expr>
  //
  and private condition tokens symbolTable = 
    let (T2, type1) = expr tokens symbolTable
    (T2, type1)
  //
  // <then-part> -> <stmt>
  //
  and private then_part tokens symbolTable = 
    let T2 = stmt tokens symbolTable
    T2
  //
  // <else-part> -> else <stmt>
  //              | EMPTY
  //
  and private else_part tokens symbolTable = 
    let next_token = List.head tokens
    //
    if next_token = "else" then
      let T2 = matchToken "else" tokens
      let T3 = stmt T2 symbolTable
      T3
    else
      tokens


  //
  // <morestmts> -> <stmt> <morestmts>
  //              | EMPTY
  //
  let rec private morestmts (tokens : string list) symbolTable = 
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
      let T2 = stmt tokens symbolTable
      let T3 = morestmts T2 symbolTable
      T3
    else
      tokens


  //
  // <stmts> -> <stmt> <morestmts>
  // 
  let rec private stmts tokens symbolTable = 
    let T2 = stmt tokens symbolTable
    let T3 = morestmts T2 symbolTable
    T3


  //
  // <simpleC> -> void main ( ) { <stmts> } $
  //
  let private simpleC tokens symbolTable = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6 symbolTable
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8
    T9


  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message
