---@class token Structure of a token
---@field Type string | number The token type (ASSIGN,LET, etc)
---@field Literal string The actual char 

local tokens = {
  ILLEGAL = "ILLEGAL",
  EOF = "EOF",

  IDENT = "IDENT", -- add, foobar, x, y,...
  INT = "INT", -- 13,5,53

  -- operators
  ASSIGN = "=",
  PLUS = "+",
  MINUS = "-",
  MULT = "*",
  DIV = "/",
  BANG = "!",

  LT = "<",
  GT = ">",

  COMMA = ",",
  SEMICOLON = ";",

  LPAREN = "(",
  RPAREN = ")",
  LBRACE = "{",
  RBRACE = "}",

  FUNCTION = "FUNCTION",
  LET = "LET",
  TRUE = "TRUE",
  FALSE = "FALSE",
  IF = "IF",
  ELSE = "ELSE",
  RETURN = "RETURN",

  EQ = "==",
  NOT_EQ = "!="
}

local keywords = {
  ["fn"] = tokens.FUNCTION,
  ["let"] = tokens.LET,
  ["true"] = tokens.TRUE,
  ["false"] = tokens.FALSE,
  ["if"] = tokens.IF,
  ["else"] = tokens.ELSE,
  ["return"] = tokens.RETURN
}

---@param ident string The identifier to check for a reserved keyword
---@return string token The correct token for the identifier
function tokens.LookupKeyword(ident)
  return keywords[ident] or tokens["IDENT"]
end

return tokens