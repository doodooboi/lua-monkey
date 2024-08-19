local oo = require("oo")
local tokens = require("tokens")

---@param type string The token type
---@param literal string | number The literal char 
---@return token newToken The newly created token
local function newToken(type, literal)
  return setmetatable({Type = type, Literal = literal}, {
		__tostring = function(t)
			return string.format("{Type: %s, Literal: %s}", t.Type, t.Literal)
		end
	})
end

local singleOperators = {
  ["="] = newToken(tokens.ASSIGN, "="),
  ["-"] = newToken(tokens.MINUS, "-"),
  ["+"] = newToken(tokens.PLUS, "+"),
  ["*"] = newToken(tokens.MULT, "*"),
  ["/"] = newToken(tokens.DIV, "/"),
  ["!"] = newToken(tokens.BANG, "!"),
  ["<"] = newToken(tokens.LT, "<"),
  [">"] = newToken(tokens.GT, ">"),
  [";"] = newToken(tokens.SEMICOLON, ";"),
  [","] = newToken(tokens.COMMA, ","),
  ["("] = newToken(tokens.LPAREN, "("),
  [")"] = newToken(tokens.RPAREN, ")"),
  ["{"] = newToken(tokens.LBRACE, "{"),
  ["}"] = newToken(tokens.RBRACE, "}")
}

---@class lexer
---@field input string The input of the source
---@field position number The position of the char we just read
---@field readPosition number The next char position
---@field char string Current char we are reading
---@field readChar fun(self: lexer, peek: boolean?): string | number?
---@field readIdenOrNumber fun(self: lexer, tok: string): string
---@field nextToken fun(self: lexer): token
---@field iterateTokens fun(self: lexer): fun(): token
---@field skipWhitespace fun(self: lexer): nil
---@field new fun(source: string): lexer
local lexer = oo.class()

---@param c number The character's ascii code to check
---@return boolean result True if character is a letter [a-zA-Z_]
local function isLetter(c)
	if not c then return false end
	
  return (c >= 65 and c <=90) or (c >= 97 and c <= 122) or (c == 95)
end

---@param c number The character's ascii code to check
---@return boolean result True if character is a digit from 0-9 
local function isDigit(c) -- ASCII codes
	if not c then return false end
	
  return (c >= 48 and c <= 57)
end

---@param source string
function lexer:init(source)
  self.input = source
  self.position = 0
  self.readPosition = 1 -- next char pointer
  self.char = ''

  self:readChar()
end

function lexer:readChar(peek)
  if self.readPosition >= #self.input + 1 then
    if peek then
      return 0
    end

    self.char = ''
  else
    if peek then
      return self.input:sub(self.readPosition, self.readPosition)
    end

    self.char = self.input:sub(self.readPosition, self.readPosition)
  end

  self.position = self.readPosition
  self.readPosition = self.readPosition + 1
end

function lexer:readIdenOrNumber(tok)
  local start = self.position -- Our current character

  if tok == "number" then
    while isDigit(self.char:byte()) do
      self:readChar()
    end
  elseif tok == "iden" then
    while isLetter(self.char:byte()) do
      self:readChar()
    end
  end

  return self.input:sub(start, self.position-1)
end

function lexer:nextToken()
  ---@type token
  local token

  self:skipWhitespace()
  local char = self.char
  local SingleOperator = singleOperators[char]

  if SingleOperator then
    if char == "=" and self:readChar(true) == "=" then
      token = newToken(tokens.EQ, "==")
      self:readChar()
    elseif char == "!" and self:readChar(true) == "=" then
      token = newToken(tokens.NOT_EQ, "!=")
      self:readChar()
    else
      token = SingleOperator
    end
  elseif char == '' then
    token = newToken(tokens.EOF, "")
  else
    if isLetter(char:byte()) then
      local literal = self:readIdenOrNumber("iden")

      return newToken(tokens.LookupKeyword(literal), literal)
    elseif isDigit(char:byte()) then
      return newToken(tokens.INT, self:readIdenOrNumber("number"))
    else
      token = newToken(tokens.ILLEGAL, char)
    end
  end

  self:readChar()
  return token
end

function lexer:iterateTokens()
  return function()
    return self:nextToken()
  end
end

function lexer:skipWhitespace()
  while (self.char == " " or self.char == "\t" or self.char == "\n" or self.char == "\r") do
    self:readChar()
  end
end

return lexer