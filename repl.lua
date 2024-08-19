--[[
The Monkey language needs a REPL. REPL stands for “Read Eval Print Loop” and you probably know what it is from other interpreted languages: Python has a REPL, Ruby has one, every
JavaScript runtime has one, most Lisps have one and a lot of other languages too. Sometimes
the REPL is called “console”, sometimes “interactive mode”. The concept is the same: the
REPL reads input, sends it to the interpreter for evaluation, prints the result/output of the
interpreter and starts again. Read, Eval, Print, Loop.
]]

local lexer = require("lexer")

local repl = {}
local PROMPT = ">> "

function repl.Start()
  while true do
    io.write(PROMPT)
    local input = io.read()

		if input == 'exit' then
			break
		end

    if #input > 0 then
      local lineLexer = lexer.new(input)

      for token in lineLexer:iterateTokens() do
        if token.Type == "EOF" then break end
        print("{Type: "..token.Type..", Literal: "..token.Literal.."}")
      end
    end
  end
end

return repl