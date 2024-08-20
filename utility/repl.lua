--[[
The Monkey language needs a REPL. REPL stands for “Read Eval Print Loop” and you probably know what it is from other interpreted languages: Python has a REPL, Ruby has one, every
JavaScript runtime has one, most Lisps have one and a lot of other languages too. Sometimes
the REPL is called “console”, sometimes “interactive mode”. The concept is the same: the
REPL reads input, sends it to the interpreter for evaluation, prints the result/output of the
interpreter and starts again. Read, Eval, Print, Loop.
]]

local lexer = require("components.lexer")
local parser = require("components.parser")
local object = require("components.object")

require("components.evaluator")

local repl = {}
local PROMPT = ">> "

function repl.Start()
  print("Monkey REPL:")
	print(" Press CTRL-C or type exit to exit.")

  local env = object.Environment.new()

  while true do
    io.write(PROMPT)
    local input = io.read()

    if input == 'exit' then
      break
    end

    if #input > 0 then
      local lineLexer = lexer.new(input)
      local parsed = parser.new(lineLexer)

      local program = parsed:ParseProgram()
      if #parsed.errors > 0 then
        print("Encountered " .. #parsed.errors .. " parsing errors!")
        for _, err in ipairs(parsed.errors) do
          print("  " .. err)
        end

        return
      end

      local evaluated = eval(program, env)
      if evaluated then
        print(evaluated:Inspect())
      end
    end
  end
end

return repl
