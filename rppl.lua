-- Almost identical to the REPL except this is a Read-Parse-Print-Loop

local lexer = require("lexer")
local parser = require("parser")

local rppl = {}
local PROMPT = ">> "

function rppl.Start()
    while true do
        io.write(PROMPT)
        local input = io.read()

        if input == 'exit' then
            break
        end

        if #input > 0 then
            local lexer = lexer.new(input)
            local parser = parser.new(lexer)

            local program = parser:ParseProgram()
            if #parser.errors > 0 then
                print("Encountered "..#parser.errors.." parsing errors!")
                for _, err in ipairs(parser.errors) do
                    print("  "..err)
                end

                return
            end

            print(program)
        end
    end
end

return rppl
