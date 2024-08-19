local repl = require("repl")

local function main()
  print("Monkey REPL:")
	print(" Press CTRL-C or type exit to exit.")
  repl.Start()
end

main()