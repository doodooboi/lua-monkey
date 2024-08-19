local tokens = require("tokens")

return {
  ["input"] = [[
    let x = 5 + 5
  ]],

  ["expected"] = {
    {tokens.ASSIGN, ""}
  }
}