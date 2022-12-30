const fs = require("fs")

const readInput = (fileName) => {
  const input = fs.readFileSync(fileName)
  return input.toString()
}

module.exports = { readInput }