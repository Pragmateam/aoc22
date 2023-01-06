const partOne = (input) => {
  const lines = input.split('\n')
  let result = 0
  for (const line of lines) {
    result += checkLine(line)
  }
  return result
}

const partTwo = (input) => {

}

module.exports = { partOne, partTwo }
