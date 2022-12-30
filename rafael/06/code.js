const hasDuplicate = (input) => {
  let hasDup = false
  for (let i = 0; i < input.length; i++) {
    for (let j = i + 1; j < input.length; j++) {
      hasDup = hasDup || input[i] === input[j]
    }
  }
  return hasDup
}

const partOne = (input) => {
  let position = 0
  let lastFour = []
  let duplicate = false
  while (lastFour.length < 4 || duplicate) {
    lastFour.push(input[position])
    if (lastFour.length > 4) {
      lastFour.shift()
    }
    duplicate = hasDuplicate(lastFour)
    position++
  }
  return position
}

const partTwo = (input) => {
  let position = 0
  let lastFour = []
  let duplicate = false
  while (lastFour.length < 14 || duplicate) {
    lastFour.push(input[position])
    if (lastFour.length > 14) {
      lastFour.shift()
    }
    duplicate = hasDuplicate(lastFour)
    position++
  }
  return position
}

module.exports = { partOne, partTwo, hasDuplicate }
