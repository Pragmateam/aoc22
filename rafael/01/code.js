const splitAndSum = (input) => {
  const elves = input.split('\n\n')

  for (let i in elves) {
    const cals = elves[i].split(`\n`)
    const sum = cals.map(str => parseInt(str)).reduce((prev, curr) => prev + curr, 0)
    elves[i] = sum
  }
  return elves.sort((a, b) => b - a)
}

const partOne = (input) => {
  const elves = splitAndSum(input)
  return elves[0]
}

const partTwo = (input) => {
  const elves = splitAndSum(input)
  return elves[0] + elves[1] + elves[2]
}

module.exports = { partOne, partTwo }
