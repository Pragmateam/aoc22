let supermodulo = 0;

const partOne = (input) => {
  const monkeys = parseInput(input)
  playXRounds(20, monkeys)
  const result = multiplyMostActive(monkeys)
  return result
}

const parseInput = (input) => {
  const monkeyInputs = input.split('\n\n')
  const monkeys = monkeyInputs.map(val => parseMonkeyInput(val))
  supermodulo = monkeys.reduce((prev, monkey) => prev * monkey.divisibleBy, 1)
  console.log(supermodulo)
  return monkeys
}

const parseMonkeyInput = (monkeyInput) => {
  const lines = monkeyInput.split("\n")
  const monkey = {}

  monkey.items = lines[1].trim().replace('Starting items: ', "").split(', ').map(val => parseInt(val))

  const op = lines[2].trim().split(' ')
  monkey.operation = op[4]
  monkey.operationValue = op[5]

  monkey.divisibleBy = parseInt(lines[3].trim().split(' ')[3])
  monkey.ifTrue = parseInt(lines[4].trim().split(" ")[5])
  monkey.ifFalse = parseInt(lines[5].trim().split(" ")[5])
  monkey.itemsInpected = 0
  return monkey
}

const playXRounds = (rounds, monkeys) => {
  for (let round = 0; round < rounds; round++) {
    playRound(monkeys)
    if ([1, 20, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000].includes(round + 1)) {
      // console.log(round + 1, monkeys.map(monkey => monkey.itemsInpected))
      console.log(round + 1, monkeys)
    }
  }
}

const multiplyMostActive = (monkeys) => {
  const sortedEntries = Object.values(monkeys).sort((a, b) => b.itemsInpected - a.itemsInpected)
  return sortedEntries[0].itemsInpected * sortedEntries[1].itemsInpected
}

const playRound = (monkeys) => {
  for (let index in monkeys) {
    playMonkeyTurn(index, monkeys)
  }
}

const playMonkeyTurn = (index, monkeys) => {
  const monkey = monkeys[[index]]
  while (monkey.items.length > 0) {
    const [newValue, index] = inspectNextItem(monkey)
    monkeys[index].items.push(newValue)
  }
}

const inspectNextItem = (monkey) => {
  const item = monkey.items.shift()
  let newValue = calculateNewValue(item, monkey.operation, monkey.operationValue)
  newValue = newValue % supermodulo
  const index = newValue % monkey.divisibleBy === 0 ? monkey.ifTrue : monkey.ifFalse
  monkey.itemsInpected++
  return [newValue, index]
}

const calculateNewValue = (old, op, opValue) => {
  return eval(`${old} ${op} ${eval(opValue)}`)
}

const partTwo = (input) => {
  const monkeys = parseInput(input)
  playXRounds(10000, monkeys)
  const result = multiplyMostActive(monkeys)
  return result
}

module.exports = { partOne, partTwo, parseInput, parseMonkeyInput, calculateNewValue, playMonkeyTurn, inspectNextItem }
