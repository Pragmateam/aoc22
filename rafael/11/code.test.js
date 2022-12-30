const { partOne, partTwo, parseMonkeyInput, calculateNewValue, inspectNextItem, parseInput, playMonkeyTurn } = require("./code")

const testInput = `Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1`

test('should calculate the value after twenty rounds', () => {
  expect(partOne(testInput)).toBe(10605)
})

//================================================

test('should calculate the value after 10000 rounds', () => {
  const result = partTwo(testInput)
  expect(result).toEqual(2713310158)
})

//================================================

test('should parse the monkeys', () => {
  const input = parseMonkeyInput(`Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3`)

  expect(input).toEqual({
    items: [79, 98],
    operation: '*',
    operationValue: "19",
    divisibleBy: 23,
    ifTrue: 2,
    ifFalse: 3,
    itemsInpected: 0
  })
})

test('should calculate the new value', () => {
  let calc = calculateNewValue(79, '*', "23")
  expect(calc).toEqual(1817)

  calc = calculateNewValue(13, '+', "15")
  expect(calc).toEqual(28)

  calc = calculateNewValue(23, '*', 'old')
  expect(calc).toEqual(529)
})

test('should inspect the next item', () => {
  const monkey = {
    items: [79, 98],
    operation: '*',
    operationValue: "19",
    divisibleBy: 23,
    ifTrue: 2,
    ifFalse: 3,
    itemsInpected: 0
  }

  const [newValue, index] = inspectNextItem(monkey)
  expect(newValue).toEqual(500)
  expect(index).toEqual(3)
  expect(monkey.items).toEqual([98])
  expect(monkey.itemsInpected).toEqual(1)
})

// test('should play a monkey turn', () => {
//   const monkeys = parseInput(testInput)
//   playMonkeyTurn(0, monkeys)
//   console.log(monkeys)
// })
