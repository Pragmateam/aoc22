const {
  partOne,
  partTwo,
  startLogs,
  executeLine,
} = require("./code")

const testInput = `R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2`

test('should count 13 positions that Tail visited', () => {
  expect(partOne(testInput)).toBe(13)
})

//================================================

test('should count 1 positions that Tail visited', () => {
  expect(partTwo(testInput)).toBe(1)
})

test('should count 36 positions that Tail visited', () => {
  const testInput2 = `R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20`
  expect(partTwo(testInput2)).toBe(36)
})

//================================================

test('should move tail one move closer to head', () => {
  const logs = startLogs()
  logs[0] = [1, 1]
  logs[9] = [0, 1]
  executeLine("R 1", logs)
  expect(logs[9]).toEqual([1, 1])
  executeLine("R 1", logs)
  expect(logs[9]).toEqual([2, 1])

  logs[0] = [2, 2]
  logs[9] = [3, 3]
  executeLine("L 1", logs)
  expect(logs[9]).toEqual([2, 2])
})

test('should no move tail if it is already close to the head', () => {
  const logs = startLogs()
  logs[0] = [3, 1]
  logs[9] = [2, 1]
  executeLine("U 1", logs)
  expect(logs[0]).toEqual([3, 2])
  expect(logs[9]).toEqual([2, 1])
})

test('should log the tail movement', () => {
  const logs = startLogs()
  logs[0] = [1, 1]
  logs[9] = [0, 1]
  executeLine("R 1", logs)
  expect(logs[9]).toEqual([1, 1])
  expect(logs['9logs']['1,1']).toBeDefined()
  expect(logs['9logs']['1,1']).toEqual(1)
})

test('should not log the tail movement if it doesnt move', () => {
  const logs = startLogs()
  logs[0] = [2, 2]
  logs[9] = [1, 2]
  executeLine("R 1", logs)
  expect(logs['9logs']['2,2']).toBeDefined()
  expect(logs['9logs']['2,2']).toEqual(1)
  executeLine("U 1", logs)
  expect(logs['9logs']['2,2']).toEqual(1)
})