const { partOne, partTwo } = require("./code")

test('A Y should return 8', () => {
  expect(partOne(`A Y`)).toBe(8)
})

test('B X should return 1', () => {
  expect(partOne(`B X`)).toBe(1)
})

test('C Z should return 6', () => {
  expect(partOne(`C Z`)).toBe(6)
})

test('A Y, B X should return 9', () => {
  expect(partOne('A Y\nB X')).toBe(9)
})

test('A Y, B X, C Z should return 15', () => {
  expect(partOne(`A Y\nB X\nC Z`)).toBe(15)
})

//================================================

test('A Y should return 4', () => {
  expect(partTwo(`A Y`)).toBe(4)
})

test('B X should return 1', () => {
  expect(partTwo(`B X`)).toBe(1)
})

test('C Z should return 7', () => {
  expect(partTwo(`C Z`)).toBe(7)
})

test('A Y, B X should return 5', () => {
  expect(partTwo('A Y\nB X')).toBe(5)
})

test('A Y, B X, C Z should return 12', () => {
  expect(partTwo(`A Y\nB X\nC Z`)).toBe(12)
})