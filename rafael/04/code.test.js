const { partOne, partTwo, checkIntevalContains, parseIntervals } = require("./code")

const testInput = `2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8`

test('should have 2 pairs with full overlap', () => {
  expect(partOne(testInput)).toBe(2)
})

//================================================

test('should have 4 pairs with some overlap', () => {
  expect(partTwo(testInput)).toBe(4)
})

//================================================
test('should parse the intervals', () => {
  expect(parseIntervals('2-4,6-8')).toEqual([2, 4, 6, 8])
})

test('should check if an interval contains the other', () => {
  expect(checkIntevalContains(2, 4, 6, 8)).toBe(false)
  expect(checkIntevalContains(2, 8, 3, 7)).toBe(true)
  expect(checkIntevalContains(3, 7, 2, 8)).toBe(false)
  expect(checkIntevalContains(6, 6, 4, 6)).toBe(false)
  expect(checkIntevalContains(4, 6, 6, 6)).toBe(true)
})

