const { partOne, partTwo, hasDuplicate } = require("./code")

test('should', () => {
  expect(partOne('mjqjpqmgbljsphdztnvjfqwrcgsmlb')).toBe(7)
})

test('should', () => {
  expect(partOne('bvwbjplbgvbhsrlpgdmjqwftvncz')).toBe(5)
})

test('should', () => {
  expect(partOne('nppdvjthqldpwncqszvftbrmjlhg')).toBe(6)
})

test('should', () => {
  expect(partOne('nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg')).toBe(10)
})

test('should', () => {
  expect(partOne('zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw')).toBe(11)
})

//================================================

test('should', () => {
  expect(partTwo('mjqjpqmgbljsphdztnvjfqwrcgsmlb')).toBe(19)
})

test('should', () => {
  expect(partTwo('bvwbjplbgvbhsrlpgdmjqwftvncz')).toBe(23)
})

test('should', () => {
  expect(partTwo('nppdvjthqldpwncqszvftbrmjlhg')).toBe(23)
})

test('should', () => {
  expect(partTwo('nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg')).toBe(29)
})

test('should', () => {
  expect(partTwo('zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw')).toBe(26)
})

//================================================

test('should', () => {
  expect(hasDuplicate("qwer")).toBe(false)
  expect(hasDuplicate("qwqr")).toBe(true)
})
