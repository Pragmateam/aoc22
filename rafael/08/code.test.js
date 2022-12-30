const {
  partOne,
  partTwo,
} = require("./code")

const testInput = ``

test('should count 21 trees visible', () => {
  expect(partOne(testInput)).toBe(21)
})

//================================================

test('should find the highest scenic view, 8', () => {
  expect(partTwo(testInput)).toBe(8)
})

//================================================

// test('should navigate to the main path', () => {
//   const hd = startHD()
//   navigate("/", hd)
//   expect(hd.__pwd).toEqual(`.${main}`)
// })