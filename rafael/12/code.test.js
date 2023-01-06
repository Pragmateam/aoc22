const { partOne, partTwo } = require("./code")

const testInput = `Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi`

test('should calculate the shortest path', () => {
  expect(partOne(testInput)).toBe(31)
})

//================================================

// test('should have 4 pairs with some overlap', () => {
//   const result = partTwo(testInput)
//   console.log(result)
// })

//================================================

test('should find the start', () => {

})