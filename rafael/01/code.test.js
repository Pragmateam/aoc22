const { partOne, partTwo } = require("./code")

const inputOne = `1000
2000
3000

4000

5000
6000

7000
8000
9000

10000`

const inputTwo = `10
20
30

40
50
60

70
90`
describe("part one", () => {
  test('should get the biggest amount of calories', () => {
    let result

    result = partOne(inputOne)
    expect(result).toBe(24000)

    result = partOne(inputTwo)
    expect(result).toBe(160)
  })
})

describe("part two", () => {
  test('should get the total amount of the top 3 elves', () => {
    let result

    result = partTwo(inputOne)
    expect(result).toBe(45000)

    result = partTwo(inputTwo)
    expect(result).toBe(370)
  })
})