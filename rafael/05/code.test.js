const { partOne, partTwo, splitInput, splitContainers, parseArrayOfContainers, parseMove } = require("./code")

const testInput = `    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2`

test('should return the top of each container after executing the moves', () => {
  expect(partOne(testInput)).toBe("CMZ")
})

//================================================

test('should return the top of each container after executing the moves', () => {
  expect(partTwo(testInput)).toBe("MCD")
})

//================================================

test('should split the input in two parts', () => {
  const parts = splitInput(testInput)
  expect(parts[0]).toEqual('    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 ')
  expect(parts[1]).toEqual('move 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2')
})

test('should split the container in a array', () => {
  const container = splitContainers('[Z] [M] [P]')
  expect(container).toEqual(['Z', 'M', 'P'])
})

test('should parse the containers input', () => {
  const containers = parseArrayOfContainers(splitInput(testInput)[0])
  expect(containers).toEqual([['Z', 'N'], ['M', 'C', 'D'], ['P']])
})

test('should parse a string of move', () => {
  expect(parseMove('move 1 from 2 to 3')).toEqual([1, 2, 3])
  expect(parseMove('move 13 from 4 to 9')).toEqual([13, 4, 9])
})
