const { readInput } = require("./fileReader")

test('should read the input from a file', () => {
  expect(readInput('2015/fileReader.input.txt')).toBe("File read correctly")
})

test('should read input with multiple lines', () => {
  expect(readInput('2015/fileReader_multiLine.input.txt')).toBe("Awesome\nGood Work")
})