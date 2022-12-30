const path = require('path')
const { partOne, partTwo } = require("./code");
const { readInput } = require("../fileReader/fileReader");

const day = 06
const input = readInput(path.join(__dirname, "/input.txt"))

console.log(`day ${day}, part one:`, partOne(input));

console.log(`day ${day}, part two:`, partTwo(input));
