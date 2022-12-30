const partOne = (input) => {
  const lines = input.split('\n')
  let lineIndex = 0
  let result = 0
  let cycle = 0
  let X = 1
  let buffer = 0
  let toAdd = 0
  while (lineIndex < lines.length || buffer > 0) {
    if (!buffer) {
      X += toAdd
      toAdd = 0

      const cmd = lines[lineIndex];
      [buffer, toAdd] = parseLine(cmd)
      lineIndex++
    }
    if (buffer) {
      buffer--
      cycle++
      if ([20, 60, 100, 140, 180, 220].includes(cycle)) {
        result += cycle * X
      }
    }
  }
  return result
}

const parseLine = (line) => {
  const [command, val = '0'] = line.split(" ")
  const cycles = command === 'addx' ? 2 : 1
  return [cycles, parseInt(val)]
}

const partTwo = (input) => {
  const lines = input.split('\n')
  let lineIndex = 0
  let result = '\n'
  let cycle = 0
  let X = 1
  let buffer = 0
  let toAdd = 0
  while (lineIndex < lines.length || buffer > 0) {
    const pos = cycle % 40
    if (!buffer) {
      X += toAdd
      toAdd = 0

      const cmd = lines[lineIndex];
      [buffer, toAdd] = parseLine(cmd)
      lineIndex++
    }
    if (buffer) {
      buffer--
      cycle++
      result += (pos === X || pos === X - 1 || pos === X + 1) ? "#" : '.'
      if (cycle % 40 === 0)
        result += '\n'
    }
  }
  return result
}

module.exports = { partOne, partTwo }
