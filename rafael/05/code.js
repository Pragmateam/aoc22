const splitInput = (input) => {
  return input.split('\n\n')
}

const splitContainers = (line) => {
  const containers = []
  let i = 0;
  do {
    if (i + 2 <= line.length) {
      const container = line.slice(i, i + 3).match(/[A-Z]/g)

      containers.push(container ? container[0] : "")
      i += 4
    }
  } while (i < line.length)
  return containers
}

const parseArrayOfContainers = (input) => {
  const lines = input.split('\n')
  const stacks = lines[lines.length - 1].match(/[0-9]/g).map(i => [])
  for (let line = lines.length - 2; line >= 0; line--) {
    const containers = splitContainers(lines[line])
    for (var [i, container] of containers.entries()) {
      if (container !== "")
        stacks[i].push(container)
    }
  }
  return stacks
}

const parseMove = (moveString) => {
  let beforeSplit = moveString.replace("move ", "")
  beforeSplit = beforeSplit.replace("from ", "")
  beforeSplit = beforeSplit.replace("to ", "")
  return beforeSplit.trim().split(" ").map(i => parseInt(i))
}

const parseMoves = (input) => {
  const lines = input.split('\n')
  const moves = []
  for (const line of lines) {
    moves.push(parseMove(line))
  }
  return moves
}



const partOne = (input) => {
  const [containersString, movesString] = splitInput(input)
  const stacks = parseArrayOfContainers(containersString);
  const moves = parseMoves(movesString)
  let result = ""
  for (const move of moves) {
    const [qty, a, b] = move
    for (let i = 0; i < qty; i++) {
      const container = stacks[a - 1].pop()
      stacks[b - 1].push(container)
    }
  }
  for (const stack of stacks) {
    result += stack[stack.length - 1]
  }
  return result
}

const partTwo = (input) => {
  const [containersString, movesString] = splitInput(input)
  const stacks = parseArrayOfContainers(containersString);
  const moves = parseMoves(movesString)
  let result = ""
  for (const move of moves) {
    const [qty, a, b] = move
    const temp = []
    for (let i = 0; i < qty; i++) {
      const container = stacks[a - 1].pop()
      temp.push(container)
    }
    for (let i = 0; i < qty; i++) {
      const container = temp.pop()
      stacks[b - 1].push(container)
    }
  }
  for (const stack of stacks) {
    result += stack[stack.length - 1]
  }
  return result
}

module.exports = { partOne, partTwo, splitInput, splitContainers, parseArrayOfContainers, parseMove }
