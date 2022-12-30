const partOne = (input) => {
  const lines = input.split('\n')
  let result = 0

  const logs = startLogs()
  for (const line of lines) {
    executeLine(line, logs)
  }
  result = Object.keys(logs['9logs']).length
  return result
}

const startLogs = () => {
  return {
    0: [0, 0],
    1: [0, 0],
    2: [0, 0],
    3: [0, 0],
    4: [0, 0],
    5: [0, 0],
    6: [0, 0],
    7: [0, 0],
    8: [0, 0],
    9: [0, 0],
    '9logs': {
      "0,0": 1
    },

  }
}

const executeLine = (line, logs) => {
  const [direction, amount] = line.split(' ')
  for (let i = 0; i < amount; i++) {
    moveHeader(direction, logs)
    moveTail(logs)
  }
}

const moveHeader = (direction, logs) => {
  switch (direction) {
    case "R":
      logs[0][0]++
      break
    case "L":
      logs[0][0]--
      break
    case "U":
      logs[0][1]++
      break
    case "D":
      logs[0][1]--
      break
  }
}

const moveTail = (logs) => {
  const diff = [logs[0][0] - logs[9][0], logs[0][1] - logs[9][1]]
  // console.log(diff)
  if (Math.abs(diff[0]) > 1 || Math.abs(diff[1]) > 1) {
    const xMove = Math.sign(diff[0])
    const yMove = Math.sign(diff[1])
    logs[9][0] += xMove
    logs[9][1] += yMove
    logTail(logs)
  }
}

const logTail = (logs) => {
  const position = `${logs[9][0]},${logs[9][1]}`
  if (!logs['9logs'][position]) {
    logs['9logs'][position] = 0
  }
  logs['9logs'][position]++
}

const partTwo = (input) => {
  const lines = input.split('\n')
  let result = 0

  const logs = startLogs()
  for (const line of lines) {
    executeLine2(line, logs)
  }
  result = Object.keys(logs['9logs']).length
  return result
}

const executeLine2 = (line, logs) => {
  const [direction, amount] = line.split(' ')
  for (let i = 0; i < amount; i++) {
    moveHeader(direction, logs)
    for (let i = 1; i <= 9; i++) {
      moveKnot(i, logs)
    }
  }
}

const moveKnot = (index, logs) => {
  const diff = [logs[index - 1][0] - logs[index][0], logs[index - 1][1] - logs[index][1]]
  if (Math.abs(diff[0]) > 1 || Math.abs(diff[1]) > 1) {
    const xMove = Math.sign(diff[0])
    const yMove = Math.sign(diff[1])
    logs[index][0] += xMove
    logs[index][1] += yMove
    if (index == 9)
      logTail(logs)
  }
}

module.exports = {
  partOne,
  partTwo,
  startLogs,
  executeLine,
}


// case "RU":
// case "UR":
//   position[0]++
//   position[1]++
//   break
// case "LU":
// case "UL":
//   position[0]--
//   position[1]++
//   break
// case "RD":
// case "DR":
//   position[0]++
//   position[1]--
//   break
// case "DL":
// case "LD":
//   position[0]--
//   position[1]--
//   break