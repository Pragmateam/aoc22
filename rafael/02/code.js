const shapeScore = (shape) => {
  switch (shape) {
    case 'X':
    case 'A':
    case 'Rock':
      return 1
    case 'Y':
    case 'B':
    case 'Paper':
      return 2
    case 'Z':
    case 'C':
    case 'Scissors':
      return 3
  }
}

const roundScore = (op, me) => {
  if (me === 1) {
    if (op === 1) {
      return 3
    } else if (op === 2) {
      return 0
    } else {
      return 6
    }
  } else if (me === 2) {
    if (op === 1) {
      return 6
    } else if (op === 2) {
      return 3
    } else {
      return 0
    }
  } else {
    if (op === 1) {
      return 0
    } else if (op === 2) {
      return 6
    } else {
      return 3
    }
  }
}

const findPlay = (op, me) => {
  if (me === "X") {
    if (op === 1) {
      return "C"
    } else if (op === 2) {
      return "A"
    } else {
      return "B"
    }
  } else if (me === "Y") {
    if (op === 1) {
      return "A"
    } else if (op === 2) {
      return "B"
    } else {
      return "C"
    }
  } else {
    if (op === 1) {
      return "B"
    } else if (op === 2) {
      return "C"
    } else {
      return "A"
    }
  }
}

const partOne = (input) => {
  const lines = input.split('\n')
  let result = 0
  for (const line of lines) {
    const [op, me] = line.split(" ")
    const meScore = shapeScore(me)
    const lineScore = roundScore(shapeScore(op), meScore)
    result += (meScore + lineScore)
  }
  return result
}

const partTwo = (input) => {
  const lines = input.split('\n')
  let result = 0
  for (const line of lines) {
    const [op, me] = line.split(" ")
    const opScore = shapeScore(op)
    const myPlay = findPlay(opScore, me)
    const lineScore = roundScore(opScore, shapeScore(myPlay))
    result += (shapeScore(myPlay) + lineScore)
  }
  return result
}

module.exports = { partOne, partTwo }

/*
    Rock A - Y Paper    2
   Paper B - X Scissors 3
Scissors C - Z Rock     1
*/

/*
    Rock A - Y Draw 2 - A
   Paper B - X Lose 3 - A
Scissors C - Z Win  1 - A
*/