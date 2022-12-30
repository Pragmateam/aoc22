const partOne = (input) => {
  const lines = input.split('\n')
  let result = (lines[0].length * 2) + (lines.length * 2) - 4
  const tallesBehindHorizontal = [...lines[0]]
  const tallestBehindVertical = lines.map(line => line[0])
  for (let v = 1; v < lines.length - 1; v++) {
    for (let h = 1; h < lines[v].length - 1; h++) {
      const p = lines[v][h]
      let isVisibleBehind = false
      let isVisibleAhead = true

      if (p > tallesBehindHorizontal[h]) {
        isVisibleBehind = true
        tallesBehindHorizontal[h] = p
      }
      if (p > tallestBehindVertical[v]) {
        isVisibleBehind = true
        tallestBehindVertical[v] = p
      }

      if (!isVisibleBehind) {
        let hAhead = h + 1;
        do {
          isVisibleAhead = p > lines[v][hAhead]
          hAhead++
        } while (hAhead < lines[v].length && isVisibleAhead)
        if (!isVisibleAhead) {
          let vAhead = v + 1;
          do {
            isVisibleAhead = p > lines[vAhead][h]
            vAhead++
          } while (vAhead < lines.length && isVisibleAhead)
        }
      }

      if (isVisibleBehind || isVisibleAhead) {
        result++
      }
    }
  }
  return result
}
/**
 * 30373
 * 25512
 * 65332
 * 33549
 * 35390
 */

const partTwo = (input) => {
  const lines = input.split('\n')
  let highestScenicScore = 0
  for (let v = 1; v < lines.length - 1; v++) {
    for (let h = 1; h < lines[v].length - 1; h++) {
      const p = lines[v][h]
      let scoreUp = 0
      let scoreRight = 0
      let scoreDown = 0
      let scoreLeft = 0

      let vUp = v
      do {
        vUp--
        scoreUp++
      } while (vUp > 0 && p > lines[vUp][h])

      let hRight = h
      do {
        hRight++
        scoreRight++
      } while (hRight < lines[v].length - 1 && p > lines[v][hRight])

      let vDown = v
      do {
        vDown++
        scoreDown++
      } while (vDown < lines.length - 1 && p > lines[vDown][h])

      let hLeft = h
      do {
        hLeft--
        scoreLeft++
      } while (hLeft > 0 && p > lines[v][hLeft])

      const total = scoreUp * scoreRight * scoreDown * scoreLeft
      highestScenicScore = total > highestScenicScore ? total : highestScenicScore
    }
  }
  return highestScenicScore
}

module.exports = {
  partOne,
  partTwo,
}
