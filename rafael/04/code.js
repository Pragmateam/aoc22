const checkIntevalContains = (containerStart, containerEnd, checkStart, checkEnd) => {
  return (checkStart >= containerStart && checkEnd <= containerEnd)
}

const checkOverlap = (containerStart, containerEnd, checkStart, checkEnd) => {
  return (checkStart >= containerStart && checkStart <= containerEnd ||
    checkEnd <= containerEnd && checkEnd >= containerStart)
}

const parseIntervals = (intervalsString) => {
  const intervals = intervalsString.split(',')
  const first = intervals[0].split('-')
  const second = intervals[1].split('-')
  return [...first, ...second].map(str => parseInt(str))
}

const checkLine = (line) => {
  const limits = parseIntervals(line)
  if (checkIntevalContains(limits[0], limits[1], limits[2], limits[3])
    || checkIntevalContains(limits[2], limits[3], limits[0], limits[1]))
    return 1
  return 0
}

const checkLine2 = (line) => {
  const limits = parseIntervals(line)
  if (checkOverlap(limits[0], limits[1], limits[2], limits[3])
    || checkOverlap(limits[2], limits[3], limits[0], limits[1]))
    return 1
  return 0
}

const partOne = (input) => {
  const lines = input.split('\n')
  let result = 0
  for (const line of lines) {
    result += checkLine(line)
  }
  return result
}

const partTwo = (input) => {
  const lines = input.split('\n')
  let result = 0
  for (const line of lines) {
    result += checkLine2(line)
  }
  return result
}

module.exports = { partOne, partTwo, checkIntevalContains, parseIntervals }
