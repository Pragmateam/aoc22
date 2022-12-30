const main = "main"

const partOne = (input) => {
  const hd = startHD()
  const lines = input.split('\n')
  let result = 0
  for (const line of lines) {
    parseCommandLine(line, hd)
  }
  // console.log(JSON.stringify(hd, null, 4))
  return sumSubFolders(hd[main])
}

const startHD = () => {
  const hd = {
    __pwd: ""
  }
  mkdir(`${main}`, hd)
  return hd
}

const parseCommandLine = (line, hd) => {
  const params = line.split(" ")
  switch (params[0]) {
    case '$':
      if (params[1] === "cd") {
        navigate(params[2], hd)
      }
      break
    case 'dir':
      mkdir(params[1], hd)
      break
    default:
      touch(params[1], parseInt(params[0]), hd)
  }
}

const sumSubFolders = (folder) => {
  let sum = 0
  const items = Object.values(folder)
  for (const item of items) {
    if (typeof item === 'object') {
      if (item.__size <= 100000) {
        sum += item.__size
      }
      sum += sumSubFolders(item)
    }
  }
  return sum
}

const mkdir = (dirName, hd) => {
  const newDir = { __size: 0 }
  eval(`hd${hd.__pwd}`)[dirName] = newDir
}

const navigate = (dir, hd) => {
  if (dir === "/") {
    hd.__pwd = `.${main}`
  } else if (dir === "..") {
    hd.__pwd = getParentFolder(hd.__pwd)
  } else {
    hd.__pwd += `.${dir}`
  }
}

const touch = (fileName, size, hd) => {
  eval(`hd${hd.__pwd}`)[fileName] = size
  updateParentSize(hd.__pwd, size, hd)
}

const getParentFolder = (pwd) => {
  return pwd.substring(0, pwd.lastIndexOf('.'))
}

const updateParentSize = (pwd, size, hd) => {
  eval(`hd${pwd}`)['__size'] += size
  if (pwd !== `.${main}`) {
    updateParentSize(getParentFolder(pwd), size, hd)
  }
}

const partTwo = (input) => {
  const hd = startHD()
  const lines = input.split('\n')
  let result = 0
  for (const line of lines) {
    parseCommandLine(line, hd)
  }
  const missing = 30000000 - (70000000 - hd[main].__size)
  return findSmallestFolder(30000000, missing, hd[main])
}

const findSmallestFolder = (currentVal, missing, folder) => {
  let smallest = currentVal
  const items = Object.values(folder)
  for (const item of items) {
    if (item.__size < smallest && item.__size >= missing) {
      smallest = item.__size
    }
    if (typeof item === 'object') {
      const sub = findSmallestFolder(smallest, missing, item)
      smallest = sub < smallest ? sub : smallest
    }
  }
  return smallest
}

module.exports = {
  partOne,
  partTwo,
  main,
  startHD,
  navigate,
  mkdir,
  touch,
  parseCommandLine
}
