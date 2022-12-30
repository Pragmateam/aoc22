const {
  partOne,
  partTwo,
  startHD,
  navigate,
  mkdir,
  touch,
  main,
  parseCommandLine
} = require("./code")

const testInput = `$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k`

test('should sum all folders with size smaller than 100000 ', () => {
  expect(partOne(testInput)).toBe(95437)
})

//================================================

test('should find the smaller folder to free anough space', () => {
  expect(partTwo(testInput)).toBe(24933642)
})

//================================================

test('should navigate to the main path', () => {
  const hd = startHD()
  navigate("/", hd)
  expect(hd.__pwd).toEqual(`.${main}`)
})

test('should navigate to a sub directory path', () => {
  const hd = startHD()
  hd[main].folder = { file: 12345 }
  navigate("/", hd)
  navigate("folder", hd)
  expect(hd.__pwd).toEqual(`.${main}.folder`)
})

test('should navigate up in the dir path', () => {
  const hd = startHD()
  hd[main].dir = { file: 12345 }
  navigate("/", hd)
  navigate("dir", hd)
  expect(hd.__pwd).toEqual(`.${main}.dir`)
  navigate("..", hd)
  expect(hd.__pwd).toEqual(`.${main}`)
})

test('should create a dir in the current path', () => {
  const hd = startHD()
  navigate("/", hd)
  expect(hd.__pwd).toEqual(`.${main}`)
  mkdir("blah", hd)
  expect(typeof hd[main].blah).toEqual('object')
  expect(hd[main].blah.__size).toEqual(0)
})

test('should create a file in the current path', () => {
  const hd = startHD()
  navigate("/", hd)
  mkdir("blah", hd)
  navigate('blah', hd)
  touch("file.txt", 101350, hd)
  expect(typeof hd[main].blah['file.txt']).toEqual('number')
  expect(hd[main].blah['file.txt']).toEqual(101350)
})

test('should update the folders size up the hierarchy when a file is created', () => {
  const hd = startHD()
  navigate("/", hd)
  mkdir("foo", hd)
  navigate('foo', hd)
  touch("file.txt", 10, hd)
  expect(typeof hd[main].foo['file.txt']).toEqual('number')
  expect(hd[main].foo['file.txt']).toEqual(10)
  expect(hd[main].foo.__size).toEqual(10)
  expect(hd[main].__size).toEqual(10)

  navigate('..', hd)
  mkdir('bar', hd)
  navigate('bar', hd)
  touch('another', 5, hd)
  expect(hd[main].foo.__size).toEqual(10)
  expect(hd[main].bar.__size).toEqual(5)
  expect(hd[main].__size).toEqual(15)
})


test('should execute parsed commands', () => {
  const hd = startHD()
  parseCommandLine("$ cd /", hd)
  parseCommandLine("dir blah", hd)
  parseCommandLine("$ cd blah", hd)
  parseCommandLine("123 file", hd)
  expect(hd[main].blah.file).toEqual(123)
  expect(hd.__pwd).toEqual(`.${main}.blah`)
})

