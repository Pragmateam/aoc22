const fs = require("fs/promises");

function readInput(filename) {
  return fs.readFile(filename, { encoding: "utf8" }).catch(console.error);
}

function parseContents(contents) {
  return contents
    .split(/\r?\n\s*\r?\n/g)
    .map((group) => group.split(/\r?\n/g).map((number) => Number(number)));
}

function reduceSort(groups) {
  return groups
    .map((g) => g.reduce((sum, value) => sum + value, 0))
    .sort((a, b) => b - a);
}

function max() {
  return Math.max(...Array.from(arguments));
}

function sum() {
  return Array.from(arguments).reduce((sum, value) => sum + value, 0);
}

(async function main() {
  if (process.argv.length !== 3) {
    console.error("Usage: node main.js <input file>");
    process.exit(1);
  }

  const fileContents = await readInput(process.argv[2]);

  const groups = parseContents(fileContents);

  const elves = reduceSort(groups);
  const top3elves = elves.slice(0, 3);

  console.log("calories (max)", max(...elves));
  console.log("calories (top 3)", sum(...top3elves));
})();
