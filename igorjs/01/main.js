const assert = require("assert");
const { readFileInput, pipe } = require("../utils");

const log = console.log;

function readInput(input) {
  return readFileInput(__filename)(input);
}

function parseContents(contents) {
  return contents
    .split(/\r?\n\s*\r?\n/g)
    .map((group) => group.split(/\r?\n/g).map((number) => Number(number)));
}

function mapReduceSort(groups) {
  return groups
    .map((g) => g.reduce((sum, value) => sum + value, 0))
    .sort((a, b) => b - a);
}

function max(values) {
  return Math.max(...Array.from(values));
}

function sum(values) {
  return Array.from(values).reduce((sum, value) => sum + value, 0);
}

function top3(values) {
  return Array.from(values).slice(0, 3);
}

function getCalories() {
  // prettier-ignore
  const getMaxCalories = pipe(
    readInput,
    parseContents,
    mapReduceSort,
    max
  );

  const getSumCalories = pipe(
    readInput,
    parseContents,
    mapReduceSort,
    top3,
    sum
  );

  return {
    max: getMaxCalories(...arguments),
    sum: getSumCalories(...arguments),
  };
}

(async function main() {
  assert.strictEqual(getCalories("test/1").max, 24000);
  assert.strictEqual(getCalories("test/1").sum, 45000);

  log(getCalories("input").sum);
})();
