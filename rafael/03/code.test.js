const {
  partOne,
  partTwo,
  splitInHalf,
  calculateItemPriority,
  findDuplicatedItem,
} = require("./code");

const input = `vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw`;

test("should return 157", () => {
  expect(partOne(input)).toBe(157);
});

//================================================

test("should return 70", () => {
  expect(partTwo(input)).toBe(70);
});

//================================================

test("should split string in half", () => {
  expect(splitInHalf("vJrwpWtwJgWrhcsFMMfFFhFp")).toEqual([
    "vJrwpWtwJgWr",
    "hcsFMMfFFhFp",
  ]);
  expect(splitInHalf("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")).toEqual([
    "jqHRNqRjqzjGDLGL",
    "rsFMfFZSrLrFZsSL",
  ]);
  expect(splitInHalf("PmmdzqPrVvPwwTWBwg")).toEqual(["PmmdzqPrV", "vPwwTWBwg"]);
});

test("should calculate item priority", () => {
  expect(calculateItemPriority("a")).toBe(1);
  expect(calculateItemPriority("z")).toBe(26);
  expect(calculateItemPriority("A")).toBe(27);
  expect(calculateItemPriority("Z")).toBe(52);
});

test("should find the duplicated item", () => {
  expect(findDuplicatedItem(splitInHalf("vJrwpWtwJgWrhcsFMMfFFhFp"))).toBe("p");
  expect(
    findDuplicatedItem(splitInHalf("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"))
  ).toBe("L");
});
