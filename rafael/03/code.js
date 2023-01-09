const splitInHalf = (items) => {
  return [items.slice(0, items.length / 2), items.slice(items.length / 2)];
};

const calculateItemPriority = (item) => {
  const code = item.charCodeAt();
  if (code >= 97) return code - 96;
  else return code - 38;
};

const findDuplicatedItem = (parts) => {
  const [first, second] = parts;
  for (let item of first) {
    for (let compare of second) {
      if (item === compare) return item;
    }
  }
};

const findDuplicatedItems = (parts) => {
  const [first, second] = parts;
  const duplicates = {};
  for (let item of first) {
    for (let compare of second) {
      if (item === compare) {
        if (duplicates[item]) duplicates[item] += 1;
        else duplicates[item] = 1;
      }
    }
  }
  return duplicates;
};

const partOne = (input) => {
  const lines = input.split("\n");
  let result = 0;
  for (const line of lines) {
    const dup = findDuplicatedItem(splitInHalf(line));
    result += calculateItemPriority(dup);
  }
  return result;
};

const partTwo = (input) => {
  const lines = input.split("\n");
  let result = 0;
  for (let i = 0; i < lines.length; i += 3) {
    const dup = findDuplicatedItems([lines[i], lines[i + 1]]);
    const badge = findDuplicatedItems([Object.keys(dup), lines[i + 2]]);
    result += calculateItemPriority(Object.keys(badge)[0]);
  }
  return result;
};

module.exports = {
  partOne,
  partTwo,
  splitInHalf,
  findDuplicatedItem,
  calculateItemPriority,
};
