const fs = require("fs");
const path = require("path");

module.exports.pipe = function (...fns) {
  return function (value) {
    return fns.reduce((acc, fn) => fn(acc), value);
  };
};

module.exports.trace = function (message) {
  return function trace(value) {
    console.log(`${message}:`, value);
    return value;
  };
};

const cache = {};
module.exports.readFileInput = function (caller) {
  const basedir = path.dirname(caller);
  return function readFileInput(file) {
    if (file in cache) {
      return cache[file];
    } else {
      cache[file] = fs.readFileSync(path.resolve(basedir, file), {
        encoding: "utf8",
        flag: "r",
      });
      return cache[file];
    }
  };
};
