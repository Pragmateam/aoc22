const fs = require("fs");
const path = require("path");

function log() {
  console.log.apply(console, arguments);
}

function pipe(...fns) {
  return function (value) {
    return fns.reduce((acc, fn) => fn(acc), value);
  };
}

function trace(message) {
  return function (value) {
    console.log(`${message}:`, value);
    return value;
  };
}

const readFileInputCache = {};

function readFileInput(caller) {
  const basedir = path.dirname(caller);

  return function (file) {
    if (file in readFileInputCache) {
      return readFileInputCache[file];
    } else {
      readFileInputCache[file] = fs.readFileSync(path.resolve(basedir, file), {
        encoding: "utf8",
        flag: "r",
      });
      return readFileInputCache[file];
    }
  };
}

module.exports = {
  readFileInput,
  trace,
  pipe,
  log,
};
