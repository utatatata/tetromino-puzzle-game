"use scrict";

const os = require("os");

exports.cols = () => process.stdout.columns;

exports.rows = () => process.stdout.rows;

exports.eol = () => os.EOL;

exports.write = str => () => process.stdout.write(str);

exports.cursorTo = ({ x, y }) => () => process.stdout.cursorTo(x, y);

exports.moveCursor = ({ x, y }) => () => process.stdout.moveCursor(x, y);

exports.clearScreenDown = callback => () =>
  process.stdout.clearScreenDown(() => callback());
