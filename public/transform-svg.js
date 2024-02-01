const lib = require("./transform-svg_elm.js");

async function transformSvg(commandString, scaleX = 1, scaleY = 1) {
  const flags = { commandString, scaleX, scaleY };
  const { ports } = lib.Elm.Transform.init({ flags });
  return new Promise((resolve) => ports.output.subscribe(resolve));
}

module.exports = {
  transformSvg,
};
