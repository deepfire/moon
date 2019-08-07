var browserfsPath = require.resolve('browserfs');
var browserifyConfig = {
  // Override Browserify's builtins for buffer/fs/path.
  builtins: Object.assign({}, require('browserify/lib/builtins'), {
    "buffer": require.resolve('browserfs/dist/shims/buffer.js'),
    "fs": require.resolve("browserfs/dist/shims/fs.js"),
    "path": require.resolve("browserfs/dist/shims/path.js")
  }),
  insertGlobalVars: {
    // process, Buffer, and BrowserFS globals.
    // BrowserFS global is not required if you include browserfs.js
    // in a script tag.
    "process": function () { return "require('browserfs/dist/shims/process.js')" },
    'Buffer': function () { return "require('buffer').Buffer" },
    "BrowserFS": function() { return "require('" + browserfsPath + "')" }
  }
};
var browserify = require('browserify');
console.warn("browserifying %s", process.argv[2]);
var b = browserify(process.argv[2], browserifyConfig);
b.require("react",                    { expose: "react" });
b.require("react-dom",                { expose: "react-dom" });
b.require("./js/atom-callback.js",    { expose: "./js/atom-callback.js" });
b.require("./js/luna-visualizers.js", { expose: "./js/luna-visualizers.js" });
b.require("./js/lexer-classes.js",    { expose: "./js/lexer-classes.js" });
b.require("./js/visualizers/internal/config.js", { expose: "./js/visualizers/internal/config.js" });
b.require("./js/visualizers/data/map/config.js", { expose: "./js/visualizers/data/map/config.js" });
b.require("./js/visualizers/data/echarts/config.js", { expose: "./js/visualizers/data/echarts/config.js" });
b.require("./js/visualizers/data/base/config.js", { expose: "./js/visualizers/data/base/config.js" });
b.require("./js/visualizers/data/paper/config.js", { expose: "./js/visualizers/data/paper/config.js" });
b.bundle().pipe(process.stdout);
