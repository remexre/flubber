#!/usr/bin/env node

const stream = require("stream");
const StreamValues = require("stream-json/streamers/StreamValues");

class GetValue extends stream.Transform {
  constructor(options) {
    if(!options)
      options = {};
    options.objectMode = true;
    super(options);
  }
  _transform(chunk, enc, cb) {
    this.push(chunk.value);
    cb();
  }
}

const stdin = process.stdin
  .pipe(StreamValues.withParser())
  .pipe(new GetValue());
const send = value => console.log(JSON.stringify(value));

// InitInfo
send({
  pluginName: "trivial",
  pluginVersion: [0, 1, 0],
  protocolVersion: [0, 1, 0],
});

stdin.on("data", request => {
  console.error("request:", request);
});
