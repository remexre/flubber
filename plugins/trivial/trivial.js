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
const time = () => Math.floor(Date.now() / 1000);

// InitInfo
send({
  pluginName: "trivial",
  pluginVersion: [0, 1, 0],
  protocolVersion: [0, 1, 0],
});

stdin.on("data", request => {
  console.error("request:", request);
});

const startTime = time();
setInterval(() => {
  console.error("Sending update...");
  send({
    type: "MessageUpsert",
    value: {
      id: "test",
      sender: "tester",
      recipient: {
        type: "RoomID",
        value: "#general",
      },
      attachments: [],
      content: {
        type: "Text",
        value: "Hello!",
      },
      createTime: startTime,
      editTime: time(),
      extra: null
    },
  });
}, 1000);
