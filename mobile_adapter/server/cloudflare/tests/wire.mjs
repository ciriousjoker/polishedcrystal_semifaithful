// Shared protocol oracle for actual local workerd transport tests.
import assert from 'node:assert/strict';
import { setTimeout as sleep } from 'node:timers/promises';

export const CAPACITY = 4;
export const SCHEMA = 5;
export const COUNTERPART = Buffer.alloc(32, 0x42);
export function frame(kind, sequence, payload = []) {
  const data = Buffer.alloc(8 + payload.length);
  data.set([0x50, 0x43, 1, kind]); data.writeUInt16BE(sequence, 4);
  data.writeUInt16BE(payload.length, 6); data.set(payload, 8); return data;
}
export function hello(room, resume = 0, token = Buffer.alloc(8), counterpart = COUNTERPART) {
  assert.equal(token.length, 8); assert.equal(counterpart.length, 32);
  const data = Buffer.alloc(47); data.set([SCHEMA, 0, CAPACITY]);
  data.writeUInt16BE(room, 3); data.writeUInt16BE(resume, 5); data.set(token, 7); data.set(counterpart, 15);
  return frame(16, 1, data);
}
export function presence(welcome, sequence, x) {
  const data = Buffer.alloc(15); welcome.copy(data, 0, 0, 4);
  data.set([24, 5, 0, 1, 0, 1], 4);
  data[10] = x >> 4; data[11] = (x & 15) << 4; data[13] = 1;
  return frame(18, sequence, data);
}
export class Client {
  constructor(ws, stream = 1) {
    this.stream = stream; this.ws = ws; this.queue = []; this.pending = null; this.closed = false;
    ws.binaryType = 'arraybuffer';
    ws.addEventListener('message', event => {
      const envelope = Buffer.from(event.data);
      assert.equal(envelope.subarray(0, 3).toString('hex'), '4d4201');
      if (envelope.readUInt16BE(4) !== this.stream) return;
      if (envelope[3] === 0) return;
      const data = envelope.subarray(6);
      if (this.pending) { const done = this.pending; this.pending = null; done(data); }
      else this.queue.push(data);
      assert(this.queue.length < 32, 'Unbounded pushed-message queue');
    });
    ws.addEventListener('close', () => { this.closed = true; });
  }
  async next(predicate = () => true) {
    const deadline = Date.now() + 2000;
    while (Date.now() < deadline) {
      const message = this.queue.shift() ?? await Promise.race([
        new Promise(resolve => { this.pending = resolve; }),
        sleep(Math.max(1, deadline - Date.now())).then(() => { throw Error('Missing WS response'); })
      ]);
      assert.equal(message.readUInt16BE(6) + 8, message.length);
      if (predicate(message)) return message;
    }
    throw Error('Missing expected response');
  }
  send(data) {
    const envelope = Buffer.alloc(6 + data.length);
    envelope.set([0x4d, 0x42, 1, 1, 0, 0]); envelope.writeUInt16BE(this.stream, 4); envelope.set(data, 6);
    this.ws.send(envelope);
  }
  sendRaw(data) { this.ws.send(data); }
  close() { this.ws.close(); }
}
export async function client(origin, room) {
  const ws = new WebSocket(`${origin.replace('http:', 'ws:')}/lobbies/${room}/ws`);
  const result = new Client(ws);
  await Promise.race([
    new Promise((resolve, reject) => {
      ws.addEventListener('open', resolve, { once: true });
      ws.addEventListener('error', reject, { once: true });
    }), sleep(3000).then(() => { throw Error('WS connect deadline'); })
  ]);
  return result;
}
