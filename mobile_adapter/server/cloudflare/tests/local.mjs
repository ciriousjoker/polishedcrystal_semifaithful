// Actual Rust/WASM Worker and Durable Objects under local workerd. No account.
import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { mkdtemp, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import net from 'node:net';
import { setTimeout as sleep } from 'node:timers/promises';

import { CAPACITY, frame, hello, presence, Client, client } from './wire.mjs';

const temp = await mkdtemp(join(tmpdir(), 'mobile-workerd-'));
const reserve = net.createServer();
await new Promise(resolve => reserve.listen(0, '127.0.0.1', resolve));
const port = reserve.address().port;
await new Promise(resolve => reserve.close(resolve));
const origin = `http://127.0.0.1:${port}`;
const proc = spawn('node_modules/.bin/wrangler', ['dev', '--local', '--ip', '127.0.0.1',
  '--port', String(port), '--inspector-port', '0', '--persist-to', temp, '--log-level', 'warn'],
  { env: { ...process.env, WRANGLER_SEND_METRICS: 'false' }, stdio: ['ignore', 'pipe', 'pipe'] });
let log = ''; proc.stdout.on('data', data => { log += data; });
proc.stderr.on('data', data => { log += data; });
const clients = [];
try {
  let ready = false;
  for (let i = 0; i < 600; i++) {
    if (proc.exitCode !== null) throw Error(`Wrangler exited: ${log}`);
    try { ready = (await fetch(`${origin}/health`)).ok; } catch {}
    if (ready) break;
    await sleep(200);
  }
  assert(ready, `Local Worker not ready: ${log}`);
  assert.equal((await fetch(`${origin}/lobbies/0/ws`)).status, 404);
  assert.equal((await fetch(`${origin}/lobbies/7/ws`)).status, 426);
  const identities = [];
  for (let i = 0; i < CAPACITY; i++) {
    const c = await client(origin, 7); clients.push(c); c.send(hello(7));
    const welcome = await c.next(data => data[3] === 17);
    identities.push(welcome.subarray(8));
    await c.next(data => data[3] === 19);
    c.send(presence(identities[i], 2, 30 + i));
    c.send(frame(23, 0));
  }
  // Exercise full three-peer snapshots repeatedly, not only empty join state.
  // This also forces serialization of the largest hibernation attachment.
  for (let round = 0; round < 6; round++) {
    const positions = identities.map((_, i) => 100 + round * 8 + i);
    for (let i = 0; i < CAPACITY; i++) clients[i].send(presence(identities[i], 3 + round, positions[i]));
    for (let i = 0; i < CAPACITY; i++) {
      const expected = positions.filter((_, index) => index !== i).sort((a, b) => a - b);
      await clients[i].next(data => {
        if (data[3] !== 19 || data[12] !== CAPACITY - 1) return false;
        const xs = [];
        for (let j = 0; j < CAPACITY - 1; j++) xs.push((data[25 + j * 17] << 4) | (data[26 + j * 17] >> 4));
        return xs.sort((a, b) => a - b).every((x, j) => x === expected[j]);
      });
    }
    await sleep(55);
  }
  const extra = await client(origin, 7); clients.push(extra); extra.send(hello(7));
  assert.equal((await extra.next(data => data[3] === 22))[8], 3, 'Room capacity');
  const other = await client(origin, 8); clients.push(other); other.send(hello(8));
  const identity = (await other.next(data => data[3] === 17)).subarray(8);
  assert.equal((await other.next(data => data[3] === 19))[12], 0);
  other.send(presence(identity, 2, 200)); other.send(frame(23, 0));
  // A correct HELLO for another room must not escape the routed DO.
  const wrong = await client(origin, 8); clients.push(wrong); wrong.send(hello(7));
  assert.equal((await wrong.next(data => data[3] === 22))[8], 1);
  const bad = await client(origin, 9); clients.push(bad); bad.sendRaw(Buffer.alloc(4096));
  for (let i = 0; i < 100 && !bad.closed; i++) await sleep(10);
  assert(bad.closed, 'Malformed Bridge envelope closes only that group');
  // A local poll is not a forced upstream push. Publish a fresh accepted change
  // and verify the unrelated room fault did not affect this room's authority.
  clients[1].send(presence(identities[1], 9, 200));
  assert.equal((await clients[0].next(data => data[3] === 19 && data[12] === CAPACITY - 1))[12], CAPACITY - 1);
  const leave = frame(20, 3, identity.subarray(0, 4)); other.send(leave);
  assert.equal((await other.next(data => data[3] === 21))[3], 21);
  // Two emulator streams share one household WS. A second household joins it.
  const household = await client(origin, 10); clients.push(household);
  const sibling = new Client(household.ws, 2); clients.push(sibling);
  const visitor = await client(origin, 10); clients.push(visitor);
  const groupMembers = [household, sibling, visitor];
  const groupIdentities = [];
  for (const [i, c] of groupMembers.entries()) {
    c.send(hello(10));
    groupIdentities.push((await c.next(data => data[3] === 17)).subarray(8));
    c.send(presence(groupIdentities[i], 2, 40 + i));
    c.send(frame(23, 0));
  }
  household.send(frame(23, 0)); sibling.send(frame(23, 0)); visitor.send(frame(23, 0));
  for (const c of groupMembers) assert.equal((await c.next(data => data[3] === 19 && data[12] === 2))[12], 2);
  household.send(frame(18, 3)); // Valid envelope, invalid one player's payload.
  assert.equal((await household.next(data => data[3] === 22))[8], 1);
  sibling.send(frame(23, 0)); visitor.send(frame(23, 0));
  for (const c of [sibling, visitor]) assert.equal((await c.next(data => data[3] === 19 && data[12] === 1))[12], 1);
  sibling.send(presence(groupIdentities[1], 3, 200)); visitor.send(frame(23, 0));
  const continued = await visitor.next(data => data[3] === 19 && data[12] === 1 && data[25] === 12);
  assert.equal(continued[25], 12, 'Healthy sibling still publishes x=200 after the bad stream leaves');
  // Bound one aggressive Bridge without closing another lobby's sockets.
  const flood = new WebSocket(`${origin.replace('http:', 'ws:')}/lobbies/11/ws`);
  await new Promise((resolve, reject) => { flood.addEventListener('open', resolve, {once:true}); flood.addEventListener('error', reject, {once:true}); });
  let rejected = false; flood.addEventListener('close', () => { rejected = true; });
  const floodEnvelope = Buffer.concat([Buffer.from([0x4d, 0x42, 1, 1, 0, 1]), frame(23, 0)]);
  for (let i = 0; i < 2048; i++) flood.send(floodEnvelope);
  for (let i = 0; i < 200 && !rejected; i++) await sleep(10);
  assert(rejected, 'Single-group message budget closes the aggressive Bridge');
  assert(!sibling.closed && !visitor.closed, 'Unrelated households survive the aggressive Bridge');
  console.log(JSON.stringify({ passed: true, runtime: 'local workerd', cases: [
    'routing', 'upgrade', 'capacity', 'presence', 'full-roster-checkpoint', 'room-isolation', 'wrong-room', 'malformed', 'leave', 'household-multiplex', 'stream-fault-isolation', 'message-budget'
  ] }));
} catch (error) {
  console.error(log); throw error;
} finally {
  for (const c of clients) c.close();
  proc.kill('SIGTERM');
  await Promise.race([new Promise(resolve => proc.once('exit', resolve)), sleep(3000)]);
  if (proc.exitCode === null) proc.kill('SIGKILL');
  await rm(temp, { recursive: true, force: true });
}
