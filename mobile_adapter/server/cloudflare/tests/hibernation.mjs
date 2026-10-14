// Force real workerd hibernation with four players across two household sockets.
import assert from 'node:assert/strict';
import { fileURLToPath } from 'node:url';
import { setTimeout as sleep } from 'node:timers/promises';
import { Miniflare, convertV4MiniflareOptions } from 'miniflare';
import { COUNTERPART, Client, client, frame, hello, presence } from './wire.mjs';

const name = 'polishedcrystal-mobile';
const mf = new Miniflare(convertV4MiniflareOptions({
  name, host: '127.0.0.1', port: 0,
  modulesRoot: fileURLToPath(new URL('../build/worker/', import.meta.url)),
  modules: [
    { type: 'ESModule', path: fileURLToPath(new URL('../build/worker/shim.mjs', import.meta.url)) },
    { type: 'CompiledWasm', path: fileURLToPath(new URL('../build/worker/index.wasm', import.meta.url)) },
  ],
  compatibilityDate: '2026-10-11',
  durableObjects: { LOBBIES: { className: 'LobbyObject', useSQLite: true } },
}));
const clients = [];
try {
  const origin = (await mf.ready).origin;
  const first = await client(origin, 17);
  const second = await client(origin, 17);
  clients.push(first, new Client(first.ws, 2), second, new Client(second.ws, 2));
  const incompatible = Buffer.from(COUNTERPART);
  incompatible[31] ^= 1;
  // Rejected streams share each healthy household's actual accepted socket.
  const rejected = [new Client(first.ws, 3), new Client(second.ws, 3)];
  async function rejectIdentity(c, identity, room = 17) {
    c.send(hello(room, identity?.readUInt16BE(0) ?? 0,
      identity?.subarray(6, 14) ?? Buffer.alloc(8), incompatible));
    // Inspect the next response directly: an accidental WELCOME must fail.
    const response = await c.next();
    assert.equal(response[3], 22, 'Mismatched identity never participates');
    assert.equal(response[8], 2, 'Mismatched counterpart is incompatible');
  }
  const identities = [];
  for (const c of clients) {
    c.send(hello(17));
    identities.push((await c.next(data => data[3] === 17)).subarray(8));
    if (identities.length === 1) await rejectIdentity(rejected[1]);
  }
  for (let round = 0; round < 4; round++) {
    const xs = clients.map((_, i) => 100 + 8 * round + i);
    for (const [i, c] of clients.entries()) c.send(presence(identities[i], 2 + round, xs[i]));
    for (const [i, c] of clients.entries()) {
      const expected = xs.filter((_, peer) => peer !== i).sort((a, b) => a - b);
      await c.next(data => {
        if (data[3] !== 19 || data[12] !== 3) return false;
        const actual = [0, 1, 2].map(peer =>
          (data[25 + 17 * peer] << 4) | (data[26 + 17 * peer] >> 4)).sort((a, b) => a - b);
        return actual.every((x, peer) => x === expected[peer]);
      });
    }
    // Clear a previous round's timer and retire the Rust object instance while
    // retaining actual accepted WebSockets and their serialized attachments.
    await sleep(60);
    await mf.unsafeEvictDurableObject(name, 'LobbyObject', {
      name: 'lobby:17', webSockets: 'hibernate',
    });
    assert(clients.every(c => !c.closed), 'Hibernation must retain both household sockets');
  }
  // Wake once more and prove each pre-hibernation identity remains authorized.
  clients[0].send(presence(identities[0], 6, 220));
  await clients[1].next(data => data[3] === 19 && data[12] === 3 &&
    [0, 1, 2].some(peer => data[25 + 17 * peer] === 13 && data[26 + 17 * peer] >> 4 === 12));
  // Empty ownership may adopt another counterpart, including after restoration.
  for (const [i, c] of clients.entries()) {
    c.send(frame(20, 7, identities[i].subarray(0, 4)));
    await c.next(data => data[3] === 21);
  }
  await sleep(60);
  await mf.unsafeEvictDurableObject(name, 'LobbyObject', {
    name: 'lobby:17', webSockets: 'hibernate',
  });
  first.send(hello(17, 0, Buffer.alloc(8), incompatible));
  const rebound = await first.next(data => data[3] === 17);
  assert.equal(rebound.readUInt16BE(12), 17, 'Empty restored owner retains routed room');
  assert.equal((await first.next(data => data[3] === 19))[12], 0,
    'Recreated empty owner does not restore departed peers');
  // Leave one capacity slot available so the owner reaches compatibility checks.
  // Full owners reject new streams with Full before inspecting resume credentials.
  const a = await client(origin, 18);
  const b = await client(origin, 18);
  const members = [a, new Client(a.ws, 2), b];
  clients.push(...members);
  const denied = new Client(b.ws, 3);
  const retained = [];
  for (const c of members) {
    c.send(hello(18));
    retained.push((await c.next(data => data[3] === 17)).subarray(8));
  }
  await rejectIdentity(denied, retained[0], 18);
  await sleep(60);
  await mf.unsafeEvictDurableObject(name, 'LobbyObject', {
    name: 'lobby:18', webSockets: 'hibernate',
  });
  // The mismatch wakes the actual Rust owner and deserializes its ES Maps.
  await rejectIdentity(denied, retained[0], 18);
  const fourth = new Client(b.ws, 2);
  clients.push(fourth);
  fourth.send(hello(18));
  retained.push((await fourth.next(data => data[3] === 17)).subarray(8));
  members.push(fourth);
  for (const [i, c] of members.entries()) c.send(presence(retained[i], 2, 230 + i));
  for (const [i, c] of members.entries()) {
    const expected = retained.filter((_, peer) => peer !== i)
      .map(identity => identity.subarray(0, 4).toString('hex')).sort();
    const roster = await c.next(data => data[3] === 19 && data[12] === 3 &&
      [0, 1, 2].every(peer => (data[25 + peer * 17] << 4 |
        data[26 + peer * 17] >> 4) >= 230));
    assert.deepEqual([0, 1, 2].map(peer => roster.subarray(13 + peer * 17,
      17 + peer * 17).toString('hex')).sort(), expected,
    'Rejected restored takeover preserves every healthy player and generation');
  }
  console.log(JSON.stringify({ passed: true, runtime: 'local workerd',
    cases: ['four-players-two-households', 'full-roster-attachment',
      'four-forced-hibernations', 'identity-and-position-recovery',
      'fresh-counterpart-rejection', 'resume-counterpart-rejection-before-and-after-hibernation',
      'healthy-household-rosters-preserved', 'empty-restored-owner-counterpart-rebind'] }));
} finally {
  for (const c of clients) c.close();
  await mf.dispose();
}
