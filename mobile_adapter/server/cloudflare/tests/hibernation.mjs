// Force real workerd hibernation with four players across two household sockets.
import assert from 'node:assert/strict';
import { fileURLToPath } from 'node:url';
import { setTimeout as sleep } from 'node:timers/promises';
import { Miniflare, convertV4MiniflareOptions } from 'miniflare';
import { Client, client, hello, presence } from './wire.mjs';

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
  const identities = [];
  for (const c of clients) {
    c.send(hello(17));
    identities.push((await c.next(data => data[3] === 17)).subarray(8));
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
  console.log(JSON.stringify({ passed: true, runtime: 'local workerd',
    cases: ['four-players-two-households', 'full-roster-attachment',
      'four-forced-hibernations', 'identity-and-position-recovery'] }));
} finally {
  for (const c of clients) c.close();
  await mf.dispose();
}
