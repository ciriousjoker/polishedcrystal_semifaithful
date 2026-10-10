# Milestone 1 transport contract

This is the implemented echo transport, not the complete lobby/battle/trade
schema. Each ROM uses one TCP stream to the server. Server connection IDs are
independent, monotonically assigned session IDs; no global opponent exists.
Milestone 2 adds player identity, compatibility negotiation and lobby messages.

## Application framing

| Offset | Size | Meaning |
| --- | --- | --- |
| 0 | 2 | ASCII `PC` (`50 43`) |
| 2 | 1 | Version 1 |
| 3 | 1 | Type: 1 echo request, 2 echo reply |
| 4 | 2 | Attempt sequence, unsigned big endian |
| 6 | 2 | Payload length, unsigned big endian, maximum 120 |
| 8 | 0..120 | Payload |

The proof sends `TEST` and accepts the matching sequence, reply type and four
payload bytes. The server parses successive frames from a stream, rejecting
invalid headers before allocating a body. EOF inside a frame is malformed input.
The ROM reassembles successive adapter DATA responses into a checked 128-byte
buffer. Socket reads and adapter responses are not application frame boundaries.
An oversized, truncated, incorrect or stale response cannot count as success.

Request vector, sequence 1:

```text
50 43 01 01 00 01 00 04 54 45 53 54
```

Reply vector:

```text
50 43 01 02 00 01 00 04 54 45 53 54
```

The request is exported as `MobileRequestVector` from the production assembly.
The host assembly contract compiles that actual driver and compares its emitted
bytes with the Rust codec's fixed wire vector. These are ABI checks, not executed
assembly unit tests.

## Adapter transcript

Pinned DoubleCherryGB uses blue device ID `88`, embedded libmobile, 8-bit full
duplex and a one-transfer output latch. The request has magic `99 66`, command,
reserved zero, high length zero, low length, payload, unsigned 16-bit checksum of
command/header/payload in big endian, then `80 00 4b 4b`. The first two trailing
clocks receive device ID and `command XOR 80`. Poll `4b` for response magic;
acknowledge its final device/acknowledgement clocks with `80 00`.

| Step | Command | Request payload | Successful response payload size |
| --- | --- | --- | --- |
| 0 | `10` start | `4e 49 4e 54 45 4e 44 4f` | 8 |
| 1 | `12` dial ISP | `00 23 39 36 37 37` | 0 |
| 2 | `21` PPP login | ten zero bytes | 12 |
| 3 | `23` TCP connect | `7f 00 00 01 44 0f` | 1 connection ID |
| 4 | `15` data | connection ID, then application request once | connection ID and 0..254 data bytes |
| 5 | `24` TCP close | connection ID | 1 |
| 6 | `22` PPP logout | empty | 0 |
| 7 | `13` offline | empty | 0 |
| 8 | `11` end | empty | 0 |

The response command is `request OR 80`; `ee` reports an adapter error. Validate
reserved bytes, length, checksum and device ID before consuming application data.
Do not connect to libmobile's telephone relay port 31227 for this protocol.
The implemented proof dials the ISP path and connects directly to TCP
`127.0.0.1:17423`; telephone relay is disabled in the test frontend.

## Ownership, deadlines and errors

The ROM pumps bounded work after DelayFrame's completed VBlank wait. Serial IRQ
work is a bounded byte state machine, with bursts capped at 24 bytes. No adapter
wait, application parser, `DelayFrame` or `ei` is executed in its handler. The
existing serial handler remains available when the mobile owner is zero.
Cable/printer entry hooks first release mobile ownership.

There are 120 service ticks per command and 600 per attempt. Failures release
SC/SB and restore the previous serial IE bit, preserving other IE bits. A 240-tick
cooldown lets libmobile's partial-packet timeout expire before another START.
An accepted Select+B chord starts or cancels the developer proof and consumes
only those two buttons, avoiding the original registered-item Select script.

| Error | Meaning |
| --- | --- |
| 1 | User cancellation |
| 2 | Command or attempt timeout |
| 3 | Malformed adapter packet or unexpected command |
| 4 | Adapter failure, unavailable adapter or TCP endpoint |
| 5 | Invalid application response |
| 6 | Cable or printer acquired serial ownership |

The emulator adapter's timeout callbacks use emulated CPU clocks. Actual sockets
use host time. Live acceptance is paced at the core's reported frame rate after
fast offline startup; accelerated startup is not a latency measurement.
