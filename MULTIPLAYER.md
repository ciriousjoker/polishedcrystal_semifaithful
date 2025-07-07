# Polished Crystal Multiplayer Protocol

This document outlines the multiplayer communication protocol used in Polished Crystal. The protocol is a full-duplex, piggybacked-acknowledgment system designed for the Game Boy's serial link cable, operating on a per-VBlank frame basis.

## Core Concepts

- **Full-Duplex:** Both Game Boys can send and receive data simultaneously during each serial transfer.
- **Chunk-Based:** Communication is broken down into 2-bit chunks. A full nibble requires two separate transfers (one for the high 2 bits, one for the low 2 bits), and a full byte requires four transfers.
- **Piggybacked ACK:** Instead of sending dedicated acknowledgment packets, the acknowledgment for a received chunk is "piggybacked" onto the next outgoing data chunk. This is highly efficient.
- **Stateful Handshake:** A `SEQ` (sequence) and `ACK` (acknowledgment) bit are used to ensure each chunk is sent and received correctly, preventing data loss or duplication.
- **Packages:** Data is transmitted in fixed-size blocks called packages. A new package transmission is always preceded by a special `noop` byte to signal its start and synchronize the link.
- **Valid Bit:** Bit 3 is always 0 in valid transmissions, allowing differentiation from floating serial lines (which read as `0xFF`).

---

## The Transmission Byte

Each 8-bit value placed in the `rSB` (Serial Transfer Data) register is meticulously structured. It contains one 2-bit data chunk plus 6 bits of metadata for the handshake.

| Bit | Name                | Description                                                                                                          |
| :-- | :------------------ | :------------------------------------------------------------------------------------------------------------------- |
| 7   | `V` (Valid)         | Always `0` to differentiate valid transmissions from floating line (`0xFF`).                                        |
| 6   | `SEQ` (Sequence)    | Flips (0/1) for each new chunk sent. Combined with full rSB comparison, provides robust duplicate detection.                     |
| 5   | `ACK` (Acknowledge) | Flips (0/1) to acknowledge the successful receipt of the remote player's last chunk.                                |
| 4   | `M` (Master/Slave)  | Identifies the device's role. `1` for the master, `0` for the slave.                                                 |
| 3   | `N` (Nibble Index)  | Indicates which nibble of a byte is being sent. `1` for the high nibble (bits 7-4), `0` for the low nibble (bits 3-0). |
| 2   | `C` (Chunk Index)   | Indicates which 2-bit chunk of a nibble is being sent. `1` for high chunk (bits 3-2), `0` for low chunk (bits 1-0). |
| 1-0 | `Payload`           | The 2-bit data chunk.                                                                                               |

In summary:
- "Package": A 8-byte block of data sent over the serial link
- "Transmission Byte": A single byte sent over the serial link via `rSB`.
- "Nibble": 4 bits of data, aka two 2-bit chunks
- "Chunk": A 2-bit part of a nibble, sent over a single transmission byte

---

## The Handshake Process

The entire transmission is managed by a state machine that runs on every VBlank interrupt.

1.  **Check Transfer Status:** The routine first checks if a serial transfer is already in progress. If so, it waits for the next frame.

2.  **Validate Received Chunk:** Upon completion of a transfer, the received byte from `rSB` is validated.

    - **Duplicate Detection:** The received `rSB` value is compared with the last received `rSB` value. If they are identical, this indicates a duplicate packet (common with emulator speed mismatches) and the packet is ignored. This provides robust tolerance for timing differences between emulators.
    - Bit 7 must be 0 (Valid bit). If it's 1, this indicates a floating line or invalid transmission.
    - Received `AckIn` (bit 5) must be a flipped version of the last sent `SeqOut` (and vice versa). If it doesn't match, it means the other player did not correctly receive the last chunk. This is a critical error, and the entire package transmission is restarted from the beginning (`.restart_package`).

3.  **Process Valid Chunk:** If the `ACK` and `SEQ` bits are valid:

    - The 2-bit `Payload` (bits 1-0) is extracted and stored.
    - The `N` bit (bit 3) determines if this is the high or low nibble of a byte.
    - The `C` bit (bit 2) determines if this is the high or low chunk of a nibble.
    - Chunks are assembled into nibbles, nibbles into bytes, and bytes into packages.
    - Once all bytes for a package are received, the package is marked as ready for execution.

4.  **Prepare Next Outgoing Chunk:**

    - The state machine prepares the next chunk to send.
    - The `ACK` bit (bit 5) is flipped to acknowledge the valid chunk that was just received.
    - The `SEQ` bit (bit 6) is flipped to mark this new outgoing chunk as unique.
    - The `N` bit (bit 3) is set based on whether the high or low nibble of the current byte in the send buffer is next.
    - The `C` bit (bit 2) is set based on whether the high or low chunk of the current nibble is next.
    - The payload (bits 1-0) is loaded from the buffered package.

5.  **Start Transfer:** The newly constructed 8-bit value is written to `rSB`, and a serial transfer is initiated.

This creates a reliable, lock-step sequence:

- Master sends Chunk A (with `SEQ=0`).
- Slave receives Chunk A, validates it, and prepares a response.
- Slave sends Chunk B (with its own `SEQ=0` and an `ACK=1` to acknowledge Master's `SEQ=0`).
- Master receives Chunk B, validates the `ACK`, and prepares its next chunk.
- Master sends Chunk C (with `SEQ=1` and an `ACK=1` to acknowledge Slave's `SEQ=0`).
  ...and so on.

---

## Simulation: Transmitting "123" ↔ "abc"

The following table demonstrates a full transmission of a 3-byte package between a Master and a Slave device. The master sends "123" and the slave sends "abc".

- **Package Start:** The transmission begins with a `noop` byte from both sides to signal the start of a new package and synchronize.
- **Payload Data:**
  - Master sends `'1'` (`0x31`), `'2'` (`0x32`), `'3'` (`0x33`).
  - Slave sends `'a'` (`0x61`), `'b'` (`0x62`), `'c'` (`0x63`).

Note:
In this example, imagining the initial `noop` byte on the master is left as an exercise for the reader (ie I forgot).

| comment                                  | V | Seq | Ack | M/S | N | C | data | send        | Ack success | Sync Status | Ack Success | V | Seq | Ack | M/S | N | C | data | send   |
| comment                                  | V | Seq | Ack | M/S | N | C | data | send        | Ack success | Sync Status | Ack Success | V | Seq | Ack | M/S | N | C | data | send   |
| ---------------------------------------- | - | --- | --- | --- | - | - | ---- | ----------- | ----------- | ----------- | ----------- | - | --- | --- | --- | - | - | ---- | ------ |
| both floating                            | 1 | 1   | 1   | 1   | 1 | 1 | 1 1  |             | 0           | failed      | 0           | 1 | 1   | 1   | 1   | 1 | 1 | 1 1  |        |
| master: starts packet (default Seq, ...) | 0 | 0   | 0   | 1   | 0 | 0 | 1 1  | h-1         | 0           | failed      | 0           | 1 | 1   | 1   | 1   | 1 | 1 | 1 1  |        |
| master restarts packet (no ack)          | 0 | 0   | 0   | 1   | 0 | 0 | 1 1  | h-1         | 1           | success     | 1           | 0 | 0   | 0   | 0   | 1 | 1 | 1 1  | h-noop |
|                                          | 0 | 1   | 1   | 1   | 0 | 0 | 0 1  | l-1         | 1           | success     | 1           | 0 | 1   | 1   | 0   | 1 | 1 | 1 1  | l-noop |
| Hardware error lead to failed ack        | 0 | 0   | 0   | 1   | 0 | 0 | 1 1  | h-2         | 0           | failed      | 1           | 0 | 0   | 1   | 0   | 1 | 1 | 1 1  | h-noop |
| master restarts packet (no ack)          | 0 | 0   | 0   | 1   | 0 | 0 | 1 1  | h-1         | 0           | failed      | 0           | 0 | 1   | 1   | 0   | 1 | 1 | 1 1  | l-noop |
| both restart packet                      | 0 | 0   | 0   | 1   | 0 | 0 | 1 1  | h-1         | 1           | success     | 1           | 0 | 0   | 0   | 0   | 1 | 1 | 1 1  | h-noop |
| both in sync again                       | 0 | 1   | 1   | 1   | 0 | 0 | 0 1  | l-1         | 1           | success     | 1           | 0 | 1   | 1   | 0   | 1 | 1 | 1 1  | l-noop |
|                                          | 0 | 0   | 0   | 1   | 0 | 0 | 1 1  | h-2         | 1           | success     | 1           | 0 | 0   | 0   | 0   | 0 | 1 | 1 0  | h-a    |
|                                          | 0 | 1   | 1   | 1   | 0 | 0 | 1 0  | l-2         | 1           | success     | 1           | 0 | 1   | 1   | 0   | 0 | 0 | 0 1  | l-a    |
|                                          | 0 | 0   | 0   | 1   | 0 | 0 | 1 1  | h-3         | 1           | success     | 1           | 0 | 0   | 0   | 0   | 0 | 1 | 1 0  | h-b    |
| package fully transmitted to slave       | 0 | 1   | 1   | 1   | 0 | 0 | 1 1  | l-3         | 1           | success     | 1           | 0 | 1   | 1   | 0   | 0 | 0 | 0 1  | l-b    |
| master: nothing else to send, send noop  | 0 | 0   | 0   | 1   | 1 | 1 | 1 1  | noop part 1 | 1           | success     | 1           | 0 | 0   | 0   | 0   | 0 | 1 | 1 0  | h-c    |
| package fully transmitted to master      | 0 | 1   | 1   | 1   | 1 | 1 | 1 1  | noop part 2 | 1           | success     | 1           | 0 | 1   | 1   | 0   | 0 | 0 | 0 1  | l-c    |
