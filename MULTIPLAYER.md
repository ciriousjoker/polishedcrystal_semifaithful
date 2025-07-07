# Polished Crystal Multiplayer Protocol

This document outlines the multiplayer communication protocol used in Polished Crystal. The protocol is a full-duplex, piggybacked-acknowledgment system designed for the Game Boy's serial link cable, operating on a per-VBlank frame basis.

## Core Concepts

- **Full-Duplex:** Both Game Boys can send and receive data simultaneously during each serial transfer.
- **Nibble-Based:** Communication is broken down into 4-bit chunks called "nibbles". A full byte requires two separate transfers (one for the high nibble, one for the low nibble).
- **Piggybacked ACK:** Instead of sending dedicated acknowledgment packets, the acknowledgment for a received nibble is "piggybacked" onto the next outgoing data nibble. This is highly efficient.
- **Stateful Handshake:** A `SEQ` (sequence) and `ACK` (acknowledgment) bit are used to ensure each nibble is sent and received correctly, preventing data loss or duplication.
- **Packages:** Data is transmitted in fixed-size blocks called packages. A new package transmission is always preceded by a special `noop` byte (`0xFF`) to signal its start and synchronize the link.

---

## The Transmission Byte

Each 8-bit value placed in the `rSB` (Serial Transfer Data) register is meticulously structured. It contains one 4-bit data nibble plus 4 bits of metadata for the handshake.

| Bit | Name                | Description                                                                                                          |
| :-- | :------------------ | :------------------------------------------------------------------------------------------------------------------- |
| 7   | `SEQ` (Sequence)    | Flips (0/1) for each new nibble sent. Used by the receiver to detect duplicate or missed nibbles.                    |
| 6   | `ACK` (Acknowledge) | Flips (0/1) to acknowledge the successful receipt of the remote player's last nibble.                                |
| 5   | `H/L` (High/Low)    | Indicates which part of a byte is being sent. `1` for the high nibble (bits 7-4), `0` for the low nibble (bits 3-0). |
| 4   | `M` (Master/Slave)  | Identifies the device's role. `1` for the master, `0` for the slave.                                                 |
| 3-0 | `Payload`           | The 4-bit data nibble.                                                                                               |

---

## The Handshake Process

The entire transmission is managed by a state machine that runs on every VBlank interrupt.

1.  **Check Transfer Status:** The routine first checks if a serial transfer is already in progress. If so, it waits for the next frame.

2.  **Validate Received Nibble:** Upon completion of a transfer, the received byte from `rSB` is validated.

    - Received `AckIn` must be a flipped version of the last sent `SeqOut` (and vice versa). If it doesn't match, it means the other player did not correctly receive the last nibble. This is a critical error, and the entire package transmission is restarted from the beginning (`.restart_package`).
    - The received `SEQ` bit is checked. If it's the same as the last `SEQ` received from that player, the nibble is ignored as a duplicate.

3.  **Process Valid Nibble:** If the `ACK` and `SEQ` bits are valid:

    - The 4-bit `Payload` is extracted and stored.
    - The `H/L` bit determines if this is the high or low part of a byte, and the byte is assembled accordingly.
    - Once a full byte is received, the byte counter is incremented.
    - Once all bytes for a package are received, the package is marked as ready for execution.

4.  **Prepare Next Outgoing Nibble:**

    - The state machine prepares the next nibble to send.
    - The `ACK` bit is flipped to acknowledge the valid nibble that was just received.
    - The `SEQ` bit is flipped to mark this new outgoing nibble as unique.
    - The `H/L` bit is set based on whether the high or low nibble of the current byte in the send buffer is next.
    - The payload is loaded from the buffered package.

5.  **Start Transfer:** The newly constructed 8-bit value is written to `rSB`, and a serial transfer is initiated.

This creates a reliable, lock-step sequence:

- Master sends Nibble A (with `SEQ=0`).
- Slave receives Nibble A, validates it, and prepares a response.
- Slave sends Nibble B (with its own `SEQ=0` and an `ACK=1` to acknowledge Master's `SEQ=0`).
- Master receives Nibble B, validates the `ACK`, and prepares its next nibble.
- Master sends Nibble C (with `SEQ=1` and an `ACK=1` to acknowledge Slave's `SEQ=0`).
  ...and so on.

---

## Simulation: Transmitting "123" ↔ "abc"

The following table demonstrates a full transmission of a 3-byte package between a Master and a Slave device. The master sends "123" and the slave sends "abc".

- **Package Start:** The transmission begins with a `noop` byte (`0xFF`) from both sides to signal the start of a new package and synchronize.
- **Payload Data:**
  - Master sends `'1'` (`0x31`), `'2'` (`0x32`), `'3'` (`0x33`).
  - Slave sends `'a'` (`0x61`), `'b'` (`0x62`), `'c'` (`0x63`).

Note:
In this example, imagining the initial `noop` byte on the master is left as an exercise for the reader (ie I forgot).

| comment                                  | Seq | Ack | H/L | M/S | data    | send        | Ack success | Sync Status | Ack Success | Seq | Ack   | H/L | M/S | data    | send   |
| ---------------------------------------- | --- | --- | --- | --- | ------- | ----------- | ----------- | ----------- | ----------- | --- | ----- | --- | --- | ------- | ------ |
| both floating                            | 1   | 1   | 1   | 1   | 1 1 1 1 |             | 0           | failed      | 0           | 1   | 1     | 1   | 1   | 1 1 1 1 |        |
| master: starts packet (default Seq, ...) | 0   | 0   | 0   | 1   | 0 0 1 1 | h-1         | 0           | failed      | 0           | 1   | 1     | 1   | 1   | 1 1 1 1 |        |
| master restarts packet (no ack)          | 0   | 0   | 0   | 1   | 0 0 1 1 | h-1         | 1           | success     | 1           | 0   | 0     | 0   | 0   | 1 1 1 1 | h-noop |
|                                          | 1   | 1   | 1   | 1   | 0 0 0 1 | l-1         | 1           | success     | 1           | 1   | 1     | 1   | 0   | 1 1 1 1 | l-noop |
| Hardware error lead to failed ack        | 0   | 0   | 0   | 1   | 0 0 1 1 | h-2         | 0           | failed      | 1           | 0   | 1 (E) | 0   | 0   | 1 1 1 1 | h-noop |
| master restarts packet (no ack)          | 0   | 0   | 0   | 1   | 0 0 1 1 | h-1         | 0           | failed      | 0           | 1   | 1     | 1   | 0   | 1 1 1 1 | l-noop |
| both restart packet                      | 0   | 0   | 0   | 1   | 0 0 1 1 | h-1         | 1           | success     | 1           | 0   | 0     | 0   | 0   | 1 1 1 1 | h-noop |
| both in sync again                       | 1   | 1   | 1   | 1   | 0 0 0 1 | l-1         | 1           | success     | 1           | 1   | 1     | 1   | 0   | 1 1 1 1 | l-noop |
|                                          | 0   | 0   | 0   | 1   | 0 0 1 1 | h-2         | 1           | success     | 1           | 0   | 0     | 0   | 0   | 0 1 1 0 | h-a    |
|                                          | 1   | 1   | 1   | 1   | 0 0 1 0 | l-2         | 1           | success     | 1           | 1   | 1     | 1   | 0   | 0 0 0 1 | l-a    |
|                                          | 0   | 0   | 0   | 1   | 0 0 1 1 | h-3         | 1           | success     | 1           | 0   | 0     | 0   | 0   | 0 1 1 0 | h-b    |
| package fully transmitted to slave       | 1   | 1   | 1   | 1   | 0 0 1 1 | l-3         | 1           | success     | 1           | 1   | 1     | 1   | 0   | 0 0 0 1 | l-b    |
| master: nothing else to send, send noop  | 0   | 0   | 0   | 1   | 1 1 1 1 | noop part 1 | 1           | success     | 1           | 0   | 0     | 0   | 0   | 0 1 1 0 | h-c    |
| package fully transmitted to master      | 1   | 1   | 1   | 1   | 1 1 1 1 | noop part 2 | 1           | success     | 1           | 1   | 1     | 1   | 0   | 0 0 0 1 | l-c    |
