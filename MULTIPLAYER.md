# Polished Crystal Multiplayer Protocol

This document outlines the multiplayer communication protocol used in Polished Crystal. The protocol is a simplified, full-duplex system designed for the Game Boy's serial link cable, operating on a per-VBlank frame basis.

## Core Concepts

- **Full-Duplex:** Both Game Boys can send and receive data simultaneously during each serial transfer.
- **Nibble-Based:** Communication is broken down into 4-bit nibbles. A full byte requires two separate transfers (one for the high nibble, one for the low nibble).
- **Stateless:** No acknowledgment or sequence tracking is needed, making the protocol simple and reliable.
- **Packages:** Data is transmitted in fixed-size blocks called packages. A new package transmission is always preceded by a special `noop` byte to signal its start and synchronize the link.
- **Valid Bit:** Bit 7 is always 0 in valid transmissions, allowing differentiation from floating serial lines (which read as `0xFF`).

---

## The Transmission Byte

Each 8-bit value placed in the `rSB` (Serial Transfer Data) register is structured to contain one 4-bit data nibble plus 4 bits of metadata for routing and validation.

| Bit | Name                | Description                                                                                                          |
| :-- | :------------------ | :------------------------------------------------------------------------------------------------------------------- |
| 7   | `V` (Valid)         | Always `0` to differentiate valid transmissions from floating line (`0xFF`).                                        |
| 6   | `M` (Master/Slave)  | Identifies the device's role. `1` for the master, `0` for the slave.                                                 |
| 5   | `U` (Unused)        | Always `0` - reserved for future use.                                                                               |
| 4   | `N` (Nibble Index)  | Indicates which nibble of a byte is being sent. `0` for the high nibble (bits 7-4), `1` for the low nibble (bits 3-0). |
| 3-0 | `Payload`           | The 4-bit data nibble.                                                                                               |

In summary:
- "Package": A 8-byte block of data sent over the serial link
- "Transmission Byte": A single byte sent over the serial link via `rSB`.
- "Nibble": 4 bits of data, sent over a single transmission byte

---

## The Communication Process

The transmission is managed by a simple state machine that runs on every VBlank interrupt.

1.  **Check Transfer Status:** The routine first checks if a serial transfer is already in progress. If so, it waits for the next frame.

2.  **Validate Received Nibble:** Upon completion of a transfer, the received byte from `rSB` is validated.

    - **Own Nibble Detection:** The M/S bit (bit 6) is checked to determine if we received our own echoed nibble (which should be ignored).
    - **Valid Bit Check:** Bit 7 must be 0. If it's 1, this indicates a floating line or invalid transmission.
    - **Nibble Index Check:** The N bit (bit 4) must match the expected nibble type (high or low). If not, a desync is detected.

3.  **Process Valid Nibble:** If the nibble is valid:

    - The 4-bit `Payload` (bits 3-0) is extracted and stored.
    - The `N` bit (bit 4) determines if this is the high or low nibble of a byte.
    - Nibbles are assembled into bytes, and bytes into packages.
    - Once all bytes for a package are received, the package is marked as ready for execution.

4.  **Prepare Next Outgoing Nibble:**

    - The state machine prepares the next nibble to send.
    - The `M` bit (bit 6) is set based on whether this device is master or slave.
    - The `N` bit (bit 4) is set based on whether the high or low nibble of the current byte in the send buffer is next.
    - The payload (bits 3-0) is loaded from the buffered package.

5.  **Start Transfer:** The newly constructed 8-bit value is written to `rSB`, and a serial transfer is initiated.

This creates a simple, lock-step sequence where both sides exchange nibbles simultaneously:

- Master sends Nibble A (high nibble of byte 1).
- Slave sends Nibble A' (high nibble of byte 1).
- Master sends Nibble B (low nibble of byte 1).
- Slave sends Nibble B' (low nibble of byte 1).
- Master sends Nibble C (high nibble of byte 2).
- Slave sends Nibble C' (high nibble of byte 2).
  ...and so on.

---

## Simulation: Transmitting "123" ↔ "abc"

The following table demonstrates a full transmission of a 3-byte package between a Master and a Slave device. The master sends "123" and the slave sends "abc".

- **Package Start:** The transmission begins with a `noop` byte ($FF) from both sides to signal the start of a new package and synchronize.
- **Payload Data:**
  - Master sends `'1'` (`0x31`), `'2'` (`0x32`), `'3'` (`0x33`).
  - Slave sends `'a'` (`0x61`), `'b'` (`0x62`), `'c'` (`0x63`).

| Transfer | Comment                    | Master Sends                     | Slave Sends                      |
| :------- | :------------------------- | :------------------------------- | :------------------------------- |
|          |                            | V M U N Data \| Hex \| ASCII       | V M U N Data \| Hex \| ASCII       |
| 1        | Noop high nibble           | 0 1 0 0 1111 \| 4F  \| noop-high   | 0 0 0 0 1111 \| 0F  \| noop-high   |
| 2        | Noop low nibble            | 0 1 0 1 1111 \| 5F  \| noop-low    | 0 0 0 1 1111 \| 1F  \| noop-low    |
| 3        | '1' high nibble (0x3)      | 0 1 0 0 0011 \| 43  \| '1'-high    | 0 0 0 0 0110 \| 06  \| 'a'-high    |
| 4        | '1' low nibble (0x1)       | 0 1 0 1 0001 \| 51  \| '1'-low     | 0 0 0 1 0001 \| 11  \| 'a'-low     |
| 5        | '2' high nibble (0x3)      | 0 1 0 0 0011 \| 43  \| '2'-high    | 0 0 0 0 0110 \| 06  \| 'b'-high    |
| 6        | '2' low nibble (0x2)       | 0 1 0 1 0010 \| 52  \| '2'-low     | 0 0 0 1 0010 \| 12  \| 'b'-low     |
| 7        | '3' high nibble (0x3)      | 0 1 0 0 0011 \| 43  \| '3'-high    | 0 0 0 0 0110 \| 06  \| 'c'-high    |
| 8        | '3' low nibble (0x3)       | 0 1 0 1 0011 \| 53  \| '3'-low     | 0 0 0 1 0011 \| 13  \| 'c'-low     |

**Notes:**
- V (Valid) is always 0
- M (Master/Slave): 1 for master, 0 for slave
- U (Unused) is always 0
- N (Nibble): 0 for high nibble, 1 for low nibble
- Data: 4-bit payload nibble

The transmission is now twice as fast as the previous chunk-based protocol since we send 4 bits per transfer instead of 2 bits.
