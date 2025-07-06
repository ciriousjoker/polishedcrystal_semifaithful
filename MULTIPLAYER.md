# Frame-Based Multiplayer System Documentation

## Overview

This implementation provides a full-duplex, frame-based multiplayer communication system for Pokemon Crystal using the Z80 Game Boy Link Cable. The system operates on a piggy-back-ACK protocol that allows two Game Boys to exchange 8-byte packages reliably.

## Architecture

### Package Structure

Each multiplayer package consists of 8 bytes of data that can contain any type of information (player movement, battle commands, chat messages, etc.).

### Communication Flow

1. **Package Queuing**: `MultiplayerQueuePackage()` queues an 8-byte package for transmission
2. **Package Buffering**: `MultiplayerSendPackage()` moves queued packages to the transmission buffer
3. **Nibble Transmission**: `MultiplayerSendReceiveNibble()` handles the actual bit-level communication
4. **Package Reception**: `MultiplayerOnPackageReceived()` processes complete received packages

## Implementation Details

### Core Functions

#### `MultiplayerInitialize()`

Initializes all multiplayer buffers and state variables. Should be called once at startup.

#### `MultiplayerQueuePackage(HL)`

Queues an 8-byte package for transmission.

- Input: HL = pointer to 8-byte data to send
- The package will be transmitted when the link is available

#### `MultiplayerSendPackage()`

Called once per frame. Moves queued packages to the transmission buffer if no package is currently being transmitted.

#### `MultiplayerSendReceiveNibble()`

The core communication function called once per frame. Handles:

- Processing incoming nibbles with sequence validation
- Managing ACK/timeout for outgoing nibbles
- Preparing and transmitting the next nibble

#### `HandleMultiplayerPackage()`

Processes complete received packages. Override this function to handle specific package types.

### VBlank Integration

The multiplayer system integrates with the VBlank interrupt through `MultiplayerVBlankHandler()`, which is called every frame from the main VBlank routine.

## SB Register Format

Every frame, an 8-bit value is transmitted via the SB register with the following bit layout:

```
Bit: [ 7    6    5    4 ][3 2 1 0]
     [Seq][Ack][H/L][M/S][Payload]
```

- **Bit 7 (SeqOut)**: Toggle bit that changes 0→1→0... each time a nibble is successfully acknowledged
- **Bit 6 (AckOut)**: Mirror of the last received SeqIn bit (acknowledgment)
- **Bit 5 (H/L)**: 1 = High nibble (bits 7-4), 0 = Low nibble (bits 3-0)
- **Bit 4**: Master (1) or Slave (0) indicator. Necessary so the cartridge can ignore its own nibbles
- **Bits 3-0**: 4-bit payload nibble

## Frame-by-Frame Example

Here's a simulation of sending "123" from Master to Slave, with "abc" being sent back starting at frame 3:

```
| Frame | SB Master    | M→S nibble | SB Slave     | S→M nibble | Comment                                  |
| ----- | ------------ | ---------- | ------------ | ---------- | ---------------------------------------- |
| 0     | 0 0 1 1 0011 | h-1 (0x3)  | 1 1 1 1 1111 | disconnect | Master sends high-'1'. slave isn't connected, therefore rSB is floating at 0xFF.        |
| 1     | 0 0 1 1 0011 | h-1 (0x3)  | 0 0 0 0 1111 | ack h-1 ---| Master sends high-'1' again, because 0xFF is invalid. slave acks high-'1' and payload of 0xF, representing "nothing"       |
| 2     | 1 0 0 1 0001 | l-1 (0x1)  | 0 0 0 0 1111 | ack h-1 ---| Master acks nothing because payload 0xF is invalid and sends low-'1'. slave acks high-'1' (again) and sends 0xF, ie "nothing" |
| 3     | 1 0 1 1 0011 | 2 H (0x3)  | 0 1 1 0 0110 | a H (0x6)  | Master acks nothing because 0xF is invalid and sends high-'2'. slave acks low-'1' sends high-'a'                           |
| 4     | 1 0 0 1 0010 | 2 L (0x2)  | 1 0 0 0 0001 | a L (0x1)  | Master acks high-'a' and sends low-'2'. slave acks high-'2' and sends low-'a'      |
| 5     | 0 1 1 1 0011 | 3 H (0x3)  | 0 0 1 0 0110 | b H (0x6)  | Master acks low-'a' and sends high-'3'. slave acks low-'2' and sends high-'b'      |
| 6     | 1 0 0 1 0011 | 3 L (0x3)  | 1 0 0 0 0010 | b L (0x2)  | Master acks high-'b' and sends low-'3'. slave acks high-'3'                            |
| 7     | 1 1 0 1 0000 | idle       | 0 0 1 0 0110 | c H (0x6)  | Master done, slave continues             |
| 8     | 1 0 0 1 0000 | idle       | 1 1 0 0 0011 | c L (0x3)  | Transmission complete                    |
```

## Testing

The multiplayer system operates automatically:

- Initialization happens when toggling master/slave status
- Player movement data is sent automatically when stepping
- Package reception and processing happens in VBlank

### Package Types

The system supports different package types:

- `$01`: Player movement data
- `$02`: Battle command (future)
- `$03`: Chat message (future)
- `$FF`: Debug/test data (future)

## Error Handling

- **Timeout**: If no ACK is received within 60 frames, the package is discarded
- **Sequence Validation**: Duplicate or out-of-order nibbles are ignored
- **Buffer Overflow**: Prevents writing beyond buffer boundaries

## Performance Considerations

- The system processes one nibble per frame (60 Hz)
- An 8-byte package takes 16 frames to transmit (≈267ms)
- Full-duplex operation allows simultaneous bidirectional communication
- The system adds minimal overhead to VBlank processing

## Future Enhancements

- Compression for frequently sent data
- Priority queuing for urgent packages
- Automatic retry for critical packages
- Extended package types for specific game features

This multiplayer system provides a robust foundation for real-time Game Boy communication while maintaining the game's 60 FPS performance.

---

Prompt:

I need to write the following functions:

called on step:
QueuePackage (queues an 8 byte package of "12345678" to send it once the current package has been delivered)

called on every frame:
SendPackage

- if queued package is empty -> ret
- if buffered package isn't empty -> ret
- copy queued package to buffered package

SendReceiveNibble

- Checks if there's an incoming nibble and stores it in a buffer
- Checks if there's an incoming ack for the currently waiting-to-be-acknowledged nibble
- If there is no ack within 60 VBlanks -> timeout, ie destroy buffered package
- If there is a valid ack -> increase send counter, flip seq bit variable
- Every second nibble: combines the two nibbles in a single byte
- on every byte: stores received byte into receivedPackage (until its full)
- If receivedPackage is full: call OnPackageReceived
- Determines the next nibble to send based on the queued package and a sendCounter
- Update SB with the new buffer: seq bit, acknowledge nibble if necessary, ..., nibble data to send
- If master: trigger the transmission exchange

Called on full transmission
OnPackageReceived:

- copy received package to packageToExecute buffer
- Call HandlePackage

HandlePackage

- just simple debug textbox for now
- wipes packageToExecute buffer afterwards

---

Write a very detailed explanation on how to implement each of these functions and where to call them. (no specific code, just general guidance)
Write a very detailed explanation on what variables are necessary to make this work.

An experienced z80 gbc developer should be able to implement this in an assembly decomp of Pokemon Crystal.

Include:

- list of function stubs with a series of detailed comments & pseudocode
- list of variables
- package structure of a single nibble package and what each bit indicates and why (be very detailed here!!)
- full simulation for a transmission of "123" from master->slave + "abc" from slave->master ("a" starts at "2" to simulate the asynchronous nature)
