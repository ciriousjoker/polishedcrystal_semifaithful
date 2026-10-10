//! Bounded framing shared by the ROM and Rust transport proof.
//! Socket reads are not packet boundaries. Lobby messages are added in milestone 2.

pub const HEADER_LEN: usize = 8;
pub const MAX_PAYLOAD: usize = 120;
pub const VERSION: u8 = 1;
pub const ECHO_REQUEST: u8 = 1;
pub const ECHO_REPLY: u8 = 2;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Frame {
    pub kind: u8,
    pub sequence: u16,
    pub payload: Vec<u8>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DecodeError {
    Truncated,
    Magic,
    Version,
    Length,
}

pub fn payload_len(header: &[u8]) -> Result<usize, DecodeError> {
    if header.len() < HEADER_LEN {
        return Err(DecodeError::Truncated);
    }
    if &header[..2] != b"PC" {
        return Err(DecodeError::Magic);
    }
    if header[2] != VERSION {
        return Err(DecodeError::Version);
    }
    let length = u16::from_be_bytes([header[6], header[7]]) as usize;
    if length > MAX_PAYLOAD {
        return Err(DecodeError::Length);
    }
    Ok(length)
}

impl Frame {
    pub fn encode(&self) -> Result<Vec<u8>, DecodeError> {
        if self.payload.len() > MAX_PAYLOAD {
            return Err(DecodeError::Length);
        }
        let mut bytes = vec![b'P', b'C', VERSION, self.kind];
        bytes.extend(self.sequence.to_be_bytes());
        bytes.extend((self.payload.len() as u16).to_be_bytes());
        bytes.extend(&self.payload);
        Ok(bytes)
    }

    pub fn decode(bytes: &[u8]) -> Result<Self, DecodeError> {
        let length = payload_len(bytes)?;
        if bytes.len() != HEADER_LEN + length {
            return Err(DecodeError::Truncated);
        }
        Ok(Self {
            kind: bytes[3],
            sequence: u16::from_be_bytes([bytes[4], bytes[5]]),
            payload: bytes[HEADER_LEN..].to_vec(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_rom_request_vector_and_big_endian_fields() {
        let frame = Frame {
            kind: ECHO_REQUEST,
            sequence: 1,
            payload: b"TEST".to_vec(),
        };
        assert_eq!(
            frame.encode().unwrap(),
            [0x50, 0x43, 1, 1, 0, 1, 0, 4, 0x54, 0x45, 0x53, 0x54]
        );
        let bytes = Frame {
            sequence: 0x1234,
            ..frame.clone()
        }
        .encode()
        .unwrap();
        assert_eq!(Frame::decode(&bytes).unwrap().sequence, 0x1234);
    }

    #[test]
    fn all_supported_lengths_round_trip_without_truncation() {
        for length in 0..=MAX_PAYLOAD {
            let frame = Frame {
                kind: ECHO_REPLY,
                sequence: 65535,
                payload: vec![0xa5; length],
            };
            assert_eq!(Frame::decode(&frame.encode().unwrap()).unwrap(), frame);
        }
    }

    #[test]
    fn rejects_invalid_headers_before_allocating_payload() {
        assert_eq!(payload_len(&[]), Err(DecodeError::Truncated));
        assert_eq!(
            payload_len(&[0, 0, 1, 1, 0, 0, 0, 0]),
            Err(DecodeError::Magic)
        );
        assert_eq!(
            payload_len(&[0x50, 0x43, 2, 1, 0, 0, 0, 0]),
            Err(DecodeError::Version)
        );
        assert_eq!(
            payload_len(&[0x50, 0x43, 1, 1, 0, 0, 0xff, 0xff]),
            Err(DecodeError::Length)
        );
        let frame = Frame {
            kind: 1,
            sequence: 0,
            payload: vec![0; MAX_PAYLOAD + 1],
        };
        assert_eq!(frame.encode(), Err(DecodeError::Length));
    }

    #[test]
    fn rejects_short_and_overlong_frame_slices() {
        let mut bytes = Frame {
            kind: 1,
            sequence: 7,
            payload: vec![1, 2],
        }
        .encode()
        .unwrap();
        assert_eq!(
            Frame::decode(&bytes[..bytes.len() - 1]),
            Err(DecodeError::Truncated)
        );
        bytes.push(3);
        assert_eq!(Frame::decode(&bytes), Err(DecodeError::Truncated));
    }
}
