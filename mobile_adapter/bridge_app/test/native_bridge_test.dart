import 'dart:io';
import 'package:flutter_test/flutter_test.dart';
import 'package:mobile_adapter_bridge/native_bridge.dart';

void main() {
  test(
    'Dart FFI starts the real native listener and shuts it down',
    () async {
      final address =
          Platform.environment['MOBILE_BRIDGE_BIND_ADDRESS'] ?? '127.0.0.1';
      final reservation = await ServerSocket.bind(address, 0);
      final port = reservation.port;
      await reservation.close();
      final engine = NativeBridge();
      try {
        engine.startLocal(port, 1);
        final socket = await Socket.connect(address, port);
        socket.destroy();
        final deadline = DateTime.now().add(const Duration(seconds: 2));
        Map<String, dynamic> view;
        do {
          view = engine.snapshot();
          if (view['connected'] == true) {
            break;
          }
          await Future<void>.delayed(const Duration(milliseconds: 10));
        } while (DateTime.now().isBefore(deadline));
        expect(view['connected'], isTrue);
        expect(view['endpoint'], '$address:$port');
      } finally {
        engine.stop();
      }
      final released = await ServerSocket.bind(address, port);
      await released.close();
    },
    skip: Platform.environment['MOBILE_BRIDGE_LIBRARY'] == null,
  );
}
