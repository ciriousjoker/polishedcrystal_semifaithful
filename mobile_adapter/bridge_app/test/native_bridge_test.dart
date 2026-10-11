import 'dart:io';
import 'package:flutter_test/flutter_test.dart';
import 'package:mobile_adapter_bridge/native_bridge.dart';

void main() {
  test('Dart FFI starts the real native listener and shuts it down', () async {
    final reservation = await ServerSocket.bind(InternetAddress.loopbackIPv4, 0);
    final port = reservation.port;
    await reservation.close();
    final engine = NativeBridge();
    try {
      engine.startLocal(port, 1);
      final socket = await Socket.connect(InternetAddress.loopbackIPv4, port);
      socket.destroy();
      final deadline = DateTime.now().add(const Duration(seconds: 2));
      Map<String, dynamic> view;
      do {
        view = engine.snapshot();
        if (view['connected'] == true) { break; }
        await Future<void>.delayed(const Duration(milliseconds: 10));
      } while (DateTime.now().isBefore(deadline));
      expect(view['connected'], isTrue);
      expect(view['endpoint'], '127.0.0.1:$port');
    } finally { engine.stop(); }
    final released = await ServerSocket.bind(InternetAddress.loopbackIPv4, port);
    await released.close();
  }, skip: Platform.environment['MOBILE_BRIDGE_LIBRARY'] == null);
}
