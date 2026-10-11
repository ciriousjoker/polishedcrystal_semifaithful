import 'dart:convert';
import 'dart:ffi';
import 'dart:io';
import 'package:ffi/ffi.dart';

abstract interface class BridgeEngine {
  void startLocal(int port, int room);
  void startInternet(int port, int room, String origin);
  void stop();
  Map<String, dynamic> snapshot();
}

final class NativeBridge implements BridgeEngine {
  NativeBridge() {
    final override = Platform.environment['MOBILE_BRIDGE_LIBRARY'];
    final executable = File(Platform.resolvedExecutable).parent.path;
    final library =
        override ??
        (Platform.isMacOS
            ? '$executable/../Frameworks/libmobile_bridge.dylib'
            : Platform.isWindows
            ? '$executable/mobile_bridge.dll'
            : Platform.isAndroid
            ? 'libmobile_bridge.so'
            : '$executable/lib/libmobile_bridge.so');
    final native = Platform.isIOS
        ? DynamicLibrary.process()
        : DynamicLibrary.open(library);
    _local = native
        .lookupFunction<Int32 Function(Uint16, Uint16), int Function(int, int)>(
          'mobile_bridge_start_local',
        );
    _internet = native
        .lookupFunction<
          Int32 Function(Uint16, Uint16, Pointer<Uint8>, Size),
          int Function(int, int, Pointer<Uint8>, int)
        >('mobile_bridge_start_internet');
    _stop = native.lookupFunction<Void Function(), void Function()>(
      'mobile_bridge_stop',
    );
    _status = native
        .lookupFunction<Pointer<Utf8> Function(), Pointer<Utf8> Function()>(
          'mobile_bridge_status',
        );
  }
  late final int Function(int, int) _local;
  late final int Function(int, int, Pointer<Uint8>, int) _internet;
  late final void Function() _stop;
  late final Pointer<Utf8> Function() _status;
  @override
  void startLocal(int port, int room) {
    if (_local(port, room) != 0) {
      throw StateError(
        'Bridge could not start. Check whether port $port is already in use.',
      );
    }
  }

  @override
  void startInternet(int port, int room, String origin) {
    final bytes = utf8.encode(origin);
    final pointer = calloc<Uint8>(bytes.length);
    try {
      pointer.asTypedList(bytes.length).setAll(0, bytes);
      if (_internet(port, room, pointer, bytes.length) != 0) {
        throw StateError(
          'Bridge could not start. Check the server address and local port.',
        );
      }
    } finally {
      calloc.free(pointer);
    }
  }

  @override
  void stop() => _stop();
  @override
  Map<String, dynamic> snapshot() {
    final pointer = _status();
    if (pointer == nullptr) {
      return {'error': 'Status unavailable'};
    }
    return jsonDecode(pointer.toDartString()) as Map<String, dynamic>;
  }
}
