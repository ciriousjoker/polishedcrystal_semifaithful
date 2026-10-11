import 'package:flutter_test/flutter_test.dart';
import 'package:flutter/widgets.dart';
import 'package:mobile_adapter_bridge/main.dart';
import 'package:mobile_adapter_bridge/native_bridge.dart';

class FakeBridge implements BridgeEngine {
  bool started = false;
  bool internet = false;
  int? port;
  int? room;
  String? origin;
  @override
  void startLocal(int port, int room) {
    this.port = port;
    this.room = room;
    started = true;
    internet = false;
  }

  @override
  void startInternet(int port, int room, String origin) {
    this.port = port;
    this.room = room;
    started = true;
    internet = true;
    this.origin = origin;
  }

  @override
  void stop() {
    started = false;
  }

  @override
  Map<String, dynamic> snapshot() => {
    'connected': started,
    'players': [
      {'stream': 1, 'player': 42, 'joined': true},
      {'stream': 2, 'player': 43, 'joined': true},
    ],
  };
}

void main() {
  testWidgets(
    'development launch auto-connects the shared Bridge to the server',
    (tester) async {
      final engine = FakeBridge();
      await tester.pumpWidget(
        BridgeApp(
          engine: engine,
          launchEnvironment: const {
            'MOBILE_BRIDGE_AUTOSTART': '1',
            'MOBILE_BRIDGE_MODE': 'hosted',
            'MOBILE_BRIDGE_PORT': '17424',
            'MOBILE_BRIDGE_LOBBY': '73',
            'MOBILE_BRIDGE_ORIGIN': 'ws://127.0.0.1:18888',
            'MOBILE_BRIDGE_BIND_ADDRESS': '192.168.2.25',
          },
        ),
      );
      await tester.pump();
      expect(engine.started, isTrue);
      expect(engine.internet, isTrue);
      expect(engine.port, 17424);
      expect(engine.room, 73);
      expect(engine.origin, 'ws://127.0.0.1:18888');
      expect(find.text('Emulator address: 192.168.2.25:17424'), findsOneWidget);
      await tester.pumpWidget(const SizedBox());
      expect(engine.started, isFalse);
    },
  );
  testWidgets('one local lobby displays multiple players and stops cleanly', (
    tester,
  ) async {
    final engine = FakeBridge();
    await tester.pumpWidget(BridgeApp(engine: engine));
    await tester.tap(find.text('Start local lobby'));
    await tester.pump();
    expect(engine.started, isTrue);
    expect(engine.internet, isFalse);
    expect(find.text('Player 42'), findsOneWidget);
    expect(find.text('Player 43'), findsOneWidget);
    await tester.tap(find.text('Stop Bridge'));
    await tester.pump();
    expect(engine.started, isFalse);
    await tester.pumpWidget(const BridgeApp(engine: null));
  });
  testWidgets('Internet mode selects the same native engine and a lobby code', (
    tester,
  ) async {
    final engine = FakeBridge();
    await tester.pumpWidget(BridgeApp(engine: engine));
    await tester.tap(find.text('Internet lobby'));
    await tester.pump();
    await tester.tap(find.text('Join Internet lobby'));
    await tester.pump();
    expect(engine.internet, isTrue);
    await tester.pumpWidget(const BridgeApp(engine: null));
  });
}
