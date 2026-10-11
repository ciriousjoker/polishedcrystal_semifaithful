import 'package:flutter_test/flutter_test.dart';
import 'package:mobile_adapter_bridge/main.dart';
import 'package:mobile_adapter_bridge/native_bridge.dart';

class FakeBridge implements BridgeEngine {
  bool started = false;
  bool internet = false;
  @override
  void startLocal(int port, int room) {
    started = true;
    internet = false;
  }

  @override
  void startInternet(int port, int room, String origin) {
    started = true;
    internet = true;
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
