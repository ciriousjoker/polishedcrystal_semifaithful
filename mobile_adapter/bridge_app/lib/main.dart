import 'dart:async';
import 'dart:io';
import 'package:flutter/material.dart';
import 'native_bridge.dart';

void main() {
  BridgeEngine? engine;
  String? error;
  try {
    engine = NativeBridge();
  } catch (failure) {
    error = failure.toString();
  }
  runApp(BridgeApp(engine: engine, startupError: error));
}

class BridgeApp extends StatelessWidget {
  const BridgeApp({super.key, required this.engine, this.startupError});
  final BridgeEngine? engine;
  final String? startupError;
  @override
  Widget build(BuildContext context) => MaterialApp(
    title: 'Polished Crystal Bridge',
    debugShowCheckedModeBanner: false,
    theme: ThemeData(
      colorScheme: ColorScheme.fromSeed(seedColor: const Color(0xff286153)),
      useMaterial3: true,
    ),
    home: BridgePage(engine: engine, startupError: startupError),
  );
}

class BridgePage extends StatefulWidget {
  const BridgePage({super.key, required this.engine, this.startupError});
  final BridgeEngine? engine;
  final String? startupError;
  @override
  State<BridgePage> createState() => _BridgePageState();
}

class _BridgePageState extends State<BridgePage> {
  final origin = TextEditingController(text: 'ws://127.0.0.1:8787');
  final room = TextEditingController(text: '1');
  bool internet = false;
  bool running = false;
  Map<String, dynamic> status = {};
  String? error;
  Timer? timer;
  int get port =>
      int.tryParse(Platform.environment['MOBILE_BRIDGE_PORT'] ?? '') ?? 17423;
  @override
  void initState() {
    super.initState();
    error = widget.startupError;
    timer = Timer.periodic(const Duration(milliseconds: 250), (_) {
      if (!mounted || !running) {
        return;
      }
      try {
        setState(() {
          status = widget.engine!.snapshot();
        });
      } catch (failure) {
        setState(() {
          error = failure.toString();
        });
      }
    });
  }

  @override
  void dispose() {
    timer?.cancel();
    widget.engine?.stop();
    origin.dispose();
    room.dispose();
    super.dispose();
  }

  void start() {
    try {
      final id = int.tryParse(room.text);
      if (id == null || id < 1 || id > 65535) {
        throw ArgumentError('Enter a lobby code between 1 and 65535.');
      }
      if (internet) {
        widget.engine!.startInternet(port, id, origin.text.trim());
      } else {
        widget.engine!.startLocal(port, id);
      }
      setState(() {
        running = true;
        error = null;
        status = widget.engine!.snapshot();
      });
    } catch (failure) {
      setState(() {
        running = false;
        error = failure.toString();
      });
    }
  }

  void stop() {
    widget.engine?.stop();
    setState(() {
      running = false;
      status = {};
    });
  }

  @override
  Widget build(BuildContext context) {
    final players = status['players'] as List<dynamic>? ?? [];
    return Scaffold(
      body: SafeArea(
        child: Center(
          child: ConstrainedBox(
            constraints: const BoxConstraints(maxWidth: 600),
            child: ListView(
              padding: const EdgeInsets.all(28),
              children: [
                Text(
                  'Polished Crystal Bridge',
                  style: Theme.of(context).textTheme.headlineMedium,
                ),
                const SizedBox(height: 8),
                const Text(
                  'Connect your emulators here, then choose local multiplayer or a shared Internet lobby.',
                ),
                const SizedBox(height: 24),
                SegmentedButton<bool>(
                  segments: const [
                    ButtonSegment(
                      value: false,
                      label: Text('Local multiplayer'),
                      icon: Icon(Icons.home_outlined),
                    ),
                    ButtonSegment(
                      value: true,
                      label: Text('Internet lobby'),
                      icon: Icon(Icons.public),
                    ),
                  ],
                  selected: {internet},
                  onSelectionChanged: running
                      ? null
                      : (values) => setState(() {
                          internet = values.first;
                        }),
                ),
                const SizedBox(height: 20),
                if (internet) ...[
                  TextField(
                    controller: origin,
                    enabled: !running,
                    decoration: const InputDecoration(
                      labelText: 'Server address',
                      hintText: 'wss://your-server.example',
                    ),
                  ),
                  const SizedBox(height: 12),
                  TextField(
                    controller: room,
                    enabled: !running,
                    keyboardType: TextInputType.number,
                    decoration: const InputDecoration(labelText: 'Lobby code'),
                  ),
                  const SizedBox(height: 20),
                ],
                Text('Emulator address: 127.0.0.1:$port'),
                const SizedBox(height: 8),
                Text(
                  running
                      ? (status['connected'] == true
                            ? 'Bridge running'
                            : 'Waiting for server connection')
                      : 'Bridge stopped',
                ),
                const SizedBox(height: 16),
                FilledButton(
                  onPressed: widget.engine == null
                      ? null
                      : running
                      ? stop
                      : start,
                  child: Text(
                    running
                        ? 'Stop Bridge'
                        : internet
                        ? 'Join Internet lobby'
                        : 'Start local lobby',
                  ),
                ),
                if (error != null || status['error'] != null)
                  Padding(
                    padding: const EdgeInsets.only(top: 16),
                    child: Text(
                      error ?? status['error'].toString(),
                      style: TextStyle(
                        color: Theme.of(context).colorScheme.error,
                      ),
                    ),
                  ),
                const SizedBox(height: 28),
                Text(
                  'Emulators (${players.length})',
                  style: Theme.of(context).textTheme.titleLarge,
                ),
                if (players.isEmpty)
                  const Padding(
                    padding: EdgeInsets.only(top: 12),
                    child: Text(
                      'Start the game and press Select+B to connect.',
                    ),
                  ),
                for (final player in players)
                  ListTile(
                    leading: Icon(
                      player['joined'] == true
                          ? Icons.check_circle_outline
                          : Icons.hourglass_empty,
                    ),
                    title: Text(
                      player['player'] == null
                          ? 'Emulator ${player['stream']}'
                          : 'Player ${player['player']}',
                    ),
                    subtitle: Text(
                      player['joined'] == true ? 'Joined lobby' : 'Connecting',
                    ),
                  ),
              ],
            ),
          ),
        ),
      ),
    );
  }
}
