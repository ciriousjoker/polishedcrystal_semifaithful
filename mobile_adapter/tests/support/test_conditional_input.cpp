#include "../../tools/conditional_input.hpp"
#include <cassert>
#include <vector>
static ConditionalAction parse(const std::string& line, bool optional = false) {
    std::istringstream row(line);
    return parse_conditional_action(row, optional, 1000, {{"A", 8}, {"B", 0}},
        [](const std::string& name) { return name == "ready" || name == "menu"; });
}
static bool tick(ConditionalAction& action, unsigned frame, unsigned ready, InputAction& delivery) {
    return schedule_conditional_action(action, frame, 1000, {{"ready", ready}}, delivery);
}
int main() {
    assert(parse_functional_horizon() == 1000);
    assert(parse_functional_horizon("1000") == 1000);
    assert(parse_functional_horizon("3200") == 3200);
    assert(parse_functional_horizon("4000") == 4000);
    assert(functional_frame_limit(11002, 300, 1000) == 12303);
    assert(functional_frame_limit(11002, 300, 3200) == 14503);
    assert(functional_frame_limit(92999, 3000, 4000) == 100000);
    for (const auto& token : {"", "-1", "+1000", "1000x", "1000 ", " 1000",
            "999", "4001", "4294967296", "99999999999999999999999"}) {
        bool rejected = false;
        try { parse_functional_horizon(token); }
        catch (const std::runtime_error&) { rejected = true; }
        assert(rejected);
    }
    for (const auto& bounds : std::vector<std::vector<unsigned>>{
            {93000, 3000, 4000}, {std::numeric_limits<unsigned>::max(), 300, 3200},
            {11002, std::numeric_limits<unsigned>::max(), 3200},
            {11002, 300, 0}, {11002, 300, std::numeric_limits<unsigned>::max()}}) {
        bool rejected = false;
        try { functional_frame_limit(bounds[0], bounds[1], bounds[2]); }
        catch (const std::runtime_error&) { rejected = true; }
        assert(rejected);
    }
    InputAction delivery{};
    auto checked = parse("CHECKED_AFTER 3 ready=1 A 8");
    assert(!tick(checked, 10, 1, delivery));
    assert(!tick(checked, 11, 1, delivery));
    // True at anchor, false when legacy delivery would be due: must not fire.
    assert(!tick(checked, 13, 0, delivery));
    assert(!checked.anchored && !checked.fired && conditional_action_required_missing(checked));
    assert(!tick(checked, 20, 1, delivery));
    assert(!tick(checked, 22, 1, delivery));
    assert(tick(checked, 23, 1, delivery));
    assert(delivery.first == 24 && delivery.end == 32 && delivery.button == 8);
    assert(!conditional_action_required_missing(checked));
    assert(!tick(checked, 24, 1, delivery));
    assert(!tick(checked, 100, 1, delivery));
    unsigned held_frames = 0;
    for (unsigned frame = 0; frame < 100; ++frame)
        if (frame >= delivery.first && frame < delivery.end) ++held_frames;
    assert(held_frames == 8);
    auto legacy = parse("AFTER 3 ready=1 B 1");
    assert(!tick(legacy, 10, 1, delivery));
    assert(!tick(legacy, 11, 0, delivery));
    assert(tick(legacy, 13, 0, delivery));
    assert(delivery.first == 14 && delivery.end == 15);
    auto on = parse("ON 10 ready=1 A 8");
    assert(!tick(on, 9, 1, delivery));
    assert(!tick(on, 10, 0, delivery));
    assert(tick(on, 11, 1, delivery));
    assert(delivery.first == 12 && delivery.end == 20);
    auto immediate = parse("CHECKED_AFTER 0 ready=1 B 8", true);
    assert(!conditional_action_required_missing(immediate));
    assert(!tick(immediate, 4, 0, delivery));
    assert(tick(immediate, 5, 1, delivery));
    assert(delivery.first == 6);
    auto missing = parse("CHECKED_AFTER 2 ready=1 A 8");
    assert(!tick(missing, 10, 1, delivery));
    assert(!schedule_conditional_action(missing, 12, 1000, {}, delivery));
    assert(!tick(missing, 20, 1, delivery));
    assert(tick(missing, 22, 1, delivery));
    std::istringstream late_row("CHECKED_AFTER 1200 ready=1 B 8");
    auto late = parse_conditional_action(late_row, true, 3200, {{"B", 0}},
        [](const std::string& name) { return name == "ready"; });
    assert(!schedule_conditional_action(late, 500, 3200, {{"ready", 1}}, delivery));
    assert(!schedule_conditional_action(late, 1700, 3200, {{"ready", 0}}, delivery));
    assert(!schedule_conditional_action(late, 1800, 3200, {{"ready", 1}}, delivery));
    assert(schedule_conditional_action(late, 3000, 3200, {{"ready", 1}}, delivery));
    assert(delivery.first == 3001 && delivery.end == 3009);
    assert(!conditional_action_required_missing(late));
    std::istringstream final_row("CHECKED_AFTER 0 ready=1 B 8");
    auto final_action = parse_conditional_action(final_row, true, 3200, {{"B", 0}},
        [](const std::string& name) { return name == "ready"; });
    bool tail_rejected = false;
    try { schedule_conditional_action(final_action, 3192, 3200, {{"ready", 1}}, delivery); }
    catch (const std::runtime_error&) { tail_rejected = true; }
    assert(tail_rejected && !final_action.fired);
    auto bounded = parse("CHECKED_AFTER 0 ready=1 A 8");
    bool rejected = false;
    try { tick(bounded, 992, 1, delivery); }
    catch (const std::runtime_error&) { rejected = true; }
    assert(rejected && !bounded.fired);
    const std::vector<std::string> malformed = {
        "CHECKED_AFTER", "CHECKED_AFTER -1 ready=1 A 8", "CHECKED_AFTER 1x ready=1 A 8",
        "CHECKED_AFTER 4294967296 ready=1 A 8", "CHECKED_AFTER 1000 ready=1 A 8",
        "CHECKED_AFTER 0 ready=1 A 0", "CHECKED_AFTER 0 ready=1 A 1000",
        "CHECKED_AFTER 0 ready= A 8", "CHECKED_AFTER 0 ready=1x A 8",
        "CHECKED_AFTER 0 ready=1=2 A 8", "CHECKED_AFTER 0 ready=-1 A 8",
        "CHECKED_AFTER 0 ready=1, A 8", "CHECKED_AFTER 0 ready=1,,menu=0 A 8",
        "CHECKED_AFTER 0 ready=1,ready=0 A 8", "CHECKED_AFTER 0 unknown=1 A 8",
        "CHECKED_AFTER 0 ready=1 X 8", "CHECKED_AFTER 0 ready=1 A 8 extra"
    };
    for (const auto& line : malformed) {
        rejected = false;
        try { parse(line); } catch (const std::runtime_error&) { rejected = true; }
        assert(rejected);
    }
}
