#ifndef MOBILE_CONDITIONAL_INPUT_HPP
#define MOBILE_CONDITIONAL_INPUT_HPP
#include <limits>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
struct InputAction { unsigned first, end, button; };
struct ConditionalAction {
    unsigned earliest, button, duration;
    std::map<std::string, unsigned> conditions;
    bool fired = false, optional = false, relative = false;
    unsigned anchor = 0;
    bool anchored = false, checked_delay = false;
};
inline unsigned conditional_unsigned(const std::string& token) {
    if (token.empty() || token.find_first_not_of("0123456789") != std::string::npos)
        throw std::runtime_error("Invalid conditional input number");
    unsigned result = 0;
    for (char digit : token) {
        unsigned value = unsigned(digit - '0');
        if (result > (std::numeric_limits<unsigned>::max() - value) / 10)
            throw std::runtime_error("Conditional input number overflow");
        result = result * 10 + value;
    }
    return result;
}
// Functional completion extends only the host replay horizon. The core's
// measurement window remains 800 frames; the default completion horizon is 1000.
inline unsigned parse_functional_horizon(const std::string& token = "1000") {
    const unsigned frames = conditional_unsigned(token);
    if (frames < 1000 || frames > 4000)
        throw std::runtime_error("Functional horizon must be between 1000 and 4000");
    return frames;
}
inline unsigned functional_frame_limit(unsigned release_frame, unsigned warmup, unsigned horizon) {
    if (warmup > 3000 || horizon < 1000 || horizon > 4000 ||
            release_frame > 100000 - 1 - warmup - horizon)
        throw std::runtime_error("Functional horizon exceeds bounded frame limit");
    return release_frame + 1 + warmup + horizon;
}
template<class KnownSymbol>
ConditionalAction parse_conditional_action(std::istream& row, bool optional,
        unsigned frame_limit, const std::map<std::string, unsigned>& buttons, KnownSymbol known_symbol) {
    std::string mode, earliest, conditions, button, duration, extra;
    if (!(row >> mode >> earliest >> conditions >> button >> duration) || row >> extra ||
            (mode != "ON" && mode != "AFTER" && mode != "CHECKED_AFTER") || !buttons.count(button))
        throw std::runtime_error("Invalid conditional input action");
    ConditionalAction action = {conditional_unsigned(earliest), buttons.at(button),
        conditional_unsigned(duration), {}};
    if (!action.duration || action.earliest >= frame_limit || action.duration >= frame_limit)
        throw std::runtime_error("Invalid conditional input action bounds");
    action.optional = optional;
    action.relative = mode != "ON";
    action.checked_delay = mode == "CHECKED_AFTER";
    if (conditions.empty() || conditions.back() == ',')
        throw std::runtime_error("Conditional input requires a condition");
    std::istringstream list(conditions);
    std::string condition;
    while (std::getline(list, condition, ',')) {
        auto equals = condition.find('=');
        if (equals == std::string::npos || !known_symbol(condition.substr(0, equals)))
            throw std::runtime_error("Conditional input references unknown symbol");
        auto name = condition.substr(0, equals);
        auto value = conditional_unsigned(condition.substr(equals + 1));
        if (!action.conditions.emplace(name, value).second)
            throw std::runtime_error("Duplicate conditional input symbol");
    }
    if (action.conditions.empty()) throw std::runtime_error("Conditional input requires a condition");
    return action;
}
// Evaluated after each completed emulated frame; physical delivery starts next
// frame, as for ON/AFTER. CHECKED_AFTER resets on any nonmatching sample and
// requires DELAY frames of continuous sampled readiness before queuing delivery.
inline bool schedule_conditional_action(ConditionalAction& action, unsigned frame,
        unsigned frame_limit, const std::map<std::string, unsigned>& values, InputAction& delivered) {
    if (action.fired || (!action.relative && frame < action.earliest)) return false;
    bool matches = true;
    for (const auto& condition : action.conditions) {
        auto value = values.find(condition.first);
        if (value == values.end() || value->second != condition.second) matches = false;
    }
    if (action.relative) {
        if (action.checked_delay && !matches) action.anchored = false;
        if (!action.anchored && matches) { action.anchor = frame; action.anchored = true; }
        matches = action.anchored && frame - action.anchor >= action.earliest;
    }
    if (!matches) return false;
    if (frame >= frame_limit || action.duration >= frame_limit - frame)
        throw std::runtime_error("Conditional action exceeds frame limit");
    delivered = {frame + 1, frame + 1 + action.duration, action.button};
    action.fired = true;
    return true;
}
inline bool conditional_action_required_missing(const ConditionalAction& action) {
    return !action.fired && !action.optional;
}
#endif
