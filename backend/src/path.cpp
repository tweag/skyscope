#include <algorithm>
#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <queue>
#include <unistd.h>
#include <vector>

using namespace std;

struct StepMapEntry {
    int32_t key;
    int32_t value;
};

extern "C" int32_t c_findPath(
    int32_t origin,
    int32_t destination,
    const int64_t* stepMap,
    int32_t stepMapSize,
    int32_t* buffer,
    int32_t size
) {
    auto stepMapBegin = reinterpret_cast<const StepMapEntry*>(stepMap);
    assert(sizeof(stepMapBegin[0]) == sizeof(stepMap[0]));
    auto stepMapEnd = stepMapBegin + stepMapSize;

    vector<int32_t> path = { origin };

    while (path.back() != destination) {
        auto cmp = [](StepMapEntry entry, int32_t node) {
            return entry.key < node;
        };
        auto iter = lower_bound(stepMapBegin, stepMapEnd, path.back(), cmp);
        if (iter == stepMapEnd || iter->key != path.back()) {
            return 0;
        }
        path.push_back(iter->value);
    }

    int32_t actualSize = min(int32_t(path.size()), size);
    memcpy(buffer, path.data(), sizeof(int32_t) * actualSize);
    return actualSize;
}

extern "C" int32_t c_indexPaths(
    int32_t destination,
    const int32_t* predMap,
    int64_t* stepMap,
    int32_t nodeCount
) {
    vector<int32_t> steps(nodeCount + 1);
    queue<pair<int32_t, int32_t>> frontier;
    auto predMapIndirect = predMap + nodeCount + 1;

    auto pushPreds = [&](int32_t node) {
        auto predecessor = predMap[node];
        if (predecessor > 0) {
            frontier.push(make_pair(predecessor, node));
        } else {
            auto offset = abs(predecessor);
            auto preds = predMapIndirect + offset;
            for (int i = 1; i <= preds[0]; ++i) {
                frontier.push(make_pair(preds[i], node));
            }
        }
    };

    pushPreds(destination);
    while (!frontier.empty()) {
        auto [node, next] = frontier.front();
        frontier.pop();
        if (steps.at(node) == 0) {
            steps.at(node) = next;
            pushPreds(node);
        }
    }

    int32_t stepMapSize = 0;
    for (size_t i = 0; i < steps.size(); ++i) {
        if (steps.at(i) != 0) {
            *reinterpret_cast<StepMapEntry*>(stepMap++) =
                StepMapEntry { int32_t(i), steps.at(i) };
            ++stepMapSize;
        }
    }

    return stepMapSize;
}
