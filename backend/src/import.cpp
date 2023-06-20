#include <atomic>
#include <condition_variable>
#include <functional>
#include <iostream>
#include <memory>
#include <mutex>
#include <ranges>
#include <span>
#include <sstream>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>
#include <charconv>

#include "sqlite3.h"

using namespace std;

void raise(const char* s) {
    cerr << "error: throw: " << s << endl;
    throw s;
}

constexpr auto BATCH_SIZE = 1024;         // records per insert statement
constexpr auto CHUNK_SIZE = 1024 * 1024;  // size of stdin chunks to parse

struct Node {
    int idx;
    string hash;
    string data;
    string type;
};

struct Edge {
    int source;
    int target;
    int groupNum;
};

struct Graph {
    vector<Node> nodes;
    vector<Edge> edges;
};

enum class ParseResult {
    DONE,
    MORE,
    FAIL,
};

class ParseState {
    public:
        ParseState() : buffer(1, '\0'), position(0) { }

        void advance(int n) { position += n; }

        auto remaining() {
            auto pos = buffer.cbegin() + position;
            return string_view(pos, buffer.cend());
        }

        int source = 0;
        int group = 0;

        bool feed(int n) {
            buffer.erase(buffer.begin(), buffer.begin() + position);
            position = 0;
            auto readPosition = buffer.size() - 1;
            buffer.resize(buffer.size() + n);
            cin.read(&buffer.front() + readPosition, n);
            auto actualRead = cin.gcount();
            buffer.resize(readPosition + actualRead + 1);
            buffer.back() = '\0';
            return actualRead < n;
        }

        void error(const char* msg) {
            cerr << "\x1b[31mparse error:\x1b[0m " << msg << ":\n";
            cerr << remaining().substr(0, 256) << endl;
        }

    private:
        vector<char> buffer;
        int position;
};

ParseResult parseWarning(ParseState& state) {
    if (!state.remaining().starts_with("Warning:")) {
        state.error("no warning message");
        return ParseResult::FAIL;
    }
    auto end = state.remaining().find("\n\n");
    if (end == string_view::npos) {
        state.error("expected blank line after warning");
        return ParseResult::FAIL;
    }
    state.advance(end);
    return ParseResult::DONE;
}

int assignNodeIdx(Node& node) {
    static auto idxMap = unordered_map<size_t, int>();
    static auto hashFn = hash<string>();
    auto hash = hashFn(node.data);
    auto [idx, _] = idxMap.emplace(make_pair(hash, idxMap.size() + 1));
    node.hash = to_string(hash);
    node.idx = idx->second;
    return node.idx;
}

ParseResult parseNodeBazel5(ParseState& state, Graph& graph) { /*
    BLUE_NODE:NodeData{e247f4c}|
    RED_NODE:NodeData{6566162}|GREEN_NODE:NodeData{aab8e2e}|BLUE_NODE:NodeData{e247f4c}|RED_NODE:NodeData{5cedeee}
    RED_NODE:NodeData{5cedeee}|GREEN_NODE:NodeData{aab8e2e}
    GREEN_NODE:NodeData{aab8e2e}|RED_NODE:NodeData{5cedeee}
*/  auto source = 0;
    while (true) {
        auto eol = state.remaining().find("\n");
        if (eol == string::npos) {
            // line is partial
            return ParseResult::MORE;
        } else if (eol == 0) {
            // skip empty lines
            state.advance(1);
            return ParseResult::DONE;
        }
        auto pipe = state.remaining().find("|");
        auto end = pipe < eol ? pipe : eol;
        auto data = string(state.remaining().substr(0, end));
        auto colon = data.find(':');
        if (colon == string::npos) {
            state.error("missing colon in key");
            return ParseResult::FAIL;
        }
        state.advance(end + 1);
        string type = string(data.substr(0, colon));
        auto idx = assignNodeIdx(graph.nodes.emplace_back(0, "", data, type));
        if (source == 0) {
            source = idx;
        } else {
            graph.edges.emplace_back(source, idx, 0);
        }
        if (end == eol) return ParseResult::DONE;
    }
}

ParseResult parseNodeBazel6(ParseState& state, Graph& graph) { /*
    BLUE_NODE:NodeData{e247f4c}
    RED_NODE:NodeData{6566162}
      Group 1:
        BLUE_NODE:NodeData{e247f4c}
        GREEN_NODE:NodeData{aab8e2e}
      Group 2:
        RED_NODE:NodeData{5cedeee}
    RED_NODE:NodeData{5cedeee}
      Group 1:
        GREEN_NODE:NodeData{aab8e2e}
    GREEN_NODE:NodeData{aab8e2e}
      Group 1:
        RED_NODE:NodeData{5cedeee}
*/  
    while (true) {
        auto remaining = state.remaining();
        // cerr << "remaining: " << remaining.substr(0, 80) << "..." << endl << "[...]" << endl;
        auto eol = remaining.find("\n");
        if (eol == string::npos) {
            // line is partial
            return ParseResult::MORE;
        } else if (eol == 0) {
            // skip empty lines
            state.advance(1);
            return ParseResult::DONE;
        }
        auto indent = remaining.find_first_not_of(' ');
        // cerr << "indent: " << indent << ", eol:" << eol << endl;
        auto node = string(remaining.substr(indent, eol - indent));
        // cerr << "node: " << node << endl;
        if (indent == 0) {
            // A new node
            state.source = 0;
        } else if (indent == 2) {
            // A new group
            state.group = stoi(node.substr(6) /* "Group ".length() */);
            state.advance(eol+1);
            return ParseResult::DONE;
        } else if (indent == 4) {
            // A new link node
        } else {
            state.error("invalid indent encountered");
            return ParseResult::FAIL;
        }
        auto colon = node.find(':');
        if (colon == string::npos) {
            // cerr << node << endl;
            state.error("missing colon in key");
            return ParseResult::FAIL;
        }
        auto type = string(node.substr(0, colon));
        auto idx = assignNodeIdx(graph.nodes.emplace_back(0, "", node, type));
        if (state.source == 0) {
            state.source = idx;
        } else {
            graph.edges.emplace_back(state.source, idx, state.group);
        }
        state.advance(eol+1);
        return ParseResult::DONE;
    }
}

ParseResult parseNode(ParseState& state, Graph& graph) {
    if (getenv("SKYSCOPE_LEGACY_BAZEL")) {
        return parseNodeBazel5(state, graph);
    } else {
        return parseNodeBazel6(state, graph);
    }
}

sqlite3_stmt* memoize(auto& map, int key) {
    auto iter = map.find(key);
    if (iter !=  map.end()) {
        const auto stmt = iter->second;
        if (sqlite3_reset(stmt) != SQLITE_OK) raise("sqlite3_reset");
        if (sqlite3_clear_bindings(stmt) != SQLITE_OK) raise("sqlite3_clear_bindings");
        return stmt;
    }
    return nullptr;
}

sqlite3_stmt* prepareInsertNodeStmt(sqlite3* db, int count) {
    static auto stmts = unordered_map<int, sqlite3_stmt*>();
    auto stmt = memoize(stmts, count);
    if (stmt != nullptr) {
        return stmt;
    }
    ostringstream stream;
    stream << "INSERT INTO node (idx, hash, data, type) VALUES ";
    for (int i = 0; i < count; ++i) {
        if (i != 0) stream << ", ";
        stream << "(?, ?, ?, ?)";
    }
    stream << ";";
    const auto& sql = stream.str();
    if (sqlite3_prepare_v2(db, sql.c_str(), sql.length(), &stmt, nullptr) != SQLITE_OK) raise("prepare node stmt");
    stmts[count] = stmt;
    return stmt;
}

sqlite3_stmt* prepareInsertEdgeStmt(sqlite3* db, int count) {
    static auto stmts = unordered_map<int, sqlite3_stmt*>();
    auto stmt = memoize(stmts, count);
    if (stmt != nullptr) {
        return stmt;
    }
    ostringstream stream;
    stream << "INSERT INTO edge (source, target, group_num) VALUES ";
    for (int i = 0; i < count; ++i) {
        if (i != 0) stream << ", ";
        stream << "(?, ?, ?)";
    }
    stream << ";";
    const auto& sql = stream.str();
    if (sqlite3_prepare_v2(db, sql.c_str(), sql.length(), &stmt, nullptr) != SQLITE_OK) raise("prepare edge stmt");
    stmts[count] = stmt;
    return stmt;
}

void bindInsertNodeStmt(sqlite3_stmt* stmt, span<const Node, dynamic_extent> values) {
    int param = 0;
    for (const auto& row : values) {
        auto bindText = [&](const auto& text, const char* err) {
            if (sqlite3_bind_text(stmt, ++param, text.c_str(), text.size(), SQLITE_TRANSIENT) != SQLITE_OK) raise(err);
        };
        if (sqlite3_bind_int(stmt, ++param, row.idx) != SQLITE_OK) raise("bind idx failed");
        bindText(row.hash, "bind hash failed");
        bindText(row.data, "bind data failed");
        bindText(row.type, "bind type failed");
    }
}

void bindInsertEdgeStmt(sqlite3_stmt* stmt, span<const Edge, dynamic_extent> values) {
    int param = 0;
    for (const auto& row : values) {
        if (sqlite3_bind_int(stmt, ++param, row.source) != SQLITE_OK) raise("bind source failed");
        if (sqlite3_bind_int(stmt, ++param, row.target) != SQLITE_OK) raise("bind target failed");
        if (sqlite3_bind_int(stmt, ++param, row.groupNum) != SQLITE_OK) raise("bind group_num failed");
    }
}

extern "C" bool c_importSkyframe(const char* dbPath) {
    cerr << "c_importSkyframe: " << dbPath << endl;
    auto [nodeCount, edgeCount] = make_pair(0, 0);
    auto updateProgress = [&](bool finished = false) {
        auto ansi = finished ? "0" : "37";
        cerr << "\x1b[1F\x1b[2K\x1b[" << ansi << "mimported "
             << edgeCount << " edges" << endl;
    };
    cerr << "\n";
    updateProgress();

    // Initialise state.
    sqlite3* db = nullptr;
    auto subgraph = Graph{};
    auto state = ParseState();
    auto sqliteMutex = mutex();
    auto sqliteRunning = atomic_bool(false);
    const char* pragmaSql =
        "pragma synchronous = off;\n"
        "pragma journal_mode = MEMORY;\n"
        "pragma mmap_size = 1073741824;\n";
    if (sqlite3_initialize() != SQLITE_OK) raise("sqlite3_initialize");
    if (sqlite3_open(dbPath, &db) != SQLITE_OK) raise("sqlite3_open");
    if (sqlite3_exec(db, pragmaSql, nullptr, nullptr, nullptr) != SQLITE_OK) raise("sqlite pragma");

    // Skip over the warning heading.
    bool eof = state.feed(CHUNK_SIZE);
    if (parseWarning(state) == ParseResult::FAIL) {
        return false;
    }

    do { // Read and parse another chunk.
        eof = state.feed(CHUNK_SIZE);
        while (true) {
            switch (parseNode(state, subgraph)) {
                case ParseResult::DONE: continue;
                case ParseResult::FAIL: return false;
                case ParseResult::MORE: goto insertNodes;
            }
        }

        insertNodes: // Add parsed nodes to database.
        auto lock = unique_lock<mutex>(sqliteMutex);
        sqliteRunning = true;
        thread([&,
                lock = move(lock),
                subgraph = move(subgraph)
        ] {
            auto batched = [&](const auto& records, auto prepare, auto bind, auto& count) {
                for (auto i = records.begin(); i < records.end(); i += BATCH_SIZE) {
                    auto batch = span(i, min(i + BATCH_SIZE, records.end()));
                    auto stmt = prepare(db, batch.size());
                    bind(stmt, batch);
                    if (sqlite3_step(stmt) != SQLITE_DONE) {
                        cerr << stmt << endl;
                        cerr << sqlite3_errmsg(db) << endl;
                        raise("insert failed");
                        break;
                    }
                }
                count += records.size();
                //cerr << count << endl;
            };
            batched(subgraph.nodes, prepareInsertNodeStmt, bindInsertNodeStmt, nodeCount);
            batched(subgraph.edges, prepareInsertEdgeStmt, bindInsertEdgeStmt, edgeCount);
            updateProgress();
            sqliteRunning = false;
        }).detach();
    } while (!eof);

    // Wait for database.
    while (sqliteRunning) {
        this_thread::yield();
    }

    // Import complete.
    updateProgress("0");
    cerr << "\n";
    return true;
}
