const svgNS = "http://www.w3.org/2000/svg";

function setStyle(element, style) {
    element.setAttribute("style", Object.entries(style)
        .map(entry => entry[0] + ":" + entry[1]).join(";")
    );
}

function createElement(name, style = {}, parent) {
    const element = document.createElement(name);
    setStyle(element, style)
    if (parent !== undefined) {
        parent.appendChild(element);
    }
    return element
}

function removeAllChildren(node) {
    while (node.firstChild) {
        node.removeChild(node.firstChild);
    }
}

const theme = {};
async function loadTheme() {
    const response = await fetch("/theme", { method: "GET" });
    if (!response.ok) {
        throw new Error("GET /theme failed: " + response.statusText);
    }
    for (k in theme) {
        delete theme[k];
    }
    const json = JSON.parse(await response.text());
    const css = Object.keys(json).map(nodeType => {
        const color = json[nodeType];
        theme[nodeType] = `hsl(${color.h}, ${color.s}%, ${color.l}%)`;
        return `div.${nodeType} span.nodeType { color: ${theme[nodeType]}; }`
    }).join("\n");
    createElement("style", {}, document.head).textContent = css;
}

async function post(url, data) {
    const response = await fetch(url, { method: "POST", body: JSON.stringify(data) });
    if (!response.ok) {
        throw new Error("POST " + url + " failed: " + response.statusText);
    }
    return await response.text();
}

async function findNodes(pattern) {
    return JSON.parse(await post("/find", "%" + pattern + "%"));
}

async function renderGraph(nodeStates) {
    const parser = new DOMParser();
    const body = await post("/render", nodeStates);
    const doc = parser.parseFromString(body, "image/svg+xml");
    return doc.getElementsByTagNameNS(svgNS, "svg")[0];
}

function prettyNodeType(nodeType) {
    return [
        ...nodeType.matchAll(/[^_]+/g)
    ].map(match => match[0].charAt(0).toUpperCase()
                 + match[0].slice(1).toLowerCase()
    ).join("").replace(" (unshareable)", "");
}

function prettyNodeLabel(hash, type, nodeData) {
    var match = null;
    switch (type) {
        case "DirectoryListing":
        case "DirectoryListingState":
        case "File":
        case "FileState":
        case "WorkspaceFile":
            match = nodeData.match(/.*\[([^\]]*)\]\/\[([^\]]*)\]/);
            if (match != null) {
                return match[1] + "/" + match[2];
            }
            break;
        case "ActionExecution":
        case "ConfiguredTarget":
        case "TargetCompletion":
            match = nodeData.match(/label=(.+), config/);
            if (match != null) {
                return match[1];
            }
            break;
        case "Artifact":
            match = nodeData.match(/\[.*\]([^\[\]]+)/);
            if (match != null) {
                return match[1];
            }
            break;
        case "BzlLoad":
        case "ClientEnvironmentVariable":
        case "ContainingPackageLookup":
        case "IgnoredPackagePrefixes":
        case "Package":
        case "PackageLookup":
        case "Precomputed":
        case "RepositoryDirectory":
            match = nodeData.match(/:(.*)/);
            if (match != null) {
                return match[1];
            }
            break;
        case "Glob":
            match = nodeData.match(/subdir=(.*) pattern=(.+) globberOperation/);
            if (match != null) {
                return match[1] + (match[1].length > 0 ? "/" : "") + match[2];
            }
            break;
        case "SingleToolchainResolution":
            match = nodeData.match(/toolchainTypeLabel=(.+), targetPlatformKey/);
            if (match != null) {
                return match[1];
            }
            break;
    }
    return hash.slice(0, 32);
}

function decorateGraph(svg, Node) {
    for (const nodeElement of svg.getElementsByClassName("node")) {
        const nodeClasses = nodeElement.classList;
        const hash = nodeElement.id;
        const ellipseElements = nodeElement.getElementsByTagName("ellipse");
        const pathElements = nodeElement.getElementsByTagName("path");
        const textElements = nodeElement.getElementsByTagName("text");
        if (pathElements.length == 1 && textElements.length == 2) {
            pathElements[0].classList.add("selectable");
            const typeElement = textElements[0];
            const labelElement = textElements[1];
            const type = prettyNodeType(typeElement.textContent);
            nodeClasses.add(type);
            typeElement.textContent = type;
            typeElement.classList.add("nodeType");
            typeElement.setAttribute("fill", type in theme ? theme[type] : "#3f3f3f");
            const label = prettyNodeLabel(hash, type, labelElement.textContent);
            if (label != null) {
                const maxChars = 40;
                const ellipsis = label.length > maxChars ? "…" : "";
                labelElement.textContent = ellipsis + label.slice(-maxChars);
            }
            labelElement.classList.add("nodeLabel");
            const linkElement = nodeElement.getElementsByTagName("a")[0];
            const tooltip = linkElement.getAttribute("xlink:title");
            const setHint = hint => linkElement.setAttribute("xlink:title",
                tooltip.replace(/\n\nClick.*/, "\n\n" + hint)
            );
            nodeElement.addEventListener("click", e => {
                if (e.ctrlKey) {
                    Node.hide(hash);
                } else if (e.shiftKey) {
                    if (Node.selected(hash)) {
                        Node.unselect(hash);
                    } else {
                        Node.select(hash);
                    }
                } else {
                    if (nodeClasses.contains("Collapsed")) {
                        Node.expand(hash);
                    } else {
                        Node.collapse(hash);
                    }
                }
            });
        } else if (ellipseElements.length == 1) {
            ellipseElements[0].classList.add("node");
            nodeElement.addEventListener("click", e => Node.show(hash));
        }
    }
}

const actionStates = {};
function supersedableDelayedAction(delay, payload, action) {
    if (actionStates[action] == null) {
        actionStates[action] = {};
    }
    const state = actionStates[action];
    if (state.timeoutID != null) {
        window.clearTimeout(state.timeoutID);
        state.timeoutID = null;
    }
    state.timeoutID = window.setTimeout(() => {
        state.timeoutID = null;
        if (state.actionInProgress) {
            state.pendingPayload = payload;
        } else {
            const loop = payload => {
                state.actionInProgress = true;
                action(payload).then(() => {
                    state.actionInProgress = false;
                    if (state.pendingPayload != null) {
                        const payload = state.pendingPayload;
                        delete state.pendingPayload;
                        loop(payload);
                    }
                });
            };
            loop(payload);
        }
    }, delay);
};

window.onload = function() {
    const nodeStates = {};
    const selection = {};
    const Node = {
        collapse: hash => {
            nodeStates[hash] = true;
            updateGraph(Node);
        },
        expand: hash => {
            nodeStates[hash] = false;
            updateGraph(Node);
        },
        show: hash => {
            nodeStates[hash] = false;
            updateGraph(Node);
        },
        hide: hash => {
            delete nodeStates[hash];
            updateGraph(Node);
        },
        hidden: hash => {
            return !(hash in nodeStates);
        },
    };
    const graph = createElement("div", {
        "text-align": "center",
        "margin-top": "200px",
    });
    const updateGraph = (Node) => {
        supersedableDelayedAction(500, nodeStates, (nodeStates) => {
            return renderGraph(nodeStates).then(svg => {
                decorateGraph(svg, Node);
                removeAllChildren(graph);
                graph.appendChild(svg);
            });
        });
    };
    Node.selected = hash => {
        return selection[hash];
    }
    const updateSelection = allow => {
        for (const nodeElement of graph.getElementsByClassName("node")) {
            const nodeClasses = nodeElement.classList;
            if (allow) {
                if (Node.selected(nodeElement.id)) {
                    nodeClasses.remove("unselected");
                    nodeClasses.add("selected");
                } else {
                    nodeClasses.remove("selected");
                    nodeClasses.add("unselected");
                }
            } else {
                nodeClasses.remove("selected", "unselected");
            }
        }
    };
    const commitSelection = () => {
        if (Object.keys(selection).length > 0) {
            Object.keys(nodeStates).forEach(hash => {
                delete nodeStates[hash];
            });
            Object.keys(selection).forEach(hash => {
                nodeStates[hash] = false;
                delete selection[hash];
            });
        }
        updateSelection(false);
        updateGraph(Node);
    };
    Node.select = hash => {
        selection[hash] = true;
        updateSelection(true);
    };
    Node.unselect = hash => {
        delete selection[hash];
        updateSelection(true);
    };
    const overlay = createElement("div", {
        "justify-content": "center",
        "display": "flex",
        "position": "fixed",
        "width": "100%",
        "height": "auto",
        "max-height": "100%",
        "left": "0",
        "top": "0",
    });
    const searchBox = createElement("div", {}, overlay);
    searchBox.classList.add("searchBox");
    const setContainerMargin = margin => {
        setStyle(searchBox, {
            "margin": margin,
        });
    };
    const searchBar = createElement("div", {
        "align-items": "center",
        "display": "flex",
    }, searchBox);
    const patternInput = createElement("input", {
        "flex-grow": 1,
        "font-size": "18px",
        "padding": "5px 10px",
        "border-radius": "5px",
        "text-overflow": "ellipsis",
        "min-width": "500px",
    }, searchBar);
    patternInput.setAttribute("title", "The search pattern is a matched against SkyValues using SQLite LIKE.");
    patternInput.setAttribute("placeholder", "Enter a search pattern here to find and display nodes "
                                           + "in the Skyframe graph (you may use % as a wildcard)");
    const nodeCount = createElement("span", {
        "font-size": "18px",
        "font-style": "italic",
        "padding-left": "20px",
        "user-select": "none",
        "color": "#4f4f4f",
    }, searchBar);
    const updateNodeCount = total => {
        nodeCount.textContent = total.toLocaleString() + " nodes";
    };
    var maxTotal = 0;
    const hint = createElement("div", {
        "font-size": "12px",
        "font-style": "italic",
        "padding-left": "20px",
        "color": "#4f4f4f",
        "padding": "8px 20px",
        "text-align": "right",
        "user-select": "none",
    });
    const results = createElement("div", {}, searchBox);
    const addHint = content => {
        hint.textContent = content;
        results.appendChild(hint);
    }
    const renderResults = (total, nodes, pattern) => {
        removeAllChildren(results);
        addHint("Click the nodes below to toggle their visibility. "
            + "Press escape to collapse the search box and explore.")
        for (const hash in nodes) {
            const node = nodes[hash]
            type = prettyNodeType(node.nodeType);
            const row = createElement("div", {
                "margin-bottom": "1px",
                "user-select": "none",
                "cursor": "pointer",
                "padding": "2px 10px",
            }, results);
            row.title = node.nodeData;
            row.classList.add("resultRow", type);
            row.classList.add(Node.hidden(hash) ? "unselected" : "selected");
            const typeSpan = createElement("span", {
                "font-weight": "bold",
                "margin-right": "10px",
                "whitespace": "nowrap",
                "overflow": "ellipsis",
            }, row);
            typeSpan.textContent = type;
            typeSpan.classList.add("nodeType");
            const escapeRegExp = s => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
            const regex = new RegExp(pattern.split("%").reduce((fullRegex, piece) => {
                const pieceRegex = escapeRegExp(piece).replaceAll("_", ".");
                return fullRegex + "(" + pieceRegex + ")(.*)";
            }, "(.*)"), "i");
            const match = node.nodeData.match(regex);
            match.slice(1).map((group, groupIndex) => {
                const span = createElement("span", { }, row);
                span.classList.add(groupIndex % 2 == 0 ? "context" : "highlight");
                if (groupIndex == 0) {
                    const startIndex = Math.min(
                        Math.max(0, group.length - 16),  // Always show the last 16 chars of the first group,
                        2 * Math.max(0, match[0].length - 180)  // but show more if there is space to do so.
                    );
                    const ellipsis = startIndex != 0 ? "…" : "";
                    span.textContent = ellipsis + group.slice(startIndex);
                } else {
                    span.textContent = group;
                }
            });
            row.addEventListener("click", e => {
                if (Node.hidden(hash)) {
                    Node.show(hash);
                } else {
                    Node.hide(hash);
                }
                renderResults(total, nodes, pattern);
            });
        }
        maxTotal = Math.max(maxTotal, total);
        updateNodeCount(total);
    };
    const updateSearch = pattern => {
        supersedableDelayedAction(250, pattern, (pattern) => {
            return findNodes(pattern).then(result => {
                findNodesInProgress = false;
                renderResults(result[0], result[1], pattern);
            });
        });
    }
    patternInput.addEventListener("input", e => {
        updateSearch(e.target.value);
    });
    patternInput.addEventListener("change", e => {
        updateSearch(e.target.value);
    });
    const collapseSearch = () => {
        setContainerMargin("30px 30px 0px");
        patternInput.value = "";
        patternInput.blur();
        removeAllChildren(results);
        updateNodeCount(maxTotal);
    }
    collapseSearch();
    const expandSearch = () => {
        setContainerMargin("30px 30px");
    };
    patternInput.addEventListener("focus", e => {
        expandSearch();
        updateSearch(e.target.value);
    });
    document.addEventListener("keydown", e => {
        switch (e.key) {
            case "Shift":
                updateSelection(true);
                break;
        }
    });
    document.addEventListener("keyup", e => {
        switch (e.key) {
            case "Shift":
                commitSelection();
                break;
            case "Escape":
                collapseSearch();
                break;
        }
    });
    window.setTimeout(() => patternInput.focus(), 100);
    document.body.appendChild(overlay);
    document.body.appendChild(graph);
    loadTheme().then(() =>
        updateGraph(Node)
    );
}
