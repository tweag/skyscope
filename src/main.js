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

async function renderGraph(visibleNodes) {
    const parser = new DOMParser();
    const body = await post("/render", visibleNodes);
    const doc = parser.parseFromString(body, "image/svg+xml");
    return doc.getElementsByTagNameNS(svgNS, "svg")[0];
}

function prettyNodeType(node) {
    return [
        ...node.nodeType.matchAll(/[^_]+/g)
    ].map(match => match[0].charAt(0).toUpperCase()
                 + match[0].slice(1).toLowerCase()
    ).join("").replace(" (unshareable)", "");
}

function prettyNodeLabel(type, nodeData) {
    var match = null;
    switch (type) {
        case "File":
        case "FileState":
        case "DirectoryListing":
        case "DirectoryListingState":
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
        case "Package":
        case "PackageLookup":
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
    return null;
}

function decorateGraph(svg, visibleNodes) {
    for (const nodeElement of svg.getElementsByClassName("node")) {
        const node = visibleNodes[nodeElement.id];
        if (node != null) {
            const textElements = nodeElement.getElementsByTagName("text");
            const typeElement = textElements[0];
            const labelElement = textElements[1];
            const type = prettyNodeType(node);
            typeElement.textContent = type;
            typeElement.setAttribute("class", "nodeType");
            if (type in theme) {
                typeElement.setAttribute("fill", theme[type]);
            }
            labelElement.setAttribute("class", "nodeLabel");
            nodeElement.setAttribute("class", "node " + type);
            const label = prettyNodeLabel(type, node.nodeData);
            if (label != null) {
                const maxChars = 40;
                const ellipsis = label.length > maxChars ? "…" : "";
                labelElement.textContent = ellipsis + label.slice(-maxChars);
            }
        } else {

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
    const graph = createElement("div", {
        "text-align": "center",
        "margin-top": "200px",
    });
    const visibleNodes = {};
    const updateGraph = () => {
        supersedableDelayedAction(1000, Object.keys(visibleNodes), (hashes) => {
            return renderGraph(hashes).then(svg => {
                decorateGraph(svg, visibleNodes);
                removeAllChildren(graph);
                graph.appendChild(svg);
            });
        });
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
    const container = createElement("div", {}, overlay);
    const setContainerMargin = margin => {
        setStyle(container, {
            "background-color": "#ccd7db",
            "border-radius": "20px",
            "opacity": "95%",
            "margin": margin,
            "padding": "30px",
            "overflow": "hidden",
            "min-width": "800px",
        });
    };
    const searchBox = createElement("div", {
        "align-items": "center",
        "display": "flex",
    }, container);
    const patternInput = createElement("input", {
        "flex-grow": 1,
        "font-size": "18px",
        "padding": "5px 10px",
        "border-radius": "5px",
        "text-overflow": "ellipsis",
        "min-width": "500px",
    }, searchBox);
    patternInput.setAttribute("title", "The search pattern is a matched against SkyValues using SQLite LIKE.");
    patternInput.setAttribute("placeholder", "Enter a search pattern here to find and display nodes "
                                           + "in the Skyframe graph (you may use % as a wildcard)");
    const nodeCount = createElement("span", {
        "font-size": "18px",
        "font-style": "italic",
        "padding-left": "20px",
        "user-select": "none",
        "color": "#4f4f4f",
    }, searchBox);
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
    hint.textContent
        = "Click the nodes below to toggle their visibility. "
        + "Press escape to collapse the search box and explore.";
    const results = createElement("div", {}, container);
    const renderResults = (total, nodes, pattern) => {
        removeAllChildren(results);
        results.appendChild(hint);
        for (const hash in nodes) {
            const node = nodes[hash]
            type = prettyNodeType(node);
            const row = createElement("div", {
                "padding-bottom": "5px",
                "user-select": "none",
                "cursor": "pointer",
            }, results);
            row.title = node.nodeData;
            const classes = [ "resultRow", type, hash in visibleNodes ? "visible" : "hidden" ];
            row.setAttribute("class", classes.join(" "));
            const typeSpan = createElement("span", {
                "font-weight": "bold",
                "margin-right": "10px",
                "whitespace": "nowrap",
                "overflow": "ellipsis",
            }, row);
            typeSpan.textContent = type;
            typeSpan.setAttribute("class", "nodeType");
            const escapeRegExp = s => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
            const regex = new RegExp(pattern.split("%").reduce((fullRegex, piece) => {
                const pieceRegex = escapeRegExp(piece).replaceAll("_", ".");
                return fullRegex + "(" + pieceRegex + ")(.*)";
            }, "(.*)"), "i");
            const match = node.nodeData.match(regex);
            match.slice(1).map((group, groupIndex) => {
                const span = createElement("span", { }, row);
                span.setAttribute("class", groupIndex % 2 == 0 ? "context" : "highlight");
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
                if (hash in visibleNodes) {
                    delete visibleNodes[hash];
                } else {
                    visibleNodes[hash] = node;
                }
                renderResults(total, nodes, pattern);
                updateGraph();
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
    patternInput.addEventListener("input", e => {
        updateSearch(e.target.value);
    });
    patternInput.addEventListener("change", e => {
        updateSearch(e.target.value);
    });
    document.addEventListener("keyup", e => {
        if (e.key == "Escape") {
            collapseSearch();
        }
    });
    window.setTimeout(() => patternInput.focus(), 100);
    document.body.appendChild(overlay);
    document.body.appendChild(graph);
    loadTheme().then(() => updateGraph());
}

