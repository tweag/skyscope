const svgNS = "http://www.w3.org/2000/svg";

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

function createViewport() {
    const svg = document.createElementNS(svgNS, "svg");
    svg.setAttribute("version", "2");
    svg.setAttribute("href", "file:///tmp/render.svg");
    return svg;
}

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

function createSearchInterface() {
    const overlay = createElement("div", {
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
            "flex-grow": 1,
            "overflow": "hidden",
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
    const collapseHint = createElement("div", {
        "font-size": "12px",
        "font-style": "italic",
        "padding-left": "20px",
        "color": "#4f4f4f",
        "padding": "8px 20px",
        "text-align": "right",
        "user-select": "none",
    });
    collapseHint.textContent = "Press escape to collapse the search box and explore.";
    const results = createElement("div", {}, container);
    const clearResults = () => {
        while (results.firstChild) {
            results.removeChild(results.firstChild);
        }
    }
    const renderResults = (total, nodes, pattern) => {
        clearResults();
        results.appendChild(collapseHint);
        for (const hash in nodes) {
            const node = nodes[hash]
            const words = [ ...node.nodeType.matchAll(/[^_]+/g) ];
            const type = words.map(match => {
                return match[0].charAt(0).toUpperCase()
                     + match[0].slice(1).toLowerCase()
            }).join("");
            const row = createElement("div", {
                //"background-color": "#bacdd4",
                "padding-bottom": "5px",
                "cursor": "pointer",
                //"display": "flex",
                //"flex-direction": "row",
                //"flex-wrap": "nowrap",
            }, results);
            row.setAttribute("class", "resultRow " + type);
            const typeSpan = createElement("span", {
                "font-weight": "bold",
                "margin-right": "10px",
                "whitespace": "nowrap",
                "overflow": "ellipsis",
            }, row);
            typeSpan.setAttribute("class", "nodeType");
            typeSpan.textContent = type.replace(" (unshareable)", "");
            const escapeRegExp = s => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
            const regex = new RegExp(pattern.split("%").reduce((regex, piece) =>
                regex + "(" + escapeRegExp(piece) + ")(.*)", "(.{0,16})"
            ), "i");
            const match = node.nodeData.match(regex);
            match.slice(1).map((group, groupIndex) => {
                const span = createElement("span", { }, row);
                span.setAttribute("class", groupIndex % 2 == 0 ? "context" : "highlight");
                const ellipsis = groupIndex == 0 && match.index != 0 ? "…" : "";
                span.textContent = ellipsis + group;
            });
        }
        maxTotal = Math.max(maxTotal, total);
        updateNodeCount(total);
    };
    const activeTimeouts = {};
    const supersedableDelay = (delay, action) => {
        timeoutID = activeTimeouts[action];
        if (timeoutID != undefined) {
            window.clearTimeout(timeoutID);
        }
        activeTimeouts[action] = window.setTimeout(action, delay);
    };
    var findNodesInProgress = false;
    var findNodesPendingPattern = null;
    const update = pattern => {
        supersedableDelay(250, () => {
            if (findNodesInProgress) {
                findNodesPendingPattern = pattern;
            } else {
                const loop = pattern => {
                    findNodesInProgress = true;
                    findNodes(pattern).then(result => {
                        findNodesInProgress = false;
                        renderResults(result[0], result[1], pattern);
                        if (findNodesPendingPattern != null) {
                            const pattern = findNodesPendingPattern;
                            findNodesPendingPattern = null;
                            loop(pattern);
                        }
                    });
                };
                loop(pattern);
            }
        });
    }
    const collapseSearch = () => {
        setContainerMargin("30px 100px 0px");
        patternInput.value = "";
        patternInput.blur();
        clearResults();
        updateNodeCount(maxTotal);
    }
    collapseSearch();
    const expandSearch = () => {
        setContainerMargin("30px 100px");
    };
    patternInput.addEventListener("focus", e => {
        expandSearch();
        update(e.target.value);
    });
    patternInput.addEventListener("input", e => {
        update(e.target.value);
    });
    patternInput.addEventListener("change", e => {
        update(e.target.value);
    });
    document.addEventListener("keyup", e => {
        if (e.key == "Escape") {
            collapseSearch();
        }
    });
    window.setTimeout(() => patternInput.focus(), 100);
    return overlay;
}

window.onload = function() {
    document.body.appendChild(createSearchInterface());

    findNodes("enet")
        .then(result => renderGraph(Object.keys(result[1])))
        .then(svg => document.body.appendChild(svg));
}

