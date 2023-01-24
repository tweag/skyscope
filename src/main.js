const svgNS = "http://www.w3.org/2000/svg";

async function post(url, data) {
    const response = await fetch(url, { method: "POST", body: JSON.stringify(data) });
    if (!response.ok) {
        throw new Error("POST " + url + " failed: " + response.statusText);
    }
    return await response.text();
}

async function findNodes(pattern) {
    return JSON.parse(await post("/find", pattern));
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
    const overlay = createElement("div");
    const setOverlayStyle = function(height) {
        setStyle(overlay, {
            "display": "flex",
            "position": "fixed",
            "width": "100%",
            "height": height,
            "left": "0",
            "top": "0",
        });
    }
    const container = createElement("div", {}, overlay);
    const setContainerStyle = function(margin) {
        setStyle(container, {
            "background-color": "#ccd7db",
            "border-radius": "20px",
            "opacity": "95%",
            "margin": margin,
            "padding": "30px",
            "flex-grow": 1,
        });
    }
    const searchBox = createElement("div", {
        "align-items": "center",
        "display": "flex",
    }, container);
    const patternInput = createElement("input", {
        "flex-grow": 1,
        "font-size": "28px",
        "padding": "5px 10px",
        "border-radius": "5px",
    }, searchBox);
    const nodeCount = createElement("span", {
        "font-size": "28px",
        "font-style": "italic",
        "padding-left": "20px",
        "color": "#4f4f4f",
    }, searchBox);
    nodeCount.appendChild(document.createTextNode((61303).toLocaleString() + " nodes"))
    function expandSearch() {
        setOverlayStyle("100%");
        setContainerStyle("30px 100px");
    }
    function collapseSearch() {
        setOverlayStyle("auto");
        setContainerStyle("30px 100px 0px");
        patternInput.blur();
    }
    patternInput.addEventListener("focus", _ => expandSearch());
    document.addEventListener("keyup", (e) => {
        if (e.key == "Escape") {
            collapseSearch();
        }
    });

    collapseSearch();
    return overlay;
}

window.onload = function() {
    document.body.appendChild(createSearchInterface());

    findNodes("%enet%")
        .then(nodes => renderGraph(Object.keys(nodes)))
        .then(svg => document.body.appendChild(svg));
}

