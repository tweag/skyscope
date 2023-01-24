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

window.onload = function() {
    //alert("hello skyscope!");

    const element = document.createElement("div");
    document.body.appendChild(element);

//    const viewport = createViewport();
//    document.body.appendChild(viewport);

    findNodes("%enet%")
        .then(nodes => renderGraph(Object.keys(nodes)))
        .then(svg => document.body.appendChild(svg));
}

