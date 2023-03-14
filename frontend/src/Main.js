// getImportId :: Effect String
export function getImportId() {
    return importId;
}

// scrollIntoView :: Element -> Effect Unit
export function scrollIntoView(element) {
    return function () {
        const documentElement = document.documentElement;
        const elementBounds = element.getBoundingClientRect();
        const offset = (pageOff, elemOff, pageSize, elemSize) =>
            pageOff + elemOff - pageSize / 2 + elemSize / 2;
        window.scrollTo({
            behavior: "smooth",
            left: offset(
                window.pageXOffset,
                elementBounds.left,
                documentElement.clientWidth,
                elementBounds.width
            ),
            top: offset(
                window.pageYOffset,
                elementBounds.top,
                documentElement.clientHeight,
                elementBounds.height
            ),
        });
    }
}

// onScroll :: Element -> Effect Unit -> Effect Unit
export function onScroll(element) {
    return function (action) {
        return function () {
            element.addEventListener("scroll", _ => action());
        }
    }
}

// pushHistory :: Json -> Effect Unit
export function pushHistory(state) {
    return function () {
        window.history.pushState(state, "");
    }
}

// onPopHistory :: (Json -> Effect Unit) -> Effect Unit
export function onPopHistory(action) {
    return function () {
        window.addEventListener("popstate", event => {
            action(event.state)();
        });
    }
}

// updateSaveLink :: Effect Unit
export function updateSaveLink() {
    const link = document.getElementById("Save");
    const existingUrl = link.getAttribute("href");
    if (existingUrl) {
        URL.revokeObjectURL(existingUrl);
    }
    const svg = document.getElementById("Graph").firstChild.cloneNode(true);
    if (!svg) {
        return;
    }
    Array.from(svg.getElementsByClassName("Animation")).forEach(animation => animation.parentNode.removeChild(animation));
    Array.from(svg.getElementsByClassName("Changed")).forEach(element => element.classList.remove("Changed"));
    Array.from(svg.getElementsByTagName("a")).forEach(a => a.removeAttribute("xlink:title"));
    Array.from(svg.getElementsByTagName("title")).forEach(title => title.parentNode.removeChild(title));
    const style = document.getElementsByTagName("style")[0].cloneNode(true);
    style.textContent = style.textContent.replaceAll("cursor: pointer", "cursor: auto")
    svg.appendChild(style);
    const xml = new XMLSerializer().serializeToString(svg);
    const blob = new Blob([xml], { type: "image/svg+xml" });
    const url = URL.createObjectURL(blob);
    link.setAttribute("href", url);
    const basename = importTag.match(/[^\/]*$/)[0];
    const filename = `${basename} ${new Date().toLocaleString()}.svg`;
    link.setAttribute("download", filename);
}

// setCheckpoint :: Json -> Effect Unit
export function setCheckpoint(state) {
    return function () {
        window.localStorage.setItem(importId, JSON.stringify(state));
    }
}

// getCheckpoint :: Effect Json
export function getCheckpoint() {
    return JSON.parse(window.localStorage.getItem(importId) ?? "null");
}

// formatNode :: Object String -> Effect (Object String)
export function formatNode(node) {
    return function () {
        return _formatNode(node);
    }
}
