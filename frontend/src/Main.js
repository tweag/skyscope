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
