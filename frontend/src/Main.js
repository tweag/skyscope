export function getImportId() {
    return importId;
}

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

export function onScroll(element) {
    return function (action) {
        return function () {
            element.addEventListener("scroll", _ => action());
        }
    }
}
