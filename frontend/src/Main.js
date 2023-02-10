export function scrollIntoView(element) {
  return function () {
    const documentElement = document.documentElement;
    const elementBounds = element.getBoundingClientRect();
    const top = window.pageYOffset + elementBounds.top - documentElement.clientHeight / 2 + elementBounds.height / 2;
    const left = window.pageXOffset + elementBounds.left - documentElement.clientWidth / 2 + elementBounds.width / 2;
    window.scrollTo({ behavior: "smooth", top, left });
  }
}
