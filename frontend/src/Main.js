export function scrollIntoView(element) {
  return function () {
    element.scrollIntoView({
      block: "center",
      inline: "center",
    })
  }
}
