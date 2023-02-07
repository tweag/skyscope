export function scrollIntoView(element) {
  return function () {
    element.scrollIntoView({
      behavior: "smooth",
      block: "center",
      inline: "center",
    })
  }
}
