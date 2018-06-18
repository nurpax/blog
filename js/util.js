
function isScrolledIntoView(el) {
  var rect = el.getBoundingClientRect();
  if (rect.bottom < 0) {
    return false
  }
  if (rect.top >= window.innerHeight) {
    return false
  }
  return true
}

module.exports = { isScrolledIntoView }
