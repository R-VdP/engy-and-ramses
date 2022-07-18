// The id of the element used to determine the viewport height
const introElementId = "intro-full-viewport"

function getInnerDimensions() {
  const element = document.getElementById(introElementId)
  return {
    width:  element ? element.clientWidth  : window.innerWidth,
    height: element ? element.clientHeight : window.innerHeight
  }
}

function sendInnerDimensions(elmApp) {
  elmApp.ports.receiveWindowSize.send(getInnerDimensions())
}

function initApp() {
  let app = undefined

  // We want to add a ResizeObserver once the element with
  // id intro-full-viewport gets created.
  // We will this observe DOM changes until we find this element.
  new MutationObserver((_mutationList, observer) => {
    const introElem = document.getElementById(introElementId)
    if (introElem) {
      // We found the element, further events do not need to be handled,
      // so we can disconnect this observer.
      observer.disconnect()

      // Every time the viewport size changes, we inform the Elm app about this.
      new ResizeObserver(_entries => {
        // If the app does not exist yet, we do nothing.
        if (app) {
          sendInnerDimensions(app)
        }
      }).observe(introElem)
    }
  }).observe(document.body,
             { attributes: false, childList: true, subtree: true })

  // The MutationObserver is in place, we can launch our Elm app.
  app = Elm.Main.init({
    flags: {
      introFullVpId: introElementId,
      windowSize: getInnerDimensions()
    }
  })
}

initApp()

