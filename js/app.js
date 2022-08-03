function initApp() {
  "use strict";

  // The id of the element used to determine the viewport dimensions.
  const introElementId = "intro-full-viewport"

  const scheduledHandlers = {}
  function throttleEventHandler(handlerId, delay, callback) {
    if (! scheduledHandlers.hasOwnProperty(handlerId)) {
      scheduledHandlers[handlerId] = setTimeout(() => {
        callback()
        delete scheduledHandlers[handlerId]
      }, delay);
    }
  }

  function getIntroElement() {
    return document.getElementById(introElementId)
  }

  function getInnerDimensions() {
    const introElem = getIntroElement()
    // If the intro element does not exist yet,
    // we send the inner* dimensions instead.
    return {
      width:  introElem ? introElem.clientWidth  : window.innerWidth,
      height: introElem ? introElem.clientHeight : window.innerHeight
    }
  }

  function sendInnerDimensions(elmApp) {
    elmApp.ports.receiveWindowSize.send(getInnerDimensions())
  }

  function go() {
    let app = undefined

    function withApp(continuation) {
      // If the app does not exist yet, we do nothing.
      if (app) {
        continuation(app)
      }
    }

    // We want to add a ResizeObserver once the element with
    // id intro-full-viewport gets created.
    // We will thus observe DOM changes until we find this element.
    new MutationObserver((_mutationList, observer) => {
      const introElem = getIntroElement()
      if (introElem) {
        // We found the element, further events do not need to be handled,
        // so we can disconnect this observer.
        observer.disconnect()

        // Every time the viewport size changes, we inform the Elm app about this.
        // The observer will trigger a first time when observation starts.
        // See: https://www.w3.org/TR/resize-observer/#intro
        new ResizeObserver(entries => {
          throttleEventHandler("sendInnerDimensions", 200, () => {
            withApp(sendInnerDimensions)
          })
        }).observe(introElem)

        document.addEventListener(
          "scroll",
          (event) => {
            throttleEventHandler("sendScrollNotification", 200, () => {
              withApp(app => app.ports.notifyScrolling.send({}))
            })
          },
          { passive: true }
        );
      }
    }).observe(document.body, { childList: true, subtree: true })

    // The MutationObserver is in place, we can launch our Elm app.
    app = Elm.Main.init({
      flags: {
        introFullVpId: introElementId,
        windowSize: getInnerDimensions()
      }
    })

    app.ports.performSmoothScrollTo.subscribe(coordinates => {
      window.scroll({
        top: coordinates.y,
        left: coordinates.x,
        behavior: "smooth"
      })
    })
  }

  if (document.readyState === 'loading') {  // Loading hasn't finished yet
    document.addEventListener('DOMContentLoaded', go)
  } else {  // DOMContentLoaded has already fired
    go()
  }
}

initApp()

