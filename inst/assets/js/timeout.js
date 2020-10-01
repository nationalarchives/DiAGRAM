(function() {
  var timeoutWarningMsecs = 10 * 60 * 1000;
  var idleTimer;

  function onTimeout() {
    Shiny.setInputValue('timeout', Math.random());
  }

  function startIdleTimer() {
    console.log("time_start");
    if (idleTimer) clearTimeout(idleTimer);
    idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
  }

  $(document).on('shiny:message shiny:inputchanged', startIdleTimer);

})();

