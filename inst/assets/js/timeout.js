(function() {
  var timeoutWarningMsecs = 5 * 1000;
  var idleTimer;

  function onTimeout() {
    alert('warning: session is about to time out!');
  }

  function startIdleTimer() {
  if (idleTimer) clearTimeout(idleTimer);
  idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
  }

  $(document).on('shiny:message shiny:inputchanged', startIdleTimer);

})();
