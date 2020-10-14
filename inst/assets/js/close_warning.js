/* https://stackoverflow.com/a/7317311 */

setTimeout(function() {
    window.addEventListener("beforeunload", function (e) {
        //if (window.warnBeforeClose) {
        var confirmationMessage = 'It looks like you have been editing something. '
                                + 'If you leave before saving, your changes will be lost.';

        (e || window.event).returnValue = confirmationMessage; //Gecko + IE
        return confirmationMessage; //Gecko + Webkit, Safari, Chrome etc.
        //}
    });
}, 60000);
