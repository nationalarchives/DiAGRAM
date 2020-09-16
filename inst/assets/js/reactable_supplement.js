// JS source to modify the react tables to allow input and actions

/**
 * Turn a text value into an input value in the table
 *
 * @param id an for the input element
 * @param value attribute for input
 * @param placholder attribute for input
 */
function table_input(id, value, placeholder = "") {
  console.log('input ' + id + ' ' + value);
  return `<input class="form-control table-input" id="${id}" placeholder="${placeholder}" value="${value}" disabled>`;
}

function table_delete(id, value) {
  return `
  <span>
    <button id="${id}" type="button" class="btn btn-default action-button btn-danger">
      <i class="fa fa-trash">
    </button>
    value
  </span>`;
}


/**
 *  Wait for the element to appear in the dom, when it appears,
 * provide it to the callback.
 *
 * @param selector a jQuery selector
 * @param callback function that takes selected element
 */
function waitForEl(selector, callback) {
  console.log('wait for ' + selector);
  let poller1 = setInterval(function() {
    $jObject = jQuery(selector);
    if($jObject.length < 1) {
      return ;
    }
    clearInterval(poller1);
    callback($jObject);
  }, 100);
}


/**
 *
 */
function reactableSendInputMessage(e, inputId) {
  let t = e.target;
  console.log("send my messages");
  console.log(e);
  let _, column, row_id, rest;
  [_, column, row_id, ...rest] = t.id.match(/^(.+)_(\d+)$/);
  Shiny.onInputChange(inputId, {
    id: "select_in_table",
    column: column,
    row_id: + row_id + 1,
    new_value: t.value,
    nonce: Math.random()
  });
}
