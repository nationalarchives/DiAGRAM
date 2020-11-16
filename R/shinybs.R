addClass = function (tag, add) {
  tag$attribs$class <- paste(tag$attribs$class, add)
  return(tag)
}

removeClass = function (tag, remove) {
  if (length(remove) == 1)
    remove <- strsplit(remove, " ", fixed = TRUE)[[1]]
  class <- strsplit(tag$attribs$class, " ", fixed = TRUE)[[1]]
  class <- class[!(class %in% remove)]
  tag$attribs$class <- paste(class, collapse = " ")
  return(tag)
}

addAttribs = function (tag, ...) {
  a <- list(...)
  for (i in seq(length(a))) {
    tag$attribs[names(a)[i]] = a[[i]]
  }
  return(tag)
}

bsButton = function (inputId,
                     label,
                     icon = NULL,
                     ...,
                     style = "default",
                     size = "default",
                     type = "action",
                     block = FALSE,
                     disabled = FALSE,
                     value = FALSE) {
  btn = shiny::actionButton(inputId, label, icon, ...)
  if (type == "toggle") {
    btn = removeClass(btn, "action-button")
    btn = addClass(btn, "sbs-toggle-button")
    if (value) {
      btn == addClass(btn, "active")
    }
  }
  if (style != "default") {
    btn = removeClass(btn, "btn-default")
    btn = addClass(btn, paste0("btn-", style))
  }
  size = switch(
    size,
    `extra-small` = "btn-xs",
    small = "btn-sm",
    large = "btn-lg",
    "default"
  )
  if (size != "default") {
    btn = addClass(btn, size)
  }
  if (block) {
    btn = addClass(btn, "btn-block")
  }
  if (disabled) {
    btn = addAttribs(btn, disabled = "disabled")
  }
  btn
}

bsPopover = function (id, title, content, placement = "bottom", trigger = "hover",
          options = NULL) {
  options = buildTooltipOrPopoverOptionsList(title, placement,
                                             trigger, options, content)
  createTooltipOrPopoverOnUI(id, "popover", options)
}

buildTooltipOrPopoverOptionsList = function (title, placement, trigger, options, content)
{
  if (is.null(options)) {
    options = list()
  }
  if (!missing(content)) {
    if (is.null(options$content)) {
      options$content = shiny::div(content)
    }
  }
  if (is.null(options$placement)) {
    options$placement = placement
  }
  if (is.null(options$trigger)) {
    if (length(trigger) > 1)
      trigger = paste(trigger, collapse = " ")
    options$trigger = trigger
  }
  if (is.null(options$title)) {
    options$title = title
  }
  return(options)
}

createTooltipOrPopoverOnUI = function(id, type = "popover", options) {
  content_opt = options$content
  other_opt = options[setdiff(names(options),"content")]
  other_opt = paste0("'", paste(names(other_opt), other_opt, sep = "': '",
                                 collapse = "', '"), "'")
  options_json = glue::glue(
    "{{'content': `{options$content}`,
    {other_opt}}
    "
  )


  bsTag <- shiny::tags$script(
    shiny::HTML(
      paste0("$(document).ready(function() {setTimeout(function() {shinyBS.addTooltip('",
             id, "', '", type, "', ", options_json, ")}, 500)});"))
    )
}
