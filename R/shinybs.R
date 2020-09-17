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
}
