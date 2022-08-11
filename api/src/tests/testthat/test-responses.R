simple_req = test_req_score("simple")
parsed_json = jsonlite::parse_json(simple_req$postBody)
responses = extract_responses(parsed_json)[[1]]

# technical skills
test_that("conversion of technical skills responses to numeric", {
  to_numeric_technical_skills = .to_numeric_funcs[["Technical_Skills"]]
  technical_skills = to_numeric_technical_skills(responses$data$response$Technical_Skills)
  expect_true(length(technical_skills) == 1)
  expect_true(is.numeric(technical_skills))

  bad_responses = list(
    c("None", "None"), # too short
    rep("None", 11), # too long
    sample(10), # wrong type
    c(rep("None", 9), "Garbage") # unexpected value
  )

  purrr::walk(bad_responses, ~{
    expect_error(
      to_numeric_technical_skills(.x)
    )
  })
})

# physical disaster
test_that("conversion of physical disaster responses to numeric", {
  to_numeric_physical_disaster = .to_numeric_funcs[["Physical_Disaster"]]
  physical_disaster = to_numeric_physical_disaster(responses$data$response$Physical_Disaster)
  expect_true(length(physical_disaster) == 1)
  expect_true(is.numeric(physical_disaster))

  bad_responses = list(
    character(), # too short
    rep("Low", 2), # too long
    10, # wrong type
    "Garbage" # unexpected value
  )

  purrr::walk(bad_responses, ~{
    expect_error(
      to_numeric_physical_disaster(.x)
    )
  })
})

# system security
test_that("conversion of system security responses to numeric", {
  to_numeric_system_security = .to_numeric_funcs[["System_Security"]]
  system_security = to_numeric_system_security(responses$data$response$System_Security)
  expect_true(is.numeric(system_security))
  expect_true(length(system_security) == 1)

  bad_responses = list(
    1:4, # wrong type
    list(a = 1, b = 2, c = 3, d = 4, e = 5), # wrong length
    list(a = 1, b = 2, c = 3, d = 4), #wrong subtype
    list(a = letters[1:2], b = letters[1], c = letters[1], d = letters[1]) # wrong sublength
  )

  purrr::walk(bad_responses, ~{
    expect_error(
      to_numeric_system_security(.x)
    )
  })
})

# checksum
test_that("converstion to numeric of checksum response", {
  to_numeric_checksum = .to_numeric_funcs[["Checksum"]]
  checksum = to_numeric_checksum(responses$data$response$Checksum)
  expect_true(length(checksum) == 3)
  expect_true(is.numeric(checksum))

  bad_responses = list(
    letters[1:3], # wrong type
    1:4, # wrong length
    c(50, 50, 50) # total =/= 100
  )

  purrr::walk(bad_responses, ~{
    expect_error(
      to_numeric_checksum(.x)
    )
  })
})

# digital object
test_that("converstion to numeric of digital object response", {
  to_numeric_digital_object = .to_numeric_funcs[["Digital_Object"]]
  digital_object = to_numeric_digital_object(responses$data$response$Digital_Object)
  expect_true(length(digital_object) == 3)
  expect_true(is.numeric(digital_object))

  bad_responses = list(
    letters[1:3], # wrong type
    1:4, # wrong length
    c(50, 50, 50) # total =/= 100
  )

  purrr::walk(bad_responses, ~{
    expect_error(
      to_numeric_digital_object(.x)
    )
  })
})

# storage medium
test_that("converstion to numeric of storage medium response", {
  to_numeric_storage_medium = .to_numeric_funcs[["Storage_Medium"]]
  storage_medium = to_numeric_storage_medium(responses$data$response$Storage_Medium)
  expect_true(length(storage_medium) == 3)
  expect_true(is.numeric(storage_medium))

  bad_responses = list(
    letters[1:3], # wrong type
    1:4, # wrong length
    c(50, 50, 50) # total =/= 100
  )

  purrr::walk(bad_responses, ~{
    expect_error(
      to_numeric_storage_medium(.x)
    )
  })
})

# info management
test_that("conversion of info management responses to numeric", {
  to_numeric_info_management = .to_numeric_funcs[["Info_Management"]]
  info_management = to_numeric_info_management(responses$data$response$Info_Management)
  expect_true(is.numeric(info_management))
  expect_true(length(info_management) == 1)

  bad_responses = list(
    1:3, # wrong type
    list(a = 1, b = 2, c = 3, d = 4), # wrong length
    list(a = 1, b = 2, c = 3), #wrong subtype
    list(a = letters[1:2], b = letters[1], c = letters[1], d = letters[1]) # wrong sublength
  )

  purrr::walk(bad_responses, ~{
    expect_error(
      to_numeric_info_management(.x)
    )
  })
})

# op environment
test_that("conversion of op environment response to numeric", {
  to_numeric_op_environment = .to_numeric_funcs[["Op_Environment"]]
  op_env = to_numeric_op_environment(responses$data$response$Op_Environment)
  expect_true(is.numeric(op_env))
  expect_true(length(op_env) == 1)

  # ensure that the return condition for 100 is satisfied
  op_env = to_numeric_op_environment(list(`1` = 50, `2` = "Yes"))
  expect_equal(op_env, 100)

  bad_responses = list(
    1:2, # wrong type
    list(a = 1, b = "a", c = 2), # wrong length
    list(a = 1:2, b = "a"), # wrong sub length
    list(a = 1, b = letters[1:2]), # wrong sub length
    list(a = 1, b = 2), # wrong subtype
    list(a = "a", b = "b"), # wrong subtype
    list(a = 10, b = "maybe") # non allowed values
  )

  purrr::walk(bad_responses, ~{
    expect_error(
      to_numeric_op_environment(.x)
    )
  })
})

# rep and refresh
test_that("conversion of rep and refresh response to numeric", {
  to_numeric_rep_and_refresh = .to_numeric_funcs[["Rep_and_Refresh"]]
  rep = to_numeric_rep_and_refresh(responses$data$response$Rep_and_Refresh)
  expect_true(is.numeric(rep))
  expect_true(length(rep) == 1)

  bad_responses = list(
    1:2, # wrong type
    list(a = 1, b = 2, c = 2), # wrong length
    list(a = 1:2, b = 2), # wrong sub length
    list(a = 1, b = 1:2), # wrong sub length
    list(a = 1, b = "a"), # wrong subtype
    list(a = "a", b = 1) # wrong subtype
  )

  purrr::walk(bad_responses, ~{
    expect_error(
      to_numeric_rep_and_refresh(.x)
    )
  })
})

test_that("full conversion to numeric", {
  out = to_numeric(responses)
  expect_true(length(out) == 9)
  expect_s3_class(out, "simple")
  expect_true(all(purrr::map_lgl(out, ~{
    all(is.numeric(.x))
  })))
  expect_false(is.null(names(out)))
})

test_that("numeric conversion to probability", {
  expect_error(numeric_to_probability_single(1, "garbage"))
  single = numeric_to_probability_single(50, "Op_Environment")
  expect_s3_class(single, "table")
  expect_true(all(single >= 0 & single <= 1))
  expect_equal(sum(single), 1)
  triple = numeric_to_probability_single(c(10, 20, 70), "Digital_Object")
  expect_s3_class(triple, "table")
  expect_true(all(triple >= 0 & triple <= 1))
  expect_equal(sum(triple), 1)
  out = numeric_to_probability(to_numeric(responses))
  expect_true(is.list(out))
  expect_length(out, 9)
  purrr::walk(out, ~{
    expect_s3_class(.x, "table")
    expect_true(all(.x >= 0 & .x <= 1))
    expect_equal(sum(.x), 1)
  })
})

test_that("flagging advanced models", {
  expect_false(advanced_flags(parsed_json))
})
