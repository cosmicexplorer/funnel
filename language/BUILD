scala_library(
  name='parser',
  dependencies=[
    '3rdparty/jvm:parboiled2',
  ],
)

junit_tests(
  name='test-parser',
  dependencies=[
    ':parser',
    '3rdparty/jvm:scalatest',
  ],
)
