scala_library(
  name='parser',
  dependencies=[
    '3rdparty/jvm/org/parboiled',
  ],
)

jvm_binary(
  name='bin',
  main='funnel.language.Main',
  dependencies=[
    ':parser',
  ],
)
