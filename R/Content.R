Content <- setRefClass(
  Class = "Content",
  contains = "Parent",
  methods = list(
    show = function() {
      cat(toString())
    }
  )
)