objectSize <-
function (x) 
structure(.Internal(object.size(x)), class = "object_size")
