

val v=Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))

val c='o'
val row = v indexWhere (_.indexOf(c) != -1)
val col = v(row) indexOf c