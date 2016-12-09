import forcomp.Anagrams._


val a: List[Word] = List("Every", "student", "likes", "Scala", "ilkes")

val l = List(('a', 2), ('b', 2))

val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
val y = List(('r', 1), ('d', 1))

/*val subtractedList=for {
  yy <-y
  xx <- x
  if(xx._1 ==yy._1 )

}yield ((yy._1, xx._2-yy._2))*/

val xMap=x.toMap
val yMap=y.toMap

for {
  y <-yMap

}







