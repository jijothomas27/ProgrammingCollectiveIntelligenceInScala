package chapter1

/**
 * Created by jijo.thomas on 10/7/14.
 */
object Recommendations {

   def critics: Map[String,Map[String,Double]] =Map("Lisa Rose"-> Map("Lady in the Water"-> 2.5, "Snakes on a Plane"-> 3.5,
    "Just My Luck"-> 3.0, "Superman Returns"-> 3.5, "You, Me and Dupree"-> 2.5,
    "The Night Listener"-> 3.0),
    "Gene Seymour"-> Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 3.5,
      "Just My Luck"-> 1.5, "Superman Returns"-> 5.0, "The Night Listener"-> 3.0,
      "You, Me and Dupree"-> 3.5),
    "Michael Phillips"-> Map("Lady in the Water"-> 2.5, "Snakes on a Plane"-> 3.0,
      "Superman Returns"-> 3.5, "The Night Listener"-> 4.0),
    "Claudia Puig"-> Map("Snakes on a Plane"-> 3.5, "Just My Luck"-> 3.0,
      "The Night Listener"-> 4.5, "Superman Returns"-> 4.0,
      "You, Me and Dupree"-> 2.5),
    "Mick LaSalle"-> Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 4.0,
      "Just My Luck"-> 2.0, "Superman Returns"-> 3.0, "The Night Listener"-> 3.0,
      "You, Me and Dupree"-> 2.0),
    "Jack Matthews"-> Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 4.0,
      "The Night Listener"-> 3.0, "Superman Returns"-> 5.0, "You, Me and Dupree"-> 3.5),
    "Toby"-> Map("Snakes on a Plane"->4.5,"You, Me and Dupree"->1.0,"Superman Returns"->4.0))


  /**
   * Returns a distance-based similarity score for person1 and person2
   * @param prefs
   * @param person1
   * @param person2
   * @return
   */
  def simDistance(prefs:Map[String,Map[String,Double]],person1:String,person2:String):Double = {

    val si = for {
      (item,value) <- prefs(person1)
      if(prefs(person2) contains(item))
    } yield item -> (value,prefs(person2)(item))

    val distances = for {
      (item,pandq) <- si
      (p,q) = pandq
    }yield Math.pow(p-q,2)

    val sumOfSquares = distances.sum

    1/(1+Math.sqrt(sumOfSquares))
  }
}
