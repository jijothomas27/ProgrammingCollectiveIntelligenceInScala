package chapter1

/**
 * Created by jijo.thomas on 10/7/14.
 */
object Recommendations {

   type Preferences = Map[String,Map[String,Double]]
   type Similarity = (Preferences,String,String) => Double

   def critics: Preferences =Map("Lisa Rose"-> Map("Lady in the Water"-> 2.5, "Snakes on a Plane"-> 3.5,
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
   * @param prefs:Preferences
   * @param person1:String
   * @param person2:String
   **/
  def simDistance(prefs:Preferences,person1:String,person2:String):Double = {

    val si = for {
      (item,value) <- prefs(person1)
      if (prefs.contains(person2) && prefs(person2).contains(item))
    } yield item -> (value,prefs(person2)(item))


    if (si.nonEmpty) {
      val distances = for {
        (item,pandq) <- si
        (p,q) = pandq
      }yield Math.pow(p-q,2)

      val sumOfSquares = distances.sum

      1/(1+Math.sqrt(sumOfSquares))
    } else
      0

  }

  def simPearson(prefs:Preferences,person1:String,person2:String):Double = {
    val si = for {
      (item,value) <- prefs(person1)
      if (prefs.contains(person2) && prefs(person2).contains(item))
    } yield item -> (value,prefs(person2)(item))

    val n = si.size

    val sigmaX = si.foldLeft(0.0){case (a,(k,(x,y))) => a+x}
    val sigmaY = si.foldLeft(0.0){case (a,(k,(x,y))) => a+y}

    val sigmaXSq = si.foldLeft(0.0){case (a,(k,(x,y))) => a+Math.pow(x,2)}
    val sigmaYSq = si.foldLeft(0.0){case (a,(k,(x,y))) => a+Math.pow(y,2)}

    val sigmaXY = si.foldLeft(0.0){case (a,(k,(x,y))) => a+(x*y)}

    val num = sigmaXY-(sigmaX*sigmaY/n)
    val den = Math.sqrt((sigmaXSq-Math.pow(sigmaX,2)/n) * (sigmaYSq-Math.pow(sigmaY,2)/n))

    if (den==0)
      0
    else
      num/den
  }

  def topMatches(prefs:Preferences,person1:String,n:Int=5,similarity: Similarity=Recommendations.simPearson):List[(String,Double)] = {
    val scores = (for {
      (other,movies) <- prefs
      if(other != person1)
    } yield (other,similarity(prefs,person1,other))) toList

    val sorted = scores.sortBy{case (p,score) => score}
    sorted.reverse.take(n)
  }
}
