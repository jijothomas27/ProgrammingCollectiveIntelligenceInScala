package chapter1

import java.nio.charset.CodingErrorAction

import chapter1.Recommendations.Preferences

import scala.io.{Source, Codec}

/**
 * Created by jijo.thomas on 10/10/14.
 */
class MovieLens (dataPath:String){
  val ratingsPath = "u.data"
  val moviesPath = "u.item"

  def getData:Preferences = {
    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)

  }

  private def getMovieData: Map[Int,String] = {
    val lines = Source.fromFile(dataPath+moviesPath).getLines()
    val movieData = for {
      line <- lines
      attrs = line.split('|')
    } yield attrs(0).toInt -> attrs(1)

    movieData.toMap
  }

  
}

object MovieLens {
  def apply(path:String):String = new MovieLens(path).saySomething
}
