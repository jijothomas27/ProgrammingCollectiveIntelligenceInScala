import java.nio.charset.CodingErrorAction

import scala.io.{Codec, Source}

val dataPath = "/Users/jijo.thomas/IdeaProjects/scala/ProgrammingCollectiveIntelligenceInScala/data/"
implicit val codec = Codec("UTF-8")
codec.onMalformedInput(CodingErrorAction.REPLACE)

val lines = Source.fromFile(dataPath+"u.item").getLines()


val mm = (for {
  line <- lines
  attrs = line.split('|')

} yield (attrs(0).toInt -> attrs(1))).toMap

