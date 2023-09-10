package plugin

import exception.{EmptyTextException, InvalidCharactersLimit, SplitException, WordLengthGreaterThanLimit}

object Splitter {

  def split(text: String, perLine: Int): Either[SplitException, String] = for {
    _      <- Either.cond(text.nonEmpty, (), EmptyTextException())
    _      <- Either.cond(perLine > 0, (), InvalidCharactersLimit())
    words  <- Right(text.split(' '))
    result <- Either.cond(!words.exists(_.length > perLine), split(words.toList, perLine), WordLengthGreaterThanLimit())
  } yield result

  private def split(words: List[String], perLine: Int): String = {
    val result = new StringBuilder("")
    val currentLine = new StringBuilder("")

    words.foreach { word =>
      val withNextWord = currentLine.toString match {
        case "" => currentLine + word
        case _  => currentLine + " " + word
      }

      if (withNextWord.length > perLine) {
        result.append(currentLine + "\n")
        currentLine.clear()
      }

      currentLine.toString match {
        case "" => currentLine.append(word)
        case _  => currentLine.append(" " + word)
      }
    }

    result + currentLine.toString
  }

}
