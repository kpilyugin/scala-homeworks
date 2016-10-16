import scala.io.Source._

/*
Прочитайте содержимое данного файла.
В случае неудачи верните сообщение соответствующего исключения.
 */

val pathToFile = "/Users/kirill/Documents/study/scala/src/main/scala/HomeTask.sc"

def readThisWorksheet(): String = {
  try {
    fromFile(pathToFile).getLines().mkString("\n")
  } catch {
    case ex: Exception => ex.getMessage
  }
}

println(readThisWorksheet())