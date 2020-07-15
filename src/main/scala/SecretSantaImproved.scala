import scala.annotation.tailrec
import scala.io.StdIn._
import scala.util.Random

object SecretSantaImproved extends App {

  // This solution will stop at size of the list
  // because each pair is assigned one by one methodically

  def createListOfNames(): List[String] = {
    println("How many people are playing Secret Santa?")
    val numOfPpl: Int = readInt()

    if (numOfPpl > 0) List.fill(numOfPpl)(getNames) else throw new IllegalArgumentException
  }

  @tailrec
  def getNames: String = {
    val name = readLine("Add a name to the Secret Santa list\n")

    if (name.isEmpty) {
      println("Please enter a name")
      getNames
    } else name
  }

  def randomSelect(list: List[String]): String = list(Random.nextInt(list.size))

  def pairNames(names: List[String]): Map[String, String] = {
    @tailrec
    def pair(gifters: List[String], giftees: List[String], pairs: Map[String, String]): Map[String, String] = {
      gifters match {
        case Nil => pairs
        case gifter :: Nil => pairs + (gifter -> giftees.head)
        case gifter :: remainGifters =>
          val giftee = randomSelect(giftees.filter(_ != gifter))
          pair(remainGifters, giftees.filter(_ != giftee), pairs + (gifter -> giftee))
      }
    }

    pair(names, names, Map())
  }

  def showResult(giftPairs: Map[String, String]): Unit = giftPairs.foreach(person => println(s"${person._1} will give a gift to ${person._2}"))

  val listOfNames: List[String] = createListOfNames()
  val nameMap: Map[String, String] = pairNames(listOfNames)
  showResult(nameMap)

}
