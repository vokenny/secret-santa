import scala.annotation.tailrec
import scala.language.postfixOps
import scala.io.StdIn._
import scala.util.Random

object SecretSanta extends App {

  // This solution keeps reshuffling until the final list generates all valid pairs.
  // Terrible for large list sizes, but fine for small

  def createListOfNames(): List[String] = {
    println("How many people are playing Secret Santa?")
    val numOfPpl: Int = readInt()

    List.fill(numOfPpl)(getNames)
  }

  def getNames: String = readLine("Add a name to the Secret Santa list\n")

  def pairNamesRandomly(names: List[String]): Map[String, String] = {
    @tailrec
    def generatePairingUntilValid(): Map[String, String] = {
      val giftReceivers: List[String] = Random.shuffle(names)
      val preResult: Map[String, String] = names zip giftReceivers toMap

      //If someone is giving a gift to themselves, reshuffle
      if (names.exists(n => preResult(n) == n)) generatePairingUntilValid()
      else preResult
    }

    generatePairingUntilValid()
  }

  def showResult(giftPairs: Map[String, String]): Unit = giftPairs.foreach(p => println(s"${p._1} will give a gift to ${p._2}"))

  val listOfNames: List[String] = createListOfNames()
  val nameMap: Map[String, String] = pairNamesRandomly(listOfNames)
  showResult(nameMap)

}
