sealed trait peg{
  val colour: Char
  val position: Int
}

case class blue(position: Int) extends peg{
  val colour = 'B'
  val this.position = position}
case class green(position: Int) extends peg{
  val colour = 'G'
  val this.position = position}
case class orange(position: Int)  extends peg{
  val colour = 'O'
  val this.position = position}
case class purple(position: Int) extends peg{
  val colour = 'P'
  val this.position = position}
case class red(position: Int) extends peg{
  val colour = 'R'
  val this.position = position}
case class yellow(position: Int) extends peg{
  val colour = 'Y'
  val this.position = position}


object getCode{
  val NUMBER_OF_COLOURS = 6
  //pass params for range, rand vals - add to interface
  def populate(numOfPegs:Int=4):StringBuilder= {
    //this one

    val secretCode = new StringBuilder

    val range = 1 to numOfPegs

    for (num <- range) {
      //this one
      val rand = (((Math.random()) * NUMBER_OF_COLOURS )+1).toInt

      rand match {
        case 1 => secretCode += (blue(num)).colour
        case 2 => secretCode += (green(num)).colour
        case 3 => secretCode += (orange(num)).colour
        case 4 => secretCode += (purple(num)).colour
        case 5 => secretCode += (red(num)).colour
        case 6 => secretCode += (yellow(num)).colour
      }
    }
    return secretCode
  }
}


class GameBoard{

  val gameSize = getGameSize()
  val easy = getEasiness()
  val mycode = newGame()

  def getGameSize():Int = {

    val DEFAULTSIZE = 4

    val userInput = scala.io.StdIn.readLine("How many pegs would you like to guess? [default "+DEFAULTSIZE+"] ")

    if(userInput != null) {

      try {
        val numberPegs = userInput.toInt
        return numberPegs

      } catch {case e: NumberFormatException => println("Try an integer")}
    }
    return DEFAULTSIZE
  }

  def getEasiness():Boolean={
    val userInput = scala.io.StdIn.readLine("Do you want to make this easy? ")

    if((userInput == "y") || (userInput == "yes")){
      return true
    }
    else return false
  }

  def newGame():StringBuilder={

    return getCode.populate(gameSize)
  }

  def getGuess(): String={
    if(easy){println(mycode)}
    val userInput = (scala.io.StdIn.readLine("What do you guess? ")).toUpperCase

    return userInput
  }

  def play(): Unit ={

    var gotit = false
    val gos = 0

    while(gotit==false){

      gotit=takeTurn(getGuess())

      if(gos==12){
        println("Out of goes")
        gotit==true
      }
    }
  }

  def compareChar(char1: Char, char2: Char):Boolean={

    if (char1 == char2){
      true
    }else{
     false
    }
  }


  def takeTurn(turn: String): Boolean={

    var count = 0
    var letterNumber = 0
      for(letter <- turn) {
        printDetails(letter, mycode, easy, letterNumber)

        if (compareChar(letter, mycode.charAt(letterNumber))) {count += 1}
        letterNumber += 1
    }
    println()
    printing(count, gameSize)
    if (count == gameSize){return true}else {return false}
  }

  def printDetails(letter: Char, myCode: StringBuilder,
                   easy:Boolean, letterNumber: Int):Unit={
    if(easy==true){
      println("LETTER: " + letter)
      println("CHECKSTRING: " + mycode.charAt(letterNumber))
    }

  }

}

object printing{
  def apply(black: Int, gameSize: Int): Unit ={
    for(black <- 0 until black){print("BLACK ")}
    for(white <- 0 until (gameSize-black)){print("WHITE ")}
    println("\n---------------------")
    if(black == gameSize){println("Well done!")}

  }

}
//main
object Main extends App {
//interface
  val getNewGame = new GameBoard
  getNewGame.play()

}







