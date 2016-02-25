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

  def populate():StringBuilder= {

    val range = Vector(1,2,3,4)
    val secretCode = new StringBuilder

    for (num <- range) {
      val rand = (((Math.random()) * 6)+1).toInt

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

  val mycode = newGame()

  def newGame():StringBuilder={

    return getCode.populate()

  }

  def getGuess(): String={
    val userInput = scala.io.StdIn.readLine("What do you guess? ")
    return userInput
  }

  def play(): Unit ={

    var gotit = false
    val gos = 0

    while(gotit==false){
      println(mycode)

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
    val checkString = mycode.clone()
    var count = 0
    var letterNumber = 0
      for(letter <- turn) {
        println("LETTER: " + letter)
        println("CHECKSTRING: " + checkString.charAt(letterNumber))
        if (compareChar(letter, mycode.charAt(letterNumber))) {
          println("match")
          count += 1
      }
        letterNumber += 1
    }
    printing(count)
    if (count == 4){return true}else {return false}
  }
}

object printing{
  def apply(black: Int): Unit ={
    for(black <- 0 until black){print("BLACK ")}
    for(white <- 0 until (4-black)){print("WHITE ")}
    println("\n---------------------")
    if(black == 4){println("Well done!")}

  }

}

object tryThis extends App {

  val doit = new GameBoard
  doit.play()

}







