object TDEE {

  /**
    * Helper method used to prompt the user for their preferred metrics
    */
  def askUserMetrics(): Unit = {
    val metrics = scala.io.StdIn.readLine("Metric/Imperial: ")
  }

  /**
    * Helper method used to prompt the user for their age
    */
  def askUserGender(): Unit = {
    val checkList: List[String] = List("male", "m", "ma", "mal", "man", "boy")
    val gender = scala.io.StdIn.readLine("Gender: ")
    if(checkList.contains(gender.toLowerCase)){
      val genderBool = false
    } else {
      val genderBool = true
    }
  }

  /**
    * Helper method used to prompt the user for their age
    */
  def askUserAge(): Unit = {
    val age = scala.io.StdIn.readLine("Age: ")
  }

  /**
    * Helper method used to prompt the user for their height
    */
  def askUserHeight(): Unit = {
    val height = scala.io.StdIn.readLine("Height: ")
  }

  /**
    * Helper method used to prompt the user for their weight
    */
  def askUserWeight(): Unit = {
    val weight = scala.io.StdIn.readLine("Weight: ")
  }

  /**
    * Calls proper methods to collect the user data
    */
  def collectData(): Unit = {
    val gender = askUserGender
    val age = askUserAge
    val height = askUserHeight
    val weight = askUserWeight
  }

  /**
    * Program begins here
    */
  def main(args: Array[String]): Unit = {
    collectData
  }

}
