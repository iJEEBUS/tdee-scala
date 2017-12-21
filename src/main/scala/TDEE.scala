object TDEE {

  /** Type of metrics the user wants to input (false == metric/true == imperial) */
  var metric: Boolean = _

  /** Gender of the user (False == Male/True == Female) */
  var gender: Boolean = _

  /** Age of the user */
  var age: Int = _

  /** Height of the user */
  var height: Double = _

  /** Weight of the user */
  var weight: Double = _


  /**
    * Helper method used to prompt the user for their preferred metrics
    *
    * @return the metrics the user wishes to use
    */
  def askUserMetrics(): Boolean = {

    scala.io.StdIn.readLine("Metric/Imperial: ")
    false // FIX THIS
  }

  /**
    * Helper function that permutes a string and returns a list of said
    * permutations.
    *
    * @param text - the string to be permuted
    * @return a list of the all the permutations of the string
    */
  def permute(text: String): List[String] = text.permutations.toList

  /**
    * Helper method used to prompt the user for their age
    *
    * @return the gender of the user (false == male/true == female)
    */
  def askUserGender(): Boolean = {

    // List of permuted word 'male' to account for spelling errors
    val male_list: List[String] = permute("male") :+ "m"

    // List of permuted word 'female' to account for spelling errors
    val female_list: List[String] = permute("female") :+ "f"

    val user_input: String = scala.io.StdIn.readLine("Gender: ")

    // Checks if the input is valid
    // Continues to prompt the user until input is valid
    if (male_list.contains(user_input.toLowerCase))
      false // male
    else if (female_list.contains(user_input.toLowerCase))
      true // female
    else
      askUserGender
  }

  /**
    * Helper method used to prompt the user for their age
    *
    * @return the age of the user
    */
  def askUserAge(): Int = {
    try {
      scala.io.StdIn.readLine("Age: ").toInt
    } catch {
      case e: Exception => 0
    }

  }

  /**
    * Helper method used to prompt the user for their height
    *
    * @return the height of the user
    */
  def askUserHeight(): Double = {
    try {
      scala.io.StdIn.readLine("Height: ").toDouble
    } catch {
      case e: Exception => 0
    }
  }

  /**
    * Helper method used to prompt the user for their weight
    *
    * @return the weight of the user
    */
  def askUserWeight(): Double = {
    try {
      scala.io.StdIn.readLine("Weight: ").toDouble
    } catch {
      case e: Exception => 0
    }
  }

  /**
    * Calls proper methods to collect the user data
    */
  def collectData(): Unit = {
    this.metric = askUserMetrics
    this.gender = askUserGender
    this.age = askUserAge
    this.height = askUserHeight
    this.weight = askUserWeight
  }

  /**
    * Program begins here
    */
  def main(args: Array[String]): Unit = {
    collectData
  }

}
