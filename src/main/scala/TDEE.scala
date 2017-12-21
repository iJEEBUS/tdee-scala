object TDEE {

  /**
    * What needs to be done:
    *   1 - make askUserMetrics return a boolean based on input
    *   2 - create method to calculate BMR after finishing part one
    */

  /** Type of metrics the user wants to input (false == metric/true == imperial) */
  var metric: Boolean = _

  /** Gender of the user (false == male/true == female) */
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
    * @return the boolean value for the metrics to use (false == metric/true == imperial)
    */
  def askUserMetrics(): Boolean = {
    val metric_list: List[String] = permute("metric") :+ "m"
    val imperial_list: List[String] = permute("imperial") :+ "i"
    val user_input: String = scala.io.StdIn.readLine("Metric/Imperial: ")

    if (metric_list.contains(user_input.toLowerCase))
      false
    else if (imperial_list.contains(user_input.toLowerCase))
      true
    else
      askUserMetrics
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
    * Calculates the BMR for a male
    *
    * @return the BMR for the user
    */
  def maleBMR(): Double = {

    // if metric, calculate the BMR in proper units
    if (this.metric == false)
      66 + (13.7.*(this.weight)) + (5.*(this.height)) - (6.8.*(this.age))
    else if (this.metric == true) {
      // convert to metric then call maleBMR with new data
      this.height = convertToCM(this.height)
      this.weight = convertToKilogram(this.weight)
      this.metric = false
      maleBMR
    }
    else
      1.0
  }

  /**
    * Calculates the BMR for a female
    *
    * @return the BMR for the user
    */
  def femaleBMR(): Double = {
    // if metric, calculate the BMR in proper units
    if (this.metric == false)
      655 + (9.6.*(this.weight)) + (1.8.*(this.height)) - (4.7.*(this.age))
    else if (this.metric == true) {
      // convert to metric then call maleBMR with new data
      this.height = convertToCM(this.height)
      this.weight = convertToKilogram(this.weight)
      this.metric = false
      femaleBMR
    }
    else
      1.0
  }

  /**
    * Helper method that converts inches to centimeters
    *
    * @param num length in inches to convert
    * @return the centimeter representation of inputted number
    */
  def convertToCM(num: Double): Double = {
    num.*(2.54)
  }

  /**
    * Helper method that converts lbs to kgs
    *
    * @param num wieght in lbs to convert
    * @return the kg representation of inputted number
    */
  def convertToKilogram(num: Double): Double = {
    num.*(0.453592)
  }

  /**
    * Program begins here
    */
  def main(args: Array[String]): Unit = {
    collectData
    if (this.gender == false)
      println(maleBMR)
    else
      println(femaleBMR)
  }
}
