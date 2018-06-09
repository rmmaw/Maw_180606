import org.apache.spark._
import org.apache.spark.SparkContext._
import org.apache.log4j._
import java.io._
import sys.process._
import java.net.URL

/** Count up total number of words that occur in a file. Assuming that the python file
 *  found in the same folder is ran to create the Input.txt file. Also assuming count 
 *  words means show each word that occurs, and the number of times it appears in the 
 *  given webpage. */
object WordCount {
 
  def main(args: Array[String]) {
    
     // Create a SparkContext using the local machine
    val sc = new SparkContext("local[*]", "wordCounter") 
         
    // Load the file
    val input = sc.textFile("../Input.txt")
    
    // Split on spaces
    val words = input.flatMap(x => x.split(" "))
    
    // Count number of occurrences of each word
    val wordCounts = words.map(x => (x, 1)).reduceByKey(_+_) 
    
    // Specify output file, and write the output
    val file = "../Output.txt"
    
    val x = sc.parallelize(wordCounts.toString())
    
    val bw = new BufferedWriter(new FileWriter(file))
    for (y <- wordCounts.collect()) {
      bw.write(y + "\n")
                  }
    bw.close()    
  }
}