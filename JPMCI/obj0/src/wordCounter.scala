import org.apache.spark._
import org.apache.spark.SparkContext._
import org.apache.log4j._
import java.io._
import sys.process._
import java.net.URL

/** Count up total number word occurs in a file */
object WordCount {
 
  /** Our main function where the action happens */
  def main(args: Array[String]) {
    
     // Create a SparkContext using the local machine
    val sc = new SparkContext("local[*]", "wordCounter") //with Serializable
         
    // Load each line of my book into an RDD
    val input = sc.textFile("../Input.txt")
    
    // Split using a regular expression that extracts words
    val words = input.flatMap(x => x.split(" "))
    
    // Count number of occurrences of each word
    val wordCounts = words.map(x => (x, 1)).reduceByKey(_+_) 
    val file = "../Output.txt"
    
    val x = sc.parallelize(wordCounts.toString())
    
    // Write the output
   //val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
    val bw = new BufferedWriter(new FileWriter(file))
    for (y <- wordCounts.collect()) {
      bw.write(y + "\n")
                  }
    bw.close()    
  }
}