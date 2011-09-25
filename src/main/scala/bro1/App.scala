package bro1

import com.google.common.io.Files
import java.io.File
import java.net.URLEncoder
import java.net.URL
import java.sql.DriverManager

import java.util.Properties

/** 
 * @author Linas Jakucionis <linasj@gmail.com> 
 */
object App {

  def main(args: Array[String]) {
        
    if (ArgumentValidator.validates((args))) {
      FileUtils.create(args)
      val listFiles = Lister.getSourceTargetFilePairs(args)      
      FileUtils.move(listFiles)
    } else if (!BansheeWorker.bansheeDBFile.exists) { 
    	println("Banshee DB file does not exist. Expected to find it in " + BansheeWorker.bansheeDBFile.getCanonicalPath())
  	} else {
      println("Does not validate")
    }
  }

}

object BansheeWorker {
  def bansheeDB = {
    val userHome = System.getProperty("user.home");
    userHome + File.separator + ".config/banshee-1/banshee.db" 
  }
  
  val bansheeDBFile = new File(bansheeDB)
}


object Lister {
  
  def getSourceTargetFilePairs(args: Array[String]) = {

    val source = args(0)
    val target = args(1)

    val sources = getSources(source)
    sourcesAndTargets(sources, target)
  }

  def getSources(source: String) = {
    val sourceFile = new File(source)
    if (!sourceFile.isDirectory()) {
      List(sourceFile)
    } else {
      sourceFile.listFiles().toList;
    }
  }

  def sourcesAndTargets(sources: List[File], target: String) = {
    val targetFile = new File(target)
    for (f <- sources) yield Pair(f, getTarget(f, targetFile))
  }

  def getTarget(sourceFile: File, targetFile: File) = {
    if (targetFile.isDirectory) {

      new File(targetFile.getCanonicalPath + File.separator + sourceFile.getName);
    } else {
      targetFile.getCanonicalFile
    }
  }

}

object ArgumentValidator {

  def validates(args: Array[String]) = {

    if (args.size != 2) {
      println("Need 2 arguments")
      false
    } else {

      val sourceFile = new File(args(0))
      val targetFile = new File(args(1))

      if (!(sourceFile.exists())) {

        println("Source does not exist")
        false

      } else if (targetFile.exists() && !targetFile.isDirectory()) {

        println("Target file exists")
        false

      } else if (!sourceFile.isDirectory() && !targetFile.exists() && !targetFile.getAbsoluteFile().getParentFile().exists()) {

        println("The directory for the file does not exist")
        false

      } else if (sourceFile.isDirectory() && hasDirectories(sourceFile)) {

        println("The source directory can only contain files but it contains subdirectories as well")
        false

      } else {

        true
      }
    }

  }

  /**
   * Check if the directory has subdirectories.
   */
  private def hasDirectories(sourceFile: File) = {
    val dirs = for (f <- sourceFile.listFiles if (f.isDirectory)) yield f

    !dirs.isEmpty;
  }

}


object SQLWorker { 

  val con = SQLWorker.getConnection
  
  def getConnection = {
     Class.forName("org.sqlite.JDBC");
     DriverManager.getConnection("jdbc:sqlite:" + BansheeWorker.bansheeDBFile.getCanonicalPath);
  }
  
  def showFromDB(source : String) {
    
        val st = con.prepareStatement("select title, rating, uri from coretracks where uri = ?")
        st.setString(1, source)
        val resultSet = st.executeQuery()
        while (resultSet.next()) {
          var title = resultSet.getString("title")
          var rating = resultSet.getInt("rating")          
          println("Title: '" + title + "' and its rating: " + rating)
        }

  }
  
  def updateLocation(source : String, target : String) {
    
        val st = con.prepareStatement("update coretracks set uri = ? where uri = ?")
        st.setString(1, target)
        st.setString(2, source)
        
        val affectedCount = st.executeUpdate()
        println("Affected rows: " + affectedCount)
  }
  
  
  
}

object FileUtils {

    
  def getJavaURLEncodedURI(f:File) : String = {
    
	  val name = f.getName()
	  val parent = f.getParentFile()
	  if (parent != null) {
		  getJavaURLEncodedURI(parent) + "/" +  URLEncoder.encode(name, "utf-8")
	  } else {
	    "file://"
	  }
  }
 
  /**
   * Banshee uses glib fucntions to encode URIs. These are a bit different from what 
   * java's native URLEncoder produces so we do the following process: <ul>
   * <li>encode the URL using URL encoder
   * <li>then unencode the allowed characters
   * </ul>
   */
  def getBansheeLikeURI(file : File) = {
    unencodeAllowedURICharacters(getJavaURLEncodedURI(file))
  } 
  
  /**
   * These are the characters that are allowed in paths when encoding using the 
   * g_escape_file_uri from glib 
   * https://github.com/zsx/glib/blob/e4a9f83312a8c7e4a540c937f04efab88199e181/glib/gconvert.c   
   */
  val allowed = "!'()*-._~/&=:@+$,"
  
  def unencodeAllowedURICharacters(s : String) = {
    
    var str = s   
    for (c <- allowed) {
      var dec = URLEncoder.encode(c.toString(), "utf-8")
      str = str.replace(dec, c.toString())
    }
    
    str.replace("+", "%20")
  } 
  
  def create(args : Array[String]) {
    
      val sourceFile = new File(args(0))
      val targetFile = new File(args(1))
      
      if (sourceFile.isDirectory() && !targetFile.exists()) {
        targetFile.mkdir()
      }

  }
  
  def move(mapSourceTarget : List[Pair[File, File]]) {
      
      for (sourceTargetPair <- mapSourceTarget) {
        print(sourceTargetPair._1.getAbsoluteFile)
        print(" -> ")
        println(sourceTargetPair._2.getAbsolutePath)
                      
        val source = getBansheeLikeURI(sourceTargetPair._1)
        val target = getBansheeLikeURI(sourceTargetPair._2)

        /*
        println("source uri: " + source)
        println("target uri: " + target)
        */
          
        SQLWorker.showFromDB(source)        
        Files.copy(sourceTargetPair._1, sourceTargetPair._2)        
        SQLWorker.updateLocation(source, target)
        sourceTargetPair._1.delete()
      }
        

  }
  
}
