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
      
      ArgumentsAndActions.determineActions()
      
      FileUtils.createTargetDirectoryIfRequired(ArgumentsAndActions.source, ArgumentsAndActions.target)
      val listFiles = Lister.getSourceTargetFilePairs()      
      FileUtils.move(listFiles)
      FileUtils.deleteSourceDirIfRequired()
    } else if (!BansheeWorker.bansheeDBFile.exists) { 
    	println("Banshee DB file does not exist. Expected to find it in " + BansheeWorker.bansheeDBFile.getCanonicalPath())
  	} else {
      println("Does not validate")
    }
  }

}

/**
 * This object tracks information about which optional actions need to be performed.  
 */
object ArgumentsAndActions {  
  var deleteSourceDir = false
  var source : File = null
  var target : File = null
  
  def determineActions() = {
    
    if (source.isDirectory()) {
      deleteSourceDir = true
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
  
  def getSourceTargetFilePairs() = {

    val sources = getSources(ArgumentsAndActions.source)
    sourcesAndTargets(sources, ArgumentsAndActions.target)
  }

  protected def getSources(source: File) = {
    
    if (!source.isDirectory()) {
      List(source)
    } else {      
      source.listFiles().toList;
    }
  }

  protected def sourcesAndTargets(sources: List[File], target: File) = {    
    for (sourceFile <- sources) yield Pair(sourceFile, getTarget(sourceFile, target))
  }

  protected def getTarget(sourceFile: File, targetFile: File) = {
    
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
                 
      ArgumentsAndActions.source = new File(args(0))
      ArgumentsAndActions.target = new File(args(1))

      if (!(ArgumentsAndActions.source.exists())) {

        println("Source does not exist")
        false

      } else if (ArgumentsAndActions.target.exists() && !ArgumentsAndActions.target.isDirectory()) {

        println("Target file exists")
        false

      } else if (!ArgumentsAndActions.source.isDirectory() && !ArgumentsAndActions.target.exists() && !ArgumentsAndActions.target.getAbsoluteFile().getParentFile().exists()) {

        println("The directory for the file does not exist")
        false

      } else if (ArgumentsAndActions.source.isDirectory() && hasDirectories(ArgumentsAndActions.source)) {

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

  
  def deleteSourceDirIfRequired() {
    
    if (ArgumentsAndActions.deleteSourceDir) {
    	ArgumentsAndActions.source.delete()
    }
  }
    
  protected def getJavaURLEncodedURI(file : File) : String = {
    
	  val name = file.getName()
	  val parent = file.getParentFile()
	  if (parent != null) {
		  getJavaURLEncodedURI(parent) + "/" +  URLEncoder.encode(name, "utf-8")
	  } else {
	    "file://"
	  }
  }
 
  /**
   * Banshee uses glib functions to encode URIs. These are a bit different from what 
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
  protected val allowed = "!'()*-._~/&=:@+$,"
  
  protected def unencodeAllowedURICharacters(encodedString : String) = {
    
    var unencodedString = encodedString   
    for (c <- allowed) {
      var dec = URLEncoder.encode(c.toString(), "utf-8")
      unencodedString = unencodedString.replace(dec, c.toString())
    }
    
    unencodedString.replace("+", "%20")
  } 
  
  def createTargetDirectoryIfRequired(sourceFile : File, targetFile : File) {
    
//      val sourceFile = new File(args(0))
//      val targetFile = new File(args(1))
      
      if (sourceFile.isDirectory() && !targetFile.exists()) {
        targetFile.mkdir()
      }

  }
  
  def move(mapSourceTarget : List[Pair[File, File]]) {
      
      for (sourceTargetPair <- mapSourceTarget) {
        
        print(sourceTargetPair._1.getAbsoluteFile)
        print(" -> ")
        println(sourceTargetPair._2.getAbsolutePath)
                      
        val sourceURI = getBansheeLikeURI(sourceTargetPair._1)
        val targetURI = getBansheeLikeURI(sourceTargetPair._2)
          
        SQLWorker.showFromDB(sourceURI)        
        Files.copy(sourceTargetPair._1, sourceTargetPair._2)        
        SQLWorker.updateLocation(sourceURI, targetURI)
        sourceTargetPair._1.delete()
      }
        

  }
  
}
