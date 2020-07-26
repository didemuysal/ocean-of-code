import math._
import scala.util._
import scala.io.StdIn._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// Game depends on the map
class Map(var width: Int, var height: Int, var stringMap: Array[String]) {
  val map = Array.ofDim[Int](width, height)
  var i = 0;
  var j = 0;
  // fill the map according to the compare

  for (i <- 0 until stringMap.size) {
    for (j <- 0 until stringMap(i).length) {
      var res = stringMap(i).charAt(j).compare('x')
      var result = if (res < 0) 1 else 0

      map(i)(j) = result;

    }
  }
  
  // find empty cells in order to allocate
  def getEmptyCell(): Point = {
    
    while(true){
      val r = scala.util.Random
      val point = new Point(r.nextInt(15),r.nextInt(15));
      if(isSafeToMove(point)){
        return point;
      }
    }
    var a = 0;
    var b = 0;

    for (a <- 0 until height) {
      for (b <- 0 until width) {
        var res = map(a)(b).compare(1);
        if (!(res < 0)) {
          return new Point(j, i)
        }
      }
    }
    var point = new Point(1, 1)
    return point;

  }
  // discorver the point if it's on the map and allowed to go
  def isOnTheMap(point: Point): Boolean = {
    if (point.x < 0 || point.x >= width || point.y < 0 || point.y >= height) {
      return false;
    }
    return true;

  }
  def isSafeToMove(point: Point): Boolean = {
      println("Is it safe" + point.x)
    if (point.x < 0 || point.x >= width || point.y < 0 || point.y >= height) {
      return false
    }
    return map(point.y)(point.x) == 0;
  }
  
  //set cell as visited in order to not re-discover
  def setVisitedCell(point: Point) = {
    map(point.y)(point.x) = 2;
  }

  def resetVisitedCells() = {
    var i = 0;
    var j = 0;
    for (i <- 0 until height) {
      for (j <- 0 until width) {

        if (map(i)(j) == 2) {
          map(j)(j) = 0
        } else {
          map(i)(j) = map(i)(j)
        }
      }
    }
  }
}

class Point(var x: Int, var y: Int)


// User Class
class Submarine() {

  var position: Point = new Point(0, 0);
  var life: Int = 0;
  var torpedoCooldown: Int = 0;
  var sonarCooldown: Int = 0;
  var silenceCooldown: Int = 0;
  var mineCooldown: Int = 0;
  var map: Map = null;
  
  
  def play() = {
    move();
    shoot();
  } 

  def update(
      position: Point,
      life: Int,
      torpedoCooldown: Int,
      sonarCooldown: Int,
      silenceCooldown: Int,
      mineCooldown: Int
  ) = {

    // Invoking primary constructor
    this.life = life;
    this.position = position;
    this.torpedoCooldown = torpedoCooldown;
    this.sonarCooldown = sonarCooldown;
    this.silenceCooldown = silenceCooldown;
    this.mineCooldown = mineCooldown;
    map.setVisitedCell(position)
  }

  // move submarine with respect to the 4 routes, (if allowed)
  def move() = {
    val north = new Point(position.x, (position.y) - 1)
    val south = new Point(position.x, (position.y) + 1)
    val east = new Point((position.x) + 1, (position.y))
    val west = new Point((position.x) - 1, (position.y))


    if (map.isSafeToMove(north)) {
      moveTo("N")
    } else if (map.isSafeToMove(south)) {
      moveTo("S")
    } else if (map.isSafeToMove(east)) {
      moveTo("E")
    } else if (map.isSafeToMove(west)) {
      moveTo("W")
    } else if (!(tryToSilence())) {
      surface();
    }
  }

 // shoot enemy submarine if it's fit the move safety
  def shoot() = {
    if (torpedoCooldown == 0) {
      val north = new Point(position.x, (position.y) - 4)
      val south = new Point(position.x, (position.y) + 4)
      val east = new Point((position.x) + 4, (position.y))
      val west = new Point((position.x) - 4, (position.y))

      if (map.isSafeToMove(north)) {
        shootAt(north)
      } else if (map.isSafeToMove(south)) {
        shootAt(south)
      } else if (map.isSafeToMove(east)) {
        shootAt(east)
      } else if (map.isSafeToMove(west)) {
        shootAt(west)
      }
    }
  }

// try to be safe if shooting is unsuccessfull
  def tryToSilence():Boolean = {
    if (silenceCooldown > 0) {
      return false;
    }
    var t = 2;
    for (t <- 0 until 4) {

      val north = new Point(position.x, (position.y) - t)
      val south = new Point(position.x, (position.y) + t)
      val east = new Point((position.x) + t, (position.y))
      val west = new Point((position.x) - t, (position.y))

      if (map.isSafeToMove(north)) {
        silenceTo("N",t)
        return true;
      } else if (map.isSafeToMove(south)) {
        silenceTo("S",t)
        return true;
      } else if (map.isSafeToMove(east)) {
        silenceTo("E",t)
        return true;
      } else if (map.isSafeToMove(west)) {
        silenceTo("W",t)
        return true;
      }
    }
  return false;
  }

  def shootAt(point: Point) = {
    println("| TORPEDO " + point.x + " " + point.y)
  }

  // charge torpedo, or be quiet
  def charge() = {
    if (silenceCooldown > 0) {
      println(" SILENCE ")
    } else {
      println(" TORPEDO ")
    }
  }

  

  def moveTo(direction: String) = {
    print("MOVE ", direction)
    charge()
  }
  def silenceTo(direction: String,distance: Int) = {
    print("SILENCE " + direction + " " + distance)
    charge()
  }

  def surface() = {
    println("SURFACE")
    map.resetVisitedCells();
  }

}


/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {
    val mapLines = ArrayBuffer[String]()
    var me = new Submarine();
    var enemy = new Submarine();

    val Array(width, height, myId) = (readLine split " ").map (_.toInt)
    for(i <- 0 until height) {
        val line = readLine
        mapLines += line
        
    }
    
    // generate a map with given width, height and the generated maplines above
    val map = new Map(width,height,mapLines.toArray);
    me.map = map;
    enemy.map = map;

    val initialPosition = map.getEmptyCell();
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")
    
    println("7 7")

    // game loop
    while(true) {
        val Array(x, y, myLife, oppLife, torpedoCooldown, sonarCooldown, silenceCooldown, mineCooldown) = (readLine split " ").map (_.toInt)
        val sonarResult = readLine
        val opponentOrders = readLine

        me.update(
            new Point(x,y),
            myLife,
            torpedoCooldown,
            sonarCooldown,
            silenceCooldown,
            mineCooldown
        )

        me.play()

        
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        
        println("MOVE N TORPEDO")

    }
}