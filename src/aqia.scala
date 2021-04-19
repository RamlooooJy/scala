import scala.io.StdIn.readLine
import scala.util.Random

/**
 * A 2-dimensional aquarium simulation.
 */
class Aquarium(
  private val width: Int,
  private val height: Int,
) {

  // Initialize the aquarium
  val aquarium_elements = for ( i <- 1 to width * height ) yield None
  val aquarium_array = Array[ Option[ AquariumElement ] ]( aquarium_elements: _* )

  /**
   * Adds an AquariumElement to to aquarium.
   * Will replace any existing element in the same location.
   */
  def add( element: AquariumElement ) {
    aquarium_array( element.locY * width + element.locX ) = Some( element )
  }

  /**
   * Ask an AquariumElement to decide on its next move.
   * Try to move the element in the required direction.
   * Moving fails if the AquariumElement would leave the aquarium grid.
   * If the destination of the grid is already occupied the resulting
   * collision is handled by handleCollision.
   */
  def attemptMove( e: AquariumElement ) {
    if ( !e.has_moved ) {
      e.has_moved = true
      val move = e.move
      val newX = e.locX + (if ( move == 'E ) 1 else if ( move == 'W ) -1 else 0)
      val newY = e.locY + (if ( move == 'S ) 1 else if ( move == 'N ) -1 else 0)

      // Check that the target of this move is within bounds of the aquarium
      if ( newX >= 0 && newY >= 0 && newX < width && newY < height ) {
        aquarium_array( newY * width + newX ) match {
          case None => { // move to empty cell will always succeed
            aquarium_array( e.locY * width + e.locX ) = None
            aquarium_array( newY * width + newX ) = Some( e )
            e.locX = newX
            e.locY = newY
          }
          case Some( e2 ) => { // handle collision
            handleCollision( e, e2 )
          }
        }
      }
    }
  }

  /**
   * Handle the collision resulting from e1 moving into the location of
   * e2. If e1 can eat e2, e1 replaces e2 in its location. If e2 eats
   * e1, e1 just disappears. If neither AquariumElement can eat the other
   * nothing happens and e1 stays in its original location. This mechanism
   * will prevent Fish from moving into cells that are occupied by Rocks.
   */
  def handleCollision( e1: AquariumElement, e2: AquariumElement ) {
    if ( e1.eat( e2 ) ) { // e1 eats e2
      aquarium_array( e1.locY * width + e1.locX ) = None
      aquarium_array( e2.locY * width + e2.locX ) = Some( e1 )
      e1.locX = e2.locX
      e1.locY = e2.locY
    } else if ( e2.eat( e1 ) ) { // e2 eats e1
      aquarium_array( e1.locY * width + e1.locX ) = None
    }
  }

  /**
   * Run an iteration of the simulation by calling attempt move for each
   * AquariumElement in the aquarium
   */
  def update = {
    // Bug fix here: only move elements that have not been moved before
    for ( position <- aquarium_array; if (!position.isEmpty); element = position.get )
      element.has_moved = false
    for ( i <- 0 to aquarium_array.size - 1 ) aquarium_array( i ) match {
      case Some( element ) => if ( !element.has_moved ) attemptMove( element )
      case None => ()
    }
  }

  /**
   * Print the current state of the aquarium to the console.
   */
  def draw {
    for ( y <- 0 to height - 1 ) {
      for ( x <- 0 to width - 1 ) {
        val element: Option[ AquariumElement ] = aquarium_array( y * width + x )
        print( element match {
          case Some( e ) => e.symbol + "  "
          case None => ".  "
        }
        )
      }
      println
    }
  }

}

abstract class AquariumElement( var locX: Int, var locY: Int ) {
  val symbol: Char
  val edible: Boolean

  def move: Symbol

  def eat( other: AquariumElement ): Boolean

  var has_moved: Boolean = false;

  val power:Int

}

class Rock( locX: Int, locY: Int ) extends AquariumElement( locX, locY ) {
  val symbol = '#'
  def move = 'Stay
  val edible = false
  val power = 0
  def eat( other: AquariumElement ): Boolean = false
}
object Rock {
  def apply(locX: Int, locY: Int) = new Rock(locX: Int, locY: Int)
}
abstract class LifeForm( locX: Int, locY: Int ) extends AquariumElement( locX, locY ) {
  val edible = true
}

class Plant( locX: Int, locY: Int ) extends LifeForm( locX, locY ) {
  val symbol = '$'
  override val power: Int = -1

  def move = 'Stay

  def eat( other: AquariumElement ): Boolean = false
}
object Plant {
  def apply(locX: Int, locY: Int) = new Plant(locX: Int, locY: Int)
}

abstract class BaseFish( locX: Int, locY: Int ) extends LifeForm( locX, locY ) {
  def eat( other: AquariumElement ): Boolean = false
  override val power: Int = 0

  final def move ():Symbol = {
    val r = Random.between( 0, 4 )
    if ( r == 0 ) 'W
    else if ( r == 1 ) 'S
    else if ( r == 2 ) 'N
    else 'E
  }
}

class Fish private ( locX: Int, locY: Int ) extends BaseFish( locX, locY ) {
  val symbol = 'f'
}
object Fish {
  def apply(locX: Int, locY: Int) = new Fish(locX: Int, locY: Int)
}

class HungryFish private ( locX: Int, locY: Int, fishPower:Int = 0 ) extends Fish( locX, locY ) {
  override val symbol = 'F'
  override val power: Int = fishPower
  override def eat (other: AquariumElement) = {
    if(other.edible && other.power < this.power) true else false
  }
}
object HungryFish {
  def apply(locX: Int, locY: Int, fishPower:Int) = new HungryFish(locX: Int, locY: Int, fishPower:Int)
  def apply(locX: Int, locY: Int) = new HungryFish(locX: Int, locY: Int)
}

object AquariumSimulator {
  val aquaSize = 10
  val rockAmmount = 5

  def rand (size:Int = aquaSize) = {
    Random.nextInt(size)
  }
  /**
   * Run the aquarium simulation
   */
  def main( args: Array[ String ] ) {
    //     Add AquariumElements to the Aquarium singleton
    val aquarium = new Aquarium( aquaSize, aquaSize )
    for( w <- 0 to rockAmmount) {
      aquarium.add( Rock( rand(), rand() ) )
    }
    aquarium.add( Plant( rand(), rand() ) )
    aquarium.add( Fish( rand(), rand() ) )
    aquarium.add( Fish( rand(), rand() ) )
    aquarium.add( Fish( rand(), rand() ) )
    aquarium.add( Fish( rand(), rand() ) )
    aquarium.add( Fish( rand(), rand() ) )
    aquarium.add( Fish( rand(), rand() ) )
    aquarium.add( Fish( rand(), rand() ))
    aquarium.add( HungryFish( rand(), rand(),  9) )
    aquarium.add( HungryFish( rand(), rand(), 8) )
    aquarium.add( HungryFish( rand(), rand(), 7 ) )
    aquarium.add( HungryFish( rand(), rand(), 8 ) )
    // Simulate the aquarium
    var input: String = ""
    do {
      aquarium.draw
      aquarium.update
      Thread.sleep(1000L)
      print("\n\n\n\n\n\n\n")
    } while ( input != "quit" )
  }
//      //////////input = readLine
}
