class StepsAndFigureValue(var steps: Int, var number: Int) {
  this.steps = steps
  this.number = number
}

class practical1
(
  private var steps: Int = 0,
  private var arr: Array[Int] = Array()
) {
  def maxInArray(arrayO: Array[StepsAndFigureValue] = Array()): StepsAndFigureValue = {
    var m = 0
    var index = 0
    arrayO.foreach(a => {
      if (a.steps >= m) {
        m = a.steps
        index = a.number
      }
    })
    return new StepsAndFigureValue(m, index)
  }

  def findBiggest(num: Int) {
    var arrNums = Array.empty[StepsAndFigureValue]

    for (i <- 1 to num) {
      var index = i
      if (index % 2 != 0) {
        arrNums = arrNums :+ new StepsAndFigureValue(this.longCollapse(index), index)
      }
    }
    val max = maxInArray(arrNums)
    println(arrNums.length)
    out(max.steps, max.number)
  }

  def longCollapse(num: Int): Int = {
    this.steps += 1
    if (num == 1) {
      val out = steps;
      steps = 0
      return out
    }
    if (num % 2 == 0) return this.longCollapse(num / 2)
    else return this.longCollapse(3 * num + 1)
  }

  def out(steps: Int, number: Int) {
    printf("число %s за %s шагов", number, steps)
  }
}

object M extends App {
  var P = new practical1
  P.findBiggest(100000)
}











