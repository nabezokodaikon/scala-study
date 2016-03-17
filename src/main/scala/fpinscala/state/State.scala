package fpinscala.state

trait RNG {

  def nextInt: (Int, RNG)
}
