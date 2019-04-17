import scala.io.Source
import java.io.PrintWriter

class Sucursal(sede : String){ //...insumos : List[Insumo]){
  private var _precioDomicilio : Double = 3000.0
  private var _sede : String = sede
  //private var _insumos: List[Insumo] = _


  private var _historial : List[(String, Double, Double)] = Nil
  val source = Source.fromFile("DB/historiales/"+ _sede + ".txt")
  val lines = source.getLines
  while(lines.hasNext){
    val aux  = lines.next.split(":")
    _historial = (aux(0), aux(1).toDouble, aux(2).toDouble)::_historial
  }
  source.close


  //Setters y getters
  def getPrecioDomicilio() : Double = _precioDomicilio
  def setPrecioDomicilio(precio : Double) : Unit = _precioDomicilio = precio
  def getHistorial() : List[(String, Double, Double)] = _historial
  def setHistorial(hist : List[(String, Double, Double)]) : Unit = _historial = hist
  def getSede() : String = _sede
  //def setInsumos(ins : List[Insumo]) : Unit = _insumos = ins
  //def getInsumos() : List[Insumo] = _insumos

  //Metodos
  def obtenerUtilerias(fecha : String) : (Boolean, Double) = {
    var ok : Boolean = false
    var utilerias : Double = 0
    val it = Iterator(_historial)

    for(registro <- _historial){
      if(registro._1 == fecha){
        ok = true
        utilerias = registro._2-registro._3
      }
    }

    return (ok, utilerias)
  }
}


object interfaz{
  def main(args : Array[String]){
    var rest = new Sucursal("Ingenio")
    println(rest.obtenerUtilerias("4/2/16"))
  }
}
