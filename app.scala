import scala.io.Source
import java.io.PrintWriter


trait Sucursal{
  private var _precioDomicilio : Double = _
  private var _historial : List[(String, Double, Double)] = _  //(Fecha, Ganancias, Gastos)
  private var _sede : String = _
  //private var _insumos: List[Insumo] = _


  //Setters y getters
  def getPrecioDomicilio() : Double
  def setPrecioDomicilio(precio : Double) : Unit
  def getHistorial() : List[(String, Double, Double)]
  def setHistorial(hist : List[(String, Double, Double)]) : Unit
  //def setInsumos(ins : List[Insumo]) : Unit
  //def getInsumos() : List[Insumo]
  def getSede() : String

  //metodos
  def obtenerUtilerias(fecha : String) : (Boolean, Double)
}

class sucursalIngenio extends Sucursal{
  private var _precioDomicilio : Double = 3000.0
  private var _historial : List[(String, Double, Double)] = _
  private var _sede : String = "Ingenio"
  //private var _insumos: List[Insumo] = _


  //Setters y getters
  def getPrecioDomicilio() : Double = _precioDomicilio
  def setPrecioDomicilio(precio : Double) : Unit = _precioDomicilio = precio
  def getHistorial() : List[(String, Double, Double)] = _historial
  def setHistorial(hist : List[(String, Double, Double)]) : Unit = _historial = hist
  def getSede() : String = _sede
  //def setInsumos(ins : List[Insumo]) : Unit = _insumos = ins
  //def getInsumos() : List[Insumo] = _insumos

  def obtenerUtilerias(fecha : String) : (Boolean, Double) = {
    val source = Source.fromFile("DB/historiales/"+ _sede + ".txt")
    val lines = source.getLines
    var ok : Boolean = false
    var utilerias : Double = 0

    while(lines.hasNext){
      val aux  = lines.next.split(":")
      val line : (String, Double, Double) = (aux(0), aux(1).toDouble, aux(2).toDouble)
      if(line._1 == fecha){
        ok = true
        utilerias = line._2 - line._3
      }
    }
    source.close

    return (ok, utilerias)
  }
}


object interfaz{
  def main(args : Array[String]){
  }
}
/*
var hist : List[(String, Double, Double)] = Nil
val source = Source.fromFile("DB/historiales/Ingenio.txt")
val lines = source.getLines
println("wvgw")

while(lines.hasNext){
var aux  = lines.next.split(":")
hist = (aux(0), aux(1).toDouble, aux(2).toDouble)::hist
}
source.close

var rest = new sucursalIngenio
rest.setHistorial(hist)
println(rest.obtenerUtilerias("4/2/16")._2)*/
/*
val source = Source.fromFile("DB/historialGanancias.txt")
val files = source.getLines

while(files.hasNext) println(files.next)
source.close

val pw = new PrintWriter("DB/historialGanancias.txt")
pw.println("10/3/15:1000432")
pw.close*/
