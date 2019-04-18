import scala.io.Source
import java.io.PrintWriter

class Sucursal(sede : String){
  private var _precioDomicilio : Double = 3000.0
  private var _sede : String = sede


  //Se extrae el historial de la base de datos dependiendo de la sede
  private var _historial : List[(String, Double, Double)] = Nil
  var source = Source.fromFile("DB/historiales/"+ _sede + ".txt")
  var lines = source.getLines
  while(lines.hasNext){
    val aux  = lines.next.split(":")
    _historial = (aux(0), aux(1).toDouble, aux(2).toDouble)::_historial
  }
  source.close


  //Se extraen los insumos de la base de datos y se guarda en la lista de insumos
  private var _insumos: List[Insumo] = Nil
  source = Source.fromFile("DB/insumos.txt")
  lines = source.getLines
  var aux : List[String] = lines.toList
  for(line <- aux){
    var parsing = line.split(":")
    if(parsing.length == 6){
      var alimento = new Alimento(parsing(0), parsing(3).toDouble, parsing(2).toDouble, parsing(1), parsing(5).toDouble, parsing(4).toDouble)
      _insumos = alimento::_insumos
    }
    else{
      var util = new Util(parsing(0), parsing(3).toDouble, parsing(2).toDouble, parsing(1))
      _insumos = util::_insumos
    }
  }



  //Setters y getters
  def getPrecioDomicilio() : Double = _precioDomicilio
  def setPrecioDomicilio(precio : Double) : Unit = _precioDomicilio = precio
  def getHistorial() : List[(String, Double, Double)] = _historial
  def setHistorial(hist : List[(String, Double, Double)]) : Unit = _historial = hist
  def getSede() : String = _sede
  def setInsumos(ins : List[Insumo]) : Unit = _insumos = ins
  def getInsumos() : List[Insumo] = _insumos

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

abstract class Insumo(codigo : String, costoVenta : Double, costoProduc : Double, label : String){
  protected var _codigo : String = codigo
  protected var _costoVenta : Double = costoVenta
  protected var _costoProduc : Double = costoProduc
  protected var _label : String = label

  //setters y getters
  def getCodigo = _codigo
  def getCostoVenta = _costoVenta
  def getCostoProduc = _costoProduc
  def getLabel = _label

  def getCodigo(codigo : String) = _codigo = codigo
  def getCostoVenta(costoVenta : Double) = _costoVenta = costoVenta
  def getCostoProduc(costoProduc : Double) = _costoProduc = costoProduc
  def getLabel(label : String) = _label = label
}

class Alimento(codigo : String, costoVenta : Double, costoProduc : Double, label : String, costoVentaAg : Double, costoProducAg : Double) extends Insumo(codigo, costoVenta, costoProduc, label){
  private var _restricciones : String = _
  private var _costoVentaAgrandado : Double = costoVentaAg
  private var _costoProducAgrandano : Double = costoProducAg

  def setRestricciones(restricciones : String) = _restricciones = restricciones
  def getRestricciones = _restricciones
  def setCostoVentaAgrandado(costo : Double) = _costoVentaAgrandado = costo
  def getCostoVentaAgrandado = _costoVentaAgrandado
  def setCostoProducAgrandado(costo : Double) = _costoProducAgrandano = costo
  def getCostoProducAgrandado = _costoVentaAgrandado
}

class Util(codigo : String, costoVenta : Double, costoProduc : Double, label : String) extends Insumo(codigo, costoVenta, costoProduc, label){

}


object Pruebas{
  def main(args : Array[String]){
    var rest = new Sucursal("Ingenio")
    println(rest.obtenerUtilerias("4/2/16"))
    var insumos : List[Insumo] = rest.getInsumos
    for(i <- insumos){
      println(i.getCodigo)
    }
  }
}
