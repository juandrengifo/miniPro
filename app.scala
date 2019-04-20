import scala.io.Source
import java.io.PrintWriter


class DB{
  def descargarInsumos() : List[Insumo] = {
    var insumos : List[Insumo] = Nil
    var source = Source.fromFile("DB/insumos.txt")
    var lines = source.getLines
    var aux : List[String] = lines.toList
    for(line <- aux){
      var parsing = line.split(":")
      if(parsing.length == 7){
        var alimento = new Alimento(parsing(0), parsing(3).toDouble, parsing(2).toDouble, parsing(1), parsing(5).toDouble, parsing(4).toDouble, parsing(6))
        insumos = alimento::insumos
      }
      else{
        var util = new Util(parsing(0), parsing(3).toDouble, parsing(2).toDouble, parsing(1))
        insumos = util::insumos
      }
    }
    source.close

    return insumos
  }
  /*
  def cargarPedidos(insumos : List[Insumo]) : Boolean = {
    val pw = new PrintWriter("DB/insumos.txt")

    for(insumo <- insumos){
      if(insumo.isInstanceOf(Alimento)){
        pw.println()
      }
    }
    pw.close
  }
  */

  def descargarAlimento() : List[Alimento] = {
    var insumos : List[Alimento] = Nil
    var source = Source.fromFile("DB/insumos.txt")
    var lines = source.getLines
    var aux : List[String] = lines.toList
    for(line <- aux){
      var parsing = line.split(":")
      if(parsing.length == 7){
        var alimento = new Alimento(parsing(0), parsing(3).toDouble, parsing(2).toDouble, parsing(1), parsing(5).toDouble, parsing(4).toDouble, parsing(6))
        insumos = alimento::insumos
      }
    }
    source.close

    return insumos
  }

  def descargarPedidos(nombreSucursal : String) : List[(String, Insumo)] = {
    var pedidos : List[(String, Insumo)] = Nil
    var source = Source.fromFile("DB/pedidos/" + nombreSucursal + ".txt")
    var lines = source.getLines
    var aux : List[String] = lines.toList
    for(line <- aux){
      var parsing = line.split(":")
      if(parsing.length == 10){
        var alimento = new Alimento(parsing(1), parsing(4).toDouble, parsing(3).toDouble, parsing(2), parsing(6).toDouble, parsing(5).toDouble, parsing(7))
        alimento.setAgrandado(parsing(9))
        alimento.setRestricciones(parsing(8))
        pedidos = (parsing(0), alimento)::pedidos
      }
      else{
        var util = new Util(parsing(1), parsing(4).toDouble, parsing(3).toDouble, parsing(2))
        pedidos = (parsing(0), util)::pedidos
      }
    }
    source.close

    return pedidos
  }

  def descargarHistorial(nombreSucursal : String) : List[(String, Double, Double)] = {
    var historial : List[(String, Double, Double)] = Nil
    var source = Source.fromFile("DB/historiales/"+ nombreSucursal + ".txt")
    var lines = source.getLines
    while(lines.hasNext){
      val aux  = lines.next.split(":")
      historial = (aux(0), aux(1).toDouble, aux(2).toDouble)::historial
    }
    source.close

    return historial
  }

}



class Sucursal(nombreSucursal : String){
  private var db = new DB
  private var _precioDomicilio : Double = 3000.0
  private var _nombreSucursal : String = nombreSucursal
//  private var _caja = new Caja(nombreSucursal)
  private var _historial : List[(String, Double, Double)] = db.descargarHistorial(_nombreSucursal)
  private var _insumos: List[Insumo] = db.descargarInsumos()


  //Setters y getters
  def getPrecioDomicilio() : Double = _precioDomicilio
  def setPrecioDomicilio(precio : Double) : Unit = _precioDomicilio = precio
  def getHistorial() : List[(String, Double, Double)] = _historial
  def setHistorial(hist : List[(String, Double, Double)]) : Unit = _historial = hist
  def getnombreSucursal() : String = _nombreSucursal
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

class Alimento(codigo : String, costoVenta : Double, costoProduc : Double, label : String, costoVentaAg : Double, costoProducAg : Double, categoria : String) extends Insumo(codigo, costoVenta, costoProduc, label){
  private var _restricciones : String = _
  private var _categoria : String = _
  private var _costoVentaAgrandado : Double = costoVentaAg
  private var _costoProducAgrandano : Double = costoProducAg
  private var _agrandado : String = _

  def setRestricciones(restricciones : String) = _restricciones = restricciones
  def getRestricciones = _restricciones
  def setCostoVentaAgrandado(costo : Double) = _costoVentaAgrandado = costo
  def getCostoVentaAgrandado = _costoVentaAgrandado
  def setCostoProducAgrandado(costo : Double) = _costoProducAgrandano = costo
  def getCostoProducAgrandado = _costoVentaAgrandado
  def getCategoria = _categoria
  def serCategoria(categoria : String) = _categoria = categoria
  def setAgrandado(p : String) = _agrandado = p
  def getAgrandado : String = _agrandado
}

class Util(codigo : String, costoVenta : Double, costoProduc : Double, label : String) extends Insumo(codigo, costoVenta, costoProduc, label){
}

class Usuario(usuario : String, constrasena : String){
  private var _usuario : String = usuario
  private var _contrasena : String = constrasena

  def getUsuario = _usuario
  def getContrasena = _contrasena
  def setUsuario(usuario : String) = _usuario = usuario
  def setConstrasena(contrasena: String) = _contrasena = contrasena
}

class Cliente(usuario : String, constrasena : String, edad : String, sexo : String, telefono : String, cedula : String) extends Usuario(usuario, constrasena){
  private var _edad : String = edad
  private var _sexo : String = sexo
  private var _telefono : String = telefono
  private var _cedula : String = cedula

  def setEdad(edad : String) = _edad = edad
  def setSexo(sexo : String) = _sexo = sexo
  def setTelefono(telefono : String) = _telefono = telefono
  def setCedula(cedula : String) = _cedula = cedula
  def getEdad() = _edad
  def getSexo() = _sexo
  def getTelefono() = _telefono
  def getCedula() = _cedula

}

/*
class Caja(nombreSucursal : String){
  private var _nombreSucursal : String = nombreSucursal
  private var _catalogo : List[Alimento] = descargarAlimento()
  private var _pedidos : List[(String, Insumo)] = descargarPedidos(_nombreSucursal)

  def agregarPedido(insumo : Insumo, cedula : String) : Boolean = {

  }
*/



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
