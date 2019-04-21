import scala.io.Source
import java.io.PrintWriter
import java.text.SimpleDateFormat
import java.util.Calendar


class DB(sucursal : String){
  private var _sucursal = sucursal


  def pedidosCliente(cedula : String) : List[Insumo] = {
    var pedCliente : List[Insumo] = Nil
    var pedidos : List[Pedido] = descargarPedidos()

    for(pedido <- pedidos){
      if(pedido.getCedula == cedula){
        pedCliente = pedido.getInsumo::pedCliente
      }
    }

    return pedCliente
  }

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

  def cargarPedidos(pedidos : List[Pedido]) : Boolean = {
    val pw = new PrintWriter("DB/pedidos/" + _sucursal + ".txt")

    for(pedido <- pedidos){
      var cedula = pedido.getCedula
      if(pedido.getInsumo.isInstanceOf[Alimento]){
        var alimento : Alimento = pedido.getInsumo.asInstanceOf[Alimento]
        pw.println(cedula+":"+alimento.getCodigo+":"+alimento.getLabel+":"+alimento.getCostoProduc+":"+alimento.getCostoVenta+":"+alimento.getCostoProducAgrandado+":"+alimento.getCostoVentaAgrandado+":"+alimento.getCategoria+":"+alimento.getRestricciones+":"+alimento.getAgrandado)
      }
      else{
        var util : Util = pedido.getInsumo.asInstanceOf[Util]
        pw.println(cedula+":"+util.getCodigo+":"+util.getLabel+":"+util.getCostoProduc+":"+util.getCostoVenta)
      }
    }
    pw.close
    return true
  }


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

  def descargarPedidos() : List[Pedido] = {
    var pedidos : List[Pedido] = Nil
    var source = Source.fromFile("DB/pedidos/" + _sucursal + ".txt")
    var lines = source.getLines
    var aux : List[String] = lines.toList
    for(line <- aux){
      var parsing = line.split(":")
      if(parsing.length == 10){
        var alimento = new Alimento(parsing(1), parsing(4).toDouble, parsing(3).toDouble, parsing(2), parsing(6).toDouble, parsing(5).toDouble, parsing(7))
        alimento.setAgrandado(parsing(9))
        alimento.setRestricciones(parsing(8))
        var pedido = new Pedido(parsing(0), alimento)
        pedidos = pedido::pedidos
      }
      else{
        var util = new Util(parsing(1), parsing(4).toDouble, parsing(3).toDouble, parsing(2))
        var pedido = new Pedido(parsing(0), util)
        pedidos = pedido::pedidos
      }
    }
    source.close

    return pedidos
  }

  def descargarHistorial() : List[(String, Double, Double)] = {
    var historial : List[(String, Double, Double)] = Nil
    var source = Source.fromFile("DB/historiales/"+ _sucursal + ".txt")
    var lines = source.getLines
    while(lines.hasNext){
      val aux  = lines.next.split(":")
      historial = (aux(0), aux(1).toDouble, aux(2).toDouble)::historial
    }
    source.close

    return historial
  }

  def cargarHistorial(historiales : List[(String, Double, Double)]) : Boolean = {
    val pw = new PrintWriter("DB/historiales/" + _sucursal + ".txt")

    for(historial <- historiales){
      pw.println(historial._1+":"+historial._2.toString+":"+historial._3.toString)
    }

    pw.close()

    return true
  }

}


class Pedido(cedula : String, insumo : Insumo){
  private var _cedula : String = cedula
  private var _insumo : Insumo = insumo

  def setCedula(cedula : String) = _cedula = cedula
  def setInsumo(insumo : Insumo) = _insumo = insumo
  def getCedula = _cedula
  def getInsumo = _insumo

}


class Sucursal(nombreSucursal : String){
  private var db = new DB(nombreSucursal)
  private var _precioDomicilio : Double = 3000.0
  private var _nombreSucursal : String = nombreSucursal
  private var _caja = new Caja(nombreSucursal)
  private var _historial : List[(String, Double, Double)] = db.descargarHistorial()
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

abstract class Insumo{
  protected var _codigo : String
  protected var _costoVenta : Double
  protected var _costoProduc : Double
  protected var _label : String

  //setters y getters
  def getCodigo : String
  def getCostoVenta : Double
  def getCostoProduc : Double
  def getLabel : String
  def getCodigo(codigo : String) : Unit
  def getCostoVenta(costoVenta : Double) : Unit
  def getCostoProduc(costoProduc : Double) : Unit
  def getLabel(label : String) : Unit
}

class Alimento(codigo : String, costoVenta : Double, costoProduc : Double, label : String, costoVentaAg : Double, costoProducAg : Double, categoria : String) extends Insumo{
  private var _restricciones : String = _
  private var _categoria : String = _
  private var _costoVentaAgrandado : Double = costoVentaAg
  private var _costoProducAgrandano : Double = costoProducAg
  private var _agrandado : String = _
  protected var _codigo : String = codigo
  protected var _costoVenta : Double = costoVenta
  protected var _costoProduc : Double = costoProduc
  protected var _label : String = label

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
  def getCodigo = _codigo
  def getCostoVenta = _costoVenta
  def getCostoProduc = _costoProduc
  def getLabel = _label
  def getCodigo(codigo : String) = _codigo = codigo
  def getCostoVenta(costoVenta : Double) = _costoVenta = costoVenta
  def getCostoProduc(costoProduc : Double) = _costoProduc = costoProduc
  def getLabel(label : String) = _label = label
}

class Util(codigo : String, costoVenta : Double, costoProduc : Double, label : String) extends Insumo{
  protected var _codigo : String = codigo
  protected var _costoVenta : Double = costoVenta
  protected var _costoProduc : Double = costoProduc
  protected var _label : String = label

  def getCodigo = _codigo
  def getCostoVenta = _costoVenta
  def getCostoProduc = _costoProduc
  def getLabel = _label
  def getCodigo(codigo : String) = _codigo = codigo
  def getCostoVenta(costoVenta : Double) = _costoVenta = costoVenta
  def getCostoProduc(costoProduc : Double) = _costoProduc = costoProduc
  def getLabel(label : String) = _label = label
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


class Caja(nombreSucursal : String){
  private var db = new DB(nombreSucursal)
  private var _nombreSucursal : String = nombreSucursal
  private var _catalogo : List[Alimento] = db.descargarAlimento()
  private var _pedidos : List[Pedido] = db.descargarPedidos()
  private var _fechaActual = new SimpleDateFormat("d-M-y")

  def setCatalogo(catalogo : List[Alimento]) = _catalogo = catalogo
  def setPedidos(pedidos : List[Pedido]) = _pedidos = pedidos
  def getCatalogo = _catalogo
  def getPedidos = _pedidos

  def agregarPedido(insumo : Insumo, cedula : String) : Boolean = {
    var pedido = new Pedido(cedula, insumo)
    _pedidos = pedido::_pedidos
    if(db.cargarPedidos(_pedidos)) return true
    else return false
  }

  def quitarPedido(insumo : Insumo, cedula : String) : Boolean = {
    var pedido = new Pedido(cedula, insumo)
    val index = _pedidos.indexOf(pedido)
    if(index == -1) return false

    _pedidos = _pedidos.filter(_== pedido)
    db.cargarPedidos(_pedidos)
  }

  def calcularPrecioVentaYProduccion(insumos : List[Insumo]) : (Double, Double) = {
    var precioVenta : Double = 0
    var precioProduccion : Double = 0
    for(insumo <- insumos){
      if(insumo.isInstanceOf[Alimento]){
        var alimento : Alimento = insumo.asInstanceOf[Alimento]
        if(alimento.getAgrandado == "si"){
          precioVenta += alimento.getCostoVentaAgrandado
          precioProduccion += alimento.getCostoProducAgrandado
        }
        else{
          precioVenta += alimento.getCostoVenta
          precioProduccion += alimento.getCostoProduc
        }
      }
      else{
        var util : Util = insumo.asInstanceOf[Util]
        precioVenta += util.getCostoVenta
        precioProduccion += util.getCostoProduc
      }
    }

    return (precioProduccion, precioVenta)
  }

  def vender(cedula : String, dinero : Double) : Boolean = {
    var pedidos : List[Insumo] = db.pedidosCliente(cedula)
    var precioVentaYCosto : (Double, Double) = calcularPrecioVentaYProduccion(pedidos)
    var precioVenta : Double = precioVentaYCosto._1
    var precioProduccion : Double = precioVentaYCosto._2

    if(precioVenta <= dinero){
      for(pedido <- pedidos){
        quitarPedido(pedido, cedula)
      }

      var historiales : List[(String, Double, Double)] = db.descargarHistorial()
      var found : Boolean = false
      var idx : Int = 0

      for(historial <- historiales){
        if(historial._1 == _fechaActual.format(Calendar.getInstance().getTime()).toString) {
          found = true
          var newValue = (historial._1, historial._2+precioVenta, historial._3+precioProduccion)
          historiales = historiales.patch(idx, List(newValue), 0)
          db.cargarHistorial(historiales)
        }
        idx += 1
      }

      if(!found){
        historiales = (_fechaActual.format(Calendar.getInstance().getTime()).toString, precioVenta, precioProduccion)::historiales
        db.cargarHistorial(historiales)
      }


      return true
    }
    else return false
  }


}



object Pruebas{
  def main(args : Array[String]){
    var rest = new Sucursal("Ingenio")
    println(rest.obtenerUtilerias("4/2/16"))
    var insumos : List[Insumo] = rest.getInsumos
    for(i <- insumos){
      println(i.getCodigo)
    }

/*
    val format = new SimpleDateFormat("d-M-y")
    println(format.format(Calendar.getInstance().getTime()).toString)
*/
  }
}
