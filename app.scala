import java.io._
import scala.io.Source
import java.io.PrintWriter
import scala.sys.process._
import java.text.SimpleDateFormat
import java.util.Calendar
import java.lang.Math
import scala.math._
import java.time._

class DB(sucursal : String){
  private var _sucursal = sucursal

  def pedidosCliente(cedula: String): List[Insumo] = {
    var pedCliente : List[Insumo] = Nil
    var pedidos : List[Pedido] = descargarPedidos()

    for(pedido <- pedidos){
      if(pedido.getCedula == cedula){
        pedCliente = pedido.getInsumo::pedCliente
      }
    }

    return pedCliente
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

  def cargarHistorial(historiales : List[(String, Double, Double)]) : Boolean = {
    val pw = new PrintWriter("DB/historiales/" + _sucursal + ".txt")

    for(historial <- historiales){
      pw.println(historial._1+":"+historial._2.toString+":"+historial._3.toString)
    }

    pw.close()

    return true
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

  def cargarInsumos(insumos : List[Insumo]) : Boolean = {
    val pw = new PrintWriter("DB/insumos.txt")

    for(insumo <- insumos){
      if(insumo.isInstanceOf[Alimento]){
        var alimento : Alimento = insumo.asInstanceOf[Alimento]
        pw.println(alimento.getCodigo+":"+alimento.getLabel+":"+alimento.getCostoProduc+":"+alimento.getCostoVenta+":"+alimento.getCostoVentaAgrandado+":"+alimento.getCostoProducAgrandado)
      }
      else{
        pw.println(insumo.getCodigo+":"+insumo.getLabel+":"+insumo.getCostoProduc+":"+insumo.getCostoVenta)
      }
    }

    pw.close()

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
}



class Sucursal(nombreSucursal : String){
  private var _db = new DB(nombreSucursal)
  private var _precioDomicilio : Double = 3000.0
  private var _nombreSucursal : String = nombreSucursal
  private var _caja = new Caja(nombreSucursal)
  private var _historial : List[(String, Double, Double)] = _db.descargarHistorial()
  private var _insumos: List[Insumo] = _db.descargarInsumos()


  //Setters y getters
  def db = _db
  def caja = _caja
  def getPrecioDomicilio() : Double = _precioDomicilio
  def setPrecioDomicilio(precio : Double) : Unit = _precioDomicilio = precio
  def getnombreSucursal() : String = _nombreSucursal
  def setNombreSucursal(nombre: String) = _nombreSucursal = nombre
  def getHistorial() : List[(String, Double, Double)] = _historial
  def setHistorial(hist : List[(String, Double, Double)]) : Unit = _historial = hist
  def getInsumos() : List[Insumo] = _insumos
  def setInsumos(ins : List[Insumo]) : Unit = _insumos = ins

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

class Pedido(cedula : String, insumo : Insumo){
  private var _cedula : String = cedula
  private var _insumo : Insumo = insumo

  def setCedula(cedula : String) = _cedula = cedula
  def setInsumo(insumo : Insumo) = _insumo = insumo
  def getCedula = _cedula
  def getInsumo = _insumo

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
  def setCodigo(codigo : String) : Unit
  def setCostoVenta(costoVenta : Double) : Unit
  def setCostoProduc(costoProduc : Double) : Unit
  def setLabel(label : String) : Unit
}

class Alimento(codigo : String, costoVenta : Double, costoProduc : Double, label : String, costoVentaAg : Double, costoProducAg : Double, categoria : String) extends Insumo{
  var _codigo : String = codigo
  var _costoVenta : Double = costoVenta
  var _costoProduc : Double = costoProduc
  var _label : String = label
  private var _restricciones : String = _
  private var _categoria : String = categoria
  private var _costoVentaAgrandado : Double = costoVentaAg
  private var _costoProducAgrandado : Double = costoProducAg
  private var _agrandado : String = _

  def getCodigo = _codigo
  def getCostoVenta = _costoVenta
  def getCostoProduc = _costoProduc
  def getLabel = _label
  def getRestricciones = _restricciones
  def getCategoria = _categoria
  def getCostoVentaAgrandado = _costoVentaAgrandado
  def getCostoProducAgrandado = _costoProducAgrandado
  def getAgrandado : String = _agrandado

  def setCodigo(codigo : String) = _codigo = codigo
  def setCostoVenta(costoVenta : Double) = _costoVenta = costoVenta
  def setCostoProduc(costoProduc : Double) = _costoProduc = costoProduc
  def setLabel(label : String) = _label = label
  def setRestricciones(restricciones : String) = _restricciones = restricciones
  def setCategoria(categoria : String) = _categoria = categoria
  def setCostoVentaAgrandado(costo : Double) = _costoVentaAgrandado = costo
  def setCostoProducAgrandado(costo : Double) = _costoProducAgrandado = costo
  def setAgrandado(p : String) = _agrandado = p

}

class Util(codigo : String, costoVenta : Double, costoProduc : Double, label : String) extends Insumo{
  var _codigo : String = codigo
  var _costoVenta : Double = costoVenta
  var _costoProduc : Double = costoProduc
  var _label : String = label

  def getCodigo = _codigo
  def getCostoVenta = _costoVenta
  def getCostoProduc = _costoProduc
  def getLabel = _label
  def setCodigo(codigo : String) = _codigo = codigo
  def setCostoVenta(costoVenta : Double) = _costoVenta = costoVenta
  def setCostoProduc(costoProduc : Double) = _costoProduc = costoProduc
  def setLabel(label : String) = _label = label
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
    return db.cargarPedidos(_pedidos)
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
    var pedidos : List[Pedido] = db.descargarPedidos()
    var precioVentaYCosto : (Double, Double) = calcularPrecioVentaYProduccion(db.pedidosCliente(cedula))
    var precioVenta : Double = precioVentaYCosto._1
    var precioProduccion : Double = precioVentaYCosto._2

    if(precioVenta <= dinero){
      pedidos = pedidos.filter(_.getCedula != cedula)
      _pedidos = pedidos
      db.cargarPedidos(pedidos)

      var historiales : List[(String, Double, Double)] = db.descargarHistorial()
      var found : Boolean = false
      var idx : Int = 0

      for(historial <- historiales){
        if(historial._1 == _fechaActual.format(Calendar.getInstance().getTime()).toString) {
          found = true
          var newValue = (historial._1, historial._2+precioVenta, historial._3+precioProduccion)
          historiales = historiales.updated(idx, newValue)
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



class AppRestaurante{
	/* ATRIBUTOS */
	private var _administradores: List[Administrador] = Nil
	private var _clientes: List[Cliente] = Nil
	this.extraerUsuariosDB()

	/* MÉTODOS */

	//Getters
	def administradores(): List[Administrador] = _administradores
	def clientes(): List[Cliente] = _clientes

	//Acciones
	def registrarse(nombreUsuario: String, contraseña: String, identificacion: String, edad: Int, genero: String, numCelular: String, tipo: String): Boolean = {
		if(tipo == "administrador"){
			var administrador: Administrador = new Administrador(nombreUsuario, contraseña)
			_administradores = administrador::_administradores
			true
		}
		else{
			var cliente: Cliente = new Cliente(nombreUsuario, contraseña, identificacion, edad, genero, numCelular)
			_clientes = cliente::_clientes
			true
		}
	}

	//Comportamientos
	private def extraerUsuariosDB(): Unit = {
		var administradores = Source.fromFile("DB/usuarios/administradores.txt").getLines()
		var clientes = Source.fromFile("DB/usuarios/clientes.txt").getLines()

		administradores.foreach{
			linea => {
				var datos: Array[String] = linea.split(":")
				registrarse(datos(0), datos(1), "N/A", 0, "N/A", "N/A", "administrador")
			}
		}

		clientes.foreach{
			linea => {
				var datos: Array[String] = linea.split(":")
				registrarse(datos(0), datos(1), datos(2), datos(3).toInt, datos(4), datos(5), "cliente")
			}
		}
	}
}

abstract class Usuario(nombreUsuarioI: String, contraseñaI: String){
	/* ATRIBUTOS */
	private var _nombreUsuario: String = nombreUsuarioI
	private var _contraseña: String = contraseñaI
	private var _sucursal: Sucursal = _

	/* MÉTODOS */

	//Getters
	def nombreUsuario(): String = _nombreUsuario
	def contraseña(): String = _contraseña
	def sucursal(): Sucursal = _sucursal

	//Setters
	def nombreUsuario_(nuevoNombreUsuario: String) = _nombreUsuario = nuevoNombreUsuario
	def contraseña_(nuevaContraseña: String) = _contraseña = nuevaContraseña
	def sucursal_(nuevaSucursal: Sucursal) = _sucursal = nuevaSucursal
}

class Administrador(nombreUsuarioI: String, contraseñaI: String) extends Usuario(nombreUsuarioI, contraseñaI){
	/* MÉTODOS */

  def agregarInsumo(insumo : Insumo) : Boolean = {
    var insumos : List[Insumo] = sucursal.db.descargarInsumos()
    insumos = insumo::insumos
    sucursal.db.cargarInsumos(insumos)
    return true
  }

  def quitarInsumo(codigo : String) : Boolean = {
    var insumos : List[Insumo] = sucursal.db.descargarInsumos()
    var index = -1
    var i = 0
    for(insumo <- insumos){
      if(insumo.getCodigo == codigo) index = i
      i += 1
    }
    if(index == -1) return false

    insumos = insumos.filter(_.getCodigo == codigo)
    sucursal.db.cargarInsumos(insumos)

    return true
  }

  def cambiarPrecioVenta(codigo : String, nuevoPrecio : Double) : Boolean = {
    var insumos : List[Insumo] = sucursal.db.descargarInsumos()
    var index = -1
    var i = 0
    for(insumo <- insumos){
      if(insumo.getCodigo == codigo) index = i
      i += 1
    }
    if(index == -1) return false

    var aux : Array[Insumo] = insumos.toArray
    aux(index).setCostoVenta(nuevoPrecio)
    insumos = aux.toList
    sucursal.db.cargarInsumos(insumos)

    return true
  }

  def cambiarPrecioProduccion(codigo : String, nuevoPrecio : Double) : Boolean = {
    var insumos : List[Insumo] = sucursal.db.descargarInsumos()
    var index = -1
    var i = 0
    for(insumo <- insumos){
      if(insumo.getCodigo == codigo) index = i
      i += 1
    }
    if(index == -1) return false

    var aux : Array[Insumo] = insumos.toArray
    aux(index).setCostoProduc(nuevoPrecio)
    insumos = aux.toList
    sucursal.db.cargarInsumos(insumos)

    return true
  }

  def cambiarPrecioVentaAgrandado(codigo : String, nuevoPrecio : Double) : Boolean = {
    var insumos : List[Insumo] = sucursal.db.descargarInsumos()
    var index = -1
    var i = 0
    for(insumo <- insumos){
      if(insumo.getCodigo == codigo) index = i
      i += 1
    }
    if(index == -1) return false

    var aux : Array[Insumo] = insumos.toArray
    if(aux(index).isInstanceOf[Util]) return false
    aux(index).asInstanceOf[Alimento].setCostoVentaAgrandado(nuevoPrecio)
    insumos = aux.toList
    sucursal.db.cargarInsumos(insumos)

    return true
  }

  def cambiarPrecioProduccionAgrandado(codigo : String, nuevoPrecio : Double) : Boolean = {
    var insumos : List[Insumo] = sucursal.db.descargarInsumos()
    var index = -1
    var i = 0
    for(insumo <- insumos){
      if(insumo.getCodigo == codigo) index = i
      i += 1
    }
    if(index == -1) return false

    var aux : Array[Insumo] = insumos.toArray
    if(aux(index).isInstanceOf[Util]) return false
    aux(index).asInstanceOf[Alimento].setCostoProducAgrandado(nuevoPrecio)
    insumos = aux.toList
    sucursal.db.cargarInsumos(insumos)

    return true
  }

  def consultarUtilerias(fecha : String) : (Boolean, Double) = {
    var res : (Boolean, Double) = sucursal.obtenerUtilerias(fecha)
    return (res._1, res._2)  //El booleano de esta tupla dice si encontró la fecha o no
                            //el double es la utileria
  }

}

class Cliente(nombreUsuarioI: String, contraseñaI: String, identificacionI: String, edadI: Int, generoI: String, numCelularI: String) extends Usuario(nombreUsuarioI, contraseñaI){
	/* ATRIBUTOS */
	private var _identificacion: String = identificacionI
	private var _edad: Int = edadI
	private var _genero: String = generoI
	private var _numCelular: String = numCelularI


	/* MÉTODOS */

	//Getters
	def identificacion(): String = _identificacion
	def edad(): Int = _edad
	def genero(): String = _genero
	def numCelular(): String = _numCelular

	//Setters
	def identificacion_(nuevaIdentificacion: String) = _identificacion = nuevaIdentificacion
	def edad_(nuevaEdad: Int) = _edad = nuevaEdad
	def genero_(nuevoGenero: String) = _genero = nuevoGenero
	def numCelular_(nuevoNumCelular: String) = _numCelular = nuevoNumCelular

  def pagarCuenta(dinero : Double) : Boolean = return sucursal.caja.vender(_identificacion, dinero)

  def agregarPedido(insumo : Insumo) : Boolean = return sucursal.caja.agregarPedido(insumo, _identificacion)

  def verCatalogo() : List[Alimento] = sucursal.caja.getCatalogo

  def consultarPedido() : List[Insumo] = sucursal.db.pedidosCliente(_identificacion)
}

object Interfaz{
	var restaurante: AppRestaurante = new AppRestaurante

	def clear() = "clear".!

  	def main(args : Array[String]): Unit = {
  		clear()
  		menu()
  	}

  	def menu(): Unit = {
  		println("Bienvenido. Por favor digite una de las siguientes opciones:")
  		println("1. Iniciar sesión")
  		println("2. Registrarse")
  		println("3. Salir")
  		var opc: Int = scala.io.StdIn.readInt()
  		opc match{
  			case 1 => clear(); iniciarSesion(); clear(); menu()
  			case 2 => clear(); registrarse(); clear(); menu()
  			case _ => exportarUsuariosDB(); println("\nLo esperamos pronto.\n")
  		}
  	}

	def verClientesAdministradores(): Unit = {
		println("ADMINISTRADORES")
		restaurante.administradores.foreach{
			administrador => {
				println("---")
				println(administrador.nombreUsuario)
				println(administrador.contraseña)
				println("---")
			}
		}
		println("CLIENTES")
		restaurante.clientes.foreach{
			cliente => {
				println("---")
				println(cliente.nombreUsuario)
				println(cliente.contraseña)
				println(cliente.identificacion)
				println(cliente.edad)
				println(cliente.genero)
				println(cliente.numCelular)
				println("---")
			}
		}
		println("Presione una tecla para ir al menu...")
		var tcl: String = scala.io.StdIn.readLine()
	}

	def buscarUsuario(nombreUsuario: String, contraseña: String): Usuario = {
		restaurante.administradores.foreach{
			administrador => {
				if(administrador.nombreUsuario == nombreUsuario){
					if(administrador.contraseña == contraseña) return administrador
				}
			}
		}
		restaurante.clientes.foreach{
			cliente => {
				if(cliente.nombreUsuario == nombreUsuario){
					if(cliente.contraseña == contraseña) return cliente
				}
			}
		}
		return null
	}

	def elegirSucursal(usuario: Usuario): Unit = {
		println("Usuario: " + usuario.nombreUsuario)

		var sucursales: List[String] = Source.fromFile("DB/sucursales.txt").getLines().toList
		var i: Int = 1
		sucursales.foreach{
			linea => {
				println(i + ". " + linea)
				i = i + 1
			}
		}
		println("Por favor elija la sucursal a la que desea ingresar: ")
		var opc: Int = scala.io.StdIn.readInt()
		if(opc >= 0 && (opc - 1) < sucursales.length){
			var sucursal: Sucursal = new Sucursal(sucursales(opc - 1))
			usuario.sucursal_(sucursal)
			if(usuario.isInstanceOf[Administrador]){
				clear()
				funcionalidadesAdministrador(usuario.asInstanceOf[Administrador])
			}
			else{
				clear()
				funcionalidadesCliente(usuario.asInstanceOf[Cliente])
			}
		}
		else{
			println("Esta opción no está disponible. Por favor elija lo que desea realizar: \n")
			println("1. Volver a intentar")
			println("2. Regresar al menú")
			var opc2: Int = scala.io.StdIn.readInt()
			opc2 match{
				case 1 => clear(); elegirSucursal(usuario)
				case _ => ;
			}
		}
	}

  def esNumero(x : String) = x forall Character.isDigit
  def esFecha(f : String) : Boolean = {
    var i = 0
    for(x <- f.split("-").toList){
      if(x.length != 2) return false
      i += 1
    }
    if(i == 3) return true
    else return false
  }

  def crearInsumo(administrador : Administrador) : Insumo = {

    var insumos : List[Insumo] = administrador.sucursal.db.descargarInsumos()
    var codigo : String = "0"*(4-(insumos.length+1).toString.length) + (insumos.length+1).toString
    var valid = false
    var opc : String = ""
    while(!valid){
      clear()
      println("Elija la opcion del tipo de insumo desea agregar:")
      println("1. Alimento")
      println("2. Útil")
      opc = scala.io.StdIn.readLine()
      if(opc == "1" || opc == "2") valid = true
    }
    opc match{
      case "1" => {
        var costoProd = ""
        var costoVenta = ""
        var costoProdAg = ""
        var costoVentaAg = ""

        println("Ingrese el nombre del insumo: ")
        var nombre = scala.io.StdIn.readLine()

        valid = false
        while(!valid){
          println("Ingrese el costo de producción:")
          costoProd = scala.io.StdIn.readLine()
          if(esNumero(costoProd)) valid = true
        }

        valid = false
        while(!valid){
          println("Ingrese el costo de venta")
          costoVenta = scala.io.StdIn.readLine()
          if(esNumero(costoVenta)) valid = true
        }

        valid = false
        while(!valid){
          println("Ingrese el costo de producción  de la versión agrandada")
          costoProdAg = scala.io.StdIn.readLine()
          if(esNumero(costoProdAg)) valid = true
        }

        valid = false
        while(!valid){
          println("Ingrese el costo de venta de la version agrandada")
          costoVentaAg = scala.io.StdIn.readLine()
          if(esNumero(costoVentaAg)) valid = true
        }

        println("Ingrese la categoría a la que pertenece el alimento")
        var categoria = scala.io.StdIn.readLine()

        var nuevoInsumo = new Alimento(codigo, costoVenta.toDouble, costoProd.toDouble, nombre, costoProdAg.toDouble, costoVentaAg.toDouble, categoria)
        return nuevoInsumo
      }
      case "2" => {
        var costoProd = ""
        var costoVenta = ""

        println("Ingrese el nombre del insumo: ")
        var nombre = scala.io.StdIn.readLine()

        valid = false
        while(!valid){
          println("Ingrese el costo de producción")
          costoProd = scala.io.StdIn.readLine()
          if(esNumero(costoProd)) valid = true
        }

        valid = false
        while(!valid){
          println("Ingrese el costo de venta")
          costoVenta = scala.io.StdIn.readLine()
          if(esNumero(costoVenta)) valid = true
        }

        var nuevoInsumo = new Util(codigo, costoVenta.toDouble, costoProd.toDouble, nombre)
        return nuevoInsumo
      }
    }
  }

	def funcionalidadesAdministrador(administrador: Administrador): Unit = {
		println("Usuario: " + administrador.nombreUsuario)
		println("Sucursal: " + administrador.sucursal.getnombreSucursal)
    println("")
		println("Digite lo que desea realizar: ")
		println("1. Agregar insumo")
		println("2. Quitar insumo")
		println("3. Cambiar precio insumo")
		println("4. Consultar utilerias")
		println("5. cerrar sesión")
		var opc: String = scala.io.StdIn.readLine()
    var valid : Boolean = false
		opc match{
			case "1" => {
        clear()
        var nuevoInsumo = crearInsumo(administrador)
        if(administrador.agregarInsumo(nuevoInsumo)) println("Se ha agregado el insumo con éxito")
        else println("No se ha podido agregar el insumo")
      }
      case "2" => {
        println("Ingrese el código del insumo")
        var codigo: String = scala.io.StdIn.readLine()
        if(administrador.quitarInsumo(codigo)) println("Se ha quitado el insumo con exito")
        else println("No se ha podido quitar el insumo con exito")
      }
      case "3" =>{
        valid = false
        while(!valid){
          println("Desea cambiar el precio de un:")
          println("1. Alimento")
          println("2. Útil")
          opc = scala.io.StdIn.readLine()
          if(opc == "1" || opc == "2") valid = true
        }

        opc match{
          case "1" =>{
            valid = false
            while(!valid){
              println("Elija lo que desea modificar:")
              println("1. Precio de venta")
              println("2. Precio de produccion")
              println("3. Precio de venta agrandado")
              println("4. Precio de producción agrandado")
              opc = scala.io.StdIn.readLine()
              if(opc == "1" || opc == "2" || opc == "3" || opc == "4") valid = true
            }
            println("Ingrese el codigo del insumo")
            var codigo = scala.io.StdIn.readLine()
            var costo = ""
            valid = false
            while(!valid){
              println("Ingrese el nuevo costo")
              costo = scala.io.StdIn.readLine()
              if(esNumero(costo)) valid = true
            }

            opc match{
              case "1" => {
                if(administrador.cambiarPrecioVenta(codigo, costo.toDouble)) println("Se cambió el costo con éxito")
                else println("No se pudo cambiar el costo")
              }
              case "2" =>{
                if(administrador.cambiarPrecioProduccion(codigo, costo.toDouble)) println("Se cambió el costo con éxito")
                else println("No se pudo cambiar el costo")
              }
              case "3" =>{
                if(administrador.cambiarPrecioVentaAgrandado(codigo, costo.toDouble)) println("Se cambió el costo con éxito")
                else println("No se pudo cambiar el costo")
              }
              case "4" =>{
                if(administrador.cambiarPrecioProduccionAgrandado(codigo, costo.toDouble)) println("Se cambió el costo con éxito")
                else println("No se pudo cambiar el costo")
              }
            }
          }
          case "2" =>{
            valid = false
            while(!valid){
              println("Elija lo que desea modificar:")
              println("1. Precio de venta")
              println("2. Precio de produccion")
              opc = scala.io.StdIn.readLine()
              if(opc == "1" || opc == "2") valid = true
            }
            println("Ingrese el codigo del insumo")
            var codigo = scala.io.StdIn.readLine()
            var costo = ""
            valid = false
            while(!valid){
              println("Ingrese el nuevo costo")
              costo = scala.io.StdIn.readLine()
              if(esNumero(costo)) valid = true
            }

            opc match{
              case "1" => {
                if(administrador.cambiarPrecioVenta(codigo, costo.toDouble)) println("Se cambió el costo con éxito")
                else println("No se pudo cambiar el costo")
              }
              case "2" => {
                if(administrador.cambiarPrecioProduccion(codigo, costo.toDouble)) println("Se cambió el costo con éxito")
                else println("No se pudo cambiar el costo")
              }
            }
          }
        }
      }
    case "4" =>{
      valid = false
      var fecha = ""
      while(!valid){
        println("Digite la fecha:")
        fecha = scala.io.StdIn.readLine()
        if(esFecha(fecha)) valid = true
      }
      var utileria = administrador.consultarUtilerias(fecha)
      if(utileria._1) println("Las utilerias del " + fecha + " fueron de " + utileria._2)
      else println("La fecha no se encontró en el historial")
    }
    case "5" => menu()
    case _ => funcionalidadesAdministrador(administrador)
   }
  clear()
  funcionalidadesAdministrador(administrador)
  }

  def imprimirCatalogo(c : List[Alimento]) : Unit = {
    var catalogo = c.sortWith(_.getCategoria < _.getCategoria)
    println("%2s%18s%19s%18s".format("", "", " Normal", " Agrandado"))
    println("")
    println("-> " + catalogo.head.getCategoria)
    var i = 1
    var anterior = catalogo.head.getCategoria
    println("%2s.%18s%18s%18s".format(i, catalogo.head.getLabel, catalogo.head.getCostoVenta, catalogo.head.getCostoVentaAgrandado))
    var aux = catalogo.tail

    for(alimento <- aux){
      i += 1
      if(alimento.getCategoria != anterior){
        println("")
        println("-> " + alimento.getCategoria)
        anterior = alimento.getCategoria
      }
      println("%2s.%18s%18s%18s".format(i, alimento.getLabel, alimento.getCostoVenta, alimento.getCostoVentaAgrandado))
    }

    println("")
    println("")
  }

	def funcionalidadesCliente(cliente: Cliente): Unit = {
		println("Usuario: " + cliente.nombreUsuario)
		println("Sucursal: " + cliente.sucursal.getnombreSucursal)
    println("")
    println("Seleccione una de las siguientes opciones:")
    println("1. Pagar la cuenta")
    println("2. Agregar un pedido")
    println("3. Ver el catálogo")
    println("4. consultar el carrito de pedidos")
    println("5. Cerrar sesión")

    var opc: String = scala.io.StdIn.readLine()
    var valid = false


    opc match{
      case "1" => {
        clear()
        println("El valor a pagar es: ")
        println(cliente.sucursal.caja.calcularPrecioVentaYProduccion(cliente.sucursal.db.pedidosCliente(cliente.identificacion))._1.toString)
        valid = false
        var dinero = ""
        while(!valid){
          println("Por favor ingrese el dinero: ")
          dinero = scala.io.StdIn.readLine()
          if(esNumero(dinero)) valid = true
        }
        if(cliente.pagarCuenta(dinero.toDouble)) println("Se ha pagado la cuenta exitosamente. !Vuelve pronto!")
        else println("El monto de dinero es insufuciente")
        println("Presiona una tecla para continuar")
        var daenerys = scala.io.StdIn.readLine()
      }
      case "2" =>{
        var adicion = true
        while(adicion){
          clear()
          var catalogo = cliente.verCatalogo()
          catalogo = catalogo.sortWith(_.getCategoria < _.getCategoria)
          imprimirCatalogo(catalogo)
          println("Seleccione una opción del catálogo:")
          var opc = scala.io.StdIn.readLine()
          println("¿Desea su pedido agrandado?")
          println("1. Sí")
          println("2. No")
          valid = false
          var agrandado = ""
          while(!valid){
            agrandado = scala.io.StdIn.readLine()
            if(agrandado == "1" || agrandado == "2") valid = true
          }
          var aux : Array[Alimento] = catalogo.toArray
          var pedido = aux(opc.toInt-1)

          if(agrandado == "1") pedido.setAgrandado("si")
          else pedido.setAgrandado("no")

          println("¿Desea agregar restricciones?")
          println("1. Sí")
          println("2. No")
          valid = false
          var restricciones = ""
          while(!valid){
            restricciones = scala.io.StdIn.readLine()
            if(restricciones == "1" || restricciones == "2") valid = true
          }
          if(restricciones == "1"){
            println("Ingresa tus restricciones:")
            var restriccion = scala.io.StdIn.readLine()
            pedido.setRestricciones(restriccion)
            clear()
          }

          if(cliente.agregarPedido(pedido)){
            println("El pedio ha sido agregado exitosamente")

            valid = false
            var op = ""
            while(!valid){
              println("¿Desea agregar adiciones?")
              println("1. Sí")
              println("2. No")
              op = scala.io.StdIn.readLine()
              if(op == "1" || op == "2") valid = true
            }
            if(op == "2") adicion = false
          }
          else{
            println("No se pudo agregar el pedido")
            funcionalidadesCliente(cliente)
          }
        }
      }
      case "3" => {
        clear()
        imprimirCatalogo(cliente.verCatalogo())
        println("Presiona cualquier tecla para seguir")
        var a = scala.io.StdIn.readLine
      }
      case "4" =>{
        clear()
        println("Tus pedidos son:")
        var pedidos = cliente.consultarPedido()
        var total : Double = cliente.sucursal.caja.calcularPrecioVentaYProduccion(pedidos)._1
        for(pedido <- pedidos){
          print("-> " + pedido.getLabel)
          if(pedido.asInstanceOf[Alimento].getRestricciones != "null") println(" " + pedido.asInstanceOf[Alimento].getRestricciones)
          else println("")
        }

        println("Total: " + total)
        println("Presiona cualquier tecla para seguir")
        var a = scala.io.StdIn.readLine()
      }
      case "5" => clear(); menu()
      case _ => {
        clear()
        funcionalidadesCliente(cliente)
      }
    }
    clear()
    funcionalidadesCliente(cliente)
	}


	def iniciarSesion(): Unit = {
  		println("INICIAR SESION\n")
  		println("Nombre de usuario: ")
  		var nombreUsuario: String = scala.io.StdIn.readLine()
  		println("Contraseña: ")
  		var contraseña: String = scala.io.StdIn.readLine()

  		var usuario: Usuario = buscarUsuario(nombreUsuario, contraseña)
  		usuario match{
  			case null => {
	  			println("Por favor verifique su usuario o contraseña.")
	  			println("Si todavia no se ha registrado puede hacerlo desde el menú. Digite lo que desea hacer:")
	  			println("1. Volver a intentar")
	  			println("2. Regresar al menú")
	  			var opc: Int = scala.io.StdIn.readInt()
	  			opc match{
	  				case 1 => clear(); iniciarSesion()
	  				case _ => ;
	  			}
  			}
  			case _ => {
  				clear()
  				elegirSucursal(usuario)
  			}
  		}
	}

	def existeUsuario(nombreUsuario: String, identificacion: String, tipoUsuario: String): Boolean = {
		if(tipoUsuario == "administrador"){
	  		restaurante.administradores.foreach{
	  			administrador => {
	  				if(administrador.nombreUsuario == nombreUsuario){
	  					return true
	  				}
	  			}
	  		}
	  		restaurante.clientes.foreach{
	  			cliente => {
	  				if(cliente.nombreUsuario == nombreUsuario){
	  					return true
	  				}
	  			}
	  		}
	  		return false
		}
		else{
	  		restaurante.administradores.foreach{
	  			administrador => {
	  				if(administrador.nombreUsuario == nombreUsuario){
	  					return true
	  				}
	  			}
	  		}
	  		restaurante.clientes.foreach{
	  			cliente => {
	  				if(cliente.identificacion == identificacion){
	  					return true
	  				}
	  			}
	  		}
	  		return false
		}
	}

  	def registrarseAdministrador(): Unit = {
  		println("REGISTRARSE\n")
  		println("Nombre de usuario: ")
  		var nombreUsuario: String = scala.io.StdIn.readLine()
  		println("Contraseña: ")
  		var contraseña: String = scala.io.StdIn.readLine()
  		println("Verificar contraseña: ")
  		var contraseña2: String = scala.io.StdIn.readLine()
  		if(!existeUsuario(nombreUsuario, "N/A", "administrador")){
  			if(contraseña == contraseña2){
  				restaurante.registrarse(nombreUsuario, contraseña, "N/A", 0, "N/A", "N/A", "administrador")
  			}
  			else{
  				println("Las contraseñas no coinciden. Por favor digite lo que desea realizar: ")
  				println("1. Volver a intentar")
  				println("2. Regreasar al menú")
  				var opc: Int = scala.io.StdIn.readInt()
  				opc match{
  					case 1 => clear(); registrarseAdministrador()
  					case _ => ;
  				}
  			}
  		}
  		else{
  			println("El usuario ingresado ya existe. Por favor digite lo que desea realizar: ")
  			println("1. Volver a intentar")
  			println("2. Regreasar al menú")
  			var opc: Int = scala.io.StdIn.readInt()
  			opc match{
  				case 1 => clear(); registrarseAdministrador()
  				case _ => ;
  			}
  		}
  	}

  	def registrarseCliente(): Unit = {
  		println("REGISTRARSE\n")
		println("Nombre de usuario: ")
  		var nombreUsuario: String = scala.io.StdIn.readLine()
  		println("Contraseña: ")
  		var contraseña: String = scala.io.StdIn.readLine()
  		println("Identificacion: ")
  		var identificacion: String = scala.io.StdIn.readLine()
  		println("Edad: ")
  		var edad: Int = scala.io.StdIn.readInt()
  		println("Genero: ")
  		var genero: String = scala.io.StdIn.readLine()
  		println("Número de celular: ")
  		var numCelular: String = scala.io.StdIn.readLine()
  		if(!existeUsuario(nombreUsuario, identificacion, "cliente")){
  			restaurante.registrarse(nombreUsuario, contraseña, identificacion, edad, genero, numCelular, "cliente")
  		}
  		else{
  			println("El usuario ingresado ya existe. Por favor digite lo que desea realizar: ")
  			println("1. Volver a intentar")
  			println("2. Regreasar al menú")
  			var opc: Int = scala.io.StdIn.readInt()
  			opc match{
  				case 1 => clear(); registrarseAdministrador()
  				case _ => ;
  			}
  		}
  	}

  	def registrarse(): Unit = {
  		println("REGISTRARSE\n")
  		println("¿Cómo desea registrarse?")
  		println("1. Administrador")
  		println("2. Cliente")

  		var opc: Int = scala.io.StdIn.readInt()
  		opc match{
  			case 1 => clear(); registrarseAdministrador()
  			case 2 => clear(); registrarseCliente()
  			case 3 => println("\nEsta opción no está disponible. Por favor vuelve a intentarlo.\n"); registrarse()
  		}
  	}

  	def exportarUsuariosDB(): Unit = {
  		var writer = new FileWriter("DB/usuarios/administradores.txt")
  		var writer2 = new FileWriter("DB/usuarios/clientes.txt")
  		restaurante.administradores.foreach{
  			administrador => {
  				writer.write(administrador.nombreUsuario + ":" + administrador.contraseña + "\n")
  			}
  		}
  		restaurante.clientes.foreach{
  			cliente => {
  				writer2.write(cliente.nombreUsuario + ":" + cliente.contraseña + ":")
  				writer2.write(cliente.identificacion + ":" + cliente.edad + ":")
  				writer2.write(cliente.genero + ":" + cliente.numCelular + "\n")
  			}
  		}
  		writer.close()
  		writer2.close()
  	}
}
