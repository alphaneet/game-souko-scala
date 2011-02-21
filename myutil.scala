package myutil

trait Reflect { 
//{{{

def scalaType(value: Any):Class[_] = value match {
  case _: Byte    => classOf[Byte]
  case _: Short   => classOf[Short]
  case _: Int     => classOf[Int]
  case _: Long    => classOf[Long]
  case _: Float   => classOf[Float]
  case _: Double  => classOf[Double]
  case _: Boolean => classOf[Boolean]
  case _: Char    => classOf[Char]
  case _: Unit    => classOf[Unit]
  case ref: AnyRef=> ref.getClass
} 

def javaType(value: Any):Class[_] = value match {
  case _: Byte    => java.lang.Byte.TYPE
  case _: Short   => java.lang.Short.TYPE
  case _: Int     => java.lang.Integer.TYPE
  case _: Long    => java.lang.Long.TYPE
  case _: Float   => java.lang.Float.TYPE
  case _: Double  => java.lang.Double.TYPE
  case _: Boolean => java.lang.Boolean.TYPE
  case _: Char    => java.lang.Character.TYPE
  case _: Unit    => java.lang.Void.TYPE
  case ref: AnyRef=> ref.getClass
}

// TODO Nothing以外みたいな境界を設定したい
// TODO デフォルトの型パラメーターが指定できないか調べる
//def newInstance[T](name:String) = {
//  val packName = if(name.indexOf('.') == -1) {
//    val p = getClass.getPackage
//    if(p == null) "" else p.getName + "."
//  } else ""
//  Class.forName(packName + name)
//    .newInstance.asInstanceOf[T]
//}

def newInstance[T](name:String, args:Any*):T = {
  val packName = if(name.indexOf('.') == -1) {
    val p = getClass.getPackage
    if(p == null) "" else p.getName + "."
  } else ""

  val clazz = Class.forName(packName + name)
  (if(args.isEmpty) clazz.newInstance else {
    clazz.getConstructor(args map(scalaType):_*)
      .newInstance(args map(_.asInstanceOf[AnyRef]):_*)
  }).asInstanceOf[T]
}

//def invoke(name:String, args:Any*) = { invoke[Unit](name, args) }
//def invoke(name:String) = { invoke[Unit](name) }

//def invoke[T](name:String):T = { invoke[T](name, Array[Unit]():_*) }
//def invoke(name:String):AnyRef = invoke(name, Array[Unit]():_*)
//def invoke[T](name:String, args:Any*):T = {
//def invoke(name:String, args:Any*):AnyRef = invoke(this, name, args:_*)
//def invoke(handle:AnyRef, name:String, args:Any*):AnyRef = {
//  handle.getClass.getMethod(name, args map(scalaType):_*)
//    .invoke(handle, args map(_.asInstanceOf[AnyRef]):_*)
////    .asInstanceOf[T]
//}

def invoke(name:String, args:Any*):AnyRef = {
  getClass.getMethod(name, args map(scalaType):_*)
    .invoke(this, args map(_.asInstanceOf[AnyRef]):_*)
//    .asInstanceOf[T]
}

//def setField[T](name:String, value:T) = invoke[Unit](name+"_$eq", value)
def setField[T](name:String, value:T) = invoke(name+"_$eq", value)
//def getField[T](name:String):T = invoke[T](name)
def getField[T](name:String):T = invoke(name).asInstanceOf[T]

//}}}
}

class Method(target:AnyRef, name:String, types:Class[_]*) { 
//{{{

val method:Option[java.lang.reflect.Method] = {
  try {
    Some(target.getClass.getMethod(name, types:_*))
  } catch {
    case _:NoSuchMethodException =>
      val strTypes = if(types.isEmpty) "" else
        types.map { _.getSimpleName } mkString(", ")

      println("There is no public " +
              name + "(" + strTypes + ") " + 
              "method in the class " +
              target.getClass.getName())
      None
  }
}

def invoke(args:Any*):AnyRef = {
  method foreach { m => 
    try {
      return m.invoke(target, args.map(_.asInstanceOf[AnyRef]):_*)
    } catch {
      case e:Exception => e.printStackTrace
    }
  }
  return null
}

//}}}
}

object Applet extends processing.core.PApplet { 
//{{{

var _DEBUG_ = true
var mousePushed = false

private var _setup = () => {}
private var _scene:Scene = new Scene
def scene = _scene
def scene_=(aScene:Scene) {
  _scene.clear
  _scene = aScene
}

override def setup = _setup()
override def draw = _scene.draw
override def mousePressed {
  mousePushed = true
  _scene.mousePressed
}
override def mouseReleased {
  mousePushed = false
  _scene.mouseReleased
}
override def mouseClicked = _scene.mouseClicked
override def mouseMoved = _scene.mouseMoved
override def mouseDragged = _scene.mouseDragged
override def keyPressed = _scene.keyPressed
override def keyReleased = _scene.keyReleased
override def keyTyped = _scene.keyTyped

// debug only
def p(x:Any) = System.out.println(x)
def pp(x:Any) = System.out.print(x)
def pf(text:String, xs:Any*) = p(format(text, xs:_*))
def ppf(text:String, xs:Any*) = pp(format(text, xs:_*))

def init(setup:() => Unit) {
  _setup = setup

//  runSketch()

  import javax.swing.JFrame
  frame = new JFrame {
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }
  frame.add(this)

  super.init
  // setupが実行されるまで待機
  while(defaultSize && !finished) Thread.sleep(5)

  frame.pack
  frame.setResizable(false)
  frame.setVisible(true)
}

/**
 * pathが絶対パスならそのまま
 * 相対パスならdataPathに包んで戻す
 */
def absoluteOrDataPath(path:String):String = {
  if((new java.io.File(path)).isAbsolute) path
  else dataPath(path)
}

//}}}
}

class Scene(skinPath:String) extends Reflect {
//{{{

def this() = this(null)

object Skin extends Skin(this)
try {
  Skin.addXML(
    if(skinPath == null) getClass.getSimpleName+".xml"
    else skinPath
  )
} catch { 
  case e:java.io.FileNotFoundException => 
  case e:Exception => e.printStackTrace
}


def draw {
  Applet.background(255)
  Skin.draw
}
def mousePressed {}
def mouseReleased {}
def mouseClicked {}
def mouseMoved {}
def mouseDragged {}
def keyPressed {}
def keyReleased {}
def keyTyped {}
def buttonPressed(name:String) {}
def buttonReleased(name:String) {}
def buttonClicked(name:String) {}

def clear = {
  Skin.clear
}

//}}}
}

class Skin[T <: Reflect](val parent:T) extends Reflect { 
//{{{

import processing.core.PImage
import Applet._

/**
 * @see myutil.Method
 */
private def getButtonMethod(methodName:String) =
  new Method(parent, methodName, classOf[String])

private val buttonPressed = getButtonMethod("buttonPressed")
private val buttonReleased = getButtonMethod("buttonReleased")
private val buttonClicked = getButtonMethod("buttonClicked")

class ElementArrayBuffer[A <: Element] 
      extends collection.mutable.ArrayBuffer[A] 
{
  override def +=(elem:A):this.type = {
    super.+=(elem)
    elem.register
    this
  }

  def apply(name:String) = find(_.name == name)

  override def remove(n: Int) = { 
    val ret = super.remove(n)
    ret.unregister
    ret
  }

  override def clear {
    foreach(_.unregister)
    super.clear
  }
}

class ComponentArrayBuffer[A <: Component]
      extends ElementArrayBuffer[A]
{
  def draw = foreach(_.draw)
}

class ConstantArrayBuffer[A <: Constant] 
      extends ElementArrayBuffer[A]

val labels    = new ComponentArrayBuffer[Label]
val buttons   = new ComponentArrayBuffer[Button]
val rects     = new ConstantArrayBuffer[Rect]
val points    = new ConstantArrayBuffer[Point]

val elementBufferList   = List[ElementArrayBuffer[_ <: Element]](
  labels, buttons, rects, points
)
val componentBufferList = elementBufferList filter { 
      _.isInstanceOf[ComponentArrayBuffer[_ <: Component]]
} map(_.asInstanceOf[ComponentArrayBuffer[_ <: Component]])

val constantBufferList = elementBufferList filter { 
      _.isInstanceOf[ConstantArrayBuffer[_ <: Constant]]
} map(_.asInstanceOf[ConstantArrayBuffer[_ <: Constant]])

abstract class Element(var name:String) {
  var x, y, w, h = 0
  def register
  def unregister

  // @see processing.core.PApplet.mouseX or mouseY
  def isMosueOver = 
    (mouseX > x && mouseX < x+w && mouseY > y && mouseY < y+h) 
}

abstract class Component(name:String, regMouse:Boolean)
         extends Element(name) 
{

  // @see processing.core.PApplet.registerMouseEvent
  override def register {
    if(regMouse) registerMouseEvent(this)
  }

  override def unregister {
    if(regMouse) unregisterMouseEvent(this)
  }
  def draw {}
}

abstract class Constant(name:String) extends Element(name) 
{
  var constants = Map[String, Any]()
  def getName(str:String) = name + "_" + str
  def add(pair:Pair[String, Any]*) = for((k,v) <- pair) {
    constants += getName(k) -> v 
  }
  override def register = for((k,v) <- constants) {
    try {
      parent.setField(k, v)
    } catch {
      case _:NoSuchMethodException =>
        println("There is no public var " + k + ":" +
                scalaType(v).getName +
                " in the class " + parent.getClass().getName());
      case e:Exception => e.printStackTrace
    }
  }
  override def unregister {}
}

class Button(_name:String, var text:String, var size:Int, 
             _x:Int, _y:Int, _w:Int, _h:Int) 
      extends Component(_name, true)
{
  // ボタンの状態定数 
  // OFF:  何もしていない
  // OVER: マウスが上にきた状態
  // DOWN: 押された状態 の順番
  val OFF = 0; val OVER = 1; val DOWN = 2
  var staus = OFF

  var img:List[PImage] = _
  x = _x; y = _y; w = _w; h = _h

  def mouseEvent(event:java.awt.event.MouseEvent) {
    import java.awt.event.MouseEvent._

    event.getID match {
      case MOUSE_PRESSED => {
        if(isMosueOver) {
          staus = DOWN
          buttonPressed.invoke(name)
        }
      }
      case MOUSE_RELEASED => {
        if(staus == DOWN) {
          if(isMosueOver) {
            staus = OVER
            buttonClicked.invoke(name)
          } else {
            staus = OFF
            buttonReleased.invoke(name)
          }
        }
      }
//      case MOUSE_CLICKED => {
//        if(staus == DOWN) {
//          staus = OFF
//          buttonClicked foreach { _.invoke(parent, name) }
//        }
//      }
      case MOUSE_MOVED => {
        staus = if(isMosueOver) OVER else OFF
      }
      case _ =>
    }
  }

  override def draw = image(img(staus), x, y)

  def createDefaultImage:List[PImage] = {
    ImageFactory.createButton(text, w, h, size, 0xFFFFFF, 0x333333)
  }

  def clone(n:String=null, t:String=null):Button = {
    new Button(if(n == null) name else n, 
               if(t == null) text else t,
               size, x, y, w, h) 
    {
      img = createDefaultImage
    }
  }
}

class Label(_name:String, var text:String, var size:Int,
            _x:Int, _y:Int, _w:Int, _h:Int) 
      extends Component(_name, false) 
{
  var img:PImage = _

  x = _x; y = _y; w = _w; h = _h
  override def draw = image(img, x, y)

  def createDefaultImage:PImage = {
    ImageFactory.createLabel(text, w, h, size, 0x333333)
  }

  def clone(n:String=null, t:String=null):Button = {
    new Button(if(n == null) name else n, 
               if(t == null) text else t,
               size, x, y, w, h) 
    {
      img = createDefaultImage
    }
  }
}

class Point(name:String, cx:Int, cy:Int) 
      extends Constant(name) 
{
  x = cx-8; y = cy-8; w = 16; h = 16
  add("X" -> cx, "Y" -> cy)
}

class Rect(name:String, _x:Int, _y:Int, _w:Int, _h:Int)
      extends Constant(name)
{
  x = _x; y = _y; w = _w; h = _h
  add("X" -> x, "Y" -> y, "W" -> w, "H" -> h)
}

def addXML(path:String) {
  addXML(scala.xml.XML.loadFile(absoluteOrDataPath(path)))
}

def addXML(xml:scala.xml.Elem) {
  import scala.xml._

  (xml \ "element") foreach(create)
  def create(node:NodeSeq) {
    def str(elem:String) = (node \ elem).text
    def int(elem:String) = str(elem).toInt
    val name = str("name")
    val text:String = { 
      val t = str("text")
      if(t!="") t else name
    }

    def x = int("x"); def y = int("y")
    def w = int("w"); def h = int("h")
    def size = int("size")

    str("kind") match {
      case "button" => { 
        buttons += new Button(name, text, size, x, y, w, h) {
          img = createDefaultImage
        }
      }
      case "label" => {
        labels += new Label(name, text, size, x, y, w, h) {
          img = createDefaultImage
        }
      }
      case "point" => points += new Point(name, x, y)
      case "rect"  => rects  += new Rect(name, x, y, w, h)
    }
  }
}

def draw { 
  componentBufferList foreach(_.foreach(_.draw))
}
def registerConstants {
  constantBufferList foreach(_.foreach(_.register))
}
def clear = elementBufferList foreach(_.clear)

//}}}
}

object ImageFactory { 
//{{{

import processing.core._
import processing.core.PConstants._
import Applet._

def rgb(c:Int)(implicit g:PGraphics):(Float, Float, Float) = 
  (g.red(c), g.green(c), g.blue(c))

def hsg(c:Int)(implicit g:PGraphics):(Float, Float, Float) =
  (g.hue(c), g.saturation(c), g.brightness(c))

/**
 * 明るくした色と暗くした色を戻す
 * 使う構えにg.colorMode(HSG, 255)してないとおかしいことになるかも
 * @param c 元の色
 * @param l 変換量のレベル
 */
def lightAndShadowColor(c:Int, l:Float=1.4f)(implicit g:PGraphics)
  :List[Int] = {

  val (h, s, b) = hsg(c)
  List(g.color(h, s/l, b*l), g.color(h, s*l, b/l))
}

def createLabel(text:String, w:Int, h:Int, size:Int, front:Int)
  :PImage = {

  implicit val g:PGraphics = createGraphics(w, h, JAVA2D)
  g.beginDraw
  g.smooth
  val f = rgb(front)
  g.fill(f._1, f._2, f._3)
  g.textSize(size)
  g.textAlign(CENTER)
  val des = g.textDescent.toInt
  //val asc = g.textAscent.toInt
  g.text(text, w>>1, (h>>1) + des + (des>>1))
  g.endDraw

  val img = createImage(w, h, ARGB)
  img.set(0, 0, g)
  g.dispose

  img
}

def createButton(text:String, w:Int, h:Int, size:Int,
  frontBaseColor:Int, backBaseColor:Int):List[PImage] = {

  implicit val g:PGraphics = createGraphics(w, h, JAVA2D)

  // ボタンの状態定数 何もしていない、マウスが上にきた状態、押された状態 の順番
  val OFF = 0; val OVER = 1; val DOWN = 2; 

  // 色関係初期化
  g.beginDraw

  g.colorMode(HSB, 255)
  val frontLightColor :: frontShadwColor :: Nil = 
    lightAndShadowColor(frontBaseColor)
  val backLightColor :: backShadowColor :: Nil = 
    lightAndShadowColor(backBaseColor) 

  val label = List(
    createLabel(text, w, h, size, frontBaseColor),
    createLabel(text, w, h, size, frontLightColor),
    createLabel(text, w, h, size, frontShadwColor)
  )
  val backColor = List(backBaseColor, backLightColor, backShadowColor)
  val upperLeftColor =  
    List(backLightColor) ::: lightAndShadowColor(backLightColor)
  val lowerRightColor =
    List(backShadowColor) ::: lightAndShadowColor(backShadowColor)

  val p = 5 // padding, frame weight

  g.colorMode(RGB, 255)
  g.endDraw

  val buffer = for(i <- 0 until 3) yield {
    val img = createImage(w, h, ARGB)

    // 色データのショートカット代入に使う変数
    var c = (0.0f, 0.0f, 0.0f)

    g.beginDraw
    g.smooth

    c = rgb(backColor(i))
    g.fill(c._1, c._2, c._3)
    
    //c = rgb(frameColor(i))
    c = rgb(frontBaseColor)
    g.stroke(c._1, c._2, c._3)
    g.rect(0, 0, w-1, h-1)
    g.noStroke

    c = rgb(upperLeftColor(i))
    g.fill(c._1, c._2, c._3)
    g.quad(1, 1, w-1, 1, w-p, p, p, p);
    g.quad(1, 1, p, p, p, h-p, 1, h-2);
  
    c = rgb(lowerRightColor(i))
    g.fill(c._1, c._2, c._3)
    g.quad(w-p, p, w-1, 1, w-1, h-1, w-p, h-p);
    g.quad(p, h-p, w-p, h-p, w-1, h-1, 1, h-1); 

    g.image(label(i), 0, 0)

    g.endDraw

    img.set(0, 0, g)
    img
  }
  g.dispose
  buffer.toList
}

//}}}
}

// vim: set fdm=marker:
