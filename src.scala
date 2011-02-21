package neet.game.puzzle.souko 

import processing.core._
import processing.core.PConstants._

import myutil._
import myutil.Applet._

object Main { 
//{{{

private var _initialize = false
def initialize = _initialize

def setup {
  size(400, 300, P2D)
  frameRate(24)
  frame.setTitle("倉庫番")
  scene = new OpeningScene
  _initialize = true
}

def main(args: Array[String]) {
  myutil.Applet.init(setup _)
}

//}}}
}

class DefaultImage(w:Int, h:Int) {
//{{{

def this(size:Int) = this(size, size)

val wbox = (w.toFloat / 6 * 5).toInt
val hbox = (h.toFloat / 6 * 5).toInt

import myutil.ImageFactory._
val Space:PImage = {
  //{{{
  val g = createGraphics(w, h, JAVA2D)
  val img = createImage(w, h, ARGB)

  g.beginDraw
  g.smooth
  //g.background(150, 75, 0)
  g.background(200, 120, 20)
  //g.background(117, 45, 0)
  g.stroke(69, 6, 0)
  g.noFill
  g.rect(0, 0, w-1, h-1)
  g.line(0, h>>1, w, h>>1)
  g.line(w>>1, 0, w>>1, h)
  g.endDraw

  img.set(0, 0, g)
  g.dispose
  img
  //}}}
}
val Wall:PImage = {
  //{{{
  createButton("", w, h, 1, 0, 0x333333)(0)
  //}}}
}
val Box:PImage = {
  //{{{
  val img = createImage(w, h, ARGB)
  val x = (w - wbox) >> 1
  val y = (h - hbox) >> 1

  img.set(0, 0, Space)
  img.set(x, y, createButton("", wbox, hbox, 1, 0, 0xFF4422)(0))
  img
  //}}}
}
val Goal:PImage = {
  //{{{
  val g = createGraphics(w, h, JAVA2D)
  val img = createImage(w, h, ARGB)

  g.beginDraw
  g.set(0, 0, Space)
  g.fill(255, 255, 64)
  g.stroke(100, 100, 0)
  g.ellipse(w>>1, h>>1, w>>2, h>>2)

  g.endDraw

  img.set(0, 0, g)
  g.dispose
  img
  //}}}
}
val GoalBox:PImage = {
  //{{{
  val img = createImage(w, h, ARGB)
  val x = (w - wbox) >> 1
  val y = (h - hbox) >> 1

  img.set(0, 0, Space)
  img.set(x, y, createButton("", wbox, hbox, 1, 0, 0xFFFF44)(0))
  img
  //}}}
}
val Player:List[PImage] = {
  //{{{
  List(
    (w>>1, 0, 0, h-1, w-1, h-1),
    (0, h>>1, w-1, h-1, w-1, 0),
    (w>>1, h-1, 0, 0, w-1, 0),
    (w-1, h>>1, 0, h-1, 0, 0)
  ) map { t =>
    val g = createGraphics(w, h, JAVA2D)
    val img = createImage(w, h, ARGB)
    g.beginDraw
    g.set(0, 0, Space)
    g.fill(255, 64, 64)
    g.stroke(64, 0, 0)
    g.triangle(t._1, t._2, t._3, t._4, t._5, t._6)

    g.endDraw

    img.set(0, 0, g)
    g.dispose
    img
  }
  //}}}
}
val PlayerKeyMap = Map(
  UP    -> Player(0),
  LEFT  -> Player(1),
  DOWN  -> Player(2),
  RIGHT -> Player(3)
)

val SoukoMap = Map(
  SoukoKind.Space   -> Space,
  SoukoKind.Wall    -> Wall,
  SoukoKind.Box     -> Box,
  SoukoKind.Goal    -> Goal,
  SoukoKind.GoalBox -> GoalBox,
  SoukoKind.Player  -> Player(0)
)

//}}}
}

object SoukoKind extends Enumeration {
  val Space, Wall, Box, Goal, GoalBox, Player = Value
}

trait SoukoKindChecker {
//{{{

import SoukoKind._

def isWall     (kind:Value):Boolean = (kind == Wall)
def isBox      (kind:Value):Boolean = (kind == Box)
def isGoal     (kind:Value):Boolean = (kind == Goal)
def isGoalBox  (kind:Value):Boolean = (kind == GoalBox)
def isPlayer   (kind:Value):Boolean = (kind == Player)

def isBoxes    (kind:Value):Boolean = (kind == Box || kind == GoalBox)
def isBoxOrGoal(kind:Value):Boolean = (kind == Box || kind == Goal)

def isMove(kind:Value):Boolean = {
  (kind != Wall &&
   kind != Box  &&
   kind != GoalBox)
}

//}}}
}

class Souko(xmax:Int, ymax:Int) extends SoukoKindChecker {
//{{{

import SoukoKind._

val array = new Array[Array[Value]](xmax)
for(i <- 0 until array.length) {
  array(i) = new Array[Value](ymax)
}

def isOutOfBounds(x:Int, y:Int) =
  (x < 0 || x >= array.length ||
   y < 0 || y >= array(x).length)

def apply(x:Int, y:Int):Value = {
  if(isOutOfBounds(x, y)) Wall
  else array(x)(y)
}

def update(x:Int, y:Int, kind:Value) {
  if(!isOutOfBounds(x, y)) array(x)(y) = kind
}

def foreach(f:(Value) => Unit) { 
  for(lx <- 0 until array.length;
      ly <- 0 until array(lx).length)
  {
    f(array(lx)(ly))
  }
}

def isEnd:Boolean = {
  foreach { kind => if(isBoxOrGoal(kind)) return false }
  true
}

def countKind = {
  val count = collection.mutable.Map(SoukoKindList.map(_ -> 0):_*)
  foreach { kind => count(kind) += 1 }
  count
}

//}}}
}

trait XMLInOut { 
//{{{

def mapXMLPath(name:String) = dataPath("map\\" + name + ".xml")

def saveXML(filename:String, 
            chart:Array[Array[SoukoKind.Value]],
            pos:Point)
{
  val data = for(lx <- 0 until chart.length) yield {
    val td = for(ly <- 0 until chart(lx).length) yield {
      <td>{chart(lx)(ly)}</td>
    }
    <tr>{td}</tr>
  }
  val table = 
<table>
<px>{pos.x}</px>
<py>{pos.y}</py>
{data}
</table>

  xml.XML.save(filename, table, "utf-8")
}

def loadXML(filename:String, 
            chart:Array[Array[SoukoKind.Value]],
            pos:Point)
{
  try {
    val table = xml.XML.loadFile(filename)
    var lx = 0
    (table \ "tr").foreach { tr =>
      var ly = 0
      (tr \ "td").foreach { td =>
        chart(lx)(ly) = SoukoKindMap(td.text)
        ly += 1
      }
      lx += 1
    }
    pos.x = (table \ "px").text.toInt
    pos.y = (table \ "py").text.toInt
  } catch {
    case _:java.io.FileNotFoundException => {
      for(lx <- 0 until chart.length; 
          ly <- 0 until chart(lx).length) 
      {
        chart(lx)(ly) = SoukoKind.Wall
      }
      val (cx, cy) = (chart.length/2, chart(0).length/2)
      chart(cx)(cx) = SoukoKind.Space
      pos.x = cx 
      pos.y = cy
    }
  }
}

//}}}
}

class OpeningScene extends Scene { 
//{{{

override def buttonClicked(name:String) = name match {
  case "Game" | "Create" => {
    scene = new SelectScene(name+"Scene")
  }
  case "Exit" => exit
}

//}}}
}

class SelectScene(val nextScene:String) extends Scene { 
//{{{

val MAX_STAGE = 5

override def buttonClicked(name:String) = name match {
  case "back" => scene = new OpeningScene
  case "random" => {
    val r = util.Random.nextInt(MAX_STAGE) + 1
    val stage = "stage" + (if(r<10) "0"+r else r)
    scene = newInstance[Scene](nextScene, stage)
  }

  case name => scene = newInstance[Scene](nextScene, name)
}

//}}}
}

class GameScene(stage:String, skin:String) extends Scene(skin)
                                           with XMLInOut  
                                           with SoukoKindChecker
{
//{{{

def this(stage:String) = this(stage, null)

val MAP_CHIP_SIZE = 32
val Image = new DefaultImage(MAP_CHIP_SIZE)

//==================
// Skinからのリフレクト代入定数
var MAP_X, MAP_Y, MAP_W, MAP_H = 0
var RESULT_X, RESULT_Y, RESULT_W, RESULT_H = 0

Skin.registerConstants 

//==================
// マップ
val MAP_MAX_X = (MAP_W / MAP_CHIP_SIZE)
val MAP_MAX_Y = (MAP_H / MAP_CHIP_SIZE)

//==================
// プレイヤー
object player {
  var pos = new Point(0, 0)
  var muki = PConstants.UP
}

//==================
// ゲームデータ
var isGameEnd = false
val souko = new Souko(MAP_MAX_X, MAP_MAX_Y)
loadXML(mapXMLPath(stage), souko.array, player.pos)

Skin.labels("stage") foreach { l => 
//  l.text = stage.replaceAll("stage", "ステージ")
  l.text = stage
  l.img = l.createDefaultImage
}

//==================
// 歩数
object walk {
  var ct = 0
  val label = Skin.labels("walkText")
  def update {
    label foreach { l =>
      l.text = toString
      l.img  = l.createDefaultImage
    }
  }
  override def toString = ct.toString
}
walk.update

//==================
// スコア
object score {
  val BONUS = 100
  var num   = BONUS
  var maxGoalBoxCt = souko.countKind(SoukoKind.GoalBox)
  val label = Skin.labels("scoreText")
  def update {
    label foreach { l =>
      l.text = toString
      l.img  = l.createDefaultImage
    }
  }
  def checkMaxGoalBoxCt(ct:Int) {
    if(maxGoalBoxCt < ct) {
      num += (ct - maxGoalBoxCt) * BONUS
      maxGoalBoxCt = ct
    }
  }
  override def toString = num.toString
}
score.update



override def buttonClicked(name:String) = name match {
  case "retry"  => scene = newInstance[Scene](getClass.getSimpleName, stage)
  case "giveup" => gameOverOrClear(false)
  case "back"   => backButtonClicked
}
def backButtonClicked = scene = new SelectScene(getClass.getSimpleName)

override def keyPressed {
  if(isGameEnd) return

  if(CrossKeyMap.contains(keyCode)) {
    player.muki = keyCode
    val add = CrossKeyMap(keyCode)
    val (x, y) = (player.pos.x + add.x, player.pos.y + add.y)
    val next  = souko(x, y)
    val next2 = souko(x + add.x, y + add.y)

    var bMove = false
    if(isBoxes(next)) {
      if(isMove(next2)) {
        bMove = true
        var bGoalBox = false

        souko(x, y) = 
          if(isGoalBox(next)) SoukoKind.Goal
          else SoukoKind.Space

        souko(x + add.x, y + add.y) =
          if(isGoal(next2)) { bGoalBox = true; SoukoKind.GoalBox }
          else SoukoKind.Box

        if(bGoalBox) {
          val ct = souko.countKind(SoukoKind.GoalBox)
          score.checkMaxGoalBoxCt(ct)
        }
      }
    } else if(isMove(next)) bMove = true

    if(bMove) { 
      player.pos.x = x
      player.pos.y = y

      walk.ct += 1
      walk.update

      score.num -= 1
      score.update
      
      if(souko.isEnd) gameOverOrClear(true)
    }
  }
}

def gameOverOrClear(bClear:Boolean) {
  isGameEnd = true
  Skin.buttons.clear
  Skin.addXML("GameEnd.xml")

  if(bClear) {
    score.num += (souko.countKind(SoukoKind.GoalBox) * score.BONUS)
    score.update
  }

  // TODO uieditorのほうで色設定できるようにする。それまでのつなぎ
  Map("result" -> Option(if(bClear) "ゲームクリアー" else "ギブアップ"),
      "score2" -> None,
      "scoreText2" -> Option(score.toString)) foreach 
  { 
    name =>
    Skin.labels(name._1) foreach { l =>
      val text = name._2.getOrElse(l.text) 
      l.img = ImageFactory.createLabel(text, l.w, l.h, l.size, 0xFFFFFF)
    }
  }
}

override def draw {
  background(255)

  // 倉庫オブジェクト表示
  for(lx <- 0 until MAP_MAX_X) {
    for(ly <- 0 until MAP_MAX_Y) {
      val x = MAP_X + (lx * MAP_CHIP_SIZE)
      val y = MAP_Y + (ly * MAP_CHIP_SIZE)

      image(Image.SoukoMap(souko(lx, ly)), x, y)
    }
  }

  // プレイヤー表示
  val x = MAP_X + (player.pos.x * MAP_CHIP_SIZE)
  val y = MAP_Y + (player.pos.y * MAP_CHIP_SIZE)
  image(Image.PlayerKeyMap(player.muki), x, y)

  // 枠表示
  noFill
  stroke(64)
  rect(MAP_X-1, MAP_Y-1, MAP_W+1, MAP_H+1)
  rect(MAP_X-2, MAP_Y-2, MAP_W+3, MAP_H+3)

  if(isGameEnd) {
    stroke(255)
    fill(0,0,0,200)
    rect(RESULT_X, RESULT_Y, RESULT_W-1, RESULT_H-1)
  }

  Skin.draw
}
  
//}}}
}

class TryGameScene(stage:String) extends GameScene(stage, "GameScene.xml") 
{
//{{{

Skin.buttons("giveup") foreach { btn =>
  btn.name = "back"
  btn.text = "戻る"
  btn.img = btn.createDefaultImage

  val retry = btn.clone("retry", "もう一度")
  retry.y -= 32
  Skin.buttons += retry
}

override def backButtonClicked = scene = new CreateScene(stage)

//}}}
}

class CreateScene(stage:String) extends Scene 
                                with XMLInOut  
                                with SoukoKindChecker
{
//{{{

// マップ座標
var MAP_X, MAP_Y, MAP_W, MAP_H = 0
val MAP_CHIP_SIZE = 30

val Image = new DefaultImage(MAP_CHIP_SIZE)

// マップ座標をxmlから読み込んで登録
Skin.registerConstants 

// 周囲の壁のために上下左右に1個分余分に確保
val MAP_MAX_X = (MAP_W / MAP_CHIP_SIZE) 
val MAP_MAX_Y = (MAP_H / MAP_CHIP_SIZE) 

var focusSkinElement:Option[Skin.Element] = Skin.buttons("space")
var focusSoukoKind = SoukoKind.Space

val playerPos = new Point(0, 0)
val souko = new Souko(MAP_MAX_X, MAP_MAX_Y)
loadXML(mapXMLPath(stage), souko.array, playerPos)

Skin.labels("stage") foreach { l => 
//  l.text = stage.replaceAll("stage", "ステージ")
  l.text = stage
  l.img = l.createDefaultImage
}

object Message {
  val label = Skin.labels("message")
  def text(str:String, color:Int, size:Int = -1) {
    label foreach { l =>
      val s = if(size <= 0) l.size else size 
      l.img = ImageFactory.createLabel(str, l.w, l.h, s, color)
    }
  }
  def empty = text("", 0xFFFFFF)

  // TODO: スレッド作ってある程度に時間がたつとemptyにする
  def echo (str:String) = text(str, 0x333333)
  def error(str:String) = text(str, 0xFF0000)
}
Message.empty

override def buttonClicked(name:String) = name match {
  case "space" | "wall" | "box" | "goal" | "goalBox" | "player" => {
    focusSkinElement = Skin.buttons(name)
    focusSoukoKind = SoukoKindMap(name)
  }
  case "save" => if(isEnablePlay()) { 
    saveXML(mapXMLPath(stage), souko.array, playerPos)
  }
  case "play" => if(isEnablePlay()) {
    buttonClicked("save")
    scene = new TryGameScene(stage)
  }
  case "back" => scene = new SelectScene(getClass.getSimpleName)
  case "clear" => {
    loadXML("empty", souko.array, playerPos)
    Message.echo("保存はしていないので「戻る」を押せば" + 
                 "前回の状態に戻ります。")
  }
}

def isEnablePlay(bEcho:Boolean=true):Boolean = {
  val count = souko.countKind
  var (box, goal) = (count(SoukoKind.Box), count(SoukoKind.Goal))
  if(box == 0 || goal == 0) {
    if(bEcho) Message.error("箱とゴールは最低限一つは置いてください")
    false
  } else if(box != goal) {
    if(bEcho) Message.error("箱とゴールの個数を同じにしてください")
    false
  } else true
}

override def draw {
  background(255)

  // 倉庫オブジェクト表示
  for(lx <- 0 until MAP_MAX_X) {
    for(ly <- 0 until MAP_MAX_Y) {
      val x = MAP_X + (lx * MAP_CHIP_SIZE)
      val y = MAP_Y + (ly * MAP_CHIP_SIZE)

      image(Image.SoukoMap(souko(lx, ly)), x, y)
    }
  }

  // プレイヤー表示
  val x = MAP_X + (playerPos.x * MAP_CHIP_SIZE)
  val y = MAP_Y + (playerPos.y * MAP_CHIP_SIZE)
  image(Image.SoukoMap(SoukoKind.Player), x, y)

  // 枠表示
  noFill
  stroke(64)
  rect(MAP_X-1, MAP_Y-1, MAP_W+1, MAP_H+1)
  rect(MAP_X-2, MAP_Y-2, MAP_W+3, MAP_H+3)
 
  // フォーカス中のボタンの枠表示
  focusSkinElement foreach { f =>
    noFill
    stroke(255, 0, 0)
    rect(f.x-1, f.y-1, f.w+1, f.h+1)
    rect(f.x-2, f.y-2, f.w+3, f.h+3)
  }

  // マウスを押していたらコンポネーションを配置
  if(mousePushed) {
    val y = {
      val ydiff = (mouseY - MAP_Y)
      if(ydiff < 0) -1 else ydiff / MAP_CHIP_SIZE
    }
    val x = {
      val xdiff = (mouseX - MAP_X)
      if(xdiff < 0) -1 else xdiff / MAP_CHIP_SIZE
    }

    def isSamePlayerPos(x:Int, y:Int):Boolean = {
      (playerPos.x == x && playerPos.y == y)
    }

    if(x >= 0 && x < MAP_MAX_X && y >= 0 && y < MAP_MAX_Y) {
      val now = souko(x, y)

      if(now != focusSoukoKind) {
        if(isPlayer(focusSoukoKind)) {
          if(isMove(now)) {
            playerPos.x = x
            playerPos.y = y
          }
        } else {
          if(isSamePlayerPos(x, y)) {
            if(isMove(focusSoukoKind)) {
              souko(x, y) = focusSoukoKind
            }
          } else {
            souko(x, y) = focusSoukoKind
          }
        }
      }
    }
  }
  Skin.draw
}

//}}}
}


// vim: set fdm=marker:
