package com.tw.learning

import java.util.Date

import org.scalatest.FunSuite

class LearningSuite extends FunSuite {

  test("类型推断") {
    val intValue = 1;
    val stringValue = "This is String";

    assert(intValue.getClass.getSimpleName.equals("int"));
    assert(stringValue.getClass.getSimpleName.equals("String"));
  }

  test("隐式转换，扩展既有代码的功能") {

    val originString = "body"

    //简易写法
    implicit def wrpper2(str: String) = {
      new {
        def addPrefixAndSuffix2 = "--- " + str + " ---"
      }
    }

    assert("--- body ---".equals(originString.addPrefixAndSuffix2));

    //定义扩展实现 另外一种实现
    class StringExtension(str: String) {
      def addPrefixAndSuffix(extensionPara: String) = {
        extensionPara + " " + str + " " + extensionPara
      }
    }

    //连接实现（StringExtension）和要扩展的类(String)
    implicit def wrapper(str: String) = new StringExtension(str)

    assert("*** body ***".equals(originString.addPrefixAndSuffix("***")));

  }

  test("高阶函数") {
    //定义函数变量
    val add = (x: Int, y: Int) => x + y
    val subtract = (x: Int, y: Int) => x - y

    //定义可以接收函数并返回结果也是函数的函数(绕口令？)
    def func(x: Int, y: Int, f: (Int, Int) => Int) = (z: Int) => f(x, y) * z


    val originX = 10;
    val originY = 1;

    val expectAddedFunc = func(originX, originY, add)
    val expectSubtractedFunc = func(originX, originY, subtract)

    assert(expectAddedFunc(2) == 22)
    assert(expectSubtractedFunc(2) == 18)

  }

  test("闭包") {

    //闭包是在其词法上下文中引用了自由变量的函数
    var i:Int = 0;
    def fun() = {
      i+=1
      i
    }

    assert(fun() == 1)
    assert(fun() == 2)

  }

  test("局部函数") {
    val func = (x: Int, y: Int, z: Int) => x + y + z
    //为func固定其中y 和 z的的参数值
    val partialFunc = func(_: Int, 2, 5)
    assert(partialFunc(10) == 17)

  }

  test("柯里化函数") {
    //定义
    //把接受多个参数的函数变换成几个接受一个单一参数（最初函数的第一个参数）的函数，
    //并且返回接受余下的参数而且返回结果的新函数的技术

    //对一个多参数函数进行柯里化
    val multiParameterFunc = (x: Int, y: Int, z: Int) => x + y + z
    val curryingFuncFromExistFunc = multiParameterFunc curried
    var actualValue = curryingFuncFromExistFunc(2)(5)(17)
    assert(actualValue == 24)

    val yy = curryingFuncFromExistFunc(1)(2)

    assert(yy(3)==6)

    //柯里化函数也可以这么写
    val curryingFunc = (x: Int) => (y: Int) => x + y
    actualValue = curryingFunc(3)(5)

    val xx = curryingFunc(6)

    assert(xx(8)==14)

    assert(actualValue == 8)

    //既然能被柯里化当然也可以实现部分函数
    val partialFun = curryingFunc(_: Int)(2)
    assert(partialFun(10) == 12)

  }

  test("模式匹配") {

    abstract class BaseClass
    case class SubClass1(intValue: Int) extends BaseClass
    case class SubClass2(stringValue: String) extends BaseClass
    case class SubClass3(dateValue: Date) extends BaseClass


    def func(param: BaseClass) = {
      param match {
        case SubClass1(n) => {
          n + "是一个int"
        }
        case SubClass2(s) => {
          s + "是一个string"
        }
        case _ => {
          "不是int也不是string"
        }
      }
    }

    assert(func(SubClass1(2)).equals("2是一个int"))
    assert(func(SubClass2("XXX")).equals("XXX是一个string"))
    assert(func(SubClass3(new Date())).equals("不是int也不是string"))
  }

  test("偏函数，注意这个不是局部函数") {
    //偏函数是只能接收满足指定条件参数的函数

    val oneOrSix: PartialFunction[Int, String] = {
      case 1 => "one"
      case 6 => "six"
    }
    val two: PartialFunction[Int, String] = {
      case 2 => "two"
    }
    val three: PartialFunction[Int, String] = {
      case 3 => "three"
    }
    val other: PartialFunction[Int, String] = {
      case _ => "other"
    }

    assert(oneOrSix(1).equals("one"))
    assert(oneOrSix(6).equals("six"))
    assert(two(2).equals("two"))

    //还可以将多个偏函数进行组合 其实这么看跟局部函数也有像的地方
    val unionOneAndTwoAndOther = oneOrSix orElse two orElse other

    assert(unionOneAndTwoAndOther(5).equals("other"))
    assert(unionOneAndTwoAndOther(6).equals("six"))
    assert(unionOneAndTwoAndOther(2).equals("two"))
  }

  test("Scala的特性 trait") {

    class Obj {
      def log(str: String) = "Obj logger output " + str

      def echo(str: String) = "Obj logger output " + str
    }

    //定义特性便于覆写原始实现
    trait SimpleLogger extends Obj {
      override def log(str: String) = "Simple Logger output " + str
    }

    trait OtherLogger extends Obj {
      override def log(str: String) = "Other Logger output " + str
    }

    trait SimpleEcho extends Obj {
      override def echo(str: String) = "Simple Echo output " + str
    }

    trait OtherEcho extends Obj {
      override def echo(str: String) = "Other Echo output " + str
    }

    //多重继承(Mixin)
    class SimpleObj extends Obj with SimpleLogger with OtherEcho with SimpleEcho

    val simpleObj = new SimpleObj
    assert(simpleObj echo "OK" equals "Simple Echo output OK")
    assert(simpleObj log "OK" equals "Simple Logger output OK")

    val otherObj = new Obj with SimpleEcho
    assert(otherObj.echo("OK").equals("Simple Echo output OK"))



    //不可实例化的对象
    class Db private(val conn: String) {
      def connString: String = conn;

      def connect() = {
        "Connect to special Db : " + conn
      }
    }
    //伴生对象 充当单例工厂
    object Db {
      def apply(conn: String) = new Db(conn)
    }

    assert(Db("mysql").connect().equals("Connect to special Db : mysql"))

  }

  test("Scala集合 元组tuple , lambda") {

    def getTuple(str:String,int:Int,date:Date) = ("-- "+str+" --",int*2,date)

    val tuple = getTuple("body",2,new Date)

    assert((tuple _1).equals("-- body --"))
    assert((tuple _2).equals(4))


    val range = Range(1,10) //Range(1,2,3,4,5,6,7,8,9)

    // 在1到9中找到大于2的元素并将每项值*2后取下标为2的值 （绕口令too）
    println(range.filter(_>2).map(_*2).toList(2))

  }
}