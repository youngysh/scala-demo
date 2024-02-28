package org.example

import scala.collection.mutable
import scala.collection.mutable.ListBuffer



object ScalaDemo {
  def countDate(paramNum: Int): Unit = {

    //源数据
//    val dataList = List(
//        ("ABC17969(AB)", "1", "ABC17969", 2022),
//        ("ABC17969(AB)", "2", "CDC52533", 2022),
//        ("ABC17969(AB)", "3", "DEC59161", 2023),
//        ("ABC17969(AB)", "4", "F43874", 2022),
//        ("ABC17969(AB)", "5", "MY06154", 2021),
//        ("ABC17969(AB)", "6", "MY4387", 2022),
//        ("AE686(AE)", "7", "AE686", 2023),
//        ("AE686(AE)", "8", "BH2740", 2021),
//        ("AE686(AE)", "9", "EG999", 2021),
//        ("AE686(AE)", "10", "AE0908", 2021),
//        ("AE686(AE)", "11", "QA402", 2022),
//        ("AE686(AE)", "12", "OM691", 2022)
//      )

    //源数据
    val dataList = List(
      ("AE686(AE)", "7", "AE686", 2022),
      ("AE686(AE)", "8", "BH2740", 2021),
      ("AE686(AE)", "9", "EG999", 2021),
      ("AE686(AE)", "10", "AE0908", 2023),
      ("AE686(AE)", "11", "QA402", 2022),
      ("AE686(AE)", "12", "OA691", 2022),
      ("AE686(AE)", "12", "OB691", 2022),
      ("AE686(AE)", "12", "OC691", 2019),
      ("AE686(AE)", "12", "OD691", 2017)
    )


    //1、需求：For each peer_id, get the year when peer_id contains id_2, for example for ‘ABC17969(AB)’ year is 2022

    //1-1、存储 peer_id 和 peer_id包含id_2 的 year
    val peerYearMap:mutable.Map[String, Int] = mutable.Map[String, Int]()
    //1-2、遍历源数据，将合适的 peer_id 和 year 保存
    for(elem <- dataList){
      if(elem._1.contains(elem._3)) {
        peerYearMap.put(elem._1, elem._4)
      }
    }

    //2、需求：Given a size number, for example 3. For each peer_id count the number of each year (which is smaller or equal than the year in step1).

    // 2-1、对源数据整合，将peerId和year合并为key组装map，方便进行求和该年份的记录数，生成新的list
    val dataPeerIdYearMap:Map[String,List[(String,String,String,Int)]] = dataList.groupBy(element=>element._1+"-"+element._4)
    val dataPeerIdYearCount = dataPeerIdYearMap.mapValues({y=>y.length})
    val dataPeerIdYearCountList = dataPeerIdYearCount.toList

    // 2-2、对list遍历，过滤掉year大于 第一步中获取的 peerId 对应的 year
    val dataPeerIdYearCountListFilter = dataPeerIdYearCountList.filter( element => {
      val array = element._1.split("-")
      val year : Option[Int] = peerYearMap.get(array(0))
      array(1).toInt <= year.getOrElse(0)
    })



    //3.	Order the value in step 2 by year and check if the count number of the first year is bigger or equal than the given size number. If yes, just return the year.
    //If not, plus the count number from the biggest year to next year until the count number is bigger or equal than the given number.

    //3-1、对peerId 与 year 为组合key 的list 排序，以年份降序排序
    val dataPeerIdYearCountListFilterSort = dataPeerIdYearCountListFilter.sortBy(_._1)(Ordering[String].reverse)

    //3-2、对排序后的list进行过滤，找出需要的年份记录，先设计参数
    // paramNum -- 传入的参数；listCountKey 和 listCountNumber 是保存遍历过程中的状态
    val paramNumber = paramNum
    var listCountKey:String = ""
    var listCountNumber:Int = 0
    val resultList:mutable.ListBuffer[(String,Int)] = new ListBuffer()

    //3-3、对结果进行遍历，取出每个peerId对应的年份记录，并保存到最终结果 resultList 中
    dataPeerIdYearCountListFilterSort.foreach(element => {
      val array = element._1.split("-")
      val elementKey = array(0)
      val year:Int = array(1).toInt
      //初次进入时，直接将第一条记录放入result，并设置当前状态的 key 和 该key对应的计算总数
      if(listCountKey.isEmpty){
        listCountKey = elementKey
        listCountNumber = element._2
        resultList.append((elementKey, year))
        //非首次进入，当key与上一次循环的一样，此时不需修改状态 key，但是要求和 listCountNumber 并判断是否达到传入的参数大小
      }else if(listCountKey.equalsIgnoreCase(elementKey)){
        //当总计数未达到 传入的参数大小，将添加到最终结果并计数 listCountNumber
        if (listCountNumber < paramNumber){
          resultList.append((elementKey, year))
          listCountNumber += element._2
        }
      //非首次进入，当key与上一次循环的不一样，此时需要重置状态 key 和 listCountNumber，并将新的key对应的记录加入最终结果
      }else{
        listCountKey = elementKey
        listCountNumber = element._2
        resultList.append((elementKey, year))
      }
    })


//    println(peerYearMap)
//    println("================")
//    println(dataList)
//    println("=======dataPeerIdYearMap=========")
//    println(dataPeerIdYearMap)
//    println("=======dataPeerIdYearCount=========")
//    println(dataPeerIdYearCount)
//    println("=======dataPeerIdYearCountList=========")
//    println(dataPeerIdYearCountList)
//    println("========dataPeerIdYearCountListFilter========")
//    println(dataPeerIdYearCountListFilter)
//    println("========dataPeerIdYearCountListFilterSort========")
//    println(dataPeerIdYearCountListFilterSort)
//    println("========resultList========")
      resultList.foreach(println)








  }

}
