package com.logesh.feelers

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class DesicionTree{

  var tree:ListBuffer[Any] = ListBuffer[Any]();

  def gini_index(groups:Array[Array[Array[Double]]],class_values:Array[Double]):Double = {
    var gini = 0.0;
    for(value <- class_values){
      for(group <- groups){
        println(group.mkString(" "));
        var size = group.length;
        var proportion = group.filter((x)=>{x(2) == value}).length.toDouble / size.toDouble;
        gini += (proportion * (1 - proportion))
      }
    }

    return gini;
  }

  def test_split(index:Int, value:Double, dataset:Array[Array[Double]]):(Array[Array[Array[Double]]]) = {
    var right,left = new ArrayBuffer[Array[Double]]();
    for( row <- dataset) {
      if(row(index)<value) left+=row else right+=row
    }

    return Array(right.toArray,left.toArray);
  }

  def get_split(dataset:Array[Array[Double]]): Map[String,Any] ={
    val classValues = dataset.map(_(2)).distinct;
    var (bIndex,bValue:Double,bScore:Double,bGroups) = (999,999.00,999.00,Array[Array[Array[Double]]]());
    for(index <- 0 until (dataset(0).length-1)){
      for(row <- dataset){
        val groups = test_split(index,row(index),dataset);
        var gini = gini_index(groups,classValues);
        if(gini < bScore){
          bIndex = index
          bValue = row(index)
          bScore = gini
          bGroups = groups
        }

      }
    }
    val bestSplit = Map(("index",bIndex),("value",bValue),("score",bScore),("groups",bGroups),("isTerminal",false));
    tree.append(bestSplit);
    bestSplit;
  }

  def to_terminal(group:Array[Array[Double]]):Double = {
    val outcomes = group.map(_(2));
    return outcomes.groupBy(x=>x).map(x=> (x._2,x._2.length)).maxBy(x=>x._2)._1(0)
  }

  def split(node: Map[String,Any],max_depth:Int,min_size:Int,depth:Int):Unit = {
    val groups = node("groups").asInstanceOf[Array[Array[Array[Double]]]];
    val (right,left) = {
      if(groups.length == 2)
        (groups(0),groups(1))
      else
        return;
    }

    var  rightNode,leftNode = 0.0;
    (right,left,depth) match {

      case (x, y, z) if (x.length == 0 | y.length == 0) => {
        val terminalValue = to_terminal(x ++ y);
        leftNode = terminalValue;
        rightNode = terminalValue;
        tree.append(Map(("isTerminal", true), ("right", Some(rightNode)), ("left", Some(leftNode))));
      }

      case (x, y, z) if (z >= max_depth) => {
        leftNode = to_terminal(y);
        rightNode = to_terminal(x);
        tree.append(Map(("isTerminal", true), ("right", Some(rightNode)), ("left", Some(leftNode))));
      }

      case (right,left,depth) => {

        if(right.length<=min_size){
          rightNode = to_terminal(right);
          tree.append(Map(("isTerminal",true),("right",Some(rightNode)),("left",None)));
        }else{
          split(get_split(right),max_depth,min_size,depth+1)
        }

        if(left.length<min_size){
          leftNode = to_terminal(left);
          tree.append(Map(("isTerminal",true),("right",None),("left",Some(leftNode))));
        }else{
          leftNode = to_terminal(left);
          split(get_split(left),max_depth,min_size,depth+1);
        }

      }

    }



  }

}

object DecisionTree{

  def apply(dataset:Array[Array[Double]],max_depth:Int,min_size:Int):ListBuffer[Any] = {

    val decisionTree = new DesicionTree();
    val bestSplit = decisionTree.get_split(dataset);
    decisionTree.split(bestSplit,max_depth,min_size,1);
    return  decisionTree.tree;

  }

}