package com.intenthq.challenge

case class Node(value: Int, edges: List[Node] = List.empty)

object SConnectedGraph {

  // Find if two nodes in a directed graph are connected.
  // Based on http://www.codewars.com/kata/53897d3187c26d42ac00040d
  // For example:
  // a -+-> b -> c -> e
  //    |
  //    +-> d
  // run(a, a) == true
  // run(a, b) == true
  // run(a, c) == true
  // run(b, d) == false

  // Notes:
  // * Does not work with cyclic graphs - it doesn't seem like its possible to generate a cyclic graph
  //   the way Node is defined.
  // * Solution is recursive - may cause memory issues
  def run(source: Node, target: Node): Boolean = {

    def loop() = {
      if (source == target) true
      else if (source.edges.isEmpty) false
      else source.edges.exists(node => run(node, target))
    }
    loop()
  }

}
