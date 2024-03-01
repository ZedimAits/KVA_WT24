package ScalaGo

object SimpleList extends App:

  /**
   * Implements a basic linked list.
   *
   * @param entry entry of the list element, 'None' if not provided
   * @param next  next element of the list, 'null' if not provided
   * @tparam A type of the list's entries
   */
  class SimpleList[A](var entry: Option[A] = None, var next: SimpleList[A] = null)

  /**
   * Append an item to the SimpleList
   *
   * @param list the list
   * @param item append this item
   */
  def slAppend[A](list: SimpleList[A], item: A): Unit =
    if list == null then return
    var p = list
    while p.next != null do p = p.next
    p.next = new SimpleList[A](Some(item))

  /**
   * Determines the length of the SimpleList.
   *
   * @param list the list
   * @return the length
   */
  def slLength[A](list: SimpleList[A]): Int =
    if(list == null) -1
    else slLength(list.next) + 1

  /**
   * Tests if the SimpleList contains a given element.
   *
   * Tests for equality are done via '=='.
   *
   * @param list the list
   * @param item search for this item
   * @return true if the SimpleList contains the item, false if not
   */
  def slContains[A](list: SimpleList[A], item: A): Boolean =
    if(list == null) false
    else if(list.entry.contains(item)) true
    else slContains(list.next,item)
  
  //print SimpleList
  def printL[A](list: SimpleList[A]): Unit =
    if(list == null || list.next == null) println("Empty")
    var pointer = list.next
    while pointer != null do
      if pointer.entry.isDefined then println(pointer.entry.get)
      pointer = pointer.next

  //get element by index
  def slGet[A](list: SimpleList[A])(index: Int): A =
    if list == null then throw Exception("List is Null")
    var i = 0
    var p = list.next
    while i != index do
      i += 1
      try
        p = p.next
      catch
        case e => throw Exception("Index out of Bound")
    if(p == null || p.entry.isEmpty) throw Exception("Element is Empty or Null")
    else return p.entry.get

  //delete element by index
  def slDel[A](list: SimpleList[A], index: Int): Unit =
    var p = list
    for i <- 0 until index do p = p.next
    p.next = p.next.next

  //delete element by address
  def slDel[A](list: SimpleList[A], el: SimpleList[A]): Unit =
    var p = list
    while p != null do
      if(p.next == el)
        p.next = el.next
        return;
      p = p.next
      
  //loop over all elements and apply passed function
  def loopFunc[A](list: SimpleList[A], f: (x: A)=>Unit): Unit =
    var p = list.next
    while p != null do
      if p.entry.isDefined  then f(p.entry.get)
      p = p.next





