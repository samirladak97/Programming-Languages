object show extends App{
  println(show("jane"::"mary"::"bob"::Nil))
  def show(list: List[Any]): String = {
    //if the list is empty just return Nil
    if (list == Nil){
      "Nil"
    }
    //if there is only one element in the list return "element::Nil"
    else if (list.size==1) {
      val t = list.map(_.toString)
      t.head + "::Nil"
    }
    //use recursion for all other cases
    else{
      val t = list.map(_.toString)
      t.head + "::(" + show(list.tail) + ")"
    }
  }
}
