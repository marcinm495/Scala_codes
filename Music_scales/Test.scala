
import java.io._
import java.util.logging.Logger

class Scales(val firstNote: String, val scaleType: String) {
  
   val notes = Array("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "H")
   
   def constructScale(scale_first_note: String, intervals: Array[Int]) : Array[String] = {
      val first_note_pos : Int = notes.indexOf(scale_first_note)
      var out_scale = new Array[String](intervals.length+1)
      var s : Int = 0
      var m : Int = 0
      out_scale(0)=notes(first_note_pos)
      for(k <- intervals){
        m=m+1
        s=s+k
        out_scale(m) = notes((first_note_pos+s) % 12)
      }
      out_scale
   }
   
   def displayScale() : Unit = {
      val first_note_pos : Int = notes.indexOf(firstNote)
      if(scaleType == "dur"){
        var disp_scale = constructScale(firstNote, Array(2,2,1,2,2,2,1))
        println("scale " + firstNote + " dur:" + disp_scale.mkString(" "))
      } else if(scaleType == "moll"){
        var disp_scale = constructScale(firstNote, Array(2,1,2,2,1,2,2))
        println("scale " + firstNote + " moll:" + disp_scale.mkString(" "))
      } else{
        println("Improper scale type")
      }
   }
   
   def returnScale() : Array[String] = {
      var disp_scale = new Array[String](8)
      val first_note_pos : Int = notes.indexOf(firstNote)
      if(scaleType == "dur"){
        disp_scale = constructScale(firstNote, Array(2,2,1,2,2,2,1))
      } else if(scaleType == "moll"){
        disp_scale = constructScale(firstNote, Array(2,1,2,2,1,2,2))
      } else{
        disp_scale = Array("Improper scale type")
      }
      disp_scale 
   }
   
   def displayTriad() : Unit = {
      val first_note_pos : Int = notes.indexOf(firstNote)
      if(scaleType == "dur"){
        var disp_scale = constructScale(firstNote, Array(4,3))
        println("chord " + firstNote + " dur:" + disp_scale.mkString(" "))
      } else if(scaleType == "moll"){
        var disp_scale = constructScale(firstNote, Array(3,4))
        println("chord " + firstNote + " moll:" + disp_scale.mkString(" "))
      } else{
        println("Improper scale type")
      }
   }
   
}


object Test{
    def main(args: Array[String]) {
          val new_scale = new Scales("F", "dur");
          println(new_scale.notes(0))
          var z = new Array[String](8)
          z = new_scale.returnScale()
          println(z.mkString(" "))
          new_scale.displayTriad()
    }
}
