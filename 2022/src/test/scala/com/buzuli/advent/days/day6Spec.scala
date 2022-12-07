package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day6._

class day6Spec extends UnitSpec {
  "day6.CountingSet" when {
    "adding an item multiple times" should {
      "increase size only when new items are added" in {
        val s = new CountingSet[Int]
    
        s.add(1)
        s.size shouldBe 1
    
        s.add(1)
        s.size shouldBe 1
    
        s.add(2)
        s.size shouldBe 2
    
        s.add(2)
        s.size shouldBe 2
      }
      
      "increase count and historicalCount for every time the item is added" in {
        val s = new CountingSet[Int]
        
        s.add(1)
        s.count shouldBe 1
        s.historicalCount shouldBe 1
        
        s.add(1)
        s.count shouldBe 2
        s.historicalCount shouldBe 2
        
        s.add(2)
        s.count shouldBe 3
        s.historicalCount shouldBe 3
        
        s.add(2)
        s.count shouldBe 4
        s.historicalCount shouldBe 4
      }
    }
    
    "removing items" should {
      "decrease size only when an item is fully removed" in {
        val s = new CountingSet[Int]
        s.add(1)
        s.add(1)
        
        s.remove(1)
        s.size shouldBe 1
        
        s.remove(1)
        s.size shouldBe 0
        
        s.remove(1)
        s.size shouldBe 0
      }
      
      "decrease count for every removal until the item is fully removed" in {
        val s = new CountingSet[Int]
        s.add(1)
        s.add(1)
    
        s.remove(1)
        s.count shouldBe 1
        
        s.remove(1)
        s.count shouldBe 0
        
        s.remove(1)
        s.count shouldBe 0
      }
      
      "never decrease historicalCount" in {
        val s = new CountingSet[Int]
        s.add(1)
        s.add(1)
    
        s.remove(1)
        s.historicalCount shouldBe 2
        
        s.remove(1)
        s.historicalCount shouldBe 2
        
        s.remove(1)
        s.historicalCount shouldBe 2
      }
    }
  }
  
  "day6.p1.decode" when {
    "decoding a sequence" should {
      "correctly identify the end of the start-of-packet marker" in {
        p1.decode("mjqjpqmgbljsphdztnvjfqwrcgsmlb") shouldBe 7
        p1.decode("bvwbjplbgvbhsrlpgdmjqwftvncz") shouldBe 5
        p1.decode("nppdvjthqldpwncqszvftbrmjlhg") shouldBe 6
        p1.decode("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") shouldBe 10
        p1.decode("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") shouldBe 11
      }
    }
  }
  
  "day6.p2.decode" when {
    "decoding a sequence" should {
      "correctly identify the end of the start-of-packet marker" in {
        p2.decode("mjqjpqmgbljsphdztnvjfqwrcgsmlb") shouldBe 19
        p2.decode("bvwbjplbgvbhsrlpgdmjqwftvncz") shouldBe 23
        p2.decode("nppdvjthqldpwncqszvftbrmjlhg") shouldBe 23
        p2.decode("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") shouldBe 29
        p2.decode("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") shouldBe 26
      }
    }
  }
}
