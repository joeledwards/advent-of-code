package com.buzuli.advent.days

import com.buzuli.UnitSpec
import com.buzuli.advent.days.day3._

class day3Spec extends UnitSpec {
  "day3.Item" when {
    "priority is constructed" should {
      "contain the correct priority value" in {
        Item('a').priority shouldBe 1
        Item('b').priority shouldBe 2
        
        Item('y').priority shouldBe 25
        Item('z').priority shouldBe 26
        
        Item('A').priority shouldBe 27
        Item('B').priority shouldBe 28
        
        Item('Y').priority shouldBe 51
        Item('Z').priority shouldBe 52
      }
    }
  }
  
  "day3.RuckSack" when {
    "Item is constructed" should {
      "be okay with no overlapping item types" in {
        RuckSack(Set('a'), Set('b'))
      }
      
      "be okay with 1 overlapping item type" in {
        RuckSack(Set('a', 'b'), Set('b', 'c'))
      }
      
      "throw an Exception if there are 2 or more overlapping item types" in {
        intercept[AssertionError] { RuckSack(Set('a', 'b'), Set('a', 'b')) }
        intercept[AssertionError] { RuckSack(Set('a', 'b', 'c'), Set('b', 'c', 'd')) }
      }
    }
    
    "commonElement is constructed" should {
      "contain the common element from compartments a and b" in {
        RuckSack(Set('a'), Set('b')).commonElement shouldBe None
        RuckSack(Set('a'), Set('a')).commonElement shouldBe Some(Item('a'))
        RuckSack(Set('a', 'a', 'b', 'c'), Set('c', 'd', 'e', 'e')).commonElement shouldBe Some(Item('c'))
      }
    }
    
    "when constructed via fromString" should {
      "should partition items into a RuckSack for any string with an even, non-zero length, with no more than one overlapping item type" in {
        RuckSack.fromString("ab") shouldBe Some(RuckSack(Set('a'), Set('b')))
        RuckSack.fromString("aa") shouldBe Some(RuckSack(Set('a'), Set('a')))
        RuckSack.fromString("aabb") shouldBe Some(RuckSack(Set('a'), Set('b')))
      }
      
      "should reject any zero length string" in {
        RuckSack.fromString("") shouldBe None
      }
      
      "should reject any odd length string" in {
        RuckSack.fromString("a") shouldBe None
        RuckSack.fromString("abc") shouldBe None
        RuckSack.fromString("abcad") shouldBe None
      }
      
      "should reject any sting with more than 1 overlapping item types" in {
        intercept[AssertionError] { RuckSack.fromString("abab") }
      }
    }
  }
}
